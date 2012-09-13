//
//  AISocketManager.m
//  TravelBot
//
//  Created by Asim Ihsan on 25/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//
//  References:
//  - https://github.com/robbiehanson/CocoaAsyncSocket/wiki/Intro_GCDAsyncSocket

#import "AISocketManager.h"
#import "AIUtilities.h"
#import "JSONKit/JSONKit.h"
#import "ConciseKit/ConciseKit.h"

#import "CocoaAsyncSocket/GCDAsyncSocket.h"
#include <netinet/tcp.h>
#include <netinet/in.h>

#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;

// Header is fixed size at 4-bytes. It is a big-endian unsigned integer that
// specifies the size of the payload that immediately follows in bytes.
static const int HEADER_SIZE = 4;

// Tags are used for identifying the asynchronous callbacks of reads and
// writes.

// General header/body responses for reads.
static const long TAG_FIXED_LENGTH_HEADER_READ       = 1;
static const long TAG_RESPONSE_BODY_READ             = 2;

// General header/body tags for write.
static const long TAG_FIXED_LENGTH_HEADER_WRITE      = 3;
static const long TAG_RESPONSE_BODY_WRITE            = 4;

// Control messages. There are ASCII strings.
static const long TAG_PING_HEADER                    = 5;
static const long TAG_PING_PAYLOAD                   = 6;
static const long TAG_PONG_HEADER                    = 7;
static const long TAG_PONG_PAYLOAD                   = 8;
static const long TAG_CLOSE_HEADER                   = 9;
static const long TAG_CLOSE_PAYLOAD                  = 10;

// ----------------------------------------------------------------------------

#pragma mark - Private methods and constants.
@interface AISocketManager ()

@property (nonatomic, assign) dispatch_queue_t socketQueue;
@property (nonatomic, assign) dispatch_queue_t processingQueue;
@property (nonatomic, assign) BOOL isAttemptingConnection;
@property (nonatomic, assign) UIBackgroundTaskIdentifier processingTask;
@property (nonatomic, retain) UIApplication *application;

- (void)startProcessingTask;
- (void)stopProcessingTask;

- (void)initSocketManager;
- (void)initSocket;
- (void)startConnectToHost:(NSString *)host port:(uint16_t)port;
- (void)startSecure;

- (void)writeString:(NSString *)string;
- (void)writeString:(NSString *)string
            timeout:(NSTimeInterval)timeout
         header_tag:(long)header_tag
        payload_tag:(long)payload_tag;
- (void)writeData:(NSData *)data
      withTimeout:(NSTimeInterval)timeout
       header_tag:(long)header_tag
      payload_tag:(long)payload_tag;
- (void)readHeader;

- (int)parseHeader:(NSData *)data;
- (void)handleResponseBody:(NSData *)data;
- (BOOL)maybeHandleControlResponse:(NSString *)response;

- (void)doHeartbeat;
- (void)notification:(NSNotification *)notification;

@end

@implementation AISocketManager

@synthesize socket = _socket,
            socketQueue = _socketQueue,
            processingQueue = _processingQueue,
            isConnected = _isConnected,
            isAttemptingConnection = _isAttemptingConnection;

static AISocketManager *sharedInstance = nil;

#pragma mark - Public API.

- (void)connect
{
    DDLogVerbose(@"AISocketManager:connect() entry.");
    if (self.isConnected)
    {
        DDLogVerbose(@"Already connected.");
        return;
    }
    if (self.isAttemptingConnection)
    {
        DDLogVerbose(@"Already attempting connection.");
        return;
    }
    
    [self startConnectToHost:@"travelbot.asimihsan.com" port:8080];
    //[self startConnectToHost:@"192.168.1.99" port:8080];
    //[self startConnectToHost:@"127.0.0.1" port:8080];
    DDLogVerbose(@"AISocketManager:connect() exit.");
}

// -----------------------------------------------------------------------------
//  We're often disconnecting as the application enters the background so
//  put this onto the processing GCD queue as a background task.
// -----------------------------------------------------------------------------
- (void)disconnect
{
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
    ^{
        DDLogVerbose(@"AISocketManager:disconnect() entry.");
        if (!(self.isConnected))
        {
            DDLogVerbose(@"Already disconnected.");
            return;
        }
        if (self.isAttemptingConnection)
        {
            DDLogError(@"Attempting to connect during disconnect() call.");
            return;
        }
        static NSString *close = @"close";
        [self writeString:close
                  timeout:-1
               header_tag:TAG_CLOSE_HEADER
              payload_tag:TAG_CLOSE_PAYLOAD];
        [self.socket disconnectAfterWriting];
        self.isConnected = NO;
        DDLogVerbose(@"AISocketManager:disconnect() exit.");
        [self stopProcessingTask];
    });
}

- (NSString *)writeDictionary:(NSDictionary *)dictionary
{
    NSMutableDictionary *request = [[NSMutableDictionary alloc] initWithDictionary:dictionary];
    NSString *uuid = [[NSProcessInfo processInfo] globallyUniqueString];
    [request $obj:uuid for:@"tag"];
    DDLogVerbose(@"AISocketManager:writeDictionary entry. request: %@", request);
    NSString *request_string = [request JSONString];
    [self writeString:request_string];
    
    return uuid;
}

#pragma mark - Private methods.
- (void)writeString:(NSString *)string
{
    [self writeString:string
              timeout:-1
           header_tag:TAG_FIXED_LENGTH_HEADER_WRITE
          payload_tag:TAG_RESPONSE_BODY_WRITE];
}

- (void)startProcessingTask
{
    self.processingTask = [self.application beginBackgroundTaskWithExpirationHandler:^{
        [self stopProcessingTask];
    }];
}

- (void)stopProcessingTask
{
    [self.application endBackgroundTask:self.processingTask];
    self.processingTask = UIBackgroundTaskInvalid;
}

- (void)initSocketManager
{
    DDLogVerbose(@"AISocketManager:initSocketManager() entry.");
    
    if (self.isConnected)
    {
        DDLogVerbose(@"isConnected, so disconnect.");
        [self disconnect];
    }
    
    self.processingQueue = dispatch_queue_create("com.ai.AISocketManager.processingQueue", NULL);
    
    // Setup socket's GCD queue and the socket. Send an initial ping.
    [self initSocket];
}

- (void)initListener
{
    // Listen for 'did become active' of application
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(notification:)
                                                 name: UIApplicationDidBecomeActiveNotification
                                               object: nil];
    
    // Listen for application enters background
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(notification:)
                                                 name: UIApplicationDidEnterBackgroundNotification
                                               object: nil];
}

- (void)notification:(NSNotification *)notification
{
    DDLogVerbose(@"AISocketManager:notification entry.");
    if ([notification.name isEqualToString:UIApplicationDidBecomeActiveNotification])
    {
        DDLogVerbose(@"application did become active notification.");
        if ((!self.isConnected) && (!self.isAttemptingConnection))
        {
            DDLogError(@"Not connecting or attempting connection.");
            [self connect];
            //[self doHeartbeat];
        }
    }
    else if ([notification.name isEqualToString:UIApplicationDidEnterBackgroundNotification])
    {
        DDLogVerbose(@"application did entry background notification.");
        [self disconnect];
    }
    DDLogVerbose(@"AISocketManager:notification exit.");
}


# pragma mark Socket handling methods.
- (void)initSocket
{
    DDLogVerbose(@"AISocketManager:initSocket() entry.");
    self.socketQueue = dispatch_queue_create("com.ai.socketQueue", NULL);
    self.socket = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:self.socketQueue];
    DDLogVerbose(@"AISocketManager:initSocket() exit.");
}

- (void)startConnectToHost:(NSString *)host port:(uint16_t)port
{
    DDLogVerbose(@"AISocketManager:startConnectToHost() entry.");
    self.isAttemptingConnection = YES;
    NSError *err = nil;
    if (![self.socket connectToHost:host onPort:port error:&err]) // Asynchronous!
    {
        // If there was an error it's likely to be "already connected." or "no delegate set."
        DDLogError(@"Failed asynchronous call to socket:connectToHost: %@", err);
        self.isAttemptingConnection = NO;
    }
    DDLogVerbose(@"AISocketManager:startConnectToHost() exit.");
}

// ----------------------------------------------------------------------------
//  socket:didConnectToHost gets called when a connection is successfully made.
// ----------------------------------------------------------------------------
- (void)socket:(GCDAsyncSocket *)sender didConnectToHost:(NSString *)host port:(uint16_t)port
{
    DDLogVerbose(@"AISocketManager:socket:didConnectToHost entry. host: %@, port: %d", host, port);
    [self startSecure];
    DDLogVerbose(@"AISocketManager:socket:didConnectToHost exit.");
}
// ----------------------------------------------------------------------------

#pragma mark SSL socket methods.
- (void)startSecure
{
    DDLogVerbose(@"AISocketManager:startSecure entry.");
    
    // Do not validate the certificate peer name or the certificate chain. We're
    // just going to do a byte comparison with the public key we expect, and on
    // failure just fail the connection.
    NSMutableDictionary *settings = $mdictnew;
    
    // Set the peer name
    //[settings $obj:@"app.theclic.co.uk"
    //           for:(NSString *)kCFStreamSSLPeerName];
    
    // Do not validate the peer name
    [settings $obj:(id)kCFNull
               for:(NSString *)kCFStreamSSLPeerName];
    
    // Allow expired certificates
    [settings $obj:[NSNumber numberWithBool:YES]
               for:(NSString *)kCFStreamSSLAllowsExpiredCertificates];
    
    // Allow self-signed certificates
    [settings $obj:[NSNumber numberWithBool:YES]
               for:(NSString *)kCFStreamSSLAllowsAnyRoot];
    
    // Don't validate the certificate chain.
    [settings $obj:[NSNumber numberWithBool:NO]
               for:(NSString *)kCFStreamSSLValidatesCertificateChain];
    
    [self.socket startTLS:settings];
    
    DDLogVerbose(@"AISocketManager:startSecure exit.");
}

// ----------------------------------------------------------------------------
//  socketDidSecure gets called when a SSL socket is successfully set up.
// ----------------------------------------------------------------------------
- (void)socketDidSecure:(GCDAsyncSocket *)socket
{
    DDLogVerbose(@"AISocketManager:socketDidSecure entry.");
    [socket performBlock:^{
        // ---------------------------------------------------------------------
        //  Disable Nagle's algorithm using the TCP_NODELAY socket option in
        //  order to send packets as possible, at the cost of wasted
        //  bandwidth.
        // ---------------------------------------------------------------------
        int fd = [socket socketFD];
        int on = 1;
        if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char*)&on, sizeof(on)) == -1)
        {
            DDLogError(@"AISocketManager:socketDidSecure(). Failed to set TCP_NODELAY on socket.");
        }
        // ---------------------------------------------------------------------
        
        // TODO Verify the server's certificate against a local copy.
        // Reference: https://github.com/robbiehanson/CocoaAsyncSocket/issues/36
        CFReadStreamRef readStream = [socket readStream];
        
        // Create SecTrustRef from Stream
        CFTypeRef ref = CFReadStreamCopyProperty(readStream, kCFStreamPropertySSLPeerCertificates);
        SecPolicyRef policy = SecPolicyCreateSSL(NO, (__bridge CFStringRef)@"app.theclic.co.uk");
        SecTrustRef trust = NULL;
        SecTrustCreateWithCertificates(ref, policy, &trust);
        DDLogVerbose(@"trust: %@", trust);
        
        SecTrustResultType trustResultType = kSecTrustResultInvalid;
        OSStatus status = SecTrustEvaluate(trust, &trustResultType);
        DDLogVerbose(@"trust evaluate. status: %ld", status);
        
        DDLogVerbose(@"trust certificate count: %ld", SecTrustGetCertificateCount(trust));
        SecCertificateRef cert = SecTrustGetCertificateAtIndex(trust, 0);
        DDLogVerbose(@"cert: %@", cert);

        CFStringRef certSummary = SecCertificateCopySubjectSummary(cert);
        DDLogVerbose(@"certSummary: %@", certSummary);
        CFDataRef certData = SecCertificateCopyData(cert);
        
        // !!AI these bytes are almost exactly the same as the base64 decoded
        // bytes in server_crt.crt, i.e. the end of server_crt.pem.
        // I think they're the same. TODO copy/base base64 encoded version,
        // decode in iOS, byte compare.
        DDLogVerbose(@"certData: %@", certData);
        
        SecKeyRef keyRef = SecTrustCopyPublicKey(trust);
        DDLogVerbose(@"keyRef: %@. block size: %lu", keyRef, SecKeyGetBlockSize(keyRef));

        CFRelease(certSummary);
        CFRelease(certData);
        CFRelease(ref);
        //CFRelease(keyRef);
        
        /*
        // Get our Root
        NSString *path=[[NSBundle mainBundle] pathForResource:@"CACert-class3" ofType:@"der"];
        NSData *certData=[NSData dataWithContentsOfFile:path];
        SecCertificateRef cert = SecCertificateCreateWithData(NULL, (__bridge CFDataRef)certData);
        
        // Verify
        SecTrustSetAnchorCertificates(trust, (__bridge CFArrayRef)[NSArray arrayWithObject:(__bridge id)cert]);
        SecTrustResultType trustResultType = kSecTrustResultInvalid;
        OSStatus status = SecTrustEvaluate(trust, &trustResultType);
        
        if (status == errSecSuccess) {
            if (trustResultType == kSecTrustResultUnspecified) {
                NSLog(@"%@ is Trustworthy", [sock theHost]);
            } else {
                NSLog(@"%@ is EVIL ;)", [sock theHost]);
            }
        } else {
            NSLog(@"SecTrustEvaluate failed");
        }
         */
        
        // Configures the socket to allow it to operate when the iOS application has
        // been backgrounded.
        BOOL rc = [socket enableBackgroundingOnSocket];
        DDLogVerbose(@"result of enableBackgroundingOnSocket: %@", rc == YES ? @"YES" : @"NO");
        
    }];
    
    // Perform a heartbeat
    [self doHeartbeat];
    
    self.isConnected = YES;
    self.isAttemptingConnection = NO;
    [[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_SOCKET_OPENED
                                                        object:self];
    [self readHeader];
    DDLogVerbose(@"AISocketManager:socketDidSecure exit.");
}

// ----------------------------------------------------------------------------
//  socketDidDisconnect gets called when socket closes. If there's an SSL
//  error then 'error' input variable will have SSL error code.
//
//  Error codes are described in Apple's SecureTransport.h.
// ----------------------------------------------------------------------------
- (void)socketDidDisconnect:(GCDAsyncSocket *)socket withError:(NSError *)error
{
    DDLogVerbose(@"AISocketManager:socketDidDisconnect entry.");
    DDLogWarn(@"Socket failed with error: %@", error);
    DDLogVerbose(@"AISocketManager:socketDidDisconnect exit.");
}


#pragma mark Reading data from socket.

// ----------------------------------------------------------------------------
//  socket:didReadDataWithTag gets calls when we've successfully read data.
// ----------------------------------------------------------------------------
- (void)socket:(GCDAsyncSocket *)sender didReadData:(NSData *)data withTag:(long)tag
{
    assert(tag == TAG_FIXED_LENGTH_HEADER_READ || tag == TAG_RESPONSE_BODY_READ);
    
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
    ^{
        DDLogVerbose(@"AISocketManager:socket:didReadDataWithTag entry. tag: %ld", tag);
        if (tag == TAG_FIXED_LENGTH_HEADER_READ)
        {
            DDLogVerbose(@"TAG_FIXED_LENGTH_HEADER.");
            int bodyLength = [self parseHeader:data];
            [self.socket readDataToLength:bodyLength withTimeout:-1 tag:TAG_RESPONSE_BODY_READ];
        }
        else if (tag == TAG_RESPONSE_BODY_READ)
        {
            // Process the response, and then start reading the next response.
            DDLogVerbose(@"TAG_RESPONSE_BODY.");
            [self handleResponseBody:data];
            [self readHeader];
        }
        DDLogVerbose(@"AISocketManager:socket:didReadDataWithTag exit.");
            
        [self stopProcessingTask];
    });
}

- (void)readHeader
{
    [self.socket readDataToLength:HEADER_SIZE withTimeout:-1 tag:TAG_FIXED_LENGTH_HEADER_READ];
}

// ----------------------------------------------------------------------------
//  Parse a header, which is a big-endian unsigned integer of 4 bytes. It
//  indicates the size of the payload to follow.
// ----------------------------------------------------------------------------
- (int)parseHeader:(NSData *)data
{
    DDLogVerbose(@"AISocketManager:parseHeader entry. data: %@", data);
    assert(data.length == HEADER_SIZE);
    int return_value = CFSwapInt32BigToHost(*(int *)data.bytes);
    DDLogVerbose(@"AISocketManager:parseHeader exit. returning: %d", return_value);
    return return_value;
}
// ----------------------------------------------------------------------------

- (void)handleResponseBody:(NSData *)data
{
    DDLogVerbose(@"AISocketManager:handleResponseBody entry.");
    NSString *response = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    BOOL isHandledAsControlResponse = [self maybeHandleControlResponse:response];
    if (!(isHandledAsControlResponse))
    {
        // !!AI move this out to a caller-specified block callback
        DDLogVerbose(@"AISocketManager:handleResponseBody: Is not a control response. Must be JSON encoded.");
        DDLogVerbose(@"AISocketManager:handleResponseBody: response.length: %i", response.length);
        
        NSDictionary *json_decoded_1 = [response objectFromJSONString];
        NSDictionary *result = [json_decoded_1 $for:@"result"];
        NSNumber *status = [result $for:@"status"];
        DDLogVerbose(@"status: %@", status);
        
        NSString *value = [result $for:@"value"];
        
        DDLogVerbose(@"base64 decoding response...");
        NSData *response_base64_decoded = [AIUtilities decodeBase64WithString:value];
        
        DDLogVerbose(@"BZIP2 decompressing response...");
        NSData *response_decompressed = [AIUtilities bunzip2:response_base64_decoded];
        
        DDLogVerbose(@"JSON decoding response - encode to UTF-8...");
        NSString *response_decompressed_string = [[NSString alloc] initWithData:response_decompressed
                                                                       encoding:NSUTF8StringEncoding];
        DDLogVerbose(@"JSON decoding response - decode as JSON...");
        NSArray *response_json_decoded = [response_decompressed_string objectFromJSONString];
        DDLogVerbose(@"response_json_decoded.count: %d", response_json_decoded.count);
        
        NSString *uuid = [json_decoded_1 $for:@"tag"];
        DDLogVerbose(@"posting notification under name: %@", uuid);
        [[NSNotificationCenter defaultCenter] postNotificationName:uuid
                                                            object:response_json_decoded];
        
    }
    
    DDLogVerbose(@"AISocketManager:handleResponseBody exit.");
}

- (BOOL)maybeHandleControlResponse:(NSString *)response
{
    DDLogVerbose(@"AISocketManager:maybeHandleControlResponse entry.");
    BOOL return_value = YES;
    if ([response isEqualToString:@"ping"])
    {
        DDLogVerbose(@"AISocketManager:maybeHandleControlResponse: server is pinging us, will respond with 'pong'");
        [self writeString:@"pong"
                  timeout:-1
               header_tag:TAG_PONG_HEADER
              payload_tag:TAG_PONG_PAYLOAD];
        goto EXIT_LABEL;
    }
    else if ([response isEqualToString:@"pong"])
    {
        DDLogVerbose(@"AISocketManager:maybeHandleControlResponse: server responds 'pong' to our ping.");
        goto EXIT_LABEL;
    }
    else if ([response isEqualToString:@"close"])
    {
        DDLogVerbose(@"AISocketManager:maybeHandleControlResponse: server requests we close the connection.");
        goto EXIT_LABEL;
    }
    return_value = NO;
    
EXIT_LABEL:
    DDLogVerbose(@"AISocketManager:maybeHandleControlResponse exit. returning: %@",
                 (return_value) ? @"YES" : @"NO");
    return return_value;
}

#pragma mark Writing data to socket.

- (void)writeString:(NSString *)string
            timeout:(NSTimeInterval)timeout
         header_tag:(long)header_tag
        payload_tag:(long)payload_tag
{
    NSData *data_using_encoding = [string dataUsingEncoding:NSUTF8StringEncoding];
    [self writeData:data_using_encoding
        withTimeout:timeout
         header_tag:header_tag
        payload_tag:payload_tag];
}

- (void)writeData:(NSData *)data
      withTimeout:(NSTimeInterval)timeout
       header_tag:(long)header_tag
      payload_tag:(long)payload_tag
{
    DDLogVerbose(@"AISocketManager:writeData entry. header_tag: %ld, payload_tag: %ld, data: %@", header_tag, payload_tag, data);

    // Write the header. This is a big-endian unsigned integer of 4 bytes that indicates the
    // size of the payload to follow.
    int big_endian_data_length = CFSwapInt32HostToBig(data.length);
    NSData *header = [NSData dataWithBytes:&big_endian_data_length
                                    length:sizeof(big_endian_data_length)];
    [self.socket writeData:header withTimeout:timeout tag:header_tag];

    // Write the payload.
    [self.socket writeData:data withTimeout:-1 tag:payload_tag];
    
    DDLogVerbose(@"AISocketManager:writeData exit.");    
}

// ----------------------------------------------------------------------------
//  socket:didWriteDataWithTag gets calls when we've successfully written data.
// ----------------------------------------------------------------------------
- (void)socket:(GCDAsyncSocket *)sender didWriteDataWithTag:(long)tag
{
    DDLogVerbose(@"AISocketManager:socket:didWriteDataWithTag entry. tag: %ld", tag);
    DDLogVerbose(@"AISocketManager:socket:didWriteDataWithTag exit.");
}
// ----------------------------------------------------------------------------

- (void)doHeartbeat
{
    DDLogVerbose(@"AISocketManager:doHeartbeat entry.");
    static NSString *ping = @"ping";
    [self writeString:ping
              timeout:-1
           header_tag:TAG_PING_HEADER
          payload_tag:TAG_PING_PAYLOAD];
    DDLogVerbose(@"AISocketManager:doHeartbeat exit.");
}

#pragma mark - Singleton methods, lifecycle.
+ (void)initialize
{
    DDLogVerbose(@"AISocketManager:initialize entry.");
    if (self == [AISocketManager class])
    {
        sharedInstance = [[self alloc] init];
    }
}

+ (AISocketManager *)sharedInstance
{
    return sharedInstance;
}

- (AISocketManager *)init
{
    DDLogVerbose(@"AISocketManager:init entry.");
    self = [super init];
    if (!self)
    {
        return nil;
    }
    
    // ------------------------------------------------------------------------
    //  Initialize instance variables.
    // ------------------------------------------------------------------------
    _isAttemptingConnection = NO;
    _isConnected = NO;
    self.application = [UIApplication sharedApplication];
    // ------------------------------------------------------------------------
    
    [self initSocketManager];
    [self initListener];
    return self;
}

- (void)dealloc
{
    self.socketQueue = nil;
    self.processingQueue = nil;
}

@end
