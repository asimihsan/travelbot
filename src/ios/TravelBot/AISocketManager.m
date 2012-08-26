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
#import "CocoaAsyncSocket/GCDAsyncSocket.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static const int ddLogLevel = LOG_LEVEL_VERBOSE;

// Header is fixed size at 4-bytes. It is a big-endian unsigned integer that
// specifies the size of the payload that immediately follows in bytes.
static const NSUInteger HEADER_SIZE = 4;

// Tags are used for identifying the asynchronous callbacks of reads and
// writes.
static const long TAG_FIXED_LENGTH_HEADER       = 1;
static const long TAG_RESPONSE_BODY             = 2;
static const long TAG_PING                      = 3;
static const long TAG_CLOSE                     = 4;
// ----------------------------------------------------------------------------

#pragma mark - Private methods and constants.
@interface AISocketManager ()

@property (nonatomic, assign) dispatch_queue_t socketQueue;
@property (nonatomic, assign) BOOL isConnected;
@property (nonatomic, assign) BOOL isAttemptingConnection;

- (void)initSocketManager;
- (void)initSocket;
- (void)startConnectToHost:(NSString *)host port:(uint16_t)port;

- (void)writeData:(NSData *)data withTimeout:(NSTimeInterval)timeout tag:(long)tag;
- (void)readHeader;

- (NSUInteger)parseHeader:(NSData *)data;
- (void)handleResponseBody:(NSData *)data;

- (void)doHeartbeat;
- (void)notification:(NSNotification *)notification;

@end

@implementation AISocketManager

@synthesize socket = _socket,
            socketQueue = _socketQueue,
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
    [self startConnectToHost:@"127.0.0.1" port:8080];
    DDLogVerbose(@"AISocketManager:connect() exit.");
}

- (void)disconnect
{
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
    NSData *close_data = [close dataUsingEncoding:NSASCIIStringEncoding];
    [self writeData:close_data withTimeout:-1 tag:TAG_CLOSE];
    [self.socket disconnectAfterReadingAndWriting];
    self.isConnected = NO;
    DDLogVerbose(@"AISocketManager:disconnect() exit.");
}

#pragma mark - Private methods.

- (void)initSocketManager
{
    DDLogVerbose(@"AISocketManager:initSocketManager() entry.");
    
    if (self.isConnected)
    {
        DDLogVerbose(@"isConnected, so disconnect.");
        [self disconnect];
    }
    
    // Setup socket's GCD queue and the socket. Send an initial ping.
    [self initSocket];
    [self connect];
    [self doHeartbeat];
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
            [self doHeartbeat];
        }
    }
    else if ([notification.name isEqualToString:UIApplicationDidEnterBackgroundNotification])
    {
        DDLogVerbose(@"application did entry background notification.");
        [self disconnect];
    }
    DDLogVerbose(@"AISocketManager:notification exit.");
}


# pragma mark - Socket handling methods.
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

- (void)writeData:(NSData *)data withTimeout:(NSTimeInterval)timeout tag:(long)tag
{
    DDLogVerbose(@"AISocketManager:writeData entry.");
    // CFSwapInt32HostToBig converts a 32-bit integer from the host’s native byte order to big-endian format.
    // CFSwapInt32BigToHost converts a 32-bit integer from big-endian format to the host’s native byte order.
    int length = data.length;
    int big_endian_length = CFSwapInt32HostToBig(length);
    NSData *header = [NSData dataWithBytes:&big_endian_length
                                    length:sizeof(big_endian_length)];
    DDLogVerbose(@"header: %@", header);
    DDLogVerbose(@"data: %@", data);
    [self.socket writeData:header withTimeout:-1 tag:TAG_FIXED_LENGTH_HEADER];
    [self.socket writeData:data withTimeout:-1 tag:TAG_RESPONSE_BODY];
    DDLogVerbose(@"AISocketManager:writeData exit.");    
}

// ----------------------------------------------------------------------------
//  socket:didConnectToHost gets called when a connection is successfully made.
// ----------------------------------------------------------------------------
- (void)socket:(GCDAsyncSocket *)sender didConnectToHost:(NSString *)host port:(uint16_t)port
{
    DDLogVerbose(@"AISocketManager:socket:didConnectToHost entry. host: %@, port: %d", host, port);
    self.isConnected = YES;
    self.isAttemptingConnection = NO;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
//  socket:didReadDataWithTag gets calls when we've successfully read data.
// ----------------------------------------------------------------------------
- (void)socket:(GCDAsyncSocket *)sender didReadData:(NSData *)data withTag:(long)tag
{
    DDLogVerbose(@"AISocketManager:socket:didReadDataWithTag entry. tag: %ld", tag);
    if (tag == TAG_FIXED_LENGTH_HEADER)
    {
        DDLogVerbose(@"TAG_FIXED_LENGTH_HEADER.");
        NSUInteger bodyLength = [self parseHeader:data];
        [self.socket readDataToLength:bodyLength withTimeout:-1 tag:TAG_RESPONSE_BODY];
    }
    else if (tag == TAG_RESPONSE_BODY)
    {
        // Process the response, and then start reading the next response.
        DDLogVerbose(@"TAG_RESPONSE_BODY.");
        [self handleResponseBody:data];
        [self.socket readDataToLength:HEADER_SIZE withTimeout:-1 tag:TAG_FIXED_LENGTH_HEADER];
    }
}

// ----------------------------------------------------------------------------
//  socket:didWriteDataWithTag gets calls when we've successfully written data.
// ----------------------------------------------------------------------------
- (void)socket:(GCDAsyncSocket *)sender didWriteDataWithTag:(long)tag
{
    DDLogVerbose(@"AISocketManager:socket:didWriteDataWithTag entry. tag: %ld", tag);
}
// ----------------------------------------------------------------------------

- (void)readHeader
{
    [self.socket readDataToLength:HEADER_SIZE withTimeout:-1 tag:TAG_FIXED_LENGTH_HEADER];
}

// ----------------------------------------------------------------------------
//  Parse a header, which is a big-endian unsigned integer of 4 bytes. It
//  indicates the size of the payload to follow.
// ----------------------------------------------------------------------------
- (NSUInteger)parseHeader:(NSData *)data
{
    DDLogVerbose(@"AISocketManager:parseHeader entry. data: %@", data);
    return 4;
}
// ----------------------------------------------------------------------------

- (void)handleResponseBody:(NSData *)data
{
    DDLogVerbose(@"AISocketManager:handleResponseBody entry. data: %@", data);
}

- (void)doHeartbeat
{
    DDLogVerbose(@"AISocketManager:doHeartbeat entry.");
    static NSString *ping = @"ping";
    NSData *ping_data = [ping dataUsingEncoding:NSASCIIStringEncoding];
    [self writeData:ping_data withTimeout:-1 tag:TAG_PING];
    [self readHeader];
    DDLogVerbose(@"AISocketManager:doHeartbeat exit.");
}

#pragma mark - Singleton methods, lifecycle.
+ (void)initialize
{
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
    // ------------------------------------------------------------------------
    
    [self initSocketManager];
    [self initListener];
    return self;
}



@end
