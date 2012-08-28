//
//  AIDatabaseManager.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "AIDatabaseManager.h"
#import "FMDB/FMDatabase.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
static NSString *DATABASE_NAME = @"database.sqlite";
// ----------------------------------------------------------------------------

static AIDatabaseManager *sharedInstance = nil;

@interface AIDatabaseManager ()

@property (nonatomic, retain) UIApplication *application;
@property (nonatomic, assign) dispatch_queue_t processingQueue;
@property (nonatomic, assign) UIBackgroundTaskIdentifier processingTask;
@property (nonatomic, retain) FMDatabase *db;

- (void)initDatabaseManager;
- (void)initListener;
- (void)startProcessingTask;
- (void)stopProcessingTask;

@end


@implementation AIDatabaseManager

@synthesize application = _application;
@synthesize processingQueue = _processingQueue;
@synthesize processingTask = _processingTask;

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

#pragma mark - NSNotification listener events.
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
    DDLogVerbose(@"AIDatabaseManager:notification entry.");
    if ([notification.name isEqualToString:UIApplicationDidBecomeActiveNotification])
    {
        DDLogVerbose(@"application did become active notification.");
        [self open];
    }
    else if ([notification.name isEqualToString:UIApplicationDidEnterBackgroundNotification])
    {
        DDLogVerbose(@"application did become background notification.");
        [self close];
    }
    DDLogVerbose(@"AIDatabaseManager:notification exit.");
}

#pragma mark - Singleton methods, lifecycle.
+ (void)initialize
{
    if (self == [AIDatabaseManager class])
    {
        sharedInstance = [[self alloc] init];
    }
}

+ (AIDatabaseManager *)sharedInstance
{
    return sharedInstance;
}

- (AIDatabaseManager *)init
{
    self = [super init];
    if (!self)
    {
        return nil;
    }
    
    // ------------------------------------------------------------------------
    //  Initialize instance variables.
    // ------------------------------------------------------------------------
    self.application = [UIApplication sharedApplication];
    // ------------------------------------------------------------------------
    
    [self initDatabaseManager];
    [self initListener];
    return self;
}

- (void)initDatabaseManager
{
    self.processingQueue = dispatch_queue_create("com.ai.AIDatabaseManager.processingQueue", NULL);
    [self open];
}

- (void)dealloc
{
    self.processingQueue = nil;
}

- (BOOL)isOpened
{
    return (self.db && self.db.open);
}

- (void)open
{
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
    ^{
        DDLogVerbose(@"AIDatabaseManager:open entry.");
        NSString *dbPath = [[$ documentPath] stringByAppendingPathComponent:DATABASE_NAME];
        DDLogVerbose(@"AIDatabaseManager:open: dbPath: %@", dbPath);
        self.db = [FMDatabase databaseWithPath:dbPath];
        if (![self.db open])
        {
            DDLogError(@"AIDatabaseManager:open: failed to open database.");
            self.db = nil;
        }
        [self stopProcessingTask];
    });
}

- (void)close
{
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
   ^{
        DDLogVerbose(@"AIDatabaseManager:close entry.");
        if (![self isOpened])
        {
            DDLogWarn(@"AIDatabaseManager:close: Database not opened, so cannot close.");
        }
        else
        {
            DDLogVerbose(@"AIDatabaseManager:close: Database is open, so close it.");
            if (![self.db close])
            {
                DDLogError(@"AIDatabaseManager:close: Failed to close database.");
            }
            self.db = nil;
        }
        [self stopProcessingTask];
   });
}

@end
