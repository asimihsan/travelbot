//
//  AIDatabaseManager.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "AIDatabaseManager.h"
#import "AIUtilities.h"
#import "FMDB/FMDatabase.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

#import "ASIS3Request.h"
#import "ASIS3ObjectRequest.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
static NSString *LOCATIONS_DATABASE_NAME = @"locations.sqlite";
static NSString *LOCATIONS_DATABASE_COMPRESSED_NAME = @"locations.sqlite.bz2";

// AWS credentials for read-only access to certain keys.
static NSString *AWS_SECRET_ACCESS_KEY = @"3abl+yk7C7TaeIAV8BlWdcRF1gTZvHHnv0dJmIPI";
static NSString *AWS_ACCESS_KEY_ID = @"AKIAJXMF5MIE2I7MEFYQ";
static NSString *AWS_S3_BUCKET = @"ai-travelbot";
static NSString *AWS_LOCATIONS_DATABASE_S3_PATH = @"locations.sqlite.bz2";
// ----------------------------------------------------------------------------

static AIDatabaseManager *sharedInstance = nil;

@interface AIDatabaseManager ()

@property (nonatomic, retain) UIApplication *application;
@property (nonatomic, assign) dispatch_queue_t processingQueue;
@property (nonatomic, assign) UIBackgroundTaskIdentifier processingTask;
@property (nonatomic, retain) FMDatabase *locations_db;

- (void)initDatabaseManager;
- (void)initListener;
- (void)startProcessingTask;
- (void)stopProcessingTask;

- (FMResultSet *)executeQuery:(FMDatabase *)database
                        query:(NSString *)query
                    arguments:(NSDictionary *)arguments;
- (void)executeUpdate:(FMDatabase *)database
               update:(NSString *)update
            arguments:(NSDictionary *)arguments;

@end

@implementation AIDatabaseManager

@synthesize application = _application;
@synthesize processingQueue = _processingQueue;
@synthesize processingTask = _processingTask;
@synthesize locations_db = _locations_db;

#pragma mark - Public API
- (NSString *)getPlaceWithCountryCode:(NSString *)countryCode
                    filter:(NSString *)filter
                    index:(NSNumber *)index
{
    DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode entry. countryCode: %@, filter: %@, index: %@",
                 countryCode, filter, index);
    
    // -------------------------------------------------------------------------
    //  Initialize output variable.
    // -------------------------------------------------------------------------
    NSString *return_value;
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Initialize variables.
    // -------------------------------------------------------------------------
    NSString *selectQuery = @"SELECT name FROM locations WHERE country_code = :country_code ORDER BY name ASC LIMIT 1 OFFSET :offset";
    NSDictionary *arguments = $dict(index, @"offset",
                                    countryCode, @"country_code");
    // -------------------------------------------------------------------------
    
    FMResultSet *resultSet = [self executeQuery:self.locations_db
                                          query:selectQuery
                                      arguments:arguments];
    DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. resultSet: %@", resultSet);
    if ([resultSet next])
    {
        DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode: there is a result.");
        return_value = [resultSet stringForColumnIndex:0];
    }
    else
    {
        DDLogError(@"AIDatabaseManager:getPlaceWithCountryCode: there isn't a result.");
        return_value = nil;
    }
    [resultSet close];

    DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode returning: %@", return_value);
    return return_value;
}

- (NSNumber *)getNumberOfPlaces:(NSString *)countryCode
{
    DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces entry. countryCode: %@", countryCode);
    
    // -------------------------------------------------------------------------
    //  Initialize output variable.
    // -------------------------------------------------------------------------
    NSNumber *return_value;
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Initialize variables.
    // -------------------------------------------------------------------------
    NSString *selectQuery = @"SELECT COUNT(*) FROM locations WHERE country_code = :country_code;";
    NSDictionary *arguments = $dict(countryCode, @"country_code");
    // -------------------------------------------------------------------------
    
    FMResultSet *resultSet = [self executeQuery:self.locations_db
                                          query:selectQuery
                                      arguments:arguments];
    DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces. resultSet: %@", resultSet);
    if ([resultSet next])
    {
        DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces: there is a result.");
        NSInteger result = [resultSet intForColumnIndex:0];
        return_value = [[NSNumber alloc] initWithInt:result];
    }
    else
    {
        DDLogError(@"AIDatabaseManager:getNumberOfPlaces: there isn't a result.");
        return_value = nil;
    }
    [resultSet close];
    
    DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces returning: %@", return_value);
    return return_value;
}

// All FMDB operations are put onto a serial GCD queue. This forces all
// DB operations to become serial. Moreover, all operations are wrapped
// in a background tasks. This forces all operations to continue running
// in the background if the application gets sent to the background.
//
// FMDatabaseQueue does serialization too but I want to use my GCD queue.
#pragma mark - Wrappers around FMDB API.

- (FMResultSet *)executeQuery:(FMDatabase *)database
                        query:(NSString *)query
                    arguments:(NSDictionary *)arguments;
{
    __block FMResultSet *resultSet = [[FMResultSet alloc] init];
    [self startProcessingTask];
    dispatch_sync(self.processingQueue,
    ^{
        DDLogVerbose(@"AIDatabaseManager:executeQuery entry. query: %@, arguments: %@", query, arguments);
        if (![self isOpened])
        {
            DDLogError(@"AIDatabaseManager:executeQuery: database is not open");
            resultSet = nil;
        }
        else
        {
            DDLogVerbose(@"AIDatabaseManager:executeQuery: database is open.");
            if (arguments && arguments.count > 0)
                resultSet = [self.locations_db executeQuery:query
                                    withParameterDictionary:arguments];
            else
                resultSet = [self.locations_db executeQuery:query];
        }
        [self stopProcessingTask];
    });
    return resultSet;
}

- (void)executeUpdate:(FMDatabase *)database
               update:(NSString *)update
            arguments:(NSDictionary *)arguments;
{
    [self startProcessingTask];
    dispatch_sync(self.processingQueue,
    ^{
        DDLogVerbose(@"AIDatabaseManager:executeUpdate entry. update: %@, arguments: %@", update, arguments);
        if (![self isOpened])
        {
            DDLogError(@"AIDatabaseManager:executeUpdate: database is not open");
        }
        else
        {
            DDLogVerbose(@"AIDatabaseManager:executeUpdate: database is open.");
            [self.locations_db executeUpdate:update withParameterDictionary:arguments];
        }
        [self stopProcessingTask];
    });
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
}

- (void)dealloc
{
    self.processingQueue = nil;
}

- (BOOL)isOpened
{
    return (self.locations_db && self.locations_db.open);
}

- (void)open
{
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
    ^{
        DDLogVerbose(@"AIDatabaseManager:open entry.");
        
        // Download then decompress the locations database.
        NSString *dbCompressedPath = [[$ documentPath] stringByAppendingPathComponent:LOCATIONS_DATABASE_COMPRESSED_NAME];
        DDLogVerbose(@"AIDatabaseManager:open: downloading database from S3 bucket %@, key %@, to %@...", AWS_S3_BUCKET, AWS_LOCATIONS_DATABASE_S3_PATH, dbCompressedPath);
        ASIS3ObjectRequest *request = [ASIS3ObjectRequest requestWithBucket:AWS_S3_BUCKET
                                                                        key:AWS_LOCATIONS_DATABASE_S3_PATH];
        [request setSecretAccessKey:AWS_SECRET_ACCESS_KEY];
        [request setAccessKey:AWS_ACCESS_KEY_ID];
        [request setDownloadDestinationPath:dbCompressedPath];
        [request startSynchronous];
        if (request.error)
        {
            DDLogError(@"AIDatabaseManager:open: error during database download: %@",
                       [request.error localizedDescription]);
        }
        else
        {
            DDLogVerbose(@"AIDatabaseManager:open: database downloaded. Decompressing...");
            NSString *dbPath = [[$ documentPath] stringByAppendingPathComponent:LOCATIONS_DATABASE_NAME];
            [AIUtilities bunzip2:dbCompressedPath
                  outputFilepath:dbPath];

            // Open the locations database.
            DDLogVerbose(@"AIDatabaseManager:open: opening locations database at %@.", dbPath);
            self.locations_db = [FMDatabase databaseWithPath:dbPath];
            if (![self.locations_db open])
            {
                DDLogError(@"AIDatabaseManager:open: failed to open locations database.");
                self.locations_db = nil;
            }
            DDLogVerbose(@"AIDatabaseManager:open: locations database is open.");
        }
        
        // Notify listeners that the database has opened.
        if ([self isOpened])
        {
            DDLogVerbose(@"posting notification that database has opened.");
            [[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_DATABASE_OPENED
                                                                object:self];
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
            if (![self.locations_db close])
            {
                DDLogError(@"AIDatabaseManager:close: Failed to close database.");
            }
            self.locations_db = nil;
        }
        [self stopProcessingTask];
   });
}

@end
