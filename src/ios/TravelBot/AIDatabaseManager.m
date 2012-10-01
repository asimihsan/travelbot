//
//  AIDatabaseManager.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "AIDatabaseManager.h"
#import "AIUtilities.h"
#import "AIConfigManager.h"
#import "TravelBotPlace.h"
#import "TravelBotSavedSearch.h"
#import "FMDB/FMDatabase.h"
#import "JSONKit/JSONKit.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"
#import "ConciseKit.h"

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

// Query caches.
static int GET_PLACE_WITH_COUNTRY_CODE_CACHE_SIZE = 100;
static NSMutableDictionary *getPlaceWithCountryCodeCache;

// Favorites have their own database because we want an easy to update and
// deploy way to update locations.
static NSString *FAVORITES_DATABASE_NAME = @"favorites.sqlite";
static NSString *FAVOURITES_CREATE_SAVED_SEARCH_STATEMENT = \
    @"CREATE TABLE saved_search(" \
     "from_place_name TEXT, " \
     "from_country_code TEXT, " \
     "to_place_name TEXT, " \
     "to_country_code TEXT, " \
     "search_datetime DATETIME, " \
     "journeys BLOB);";
static NSString *FAVOURITES_CREATE_FAVOURITE_SEARCHES_STATEMENT = \
    @"CREATE TABLE favourite_search(" \
     "saved_search_id INTEGER, " \
     "FOREIGN KEY (saved_search_id) REFERENCES saved_search(rowid));";
static NSString *FAVORITES_INSERT_SAVED_SEARCH_STATEMENT = \
    @"INSERT INTO saved_search VALUES " \
     " (:from_place_name, " \
     "  :from_country_code, " \
     "  :to_place_name, "\
     "  :to_country_code, " \
     "  :search_datetime, " \
     "  :journeys);";
static NSString *FAVOURITES_GET_NUMBER_OF_SAVED_SEARCHES_STATEMENT = \
    @"SELECT COUNT(*) FROM saved_search;";
static NSString *FAVOURITES_GET_SAVED_SEARCH_STATEMENT = \
    @"SELECT from_place_name, from_country_code, to_place_name, to_country_code, search_datetime, journeys " \
     "FROM saved_search " \
     "ORDER BY search_datetime DESC LIMIT :limit OFFSET :offset;";
// ----------------------------------------------------------------------------

static AIDatabaseManager *sharedInstance = nil;

@interface AIDatabaseManager ()

@property (nonatomic, strong) UIApplication *application;
@property (nonatomic, assign) dispatch_queue_t processingQueue;
@property (nonatomic, assign) UIBackgroundTaskIdentifier processingTask;
@property (nonatomic, strong) FMDatabase *locations_db;
@property (nonatomic, strong) FMDatabase *favorites_db;
@property (nonatomic, strong) NSSet *countriesConfig;

- (void)initDatabaseManager;
- (void)initListener;
- (void)initLocationsDb;
- (void)initFavoritesDb;
- (void)startProcessingTask;
- (void)stopProcessingTask;

- (FMResultSet *)executeQuery:(FMDatabase *)database
                        query:(NSString *)query
                    arguments:(NSDictionary *)arguments;
- (void)executeUpdate:(FMDatabase *)database
               update:(NSString *)update
            arguments:(NSDictionary *)arguments;

- (BOOL)downloadDatabase:(NSString *)dbCompressedPath
                  dbPath:(NSString *)dbPath
                   error:(NSError **)error;
- (BOOL)copyDatabase:(NSString *)dbBundlePath
    dbCompressedPath:(NSString *)dbCompressedPath
              dbPath:(NSString *)dbPath
               error:(NSError **)error;

@end

@implementation AIDatabaseManager

@synthesize application = _application;
@synthesize processingQueue = _processingQueue;
@synthesize processingTask = _processingTask;
@synthesize locations_db = _locations_db;
@synthesize favorites_db = _favorites_db;

#pragma mark - Public API

- (NSString *)getPlaceWithCountryCode:(NSString *)countryCode
                    search:(NSString *)search
                    index:(NSInteger)index
{
    DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode entry. countryCode: %@, search: %@, index: %d",
                 countryCode, search, index);
    
    // -------------------------------------------------------------------------
    //  Initialize output variable.
    // -------------------------------------------------------------------------
    NSString *return_value;
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Initialize variables.
    // -------------------------------------------------------------------------
    NSString *selectQuery;
    NSDictionary *arguments;
    FMResultSet *resultSet;
    const NSInteger CACHE_HALF = GET_PLACE_WITH_COUNTRY_CODE_CACHE_SIZE / 2;
    NSInteger startIndex = (index - CACHE_HALF > 0) ? (index - CACHE_HALF) : 0;
    NSNumber *startIndexArgument = [NSNumber numberWithInteger:startIndex];
    NSNumber *cacheSizeArgument = [NSNumber numberWithInteger:GET_PLACE_WITH_COUNTRY_CODE_CACHE_SIZE];
    NSString *decomposedSearch;
    if (search)
    {
        DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. search present.");
        decomposedSearch = [search decomposedStringWithCanonicalMapping];
        selectQuery = @"SELECT provider.name \
                        FROM provider, provider_by_asciiname \
                        WHERE provider.country_code = :country_code AND \
                        provider_by_asciiname.asciiname MATCH :searchTermFTS AND \
                        provider.asciiname LIKE :searchTermLike AND \
                        provider.rowid = provider_by_asciiname.docid \
                        ORDER BY provider.name ASC LIMIT :limit OFFSET :offset;";
        /*
        selectQuery = @"SELECT geonames.name \
                        FROM geonames, geonames_by_asciiname \
                        WHERE geonames.country_code = :country_code AND \
                              geonames_by_asciiname.asciiname MATCH :searchTermFTS AND \
                              geonames.asciiname LIKE :searchTermLike AND \
                              geonames.geonameid = locations_by_asciiname.docid \
                        ORDER BY geonames.name ASC LIMIT :limit OFFSET :offset;";
        */
        NSString *searchTermFTS = [NSString stringWithFormat:@"%@*", decomposedSearch];
        NSString *searchTermLike = [NSString stringWithFormat:@"%@%%", decomposedSearch];
        arguments = $dict(startIndexArgument, @"offset",
                          cacheSizeArgument, @"limit",
                          countryCode, @"country_code",
                          searchTermFTS, @"searchTermFTS",
                          searchTermLike, @"searchTermLike");
    }
    else
    {
        DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. search not present.");
        selectQuery = @"SELECT name FROM provider \
                        WHERE country_code = :country_code \
                        ORDER BY name ASC LIMIT :limit OFFSET :offset";
        /*
        selectQuery = @"SELECT name FROM geonames \
                        WHERE country_code = :country_code \
                        ORDER BY name ASC LIMIT :limit OFFSET :offset";
        */
        arguments = $dict(startIndexArgument, @"offset",
                          cacheSizeArgument, @"limit",
                          countryCode, @"country_code");
    }
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  Perform the query. Try to dip into the cache, using it if there's a hit
    //  else update after the database query if not.
    // -------------------------------------------------------------------------
    NSString *cacheKey;
    NSNumber *indexArgument = [NSNumber numberWithInteger:index];
    if (search)
        cacheKey = [NSString stringWithFormat:@"%@_%@_%@",
                    countryCode, decomposedSearch, indexArgument];
    else
        cacheKey = [NSString stringWithFormat:@"%@_%@", countryCode, indexArgument];
    NSString *queryResult = [getPlaceWithCountryCodeCache $for:cacheKey];
    DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. cacheKey: %@", cacheKey);
    if (queryResult)
    {
        DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. cache hit.");
        return_value = queryResult;
    }
    else
    {
        DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. cache miss.");
        [getPlaceWithCountryCodeCache removeAllObjects];
        resultSet = [self executeQuery:self.locations_db
                                 query:selectQuery
                             arguments:arguments];
        DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode. resultSet: %@", resultSet);
        const NSInteger MAX = startIndex + GET_PLACE_WITH_COUNTRY_CODE_CACHE_SIZE;
        for (NSInteger i = startIndex; i < MAX; i++)
        {
            if (search)
                cacheKey = [NSString stringWithFormat:@"%@_%@_%d",
                            countryCode, decomposedSearch, i];
            else
                cacheKey = [NSString stringWithFormat:@"%@_%d", countryCode, i];
            [resultSet next];
            NSString *cacheValue = [resultSet stringForColumnIndex:0];
            if (!cacheValue)
                break;
            [getPlaceWithCountryCodeCache $obj:cacheValue for:cacheKey];
            if (i == index)
                return_value = cacheValue;
        }
        [resultSet close];
    }
    // -------------------------------------------------------------------------
    
    DDLogVerbose(@"AIDatabaseManager:getPlaceWithCountryCode returning: %@", return_value);
    return return_value;
}

- (NSNumber *)getNumberOfPlaces:(NSString *)countryCode
                         search:(NSString *)search
{
    DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces entry. countryCode: %@, search: %@", countryCode, search);
    
    // -------------------------------------------------------------------------
    //  Initialize output variable.
    // -------------------------------------------------------------------------
    NSNumber *return_value;
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Initialize variables.
    //
    //  -   If search is nil then we want all places from the country code.
    //  -   Else we want to use the FTS index to determine how many matching
    //      places there are in the country.
    // -------------------------------------------------------------------------
    NSString *selectQuery;
    NSDictionary *arguments;
    NSString *decomposedSearch;
    if (search)
    {
        DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces. search specified.");
        decomposedSearch = [search decomposedStringWithCanonicalMapping];
        selectQuery = @"SELECT COUNT(provider.name) \
                        FROM provider, provider_by_asciiname \
                        WHERE provider.country_code = :country_code AND \
                        provider_by_asciiname.asciiname MATCH :searchTermFTS AND \
                        provider.asciiname LIKE :searchTermLike AND \
                        provider.rowid = provider_by_asciiname.docid;";
        /*
        selectQuery = @"SELECT COUNT(geonames.name) \
                        FROM geonames, geonames_by_asciiname \
                        WHERE geonames.country_code = :country_code AND \
                              geonames_by_asciiname.asciiname MATCH :searchTermFTS AND \
                              geonames.asciiname LIKE :searchTermLike AND \
                              geonames.geonameid = geonames_by_asciiname.docid;";
        */
        NSString *searchTermFTS = [NSString stringWithFormat:@"%@*", decomposedSearch];
        NSString *searchTermLike = [NSString stringWithFormat:@"%@%%", decomposedSearch];
        arguments = $dict(countryCode, @"country_code",
                          searchTermFTS, @"searchTermFTS",
                          searchTermLike, @"searchTermLike");
    }
    else
    {
        DDLogVerbose(@"AIDatabaseManager:getNumberOfPlaces. search not specified.");
        selectQuery = @"SELECT COUNT(*) FROM provider WHERE country_code = :country_code;";
        /*
        selectQuery = @"SELECT COUNT(*) FROM locations WHERE country_code = :country_code;";
        */
        arguments = $dict(countryCode, @"country_code");
    }
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

- (void)addSearchResultsToSavedSearches:(TravelBotPlace *)fromPlace
                                toPlace:(TravelBotPlace *)toPlace
                         searchDatetime:(NSDate *)searchDatetime
                          searchResults:(NSArray *)searchResults
{
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
    ^{
        DDLogVerbose(@"AIDatabaseManager:addSearchResultsToSavedSearches entry. fromPlace: %@, toPlace: %@, searchDatetime: %@.",
                     fromPlace, toPlace, searchDatetime);
        NSString *searchDatetimeString =  $str(@"%f", [searchDatetime timeIntervalSince1970]);
        NSData *journeysSerialized = [NSKeyedArchiver archivedDataWithRootObject:searchResults];
        NSDictionary *arguments = $dict(fromPlace.name, @"from_place_name",
                                        fromPlace.country.code, @"from_country_code",
                                        toPlace.name, @"to_place_name",
                                        toPlace.country.code, @"to_country_code",
                                        searchDatetimeString, @"search_datetime",
                                        journeysSerialized, @"journeys");
        [self.favorites_db executeUpdate:FAVORITES_INSERT_SAVED_SEARCH_STATEMENT
                 withParameterDictionary:arguments];
        if ([self.favorites_db hadError])
        {
            DDLogError(@"AIDatabaseManager:addSearchResultsToSavedSearches. error. arguments: %@, error code: %d, error message: %@",
                       arguments, [self.favorites_db lastErrorCode], [self.favorites_db lastErrorMessage]);
        }
        DDLogVerbose(@"AIDatabaseManager:addSearchResultsToSavedSearches exit.");
        [self stopProcessingTask];
    });
}

- (NSNumber *)getNumberOfSavedSearches
{
    DDLogVerbose(@"AIDatabaseManager:getNumberOfSavedSearches entry.");
    
    // -------------------------------------------------------------------------
    //  Initialize output variable.
    // -------------------------------------------------------------------------
    NSNumber *return_value;
    // -------------------------------------------------------------------------
    
    FMResultSet *resultSet = [self executeQuery:self.favorites_db
                                          query:FAVOURITES_GET_NUMBER_OF_SAVED_SEARCHES_STATEMENT
                                      arguments:nil];
    DDLogVerbose(@"AIDatabaseManager:getNumberOfSavedSearches. resultSet: %@", resultSet);
    if ([resultSet next])
    {
        DDLogVerbose(@"AIDatabaseManager:getNumberOfSavedSearches: there is a result.");
        NSInteger result = [resultSet intForColumnIndex:0];
        return_value = [[NSNumber alloc] initWithInt:result];
    }
    else
    {
        DDLogError(@"AIDatabaseManager:getNumberOfSavedSearches: there isn't a result.");
        return_value = nil;
    }
    [resultSet close];
    
    DDLogVerbose(@"AIDatabaseManager:getNumberOfSavedSearches returning: %@", return_value);
    return return_value;
}

- (TravelBotSavedSearch *)getSavedSearch:(NSInteger)index
{
    DDLogVerbose(@"AIDatabaseManager:getSavedSearch entry. index: %d", index);
    
    // -------------------------------------------------------------------------
    //  Initialize output variables.
    // -------------------------------------------------------------------------
    TravelBotSavedSearch *return_value;
    // -------------------------------------------------------------------------
    
    NSDictionary *arguments = $dict([NSNumber numberWithInteger:1], @"limit",
                                    [NSNumber numberWithInteger:index], @"offset");
    FMResultSet *resultSet = [self executeQuery:self.favorites_db
                                          query:FAVOURITES_GET_SAVED_SEARCH_STATEMENT
                                      arguments:arguments];
    DDLogVerbose(@"AIDatabaseManager:getSavedSearch. resultSet: %@", resultSet);
    while ([resultSet next])
    {
        DDLogVerbose(@"AIDatabaseManager:getSavedSearch: there is a result.");
        NSString *fromPlaceName = [resultSet stringForColumnIndex:0];
        NSString *fromCountryCode = [resultSet stringForColumnIndex:1];
        NSString *toPlaceName = [resultSet stringForColumnIndex:2];
        NSString *toCountryCode = [resultSet stringForColumnIndex:3];
        NSDate *searchDatetime = [resultSet dateForColumnIndex:4];
        NSData *journeysSerialized = [resultSet dataForColumnIndex:5];

        TravelBotCountry *fromCountry = [self.countriesConfig member:fromCountryCode];
        TravelBotPlace *fromPlace = [[TravelBotPlace alloc] initWithName:fromPlaceName
                                                                 country:fromCountry];
        TravelBotCountry *toCountry = [self.countriesConfig member:toCountryCode];
        TravelBotPlace *toPlace = [[TravelBotPlace alloc] initWithName:toPlaceName
                                                               country:toCountry];
        NSArray *journeysDeserialized = [NSKeyedUnarchiver unarchiveObjectWithData:journeysSerialized];
        return_value = [[TravelBotSavedSearch alloc] init];
        return_value.fromPlace = fromPlace;
        return_value.toPlace = toPlace;
        return_value.searchDatetime = searchDatetime;
        return_value.searchResults = journeysDeserialized;
    }
    [resultSet close];
    
    DDLogVerbose(@"AIDatabaseManager:getSavedSearch returning: %@", return_value);
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
                resultSet = [database executeQuery:query
                           withParameterDictionary:arguments];
            else
                resultSet = [database executeQuery:query];
            if (!resultSet)
            {
                DDLogError(@"AIDatabaseManager:executeQuery. error. database: %@, query: %@, arguments: %@, error code: %d, error message: %@",
                           database, query, arguments, [database lastErrorCode], [database lastErrorMessage]);
            }
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
        DDLogVerbose(@"AIDatabaseManager:executeUpdate entry. database: %@, update: %@, arguments: %@", database, update, arguments);
        if (![self isOpened])
        {
            DDLogError(@"AIDatabaseManager:executeUpdate: database is not open");
        }
        else
        {
            DDLogVerbose(@"AIDatabaseManager:executeUpdate: database is open.");
            [database executeUpdate:update withParameterDictionary:arguments];
            if ([database lastError])
            {
                DDLogError(@"AIDatabaseManager:executeUpdate. error. database: %@, update: %@, arguments: %@, error code: %d, error message: %@",
                           database, update, arguments, [database lastErrorCode], [database lastErrorMessage]);
            }
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
    
    // Listen for application resigns active.
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(notification:)
                                                 name: UIApplicationWillResignActiveNotification
                                               object: nil];
    
    // Listen for application memory warnings.
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(notification:)
                                                 name: UIApplicationDidReceiveMemoryWarningNotification
                                               object: nil];

}

- (void)notification:(NSNotification *)notification
{
    DDLogVerbose(@"AIDatabaseManager:notification entry.");
    if ($eql(notification.name, UIApplicationDidBecomeActiveNotification))
    {
        DDLogVerbose(@"application did become active notification.");
        [self open];
    }
    else if ($eql(notification.name, UIApplicationWillResignActiveNotification))
    {
        DDLogVerbose(@"application will resign active notification.");
        [self close];
    }
    else if ($eql(notification.name, UIApplicationDidReceiveMemoryWarningNotification))
    {
        [self startProcessingTask];
        dispatch_async(self.processingQueue,
        ^{
            DDLogVerbose(@"application did receive memory notification");
            sqlite3_db_release_memory([self.locations_db sqliteHandle]);
            sqlite3_db_release_memory([self.favorites_db sqliteHandle]);
            [self stopProcessingTask];
        });
    }
    DDLogVerbose(@"AIDatabaseManager:notification exit.");
}

- (void)initDatabaseManager
{
    self.processingQueue = dispatch_queue_create("com.ai.AIDatabaseManager.processingQueue", NULL);
    
    AIConfigManager *configManager = [AIConfigManager sharedInstance];
    self.countriesConfig = [NSSet setWithArray:[configManager getCountries]];
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

- (void)dealloc
{
    self.processingQueue = nil;
    getPlaceWithCountryCodeCache = nil;
}

- (BOOL)isOpened
{
    return ((self.locations_db && self.locations_db.open) &&
            (self.favorites_db && self.favorites_db.open));
}

- (void)open
{
    [self startProcessingTask];
    dispatch_async(self.processingQueue,
    ^{
        DDLogVerbose(@"AIDatabaseManager:open entry.");
        NSString *locationsDbCompressedPath = [[$ documentPath] stringByAppendingPathComponent:LOCATIONS_DATABASE_COMPRESSED_NAME];
        NSString *locationsDbPath = [[$ documentPath] stringByAppendingPathComponent:LOCATIONS_DATABASE_NAME];
        NSString *locationsDbBundlePath = [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"locations.sqlite.bz2"];
        
        // ---------------------------------------------------------------------
        //  Maybe download and decompress the compressed database, if it
        //  doesn't already exist.
        //
        //  TODO some other logic will delete the file before we get here if
        //  e.g. the server tells us there's a new version available.
        // ---------------------------------------------------------------------
        if (!([[NSFileManager defaultManager] fileExistsAtPath:locationsDbCompressedPath isDirectory:NO]))
        {
            DDLogVerbose(@"AIDatabaseManager:open. Locations compressed database does not exist at dbCompressedPath: %@",
                         locationsDbCompressedPath);
            
            // -----------------------------------------------------------------
            // Copy compressed database from bundle resources.
            // -----------------------------------------------------------------
            NSError *error;
            BOOL rc = [self copyDatabase:locationsDbBundlePath
                        dbCompressedPath:locationsDbCompressedPath
                                  dbPath:locationsDbPath
                                   error:&error];
            if (!rc)
            {
                DDLogError(@"Copying locations database from bundle path failed. error: %@",
                           [error localizedDescription]);
            }
            // -----------------------------------------------------------------
            
        } // if locations compressed database doesn't already exist.
        // ---------------------------------------------------------------------

        // ---------------------------------------------------------------------
        //  Open the locations database.
        // ---------------------------------------------------------------------
        DDLogVerbose(@"AIDatabaseManager:open: opening locations database at %@.", locationsDbPath);
        self.locations_db = [FMDatabase databaseWithPath:locationsDbPath];
        if (![self.locations_db open])
        {
            DDLogError(@"AIDatabaseManager:open: failed to open locations database.");
            self.locations_db = nil;
        }
        if (self.locations_db)
        {
            DDLogVerbose(@"AIDatabase:open. initialize the locations database.");
            [self initLocationsDb];
        }
        getPlaceWithCountryCodeCache = [NSMutableDictionary dictionaryWithCapacity:GET_PLACE_WITH_COUNTRY_CODE_CACHE_SIZE];
        DDLogVerbose(@"AIDatabaseManager:open: locations database is open.");
        // ---------------------------------------------------------------------

        // ---------------------------------------------------------------------
        //  Open the favorites database.
        // ---------------------------------------------------------------------
        NSString *favoritesDbPath = [[$ documentPath] stringByAppendingPathComponent:FAVORITES_DATABASE_NAME];
        DDLogVerbose(@"AIDatabaseManager:open: opening favorites database at %@.", favoritesDbPath);
        self.favorites_db = [FMDatabase databaseWithPath:favoritesDbPath];
        if (![self.favorites_db open])
        {
            DDLogError(@"AIDatabaseManager:open: failed to open favorites database.");
            self.favorites_db = nil;
        }
        if (self.favorites_db)
        {
            DDLogVerbose(@"AIDatabase:open. initializing the favorites database.");
            [self initFavoritesDb];
        }
        DDLogVerbose(@"AIDatabaseManager:open: favorites database is open.");
        // ---------------------------------------------------------------------
        
        // ---------------------------------------------------------------------
        //  Notify listeners that the database has opened.
        // ---------------------------------------------------------------------
        if ([self isOpened])
        {
            DDLogVerbose(@"posting notification that database has opened.");
            [[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_DATABASE_OPENED
                                                                object:self];
        }
        // ---------------------------------------------------------------------
        
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
                DDLogError(@"AIDatabaseManager:close: Failed to close locations database.");
            }
            self.locations_db = nil;
            [getPlaceWithCountryCodeCache removeAllObjects];
            getPlaceWithCountryCodeCache = nil;
            if (![self.favorites_db close])
            {
                DDLogError(@"AIDatabaseManager:close: Failed to close favorites database.");
            }
            self.favorites_db = nil;
        }
        [self stopProcessingTask];
   });
}

- (BOOL)copyDatabase:(NSString *)dbBundlePath
    dbCompressedPath:(NSString *)dbCompressedPath
              dbPath:(NSString *)dbPath
               error:(NSError **)error
{
    NSError *localError;
    [[NSFileManager defaultManager] copyItemAtPath:dbBundlePath
                                            toPath:dbCompressedPath
                                             error:&localError];
    if (localError)
    {
        DDLogError(@"AIDatabaseManager:open: error during database copy: %@",
                   [localError localizedDescription]);
        (*error) = [localError copy];
        return NO;
    }
    else
    {
        DDLogVerbose(@"AIDatabaseManager:open: database copied. Decompressing...");
        [AIUtilities bunzip2:dbCompressedPath
              outputFilepath:dbPath];
        return YES;
    }
}

- (BOOL)downloadDatabase:(NSString *)dbCompressedPath
                  dbPath:(NSString *)dbPath
                   error:(NSError **)error
{
    // -------------------------------------------------------------------------
    // Download then decompress the locations database.
    // -------------------------------------------------------------------------
    DDLogVerbose(@"AIDatabaseManager:open: downloading database from S3 bucket %@, key %@, to %@...",
                 AWS_S3_BUCKET, AWS_LOCATIONS_DATABASE_S3_PATH, dbCompressedPath);
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
        assert(error);
        (*error) = request.error;
        return NO;
    }
    else
    {
        DDLogVerbose(@"AIDatabaseManager:open: database downloaded. Decompressing...");
        [AIUtilities bunzip2:dbCompressedPath
              outputFilepath:dbPath];
        return YES;
    } // if (request.error) for database download
}

// -----------------------------------------------------------------------------
//  If they don't already exist set up the tables and indices required for
//  the favorites database. This is already executed on the GCD processing
//  queue as a background task.
// -----------------------------------------------------------------------------
- (void)initFavoritesDb
{
    DDLogVerbose(@"AIDatabaseManager:initFavoritesDb entry. self.favorites_db: %@", self.favorites_db);
    assert(self.favorites_db);
    NSArray *queries = $arr(@"PRAGMA foreign_keys = ON;",
                            FAVOURITES_CREATE_SAVED_SEARCH_STATEMENT,
                            FAVOURITES_CREATE_FAVOURITE_SEARCHES_STATEMENT);
    for (NSString *query in queries)
    {
        DDLogVerbose(@"AIDatabaseManager:initFavoritesDb. executing query: %@", query);
        [self.favorites_db executeUpdate:query];
        if ([self.favorites_db hadError])
        {
            DDLogError(@"AIDatabaseManager:initFavoritesDb. error. query: %@, error code: %d, error message: %@",
                       query, [self.favorites_db lastErrorCode], [self.favorites_db lastErrorMessage]);
        }
    }
    DDLogVerbose(@"AIDatabaseManager:initFavoritesDb exit.");
}
// -----------------------------------------------------------------------------
//  The locationsd database is ready to go, no CREATE or DROP tables needed.
//  However we need to set pragma's if needed.
// -----------------------------------------------------------------------------
- (void)initLocationsDb
{
    DDLogVerbose(@"AIDatabaseManager:initLocationsDb entry. self.locations_db: %@", self.locations_db);
    assert(self.locations_db);
    NSArray *queries = $arr(@"PRAGMA foreign_keys = ON;");
    for (NSString *query in queries)
    {
        DDLogVerbose(@"AIDatabaseManager:initLocationsDb. executing query: %@", query);
        [self.locations_db executeUpdate:query];
        if ([self.locations_db hadError])
        {
            DDLogError(@"AIDatabaseManager:initLocationsDb. error. query: %@, error code: %d, error message: %@",
                       query, [self.locations_db lastErrorCode], [self.locations_db lastErrorMessage]);
        }
    }
    DDLogVerbose(@"AIDatabaseManager:initLocationsDb exit.");
}

@end
