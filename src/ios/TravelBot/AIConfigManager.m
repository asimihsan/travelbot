//
//  AIConfigManager.m
//  TravelBot
//
//  Created by Asim Ihsan on 27/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "AIConfigManager.h"
#import "ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

#pragma mark - Private methods and constants.
@interface AIConfigManager ()

@property (strong, nonatomic) NSDictionary *config;

- (void)initConfigManager;

@end

@implementation AIConfigManager

@synthesize config = _config;

static AIConfigManager *sharedInstance = nil;

#pragma mark - Public API
- (NSArray *)getCountries
{
    NSArray *countries = [self.config $for:@"Countries"];
    return [countries copy];
}

- (NSDictionary *)getCountryCodeToMethods
{
    NSDictionary *countryCodeToMethods = [self.config $for:@"CountryCodeToMethods"];
    return [countryCodeToMethods copy];
}

- (NSString *)getSocketServerHostname
{
    NSDictionary *socketConfig = [self.config $for:@"Socket"];
    return [[socketConfig $for:@"ServerHostname"] copy];
}

- (NSInteger)getSocketServerPort
{
    NSDictionary *socketConfig = [self.config $for:@"Socket"];
    NSNumber *port = [socketConfig $for:@"ServerPort"];
    return port.integerValue;
}

#pragma mark - Private API
- (void)initConfigManager
{
    DDLogVerbose(@"AIConfigManager:initConfigManager entry.");
    NSString *plistPath = [[NSBundle mainBundle] pathForResource:@"config" ofType:@"plist"];
    self.config = [NSDictionary dictionaryWithContentsOfFile:plistPath];
}

#pragma mark - Singleton methods, lifecycle.
+ (void)initialize
{
    DDLogVerbose(@"AIConfigManager:initialize entry.");
    if (self == [AIConfigManager class])
    {
        sharedInstance = [[self alloc] init];
    }
}

+ (AIConfigManager *)sharedInstance
{
    return sharedInstance;
}

- (AIConfigManager *)init
{
    DDLogVerbose(@"AIConfigManager:init entry.");
    self = [super init];
    if (!self)
    {
        return nil;
    }
    
    [self initConfigManager];
    return self;
}

- (void)dealloc
{
    [self setConfig:nil];
}


@end
