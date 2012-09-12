//
//  Location.m
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "JourneyLegPoint.h"
#import "Location.h"

#import "JSONKit/JSONKit.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_INFO;
// ----------------------------------------------------------------------------

@interface JourneyLegPoint ()

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary;

@end

@implementation JourneyLegPoint

@synthesize datetimeString = _datetimeString;
@synthesize location = _location;

- (id)init:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"JourneyLegPoint:init entry. jsonDictionary: %@", jsonDictionary);
    if (!(self = [super init]))
        return nil;
    if (![self.class validateJsonDictionary:jsonDictionary])
    {
        DDLogError(@"JourneyLegPoint:init. jsonDictionary is invalid.");
        return nil;
    }
    [self populateUsingJsonDictionary:jsonDictionary];
    return self;
}

+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"JourneyLegPoint:validateJsonDictionary entry.");
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    BOOL return_value = NO;
    NSDictionary *topLevelKey;
    NSString *datetime;
    NSDictionary *location;
    // -------------------------------------------------------------------------
    
    topLevelKey = [jsonDictionary $for:@"JourneyLegPoint"];
    if (!topLevelKey)
    {
        DDLogVerbose(@"Top-level key 'JourneyLegPoint' not found.");
        goto EXIT_LABEL;
    }
    datetime = [topLevelKey $for:@"datetime"];
    if (!datetime)
    {
        DDLogVerbose(@"Second-level key 'datetime' not found.");
        goto EXIT_LABEL;
    }
    location = [topLevelKey $for:@"location"];
    if (!location)
    {
        DDLogVerbose(@"Second-level key 'location' not found.");
        goto EXIT_LABEL;
    }
    if (![Location validateJsonDictionary:location])
    {
        DDLogVerbose(@"Second-level key 'location' not valid Location");
        goto EXIT_LABEL;
    }
    return_value = YES;
    
EXIT_LABEL:
    DDLogVerbose(@"JourneyLegPoint:validateJsonDictionary returning: %@.",
                 return_value == YES ? @"YES" : @"NO");
    return return_value;
}

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary
{
    NSDictionary *topLevelKey = [jsonDictionary $for:@"JourneyLegPoint"];
    NSString *datetimeString = [topLevelKey $for:@"datetime"];
    self.datetimeString = datetimeString;
    
    NSDictionary *locationDictionary = [topLevelKey $for:@"location"];
    Location *location = [[Location alloc] init:locationDictionary];
    self.location = location;
}

- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    JourneyLegPoint *other = (JourneyLegPoint *)object;
    if (!$eql(self.datetimeString, other.datetimeString))
        return NO;
    if (!$eql(self.location, other.location))
        return NO;
    
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.datetimeString.hash;
    result = prime * result + self.location.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{Location. datetimeString=%@, location: %@}",
                self.datetimeString, self.location);
}

@end