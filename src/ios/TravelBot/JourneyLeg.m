//
//  Location.m
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "JourneyLeg.h"
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

@implementation JourneyLeg

@synthesize departure = _departure;
@synthesize arrival = _arrival;
@synthesize mode_of_transport = _mode_of_transport;

#pragma mark - Public API.
- (NSDate *)getDepartureDate
{
    return [self.departure.datetime copy];
}

- (NSDate *)getArrivalDate
{
    return [self.arrival.datetime copy];
}

- (NSString *)getDeparturePointName
{
    return [self.departure.location.name copy];
}

- (NSString *)getArrivalPointName
{
    return [self.arrival.location.name copy];
}

- (id)init:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"JourneyLeg:init entry. jsonDictionary: %@", jsonDictionary);
    if (!(self = [super init]))
        return nil;
    if (![self.class validateJsonDictionary:jsonDictionary])
    {
        DDLogError(@"JourneyLeg:init. jsonDictionary is invalid.");
        return nil;
    }
    [self populateUsingJsonDictionary:jsonDictionary];
    return self;
}

+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"JourneyLeg:validateJsonDictionary entry.");
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    BOOL return_value = NO;
    NSDictionary *topLevelKey;
    NSDictionary *departure;
    NSDictionary *arrival;
    NSString *mode_of_transport;
    // -------------------------------------------------------------------------
    
    topLevelKey = [jsonDictionary $for:@"JourneyLeg"];
    if (!topLevelKey)
    {
        DDLogVerbose(@"Top-level key 'JourneyLeg' not found.");
        goto EXIT_LABEL;
    }
    departure = [topLevelKey $for:@"departure"];
    if (!departure)
    {
        DDLogVerbose(@"Second-level key 'departure' not found.");
        goto EXIT_LABEL;
    }
    if (![JourneyLegPoint validateJsonDictionary:departure])
    {
        DDLogVerbose(@"Second-level key 'departure' not valid JourneyLegPoint.");
        goto EXIT_LABEL;
    }
    arrival = [topLevelKey $for:@"arrival"];
    if (!departure)
    {
        DDLogVerbose(@"Second-level key 'arrival' not found.");
        goto EXIT_LABEL;
    }
    if (![JourneyLegPoint validateJsonDictionary:arrival])
    {
        DDLogVerbose(@"Second-level key 'arrival' not valid JourneyLegPoint.");
        goto EXIT_LABEL;
    }
    mode_of_transport = [topLevelKey $for:@"mode_of_transport"];
    if (!mode_of_transport)
    {
        DDLogVerbose(@"Second-level key 'mode_of_transport' not found.");
        goto EXIT_LABEL;
    }
    return_value = YES;
    
EXIT_LABEL:
    DDLogVerbose(@"JourneyLeg:validateJsonDictionary returning: %@.",
                 return_value == YES ? @"YES" : @"NO");
    return return_value;
}

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary
{
    NSDictionary *topLevelKey = [jsonDictionary $for:@"JourneyLeg"];

    NSDictionary *departureDictionary = [topLevelKey $for:@"departure"];
    JourneyLegPoint *departure = [[JourneyLegPoint alloc] init:departureDictionary];
    self.departure = departure;
    
    NSDictionary *arrivalDictionary = [topLevelKey $for:@"arrival"];
    JourneyLegPoint *arrival = [[JourneyLegPoint alloc] init:arrivalDictionary];
    self.arrival = arrival;
    
    NSString *mode_of_transport = [topLevelKey $for:@"mode_of_transport"];
    self.mode_of_transport = mode_of_transport;
}

#pragma mark - NSObject overrides.
- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    JourneyLeg *other = (JourneyLeg *)object;
    if (!$eql(self.departure, other.departure))
        return NO;
    if (!$eql(self.arrival, other.arrival))
        return NO;
    if (!$eql(self.mode_of_transport, other.mode_of_transport))
        return NO;
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.departure.hash;
    result = prime * result + self.arrival.hash;
    result = prime * result + self.mode_of_transport.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{JourneyLeg. departure=%@, arrival: %@, mode_of_transport: %@}",
                self.departure, self.arrival, self.mode_of_transport);
}

@end
