//
//  Journey.m
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "Journey.h"
#import "JourneyLeg.h"
#import "JourneyLegPoint.h"
#import "Location.h"

#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_INFO;
// ----------------------------------------------------------------------------

@interface Journey ()

@property (strong, nonatomic) NSArray *legs;

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary;

@end

@implementation Journey

#pragma mark - Public API
- (NSDate *)getFirstDepartureTime
{
    // -------------------------------------------------------------------------
    //  Validate assumptions,.
    // -------------------------------------------------------------------------
    assert(self.legs);
    assert([self.legs count] > 0);
    // -------------------------------------------------------------------------
    
    JourneyLeg *firstLeg = [self.legs $first];
    NSDate *firstDepartureTime = firstLeg.departure.datetime;
    return [firstDepartureTime copy];
}

- (NSDate *)getLastArrivalTime
{
    // -------------------------------------------------------------------------
    //  Validate assumptions,.
    // -------------------------------------------------------------------------
    assert(self.legs);
    assert([self.legs count] > 0);
    // -------------------------------------------------------------------------
    
    JourneyLeg *lastLeg = [self.legs $last];
    NSDate *lastArrivalTime = lastLeg.arrival.datetime;
    return [lastArrivalTime copy];
}

- (NSInteger)getNumberOfChanges
{
    // -------------------------------------------------------------------------
    //  Validate assumptions.
    // -------------------------------------------------------------------------
    if (!self.legs)
        DDLogError(@"Journey:getNumberOfChanges. self.legs is nil.");
    assert(self.legs);
    // -------------------------------------------------------------------------
    
    NSInteger return_value = [self.legs count] - 1;
    
    // -------------------------------------------------------------------------
    //  Post-validation.
    // -------------------------------------------------------------------------
    if (!(return_value >= 0))
        DDLogError(@"Journey:getNumberOfChanges. return_value %d invalid.", return_value);
    assert(return_value >= 0);
    // -------------------------------------------------------------------------
    
    return return_value;
}

- (NSInteger)getNumberOfLegs
{
    // -------------------------------------------------------------------------
    //  Validate assumptions.
    // -------------------------------------------------------------------------
    assert(self.legs);
    // -------------------------------------------------------------------------
    
    return [self.legs count];
}

- (JourneyLeg *)getJourneyLegAt:(NSInteger)index
{
    // -------------------------------------------------------------------------
    //  Validate assumptions.
    // -------------------------------------------------------------------------
    assert(self.legs);
    assert(index < [self.legs count]);
    assert(index >= 0);
    // -------------------------------------------------------------------------
    
    JourneyLeg *leg = [self.legs $at:index];
    
    // -------------------------------------------------------------------------
    //  Validate post-assumptions.
    // -------------------------------------------------------------------------
    assert(leg);
    // -------------------------------------------------------------------------
    
    return leg;
}

- (id)init:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"Journey:init entry. jsonDictionary: %@", jsonDictionary);
    if (!(self = [super init]))
        return nil;
    if (![self.class validateJsonDictionary:jsonDictionary])
    {
        DDLogError(@"Journey:init. jsonDictionary is invalid.");
        return nil;
    }
    [self populateUsingJsonDictionary:jsonDictionary];
    return self;
}

+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"Journey:validateJsonDictionary entry.");
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    BOOL return_value = NO;
    NSDictionary *topLevelKey;
    NSArray *legsArray;
    __block BOOL isAnyLegInvalid = NO;
    // -------------------------------------------------------------------------
    
    topLevelKey = [jsonDictionary $for:@"Journey"];
    if (!topLevelKey)
    {
        DDLogVerbose(@"Top-level key 'Journey' not found.");
        goto EXIT_LABEL;
    }
    legsArray = [topLevelKey $for:@"legs"];
    if (!legsArray)
    {
        DDLogVerbose(@"Second-level key 'legs' not found.");
        goto EXIT_LABEL;
    }
    [legsArray $each:^(NSDictionary *legDictionary) {
        if (![JourneyLeg validateJsonDictionary:legDictionary])
        {
            DDLogVerbose(@"legDictionary %@ is not valid.", legDictionary);
            isAnyLegInvalid = YES;
        }
    }];
    if (isAnyLegInvalid)
    {
        DDLogVerbose(@"One or more legs are invalid.");
        goto EXIT_LABEL;
    }
    
    return_value = YES;
    
EXIT_LABEL:
    DDLogVerbose(@"Journey:validateJsonDictionary returning: %@.",
                 return_value == YES ? @"YES" : @"NO");
    return return_value;
}

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary
{
    NSDictionary *topLevelKey = [jsonDictionary $for:@"Journey"];
    NSArray *legsDictionaryArray = [topLevelKey $for:@"legs"];
    NSMutableArray *legsObjectArray = [[NSMutableArray alloc] initWithCapacity:legsDictionaryArray.count];
    
    [legsDictionaryArray $each:^(NSDictionary *legDictionary) {
        JourneyLeg *journeyLeg = [[JourneyLeg alloc] init:legDictionary];
        [legsObjectArray $push:journeyLeg];
    }];
    self.legs = [NSArray arrayWithArray:legsObjectArray];
}

- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    Journey *other = (Journey *)object;
    if (!$eql(self.legs, other.legs))
        return NO;
    
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.legs.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{Journey. legs=%@}", self.legs);
}

@end
