//
//  Location.m
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "JourneyLegPoint.h"
#import "Location.h"
#import "AIUtilities.h"

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

@synthesize location = _location;
@synthesize datetime = _datetime;

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
    NSString *datetimeString;
    NSDate *datetime;
    NSDateFormatter *dateFormatter = [AIUtilities getThreadLocalNSDateFormatter];
    dateFormatter.dateFormat = @"yyyy-MM-dd'T'HH:mm:ss";
    NSDictionary *location;
    // -------------------------------------------------------------------------
    
    topLevelKey = [jsonDictionary $for:@"JourneyLegPoint"];
    if (!topLevelKey)
    {
        DDLogVerbose(@"Top-level key 'JourneyLegPoint' not found.");
        goto EXIT_LABEL;
    }
    datetimeString = [topLevelKey $for:@"datetime"];
    if (!datetimeString)
    {
        DDLogVerbose(@"Second-level key 'datetime' not found.");
        goto EXIT_LABEL;
    }
    datetime = [dateFormatter dateFromString:datetimeString];
    if (!datetime)
    {
        DDLogVerbose(@"Second-level key 'datetime' could not be parsed.");
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
    NSDateFormatter *dateFormatter = [AIUtilities getThreadLocalNSDateFormatter];
    dateFormatter.dateFormat = @"yyyy-MM-dd'T'HH:mm:ss";
    self.datetime = [dateFormatter dateFromString:datetimeString];
    
    NSDictionary *locationDictionary = [topLevelKey $for:@"location"];
    Location *location = [[Location alloc] init:locationDictionary];
    self.location = location;
}

#pragma mark - NSCoding protocol
- (void)encodeWithCoder:(NSCoder *)encoder
{
    [encoder encodeObject:self.datetime forKey:@"datetime"];
    [encoder encodeObject:self.location forKey:@"location"];
}

- (id)initWithCoder:(NSCoder *)decoder
{
    if (!(self = [super init]))
        return nil;
    self.datetime = [decoder decodeObjectForKey:@"datetime"];
    self.location = [decoder decodeObjectForKey:@"location"];
    return self;
}


- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    JourneyLegPoint *other = (JourneyLegPoint *)object;
    if (!$eql(self.datetime, other.datetime))
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
    result = prime * result + self.datetime.hash;
    result = prime * result + self.location.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{JourneyLegPoint. datetime=%@, location: %@}",
                self.datetime, self.location);
}

@end
