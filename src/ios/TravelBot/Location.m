//
//  Location.m
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "Location.h"

#import "JSONKit/JSONKit.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_INFO;
// ----------------------------------------------------------------------------

@interface Location ()

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary;

@end

@implementation Location

@synthesize name = _name;

- (id)init:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"Location:init entry. jsonDictionary: %@", jsonDictionary);
    if (!(self = [super init]))
        return nil;
    if (![self.class validateJsonDictionary:jsonDictionary])
    {
        DDLogError(@"Location:init. jsonDictionary is invalid.");
        return nil;
    }
    [self populateUsingJsonDictionary:jsonDictionary];
    return self;
}

+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary
{
    DDLogVerbose(@"Location:validateJsonDictionary entry.");
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    BOOL return_value = NO;
    NSDictionary *topLevelKey;
    NSString *name;
    // -------------------------------------------------------------------------
    
    topLevelKey = [jsonDictionary $for:@"Location"];
    if (!topLevelKey)
    {
        DDLogVerbose(@"Top-level key 'Location' not found.");
        goto EXIT_LABEL;
    }
    name = [topLevelKey $for:@"name"];
    if (!name)
    {
        DDLogVerbose(@"Second-level key 'name' not found.");
        goto EXIT_LABEL;
    }
    return_value = YES;
    
EXIT_LABEL:
    DDLogVerbose(@"Location:validateJsonDictionary returning: %@.",
                 return_value == YES ? @"YES" : @"NO");
    return return_value;
}

- (void)populateUsingJsonDictionary:(NSDictionary *)jsonDictionary
{
    NSDictionary *topLevelKey = [jsonDictionary $for:@"Location"];
    NSString *name = [topLevelKey $for:@"name"];
    self.name = name;
}

- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    Location *other = (Location *)object;
    if (!$eql(self.name, other.name))
        return NO;
    
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.name.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{Location. name=%@}", self.name);
}

@end
