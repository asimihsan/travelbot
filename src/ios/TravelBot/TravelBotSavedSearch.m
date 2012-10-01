//
//  TravelBotSavedSearch.m
//  TravelBot
//
//  Created by Asim Ihsan on 01/10/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSavedSearch.h"
#import "TravelBotPlace.h"
#import "ConciseKit.h"

@interface TravelBotSavedSearch ()

@end

@implementation TravelBotSavedSearch

@synthesize fromPlace = _fromPlace;
@synthesize toPlace = _toPlace;
@synthesize searchDatetime = _searchDatetime;
@synthesize searchResults = _searchResults;

#pragma mark - NSCoding protocol
- (void)encodeWithCoder:(NSCoder *)encoder
{
    [encoder encodeObject:self.fromPlace forKey:@"fromPlace"];
    [encoder encodeObject:self.toPlace forKey:@"toPlace"];
    [encoder encodeObject:self.searchDatetime forKey:@"searchDatetime"];
    [encoder encodeObject:self.searchResults forKey:@"searchResults"];
}

- (id)initWithCoder:(NSCoder *)decoder
{
    if (!(self = [super init]))
        return nil;
    self.fromPlace = [decoder decodeObjectForKey:@"fromPlace"];
    self.toPlace = [decoder decodeObjectForKey:@"toPlace"];
    self.searchDatetime = [decoder decodeObjectForKey:@"searchDatetime"];
    self.searchResults = [decoder decodeObjectForKey:@"searchResults"];
    return self;
}

- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    TravelBotSavedSearch *other = (TravelBotSavedSearch *)object;
    if (!$eql(self.fromPlace, other.fromPlace))
        return NO;
    if (!$eql(self.toPlace, other.toPlace))
        return NO;
    if (!$eql(self.searchDatetime, other.searchDatetime))
        return NO;
    if (!$eql(self.searchResults, other.searchResults))
        return NO;
    
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.fromPlace.hash;
    result = prime * result + self.toPlace.hash;
    result = prime * result + self.searchDatetime.hash;
    result = prime * result + self.searchResults.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{TravelBotPlace. fromPlace=%@, toPlace=%@, searchDatetime=%@, searchResults=%@}",
                self.fromPlace, self.toPlace, self.searchDatetime, self.searchResults);
}

@end
