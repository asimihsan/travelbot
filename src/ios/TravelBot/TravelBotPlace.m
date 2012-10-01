//
//  TravelBotPlace.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotPlace.h"
#import "ConciseKit/ConciseKit.h"

@interface TravelBotPlace ()

@end

@implementation TravelBotPlace

@synthesize name = _name;
@synthesize country = _country;

- (id)init
{
    return [self initWithName:nil country:nil];
}

// Designated initializer.
- (id)initWithName:(NSString *)name
           country:(TravelBotCountry *)country
{
    if (!(self = [super init]))
        return nil;
    self.name = name;
    self.country = country;
    return self;
}

#pragma mark - NSCoding protocol
- (void)encodeWithCoder:(NSCoder *)encoder
{
    [encoder encodeObject:self.name forKey:@"name"];
    [encoder encodeObject:self.country forKey:@"country"];
}

- (id)initWithCoder:(NSCoder *)decoder
{
    if (!(self = [super init]))
        return nil;
    self.name = [decoder decodeObjectForKey:@"name"];
    self.country = [decoder decodeObjectForKey:@"country"];
    return self;
}


- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    TravelBotPlace *other = (TravelBotPlace *)object;
    if (!$eql(self.name, other.name))
        return NO;
    if (!$eql(self.country, other.country))
        return NO;
    
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.name.hash;
    result = prime * result + self.country.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{TravelBotPlace. name=%@, country=%@}", self.name, self.country);
}


@end
