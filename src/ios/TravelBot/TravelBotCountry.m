//
//  TravelBotCountry.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotCountry.h"
#import "ConciseKit/ConciseKit.h"

@interface TravelBotCountry ()

@end

@implementation TravelBotCountry

@synthesize name = _name;
@synthesize image = _image;
@synthesize code = _code;

- (id)init
{
    return [self initWithName:nil image:nil code:nil];
}

// Designated initializer.
- (id)initWithName:(NSString *)name
             image:(NSString *)image
              code:(NSString *)code
{
    if (!(self = [super init]))
        return nil;
    self.name = name;
    self.image = image;
    self.code = code;
    return self;
}

- (BOOL)isEqual:(id)object
{
    if (object == self)
        return YES;
    if (!object || ![object isKindOfClass:self.class])
        return NO;
    
    TravelBotCountry *other = (TravelBotCountry *)object;
    if (!$eql(self.name, other.name))
        return NO;
    if (!$eql(self.image, other.image))
        return NO;
    if (!$eql(self.code, other.code))
        return NO;
    
    return YES;
}

- (NSUInteger)hash
{
    // Reference: Effective Java 2nd edition.
    const NSUInteger prime = 31;
    NSUInteger result = 17;
    result = prime * result + self.name.hash;
    result = prime * result + self.image.hash;
    result = prime * result + self.code.hash;
    return result;
}

- (NSString *)description
{
    return $str(@"{TravelBotCountry. name=%@, image=%@, code=%@}", self.name, self.image, self.code);
}

@end
