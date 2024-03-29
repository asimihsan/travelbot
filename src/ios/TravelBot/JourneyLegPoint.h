//
//  JourneyLegPoint.h
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@class Location;

@interface JourneyLegPoint : NSObject
<NSCoding>

@property (strong, nonatomic) Location *location;
@property (strong, nonatomic) NSDate *datetime;

- (id)init:(NSDictionary *)jsonDictionary;
+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary;

@end
