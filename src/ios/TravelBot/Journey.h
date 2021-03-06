//
//  Journey.h
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@class JourneyLeg;

@interface Journey : NSObject
<NSCoding>

- (id)init:(NSDictionary *)jsonDictionary;
+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary;
- (NSComparisonResult)compareByFirstDepartureTime:(Journey *)other;

- (NSDate *)getFirstDepartureTime;
- (NSDate *)getLastArrivalTime;
- (NSInteger)getNumberOfChanges;
- (NSArray *)getModesOfTransportForChanges;
- (NSInteger)getNumberOfLegs;
- (JourneyLeg *)getJourneyLegAt:(NSInteger)index;

@end
