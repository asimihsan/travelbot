//
//  JourneyLeg.h
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@class JourneyLegPoint;

@interface JourneyLeg : NSObject

@property (strong, nonatomic) JourneyLegPoint *departure;
@property (strong, nonatomic) JourneyLegPoint *arrival;
@property (copy, nonatomic) NSString *mode_of_transport;

- (id)init:(NSDictionary *)jsonDictionary;
+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary;

- (NSDate *)getDepartureDate;
- (NSDate *)getArrivalDate;
- (NSString *)getDeparturePointName;
- (NSString *)getArrivalPointName;

@end
