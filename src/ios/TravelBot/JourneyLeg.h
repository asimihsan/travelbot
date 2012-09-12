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

@property (retain, nonatomic) JourneyLegPoint *departure;
@property (retain, nonatomic) JourneyLegPoint *arrival;

- (id)init:(NSDictionary *)jsonDictionary;
+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary;

@end
