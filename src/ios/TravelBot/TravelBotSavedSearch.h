//
//  TravelBotSavedSearch.h
//  TravelBot
//
//  Created by Asim Ihsan on 01/10/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@class TravelBotPlace;

@interface TravelBotSavedSearch : NSObject
<NSCoding>

@property (strong, nonatomic) TravelBotPlace *fromPlace;
@property (strong, nonatomic) TravelBotPlace *toPlace;
@property (strong, nonatomic) NSDate *searchDatetime;
@property (strong, nonatomic) NSArray *searchResults;

@end
