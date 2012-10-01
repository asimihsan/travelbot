//
//  AIDatabaseManager.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

static NSString *NOTIFICATION_DATABASE_OPENED = @"AIDatabaseManager:notificationDatabaseOpened";

@class FMResultSet;
@class TravelBotPlace;
@class TravelBotSavedSearch;

@interface AIDatabaseManager : NSObject

+ (AIDatabaseManager *)sharedInstance;

- (BOOL)isOpened;
- (void)open;
- (void)close;

- (NSNumber *)getNumberOfPlaces:(NSString *)countryCode
                         search:(NSString *)search;
- (NSString *)getPlaceWithCountryCode:(NSString *)countryCode
                               search:(NSString *)search
                                index:(NSInteger)index;

- (void)addSearchResultsToSavedSearches:(TravelBotPlace *)fromPlace
                                toPlace:(TravelBotPlace *)toPlace
                         searchDatetime:(NSDate *)searchDatetime
                          searchResults:(NSArray *)searchResults;
- (NSNumber *)getNumberOfSavedSearches;
- (TravelBotSavedSearch *)getSavedSearch:(NSInteger)index;

@end
