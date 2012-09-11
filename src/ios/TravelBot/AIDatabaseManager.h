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

@interface AIDatabaseManager : NSObject

+ (AIDatabaseManager *)sharedInstance;

- (BOOL)isOpened;
- (void)open;
- (void)close;

- (NSNumber *)getNumberOfPlaces:(NSString *)countryCode;
- (NSString *)getPlaceWithCountryCode:(NSString *)countryCode
                               filter:(NSString *)filter
                                index:(NSNumber *)index;

@end
