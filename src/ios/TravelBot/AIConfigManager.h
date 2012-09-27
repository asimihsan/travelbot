//
//  AIConfigManager.h
//  TravelBot
//
//  Created by Asim Ihsan on 27/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface AIConfigManager : NSObject

+ (AIConfigManager *)sharedInstance;

// Countries.
- (NSArray *)getCountries;

// Task names for country codes.
- (NSDictionary *)getCountryCodeToMethods;

// Socket config.
- (NSString *)getSocketServerHostname;
- (NSInteger)getSocketServerPort;

@end
