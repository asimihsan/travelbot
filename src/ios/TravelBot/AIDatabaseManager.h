//
//  AIDatabaseManager.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface AIDatabaseManager : NSObject

+ (AIDatabaseManager *)sharedInstance;

- (BOOL)isOpened;

- (void)open;
- (void)close;

@end
