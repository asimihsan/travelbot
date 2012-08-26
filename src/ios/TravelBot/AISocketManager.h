//
//  AISocketManager.h
//  TravelBot
//
//  Created by Asim Ihsan on 25/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@class GCDAsyncSocket;

@interface AISocketManager : NSObject

@property (strong, nonatomic) GCDAsyncSocket *socket;

+ (AISocketManager *)sharedInstance;
- (void)connect;
- (void)disconnect;

- (void)writeString:(NSString *)string;

@end
