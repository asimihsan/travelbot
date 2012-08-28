//
//  AISocketManager.h
//  TravelBot
//
//  Created by Asim Ihsan on 25/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

static NSString *NOTIFICATION_SOCKET_OPENED = @"AISocketManager:notificationSocketOpened";
static NSString *NOTIFICATION_SOCKET_CLOSED = @"AISocketManager:notificationSocketClosed";

@class GCDAsyncSocket;

@interface AISocketManager : NSObject

@property (strong, nonatomic) GCDAsyncSocket *socket;
@property (nonatomic, assign) BOOL isConnected;

+ (AISocketManager *)sharedInstance;
- (void)connect;
- (void)disconnect;

- (NSString *)writeDictionary:(NSDictionary *)dictionary;

@end
