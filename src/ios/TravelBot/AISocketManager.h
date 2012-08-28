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
static NSString *NOTIFICATION_SOCKET_TASK_FINISHED = @"AISocketManager:notificationSocketTaskFinished";

@class GCDAsyncSocket;

@interface AISocketManager : NSObject

@property (strong, nonatomic) GCDAsyncSocket *socket;
@property (nonatomic, assign) BOOL isConnected;

+ (AISocketManager *)sharedInstance;
- (void)connect;
- (void)disconnect;

- (void)writeString:(NSString *)string;

@end
