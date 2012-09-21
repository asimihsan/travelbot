//
//  TravelBotAppDelegate.m
//  TravelBot
//
//  Created by Asim Ihsan on 26/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotAppDelegate.h"
#import "TravelBotMainMenuViewController.h"
#import "AISocketManager.h"
#import "AIDatabaseManager.h"
#import "CocoaLumberJack/DDTTYLogger.h"
#import "JSONKit/JSONKit.h"
#import "ConciseKit/ConciseKit.h"

static int ddLogLevel = LOG_LEVEL_VERBOSE;

@interface TravelBotAppDelegate ()

- (void)notification:(NSNotification *)notification;

@end

@implementation TravelBotAppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // ------------------------------------------------------------------------
    //  Set up logging.
    // ------------------------------------------------------------------------
    [DDLog addLogger:[DDTTYLogger sharedInstance]];
    // ------------------------------------------------------------------------
    
    DDLogVerbose(@"TravelBotAppDelegate:application didFinishLaunchingWithOptions entry.");
    
    // ------------------------------------------------------------------------
    //  Access the socket manager singleton. If it hasn't been initalized this
    //  will initialize it.
    // ------------------------------------------------------------------------
    [AISocketManager sharedInstance];

    // ------------------------------------------------------------------------
    //  Access the database manager singleton. If it hasn't been initalized
    //  this will initialize it.
    // ------------------------------------------------------------------------
    [AIDatabaseManager sharedInstance];
    
    // ------------------------------------------------------------------------
    //  Subscribe to notifications about when the database and socket are
    //  opened. When both are open then perform some expensive background
    //  operations.
    // ------------------------------------------------------------------------
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(notification:)
                                                 name:NOTIFICATION_DATABASE_OPENED
                                               object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(notification:)
                                                 name:NOTIFICATION_SOCKET_OPENED
                                               object:nil];
    // ------------------------------------------------------------------------
    
    
    
    //!!AI hard coded JSON request.
    /*
    NSDictionary *kwargs = $dict(@"Ljubljana", @"from_location",
                                 @"Bled", @"to_location");

    NSDictionary *request = $dict(@"1.0", @"version",
                                  @"request_tag", @"tag",
                                  @"task", @"type",
                                  @"slovenia.bus_ap.get_journeys", @"method",
                                  kwargs, @"kwargs");
    */
    /*
    NSDictionary *request = $dict(@"1.0", @"version",
                                  @"request_tag", @"1",
                                  @"task", @"type",
                                  @"slovenia.bus_ap.get_locations", @"method");
    */
    /*
    NSString *request_string = [request JSONString];
    DDLogVerbose(@"request_string: %@", request_string);
    [socketManager writeString:request_string];
     */
    // ------------------------------------------------------------------------

    return YES;
}
							
- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

- (void)notification:(NSNotification *)notification
{
    DDLogVerbose(@"TravelBotAppDelegate received notification: %@", notification);
    if ($eql(notification.name, NOTIFICATION_DATABASE_OPENED))
    {
        DDLogVerbose(@"TravelBotAppDelegate: database is open.");
    }
    else if ($eql(notification.name, NOTIFICATION_SOCKET_OPENED))
    {
        DDLogVerbose(@"TravelBotAppDelegate: socket is open.");
    }
    
    AIDatabaseManager *databaseManager = [AIDatabaseManager sharedInstance];
    AISocketManager *socketManager = [AISocketManager sharedInstance];
    if (([databaseManager isOpened] && [socketManager isConnected]))
    {
        DDLogVerbose(@"Both database and socket are up.");
    }
}

- (void)application:(UIApplication *)application didReceiveLocalNotification:(UILocalNotification *)notification {
    if (application.applicationState == UIApplicationStateInactive )
    {
        //The application received the notification from an inactive state, i.e. the user tapped the "View" button for the alert.
        //If the visible view controller in your view controller stack isn't the one you need then show the right one.
        DDLogVerbose(@"TravelBotAppDelegate:didReceiveLocalNotification. Application was inactive.");
    }
    
    if(application.applicationState == UIApplicationStateActive )
    {
        //The application received a notification in the active state, so you can display an alert view or do something appropriate.
        DDLogVerbose(@"TravelBotAppDelegate:didReceiveLocalNotification. Application was active.");
    }
}
/*
 !!AI temporary code about how to send a notification. Above is how the app
 delegate can detect notification both in foreground and in background.
 
 NSDate *now = [NSDate dateWithTimeIntervalSinceNow:5];
 [[UIApplication sharedApplication] cancelAllLocalNotifications];
 UILocalNotification *localNotification = [[UILocalNotification alloc] init];
 localNotification.fireDate = now;
 localNotification.timeZone = [NSTimeZone defaultTimeZone];
 localNotification.alertBody = [NSString stringWithFormat:@"Your notification"];
 localNotification.alertAction = @"Action";
 localNotification.soundName = UILocalNotificationDefaultSoundName;
 localNotification.applicationIconBadgeNumber = 1;
 [[UIApplication sharedApplication] scheduleLocalNotification:localNotification];
 
*/

@end
