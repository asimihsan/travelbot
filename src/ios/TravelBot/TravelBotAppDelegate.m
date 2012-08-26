//
//  TravelBotAppDelegate.m
//  TravelBot
//
//  Created by Asim Ihsan on 26/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotAppDelegate.h"
#import "AISocketManager.h"
#import "CocoaLumberJack/DDTTYLogger.h"
#import "JSONKit/JSONKit.h"
#import "ConciseKit/ConciseKit.h"

static const int ddLogLevel = LOG_LEVEL_VERBOSE;

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
    AISocketManager *socketManager = [AISocketManager sharedInstance];
    
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
    NSDictionary *request = $dict(@"1.0", @"version",
                                  @"request_tag", @"1",
                                  @"task", @"type",
                                  @"slovenia.bus_ap.get_locations", @"method");
    NSString *request_string = [request JSONString];
    DDLogVerbose(@"request_string: %@", request_string);
    [socketManager writeString:request_string];
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

@end
