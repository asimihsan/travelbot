//
//  AIStoryBoardManager.m
//  TravelBot
//
//  Created by Asim Ihsan on 30/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "AIStoryBoardManager.h"
#import "ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

@interface AIStoryBoardManager ()

@property (strong, nonatomic) NSMutableDictionary *viewControllerCache;
- (void)initStoryBoardManager;
- (UIViewController *)getCachedViewController:(UIViewController *)caller
                                          key:(NSString *)key;

@end

@implementation AIStoryBoardManager

@synthesize viewControllerCache = _viewControllerCache;

static AIStoryBoardManager *sharedInstance = nil;

#pragma mark - Public API.
- (UIViewController *)getTravelBotLeftPanelViewController:(UIViewController *)caller
{
    static NSString *key = @"LeftPanelController";
    return [self getCachedViewController:caller key:key];
}

- (UIViewController *)getTravelBotMainMenuViewController:(UIViewController *)caller
{
    static NSString *key = @"MainMenuController";
    return [self getCachedViewController:caller key:key];
}

- (UIViewController *)getTravelBotSettingsViewController:(UIViewController *)caller
{
    static NSString *key = @"SettingsController";
    return [self getCachedViewController:caller key:key];
}

- (UIViewController *)getTravelBotFavoritesViewController:(UIViewController *)caller
{
    static NSString *key = @"FavoritesController";
    return [self getCachedViewController:caller key:key];
}

#pragma mark - Private API.

- (void)initStoryBoardManager
{
    self.viewControllerCache = [[NSMutableDictionary alloc] init];
}

- (UIViewController *)getCachedViewController:(UIViewController *)caller
                                          key:(NSString *)key
{
    UIViewController *viewController = [self.viewControllerCache $for:key];
    if (!viewController || ![viewController isViewLoaded])
    {
        viewController = [caller.storyboard instantiateViewControllerWithIdentifier:key];
        [self.viewControllerCache $obj:viewController for:key];
    }
    return viewController;
}


#pragma mark - Singleton methods, lifecycle.
+ (void)initialize
{
    DDLogVerbose(@"AIStoryBoardManager:initialize entry.");
    if (self == [AIStoryBoardManager class])
    {
        sharedInstance = [[self alloc] init];
    }
}

+ (AIStoryBoardManager *)sharedInstance
{
    return sharedInstance;
}

- (AIStoryBoardManager *)init
{
    DDLogVerbose(@"AIStoryBoardManager:init entry.");
    self = [super init];
    if (!self)
    {
        return nil;
    }
    
    [self initStoryBoardManager];
    return self;
}

- (void)dealloc
{
    [self setViewControllerCache:nil];
}


@end
