//
//  AIStoryBoardManager.h
//  TravelBot
//
//  Created by Asim Ihsan on 30/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

/*
 * We need to instantiate storyboard NIBs using:
 *
 * self.storyboard instantiateViewControllerWithIdentifier
 *
 * However, we do not want to create view controllers from scratch if we don't
 * need to. Hence this singleton class will cache pre-instantiated storyboard
 * NIBs.
 *
 */

#import <Foundation/Foundation.h>

@class TravelBotLeftPanelViewController;
@class TravelBotMainMenuViewController;
@class TravelBotSettingsViewController;

@interface AIStoryBoardManager : NSObject

+ (AIStoryBoardManager *)sharedInstance;

- (UIViewController *)getTravelBotLeftPanelViewController:(UIViewController *)caller;
- (UIViewController *)getTravelBotMainMenuViewController:(UIViewController *)caller;
- (UIViewController *)getTravelBotSettingsViewController:(UIViewController *)caller;
- (UIViewController *)getTravelBotFavoritesViewController:(UIViewController *)caller;

@end
