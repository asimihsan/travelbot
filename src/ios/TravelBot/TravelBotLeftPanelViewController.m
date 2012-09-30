//
//  TravelBotLeftPanelViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 30/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotLeftPanelViewController.h"
#import "JASidePanelController.h"
#import "UIViewController+JASidePanel.h"
#import "AIStoryBoardManager.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

@interface TravelBotLeftPanelViewController ()

@end

@implementation TravelBotLeftPanelViewController

@synthesize searchCell = _searchCell;
@synthesize favoritesCell = _favoritesCell;
@synthesize settingsCell = _settingsCell;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (!self)
    {
        return nil;
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotLeftPanelViewController:didSelectRowAtIndexPath entry. indexPath: %@", indexPath);
    AIStoryBoardManager *storyBoardManager = [AIStoryBoardManager sharedInstance];
    UITableViewCell *cellClicked = [tableView cellForRowAtIndexPath:indexPath];
    if ($eql(cellClicked, self.searchCell))
    {
        DDLogVerbose(@"TravelBotLeftPanelViewController:didSelectRowAtIndexPath. search cell.");
        [self.sidePanelController setCenterPanel:[storyBoardManager getTravelBotMainMenuViewController:self]];
        [self.sidePanelController setLeftPanel:[storyBoardManager getTravelBotLeftPanelViewController:self]];
    }
    else if ($eql(cellClicked, self.favoritesCell))
    {
        DDLogVerbose(@"TravelBotLeftPanelViewController:didSelectRowAtIndexPath. favorites cell.");
        [self.sidePanelController setCenterPanel:[storyBoardManager getTravelBotFavoritesViewController:self]];
        [self.sidePanelController setLeftPanel:[storyBoardManager getTravelBotLeftPanelViewController:self]];
    }
    else if ($eql(cellClicked, self.settingsCell))
    {
        DDLogVerbose(@"TravelBotLeftPanelViewController:didSelectRowAtIndexPath. settings cell.");
        [self.sidePanelController setCenterPanel:[storyBoardManager getTravelBotSettingsViewController:self]];
        [self.sidePanelController setLeftPanel:[storyBoardManager getTravelBotLeftPanelViewController:self]];
    }
}

- (void)viewDidUnload {
    [self setSearchCell:nil];
    [self setFavoritesCell:nil];
    [self setSettingsCell:nil];
    [super viewDidUnload];
}
@end
