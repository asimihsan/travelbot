//
//  TravelBotSidePanelViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 29/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSidePanelViewController.h"
#import "AIStoryBoardManager.h"

@interface TravelBotSidePanelViewController ()

@end

@implementation TravelBotSidePanelViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view.
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)awakeFromNib
{
    AIStoryBoardManager *storyBoardManager = [AIStoryBoardManager sharedInstance];
    [self setLeftPanel:[storyBoardManager getTravelBotLeftPanelViewController:self]];
    [self setCenterPanel:[storyBoardManager getTravelBotMainMenuViewController:self]];
}

@end
