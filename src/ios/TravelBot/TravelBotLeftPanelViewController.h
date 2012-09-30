//
//  TravelBotLeftPanelViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 30/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface TravelBotLeftPanelViewController : UITableViewController

@property (weak, nonatomic) IBOutlet UITableViewCell *searchCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *favoritesCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *settingsCell;

@end
