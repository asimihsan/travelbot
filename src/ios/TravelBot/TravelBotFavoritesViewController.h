//
//  TravelBotFavoritesViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface TravelBotFavoritesViewController : UIViewController
<UITableViewDelegate, UITableViewDataSource>

@property (weak, nonatomic) IBOutlet UITableView *tableView;

@end
