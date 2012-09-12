//
//  TravelBotSearchViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TravelBotPlace;

@interface TravelBotSearchViewController : UITableViewController

@property (retain, nonatomic) TravelBotPlace *fromPlace;
@property (retain, nonatomic) TravelBotPlace *toPlace;

@end
