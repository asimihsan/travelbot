//
//  TravelBotMainMenuViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TravelBotCountry;

@interface TravelBotMainMenuViewController : UITableViewController

@property (retain, nonatomic) TravelBotCountry *selectedCountry;

@property (weak, nonatomic) IBOutlet UILabel *countryLabel;
@property (weak, nonatomic) IBOutlet UITableViewCell *fromLabelContainerCell;
@property (weak, nonatomic) IBOutlet UILabel *fromLabel;
@property (weak, nonatomic) IBOutlet UITableViewCell *toLabelContainerCell;
@property (weak, nonatomic) IBOutlet UILabel *toLabel;
@property (weak, nonatomic) IBOutlet UITableViewCell *searchButtonContainerCell;
@property (weak, nonatomic) IBOutlet UIButton *searchButton;

@end
