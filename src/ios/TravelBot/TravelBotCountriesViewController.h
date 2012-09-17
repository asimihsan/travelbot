//
//  TravelBotCountriesViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol TravelBotCountriesViewControllerDelegate;
@class TravelBotCountry;

@interface TravelBotCountriesViewController : UITableViewController

@property (strong, nonatomic) NSArray *countries;
@property (weak, nonatomic) id <TravelBotCountriesViewControllerDelegate> delegate;

@end

@protocol TravelBotCountriesViewControllerDelegate <NSObject>

- (void)travelBotCountriesViewControllerDidFinish:(TravelBotCountriesViewController *)controller
                                          country:(TravelBotCountry *)country;

@end
