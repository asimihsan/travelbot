//
//  TravelBotSearchViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol TravelBotSearchViewControllerDelegate;
@class TravelBotPlace;
@class TravelBotSearchHeader;

@interface TravelBotSearchViewController : UITableViewController

@property (strong, nonatomic) TravelBotPlace *fromPlace;
@property (strong, nonatomic) TravelBotPlace *toPlace;
@property (weak, nonatomic) id <TravelBotSearchViewControllerDelegate> delegate;
@property (strong, nonatomic) NSMutableArray *searchResults;

@property (weak, nonatomic) IBOutlet TravelBotSearchHeader *searchHeaderView;
- (IBAction)backButton:(id)sender;

@end

@protocol TravelBotSearchViewControllerDelegate <NSObject>

- (void)travelBotSearchViewControllerDidFinish:(TravelBotSearchViewController *)controller;

@end