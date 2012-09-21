//
//  TravelBotJourneyViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 21/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol TravelBotJourneyViewControllerDelegate;
@class TravelBotJourneyHeader;
@class Journey;

@interface TravelBotJourneyViewController : UITableViewController

@property (weak, nonatomic) id <TravelBotJourneyViewControllerDelegate> delegate;
@property (weak, nonatomic) IBOutlet TravelBotJourneyHeader *journeyHeaderView;
@property (strong, nonatomic) Journey *journey;
- (IBAction)backButton:(id)sender;

@end

@protocol TravelBotJourneyViewControllerDelegate <NSObject>

- (void)travelBotJourneyViewControllerDelegateDidFinish:(TravelBotJourneyViewController *)controller;

@end