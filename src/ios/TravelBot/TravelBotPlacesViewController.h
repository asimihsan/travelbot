//
//  TravelBotPlacesViewController.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol TravelBotPlacesViewControllerDelegate;
@class TravelBotCountry;
@class TravelBotPlace;

@interface TravelBotPlacesViewController : UITableViewController

@property (retain, nonatomic) TravelBotCountry *country;
@property (copy, nonatomic) NSString *placeType;
@property (weak, nonatomic) id <TravelBotPlacesViewControllerDelegate> delegate;

@end

@protocol TravelBotPlacesViewControllerDelegate <NSObject>

- (void)travelBotPlacesViewControllerDidFinish:(TravelBotPlacesViewController *)controller
                                     placeType:(NSString *)placeType
                                         place:(TravelBotPlace *)place;

@end