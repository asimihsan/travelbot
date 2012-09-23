//
//  TravelBotSearchCell.h
//  TravelBot
//
//  Created by Asim Ihsan on 13/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@class Journey;

@interface TravelBotSearchCell : UITableViewCell

@property (strong, nonatomic) Journey *journey;

@property (weak, nonatomic) IBOutlet UILabel *departValue;
@property (weak, nonatomic) IBOutlet UILabel *arriveValue;
@property (weak, nonatomic) IBOutlet UILabel *durationValue;

@property (weak, nonatomic) IBOutlet UIImageView *legImage1;
@property (weak, nonatomic) IBOutlet UIImageView *legImage2;
@property (weak, nonatomic) IBOutlet UIImageView *legImage3;
@property (weak, nonatomic) IBOutlet UIImageView *legImage4;
@property (weak, nonatomic) IBOutlet UIImageView *legImage5;

@end
