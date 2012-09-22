//
//  TravelBotJourneyLegCell.h
//  TravelBot
//
//  Created by Asim Ihsan on 21/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@class JourneyLeg;

@interface TravelBotJourneyLegCell : UITableViewCell

@property (weak, nonatomic) IBOutlet UIImageView *modeOfTransportImage;
@property (strong, nonatomic) JourneyLeg *leg;
@property (weak, nonatomic) IBOutlet UILabel *pointTime;
@property (weak, nonatomic) IBOutlet UILabel *pointName;
@property (weak, nonatomic) IBOutlet UILabel *pointDescription;
@property (weak, nonatomic) IBOutlet UILabel *arrivalFooter;

// Intended to help future optimization to calculate cell height without
// needing to instantiate all the views.
+ (NSString *)getPointTime:(JourneyLeg *)journeyLeg;
+ (NSString *)getPointName:(JourneyLeg *)journeyLeg;
+ (NSString *)getPointDescription:(JourneyLeg *)journeyLeg;
+ (NSString *)getArrivalFooter:(JourneyLeg *)journeyLeg;

@end
