//
//  TravelBotFavoriteCell.h
//  TravelBot
//
//  Created by Asim Ihsan on 01/10/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TravelBotSavedSearch;

@interface TravelBotFavoriteCell : UITableViewCell

@property (weak, nonatomic) IBOutlet UILabel *fromLabel;
@property (weak, nonatomic) IBOutlet UILabel *toLabel;
@property (weak, nonatomic) IBOutlet UILabel *searchDatetimeLabel;
@property (weak, nonatomic) TravelBotSavedSearch *savedSearch;

@end
