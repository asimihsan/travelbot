//
//  TravelBotCountryCell.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface TravelBotCountryCell : UITableViewCell

@property (weak, nonatomic) IBOutlet UILabel *label;
@property (weak, nonatomic) IBOutlet UIImageView *image;

- (NSString *)getCellText;
- (void)setCellText:(NSString *)text;
- (void)setCellImage:(NSString *)imageName;

@end
