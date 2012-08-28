//
//  TravelBotCountryCell.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>
#import "TravelBotCountryCell.h"

@implementation TravelBotCountryCell

@synthesize label = _label;
@synthesize image = _image;

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)setSelected:(BOOL)selected animated:(BOOL)animated
{
    [super setSelected:selected animated:animated];

    // Configure the view for the selected state
}

- (NSString *)getCellText
{
    return [[NSString alloc] initWithString:self.label.text];
}

- (void)setCellText:(NSString *)text
{
    self.label.text = text;
}

- (void)setCellImage:(NSString *)imageName
{
    self.image.image = [UIImage imageNamed:imageName];
    [self.image.layer setBorderColor: [[UIColor blackColor] CGColor]];
    [self.image.layer setBorderWidth: 1.0];
}

@end
