//
//  TravelBotSearchHeader.m
//  TravelBot
//
//  Created by Asim Ihsan on 13/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSearchHeader.h"

@interface TravelBotSearchHeader ()

@end

@implementation TravelBotSearchHeader

@synthesize from = _from;
@synthesize to = _to;
@synthesize when = _when;

- (id)initWithTableView:(UIView *)tableView
                   from:(NSString *)from
                     to:(NSString *)to
                   when:(NSString *)when
{
    self.from = from;
    self.to = to;
    self.when = when;
    
    CGRect headerViewRect = CGRectMake(0,
                                       0,
                                       tableView.frame.size.width,
                                       100);
    self = [super initWithFrame:headerViewRect];
    if (!self)
        return nil;
    
    CGRect fromLabelRect = CGRectMake(60,
                                      0,
                                      self.frame.size.width - 120.0,
                                      30);
    UILabel *fromLabel = [[UILabel alloc] initWithFrame:fromLabelRect];
    
    fromLabel.textAlignment = UITextAlignmentLeft;
    fromLabel.text = self.from;
    fromLabel.backgroundColor = [UIColor clearColor];
    
    [self addSubview:fromLabel];
    
    return self;
}

@end
