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
    // -------------------------------------------------------------------------
    //  Get copies of the input variables.
    // -------------------------------------------------------------------------
    self.from = from;
    self.to = to;
    self.when = when;
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Initialize using the main rect.
    // -------------------------------------------------------------------------
    CGRect headerViewRect = CGRectMake(0,
                                       0,
                                       tableView.frame.size.width,
                                       60);
    self = [super initWithFrame:headerViewRect];
    if (!self)
        return nil;
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  From header label.
    // -------------------------------------------------------------------------
    CGRect fromHeaderLabelRect = CGRectMake(10,
                                            0,
                                            self.frame.size.width - 120.0,
                                            40);
    UILabel *fromHeaderLabel = [[UILabel alloc] initWithFrame:fromHeaderLabelRect];
    
    fromHeaderLabel.textAlignment = UITextAlignmentLeft;
    fromHeaderLabel.text = @"From";
    [fromHeaderLabel setFont:[UIFont boldSystemFontOfSize:17.0]];
    fromHeaderLabel.backgroundColor = [UIColor clearColor];
    [self addSubview:fromHeaderLabel];
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  From detail label.
    // -------------------------------------------------------------------------
    CGRect fromDetailLabelRect = CGRectMake(80,
                                            5,
                                            self.frame.size.width - 120.0,
                                            30);
    UILabel *fromDetailLabel = [[UILabel alloc] initWithFrame:fromDetailLabelRect];
    
    fromDetailLabel.textAlignment = UITextAlignmentLeft;
    fromDetailLabel.text = self.from;
    fromDetailLabel.font = [UIFont systemFontOfSize:17.0];
    fromDetailLabel.backgroundColor = [UIColor clearColor];
    [self addSubview:fromDetailLabel];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  To header label.
    // -------------------------------------------------------------------------
    CGRect toHeaderLabelRect = CGRectMake(10,
                                          20,
                                          self.frame.size.width - 120.0,
                                          40);
    UILabel *toHeaderLabel = [[UILabel alloc] initWithFrame:toHeaderLabelRect];
    
    toHeaderLabel.textAlignment = UITextAlignmentLeft;
    toHeaderLabel.text = @"To";
    [toHeaderLabel setFont:[UIFont boldSystemFontOfSize:17.0]];
    toHeaderLabel.backgroundColor = [UIColor clearColor];
    [self addSubview:toHeaderLabel];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  From detail label.
    // -------------------------------------------------------------------------
    CGRect toDetailLabelRect = CGRectMake(80,
                                          25,
                                          self.frame.size.width - 120.0,
                                          30);
    UILabel *toDetailLabel = [[UILabel alloc] initWithFrame:toDetailLabelRect];
    
    toDetailLabel.textAlignment = UITextAlignmentLeft;
    toDetailLabel.text = self.to;
    toDetailLabel.font = [UIFont systemFontOfSize:17.0];
    toDetailLabel.backgroundColor = [UIColor clearColor];
    [self addSubview:toDetailLabel];
    // -------------------------------------------------------------------------
    
    return self;
}

@end
