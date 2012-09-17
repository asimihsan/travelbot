//
//  TravelBotSearchHeader.m
//  TravelBot
//
//  Created by Asim Ihsan on 13/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSearchHeader.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

@interface TravelBotSearchHeader ()

@end

@implementation TravelBotSearchHeader

@synthesize toValueLabel = _toValueLabel;
@synthesize fromValueLabel = _fromValueLabel;
@synthesize whenValueLabel = _whenValueLabel;

- (id)initWithCoder:(NSCoder *)aDecoder
{
    DDLogVerbose(@"TravelBotSearchHeader:initWithCoder entry.");
    self = [super initWithCoder:aDecoder];
    if (self)
    {
        // init here.
    }
    return self;
}

- (void)awakeFromNib
{
    DDLogVerbose(@"TravelBotSearchHeader:awakeFromNib entry.");
}

- (id)initWithTableView:(UIView *)tableView
              fromLabel:(UILabel *)fromLabel
                toLabel:(UILabel *)toLabel
              whenLabel:(UILabel *)whenLabel
                   from:(NSString *)from
                     to:(NSString *)to
                   when:(NSString *)when;
{
    DDLogVerbose(@"TravelBotSearchHeader:initWithTableView entry.");

    // -------------------------------------------------------------------------
    //  Initialize using the main rect.
    // -------------------------------------------------------------------------
    CGRect headerViewRect = CGRectMake(0,
                                       0,
                                       tableView.frame.size.width,
                                       88.0);
    self = [super initWithFrame:headerViewRect];
    if (!self)
        return nil;
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  Set up the labels.
    // -------------------------------------------------------------------------
    fromLabel.text = from;
    toLabel.text = to;
    whenLabel.text = when;
    // -------------------------------------------------------------------------

    return self;
}

@end
