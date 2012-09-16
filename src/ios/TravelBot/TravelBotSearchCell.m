//
//  TravelBotSearchCell.m
//  TravelBot
//
//  Created by Asim Ihsan on 13/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSearchCell.h"
#import "DDLog.h"

static int ddLogLevel = LOG_LEVEL_VERBOSE;

@implementation TravelBotSearchCell

@synthesize departureLabel = _departureLabel;

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    DDLogVerbose(@"TravelBotSearchCell:initWithStyle entry.");
    
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (!self)
        return nil;
    return self;
}

@end
