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

@end
