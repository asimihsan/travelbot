//
//  TravelBotSearchCell.m
//  TravelBot
//
//  Created by Asim Ihsan on 13/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSearchCell.h"
#import "Journey.h"
#import "AIUtilities.h"
#import "ConciseKit.h"
#import "DDLog.h"

static int ddLogLevel = LOG_LEVEL_VERBOSE;

@implementation TravelBotSearchCell

@synthesize journey = _journey;
@synthesize departValue = _departValue;
@synthesize arriveValue = _arriveValue;
@synthesize durationValue = _durationValue;
@synthesize legImage1 = _legImage1;
@synthesize legImage2 = _legImage2;
@synthesize legImage3 = _legImage3;
@synthesize legImage4 = _legImage4;
@synthesize legImage5 = _legImage5;

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    DDLogVerbose(@"TravelBotSearchCell:initWithStyle entry.");
    
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (!self)
        return nil;
    return self;
}

- (void)setJourney:(Journey *)journey
{
    // -------------------------------------------------------------------------
    //  Determine departure and arrival strings.
    // -------------------------------------------------------------------------
    NSDate *firstDepartureTime = [journey getFirstDepartureTime];
    NSDate *lastArrivalTime = [journey getLastArrivalTime];
    NSDateFormatter *dateFormatter = [AIUtilities getThreadLocalNSDateFormatter];
    dateFormatter.dateFormat = @"HH:mm";
    NSString *departureString = [dateFormatter stringFromDate:firstDepartureTime];
    NSString *arrivalString = [dateFormatter stringFromDate:lastArrivalTime];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Determine duration string.
    // -------------------------------------------------------------------------
    NSString *durationString = [AIUtilities getDurationFromTwoDates:firstDepartureTime
                                                 secondTimeInterval:lastArrivalTime];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Determine changes images.
    // -------------------------------------------------------------------------
    NSArray *modesOfTransportForChanges = [journey getModesOfTransportForChanges];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Set up the search cell contents.
    // -------------------------------------------------------------------------
    self.departValue.text = departureString;
    self.arriveValue.text = arrivalString;
    self.durationValue.text = durationString;
    NSArray *images = $arr(self.legImage1, self.legImage2, self.legImage3, self.legImage4, self.legImage5);
    NSInteger i;
    for (i = 0; i < modesOfTransportForChanges.count; i++)
    {
        UIImageView *imageView = [images $at:i];
        NSString *modeOfTransport = [modesOfTransportForChanges $at:i];
        NSString *imageName;
        if ($eql(modeOfTransport, @"bus"))
        {
            imageName = @"bus_aiga.png";
        }
        else if ($eql(modeOfTransport, @"train"))
        {
            imageName = @"train_aiga.png";
        } // what modeOfTransport is.
        imageView.image = [UIImage imageNamed:imageName];

    } // loop over modes of transport
    
    // Clear the rest of the images.
    for (; i < images.count; i++)
    {
        UIImageView *imageView = [images $at:i];
        imageView.image = nil;
    }
    // -------------------------------------------------------------------------
} // - (void)setJourney:(Journey *)journey

@end
