//
//  TravelBotJourneyLegCell.m
//  TravelBot
//
//  Created by Asim Ihsan on 21/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>
#import "TravelBotJourneyLegCell.h"
#import "JourneyLeg.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"
#import "AIUtilities.h"

// -----------------------------------------------------------------------------
//  Constants.
// -----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// -----------------------------------------------------------------------------

#pragma mark - Private method declarations.
@interface TravelBotJourneyLegCell ()

- (void)refreshCell;

@end

@implementation TravelBotJourneyLegCell

@synthesize leg = _leg;
@synthesize modeOfTransportImage = _modeOfTransportImage;
@synthesize pointTime = _pointTime;
@synthesize pointName = _pointName;
@synthesize pointDescription = _pointDescription;
@synthesize arrivalFooter = _arrivalFooter;

#pragma mark - Public API.
- (id)initWithStyle:(UITableViewCellStyle)style
    reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (!self)
        return nil;
    return self;
}

- (void)setLeg:(JourneyLeg *)leg
{
    _leg = leg;
    [self refreshCell];
}

#pragma mark - Class methods to help height calculations.
+ (NSString *)getPointTime:(JourneyLeg *)journeyLeg
{
    NSDateFormatter *dateFormatter = [AIUtilities getThreadLocalNSDateFormatter];
    NSDate *departureDate = [journeyLeg getDepartureDate];
    dateFormatter.dateFormat = @"HH:mm";
    NSString *departureString = [dateFormatter stringFromDate:departureDate];
    return departureString;
}

+ (NSString *)getPointName:(JourneyLeg *)journeyLeg
{
    return [journeyLeg getDeparturePointName];
}

+ (NSString *)getPointDescription:(JourneyLeg *)journeyLeg
{
    static NSString *descriptionFormat = @"Take %@ to %@.";
    NSString *description = [NSString stringWithFormat:descriptionFormat,
                             journeyLeg.mode_of_transport, journeyLeg.getArrivalPointName];
    return description;
}

+ (NSString *)getArrivalFooter:(JourneyLeg *)journeyLeg
{
    NSDateFormatter *dateFormatter = [AIUtilities getThreadLocalNSDateFormatter];
    NSDate *departureDate = [journeyLeg getDepartureDate];
    NSDate *arrivalDate = [journeyLeg getArrivalDate];
    NSString *durationString = [AIUtilities getDurationFromTwoDates:departureDate
                                                 secondTimeInterval:arrivalDate];
    dateFormatter.dateFormat = @"HH:mm";
    NSString *arrivalString = [dateFormatter stringFromDate:arrivalDate];
    DDLogVerbose(@"!!AI arrivalDate: %@, arrivalString: %@", arrivalDate, arrivalString);
    static NSString *arrivalFooterFormat = @"%@, arrive at %@";
    NSString *arrivalFooter = [NSString stringWithFormat:arrivalFooterFormat,
                               durationString, arrivalString];
    return arrivalFooter;
}

#pragma mark - Private API.
- (void)refreshCell
{
    DDLogVerbose(@"TravelBotJourneyLegCell:refreshCell entry. self.leg: %@", self.leg);
    
    // -------------------------------------------------------------------------
    //  Mode of transport image.
    // -------------------------------------------------------------------------
    if ($eql(self.leg.mode_of_transport, @"bus"))
    {
        DDLogVerbose(@"TravelBotJourneyLegCell:refreshCell. mode_of_transport is bus.");
        [self setCellImage:@"bus_aiga.png"];
    }
    else if ($eql(self.leg.mode_of_transport, @"train"))
    {
        DDLogVerbose(@"TravelBotJourneyLegCell:refreshCell. mode_of_transport is train.");
        [self setCellImage:@"train_aiga.png"];
    }
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Departure time, departure point name.
    // -------------------------------------------------------------------------
    self.pointTime.text = [self.class getPointTime:self.leg];
    self.pointName.text = [self.class getPointName:self.leg];
    [self.pointName sizeToFit];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Leg extended description.
    // -------------------------------------------------------------------------
    self.pointDescription.text = [self.class getPointDescription:self.leg];
    [self.pointDescription sizeToFit];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Arrival footer.
    // -------------------------------------------------------------------------
    self.arrivalFooter.text = [self.class getArrivalFooter:self.leg];
    [self.arrivalFooter sizeToFit];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Layout the cell and its child views.
    //  -   Departure time and image on the left stay untouched.
    //  -   Departure name, description, and arrival footer are a vertical
    //      stack of views.
    //  -   Cell height changes to match its content views, plus some padding.
    // -------------------------------------------------------------------------
    const NSInteger labelPadding = 10;
    const NSInteger cellPadding = 10;
    CGRect pointNameFrame = self.pointName.frame;
    CGRect pointDescriptionFrame = self.pointDescription.frame;
    CGRect arrivalFooterFrame = self.arrivalFooter.frame;
    
    // point description
    pointDescriptionFrame.origin.y = pointNameFrame.origin.y +
                                     pointNameFrame.size.height +
                                     labelPadding;
    self.pointDescription.frame = pointDescriptionFrame;
    
    // arrival footer
    arrivalFooterFrame.origin.y = pointDescriptionFrame.origin.y +
                                  pointDescriptionFrame.size.height +
                                  labelPadding;
    self.arrivalFooter.frame = arrivalFooterFrame;
    
    // cell frame
    CGRect cellFrame = self.frame;
    cellFrame.size.height = arrivalFooterFrame.origin.y +
                            arrivalFooterFrame.size.height +
                            cellPadding;
    self.frame = cellFrame;
    // -------------------------------------------------------------------------
    
}

- (void)setCellImage:(NSString *)imageName
{
    self.modeOfTransportImage.image = [UIImage imageNamed:imageName];
}


@end
