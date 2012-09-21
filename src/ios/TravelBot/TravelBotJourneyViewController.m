//
//  TravelBotJourneyViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 21/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotJourneyViewController.h"
#import "Journey.h"
#import "TravelBotJourneyHeader.h"

#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

@interface TravelBotJourneyViewController ()

@end

@implementation TravelBotJourneyViewController

@synthesize journey = _journey;

#pragma mark - View lifecycle
- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
}

- (void)viewWillAppear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotJourneyViewController:viewWillAppear entry.");
    
    // -------------------------------------------------------------------------
    //  Validate assumptions.
    // -------------------------------------------------------------------------
    if (!self.journey)
    {
        DDLogError(@"self.journey is nil.");
    }
    assert(self.journey);
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Determine departure and arrival string.
    // -------------------------------------------------------------------------
    DDLogVerbose(@"self.journey: %@", self.journey);
    NSDate *firstDepartureTime = [self.journey getFirstDepartureTime];
    NSDate *lastArrivalTime = [self.journey getLastArrivalTime];
    NSString *departureString = [NSDateFormatter localizedStringFromDate:firstDepartureTime
                                                               dateStyle:NSDateFormatterMediumStyle
                                                               timeStyle:NSDateFormatterMediumStyle];
    NSString *arrivalString = [NSDateFormatter localizedStringFromDate:lastArrivalTime
                                                             dateStyle:NSDateFormatterMediumStyle
                                                             timeStyle:NSDateFormatterMediumStyle];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Determine duration string.
    // -------------------------------------------------------------------------
    NSTimeInterval duration = [lastArrivalTime timeIntervalSinceDate:firstDepartureTime];
    div_t duration_hours_division = div(duration, 3600);
    int duration_hours = duration_hours_division.quot;
    div_t duration_minutes_division = div(duration_hours_division.rem, 60);
    int duration_minutes = duration_minutes_division.quot;
    //int duration_seconds = duration_minutes_division.rem; // unused
    NSString *durationString;
    if (duration_hours > 0)
    {
        durationString = [NSString stringWithFormat:@"%d hr %d min",
                          duration_hours, duration_minutes];
    }
    else
    {
        durationString = [NSString stringWithFormat:@"%d min",
                          duration_minutes];
    }
    // -------------------------------------------------------------------------
    
    self.journeyHeaderView.departLabel.text = departureString;
    self.journeyHeaderView.arriveValueLabel.text = arrivalString;
    self.journeyHeaderView.durationValueLabel.text = durationString;

    DDLogVerbose(@"TravelBotJourneyViewController:viewWillAppear exit.");
}

- (void)viewDidUnload {
    [self setJourneyHeaderView:nil];
    _journey = nil;
    [super viewDidUnload];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

// -----------------------------------------------------------------------------
//  Set up a custom header view here rather than in the storyboard.
// -----------------------------------------------------------------------------
- (UIView *)tableView:(UITableView *)tableView viewForHeaderInSection:(NSInteger)section
{
    return self.journeyHeaderView;
}

/*
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return 88.0;
}
*/

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 0;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"JourneyCell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
    
    return cell;
}

- (IBAction)backButton:(id)sender
{
    DDLogVerbose(@"TravelBotJourneyViewController:backButton entry.");
    [self.delegate travelBotJourneyViewControllerDelegateDidFinish:self];
    DDLogVerbose(@"TravelBotJourneyViewController:backButton exit.");
}
@end
