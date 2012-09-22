//
//  TravelBotJourneyViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 21/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotJourneyViewController.h"
#import "Journey.h"
#import "JourneyLeg.h"
#import "TravelBotJourneyHeader.h"
#import "TravelBotJourneyLegCell.h"
#import "AIUtilities.h"

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
    NSString *durationString = [AIUtilities getDurationFromTwoDates:firstDepartureTime
                                                 secondTimeInterval:lastArrivalTime];
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

- (void)didRotateFromInterfaceOrientation:(UIInterfaceOrientation)fromInterfaceOrientation
{
    [self.tableView reloadData];
}

#pragma mark - Table view data source

// -----------------------------------------------------------------------------
//  Set up a custom header view here rather than in the storyboard.
// -----------------------------------------------------------------------------
- (UIView *)tableView:(UITableView *)tableView viewForHeaderInSection:(NSInteger)section
{
    return self.journeyHeaderView;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [self.journey getNumberOfLegs];
}

// -----------------------------------------------------------------------------
//  heightForRowAtIndexPath is called before cellForRowAtIndexPath. This is
//  because iOS needs to know the height of all cells in the table, regardless
//  of whether they're displayed, in order to handle the scroll indication.
//
//  iOS assumes that calculating the height is cheaper than displaying the
//  cell. Hence, the wrong solution is to call cellForRowAtIndexPath from
//  heightForRowAtIndexPath in order to calculate height. One should have
//  a cheaper height calculation method on hand.
//
//  !!AI For now we assume the number of journey legs is small and we can
//  afford to create the cells in order to determine the height.
// -----------------------------------------------------------------------------
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotJourneyViewController:heightForRowAtIndexPath entry. indexPath: %@", indexPath);
    UITableViewCell *cell = [self tableView:tableView cellForRowAtIndexPath:indexPath];
    return cell.frame.size.height;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotJourneyViewController:cellForRowAtIndexPath entry. indexPath: %@", indexPath);
    
    // -------------------------------------------------------------------------
    //  Get a table cell. As we use Storyboard to set up the cell it will
    //  always be non-nil.
    // -------------------------------------------------------------------------
    static NSString *cellIdentifier = @"JourneyCell";
    TravelBotJourneyLegCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell)
        DDLogError(@"TravelBotSearchViewController:cellForRowAtIndexPath. cell is nil.");
    assert(cell);
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Set up the cell.
    // -------------------------------------------------------------------------
    JourneyLeg *leg = [self.journey getJourneyLegAt:indexPath.row];
    assert(leg);
    DDLogVerbose(@"Row %d has %@", indexPath.row, leg);
    cell.leg = leg;
    // -------------------------------------------------------------------------
    
    return cell;
}

- (IBAction)backButton:(id)sender
{
    DDLogVerbose(@"TravelBotJourneyViewController:backButton entry.");
    [self.delegate travelBotJourneyViewControllerDelegateDidFinish:self];
    DDLogVerbose(@"TravelBotJourneyViewController:backButton exit.");
}
@end
