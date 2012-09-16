//
//  TravelBotSearchViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSearchViewController.h"
#import "TravelBotSearchHeader.h"
#import "TravelBotPlace.h"
#import "AISocketManager.h"
#import "TravelBotSearchCell.h"

#import "Journey.h"
#import "JourneyLeg.h"

#import "SVProgressHUD/SVProgressHUD.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

@interface TravelBotSearchViewController ()

@property (copy, nonatomic) NSString *requestUUID;
@property (retain, nonatomic) NSDictionary *countryCodeToMethod;
@property (retain, nonatomic) NSArray *searchResults;
@property (retain, nonatomic) TravelBotSearchHeader *searchHeader;

- (void)startSearch;
- (void)stopSearch;
- (void)onRequestCompletion:(NSNotification *)notification;
- (TravelBotSearchCell *)setupCell:(TravelBotSearchCell *)cell;

@end

@implementation TravelBotSearchViewController

@synthesize fromPlace = _fromPlace;
@synthesize toPlace = _toPlace;
@synthesize countryCodeToMethod = _countryCodeToMethod;
@synthesize requestUUID = _requestUUID;

#pragma mark - Searching.
- (void)startSearch
{
    DDLogVerbose(@"TravelBotSearchViewController:startSearch entry.");
    
    // -------------------------------------------------------------------------
    //  Validate assumptions.
    // -------------------------------------------------------------------------
    assert($eql(self.fromPlace.country, self.toPlace.country));
    NSString *method = [self.countryCodeToMethod $for:self.fromPlace.country.code];
    assert(method);
    // -------------------------------------------------------------------------
    
    // TODO !!AI remove me
    //return;
    
    [SVProgressHUD showWithStatus:@"Searching..."
                         maskType:SVProgressHUDMaskTypeNone];
    
    // -------------------------------------------------------------------------
    //  Make a request to the socket manager to search for journeys using
    //  the appropriate workers, depending on the country.
    // -------------------------------------------------------------------------
    AISocketManager *socketManager = [AISocketManager sharedInstance];
    NSDictionary *kwargs = $dict(self.fromPlace.name, @"from_location",
                                 self.toPlace.name, @"to_location");
    NSDictionary *request = $dict(@"1.0", @"version",
                                  @"task", @"type",
                                  method, @"method",
                                  kwargs, @"kwargs");
    self.requestUUID = [socketManager writeDictionary:request];
    DDLogVerbose(@"TravelBotSearchViewController:startSearch. request_uuid: %@, request: %@",
                 self.requestUUID, request);
    // -------------------------------------------------------------------------
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(onRequestCompletion:)
                                                 name:self.requestUUID
                                               object:nil];
    DDLogVerbose(@"TravelBotSearchViewController:startSearch exit.");
}

- (void)stopSearch
{
    [SVProgressHUD dismiss];
    
    // If we've started a search a requestUUID exists. Remove ourselves as an
    // observer for the result, as we don't care any more.
    if (self.requestUUID)
    {
        [[NSNotificationCenter defaultCenter] removeObserver:self
                                                        name:self.requestUUID
                                                      object:nil];
        self.requestUUID = nil;
    }
}

- (void)onRequestCompletion:(NSNotification *)notification
{
    DDLogVerbose(@"TravelBotSearchViewController:onRequestCompletion entry.");
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                                    name:notification.name
                                                  object:nil];
    
    // No guarantee we're on the main thread, so dismiss HUD on main thread.
    [self performSelectorOnMainThread:@selector(stopSearch)
                           withObject:nil
                        waitUntilDone:NO];
    
    // -------------------------------------------------------------------------
    //  Get results from the notification object, then use Journey class to
    //  convert dictionaries to objects.
    // -------------------------------------------------------------------------
    NSArray *result = notification.object;
    NSMutableArray *journeys = [[NSMutableArray alloc] init];
    [result $each:^(NSDictionary *jsonDictionary) {
        Journey *journey = [[Journey alloc] init:jsonDictionary];
        [journeys $push:journey];
    }];
    DDLogVerbose(@"TravelBotSearchViewController:onRequestCompletion. journeys: %@", journeys);
    self.searchResults = [NSArray arrayWithArray:journeys];
    
    // This function is called by the notification center, hence no guarantee
    // that we're on the main thread. In fact we're not, so reloadData delayed
    // by ~5 seconds without using 'performSelectorOnMainThread'.
    [self.tableView performSelectorOnMainThread:@selector(reloadData)
                                     withObject:nil
                                  waitUntilDone:NO];
    // -------------------------------------------------------------------------
    
    DDLogVerbose(@"TravelBotSearchViewController:onRequestCompletion exit.");
}

#pragma mark - Table view data source

// -----------------------------------------------------------------------------
//  Set up a custom header view here rather than in the storyboard.
// -----------------------------------------------------------------------------
- (UIView *)tableView:(UITableView *)tableView viewForHeaderInSection:(NSInteger)section
{
    return self.searchHeader;
}

- (float)tableView:(UITableView *)tableView heightForHeaderInSection:(NSInteger)section
{
    return self.searchHeader.frame.size.height;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    // Return the number of sections.
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    DDLogVerbose(@"TravelBotSearchViewController:numberOfRowsInSection entry.");
    NSInteger return_value;
    if (self.searchResults)
    {
        DDLogVerbose(@"TravelBotSearchViewController:numberOfRowsInSection. searchResults present.");
        return_value = self.searchResults.count;
    }
    else
    {
        DDLogVerbose(@"TravelBotSearchViewController:numberOfRowsInSection. searchResults not present.");
        return_value = 0;
    }
    DDLogVerbose(@"TravelBotSearchViewController:numberOfRowsInSection. returning: %d.", return_value);
    return return_value;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *cellIdentifier = @"SearchCell";
    TravelBotSearchCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell)
    {
        // This is unnecessary in Storyboard; the cell is always non-nil as
        // it's already init'd from the NIB.
        DDLogVerbose(@"TravelBotSearchViewController:cellForRowAtIndexPath. cell is nil, create a new one.");
        cell = [[TravelBotSearchCell alloc] initWithStyle:UITableViewCellStyleDefault
                                          reuseIdentifier:cellIdentifier];
        cell = [self setupCell:cell];
    }
    if (self.searchResults)
    {
        DDLogVerbose(@"cellForRowAtIndexPath. search results are present.");
        Journey *journey = [self.searchResults $at:indexPath.row];
        NSDate *firstDepartureTime = [journey getFirstDepartureTime];
        //NSDate *lastArrivalTime = [journey getLastArrivalTime];
        //NSTimeInterval duration = [lastArrivalTime timeIntervalSinceDate:firstDepartureTime];
        NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
        dateFormatter.dateFormat = @"HH:mm";
        NSString *departureString = [dateFormatter stringFromDate:firstDepartureTime];
        //NSString *arrivalString = [dateFormatter stringFromDate:lastArrivalTime];
        
        //cell.textLabel.text = [NSString stringWithFormat:@"Depart: %@, arrive: %@, duration: %f",
        //                       departureString, arrivalString, duration];
        
        cell.departureLabel.text = departureString;
    }
    DDLogVerbose(@"cellForRowAtIndexPath. returning: %@.", cell);
    return cell;
}

- (TravelBotSearchCell *)setupCell:(TravelBotSearchCell *)cell
{
    DDLogVerbose(@"TravelBotSearchViewController::setupCell entry. cell: %@", cell);
    [cell.textLabel setFont:[UIFont fontWithName:@"Helvetica" size:7.0]];
    cell.backgroundColor = [UIColor redColor];
    return cell;
}

#pragma mark - View lifecycle.

- (void)viewWillAppear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotSearchViewController:viewWillAppear entry. fromPlace: %@, toPlace: %@",
                 self.fromPlace, self.toPlace);
    [self startSearch];
    [super viewWillAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotSearchViewController:viewWillDisappear entry.");
    [self stopSearch];
    [super viewWillDisappear:animated];
}

- (void)viewDidLoad
{
    DDLogVerbose(@"TravelBotSearchViewController:viewDidLoad entry.");
    
    // -------------------------------------------------------------------------
    //  This is an important run-time constant. This maps country codes onto
    //  methods that are used in socket calls.
    //
    //  TODO move this to the plist or some other config store.
    // -------------------------------------------------------------------------
    self.countryCodeToMethod = $dict(@"slovenia.bus_ap.get_journeys", @"SI");
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  Create the search header view that contains from, to, and when
    //  details.
    // -------------------------------------------------------------------------
    self.searchHeader = [[TravelBotSearchHeader alloc]
                         initWithTableView:self.tableView
                         from:self.fromPlace.name
                         to:self.toPlace.name
                         when:@"Now"];
    // -------------------------------------------------------------------------
    
    [super viewDidLoad];
}

- (void)viewDidUnload
{
    DDLogVerbose(@"TravelBotSarchViewController:viewDidUnload entry.");
    [self setToPlace:nil];
    [self setFromPlace:nil];
    [super viewDidUnload];
}


@end
