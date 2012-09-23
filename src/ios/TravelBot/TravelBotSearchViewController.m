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
#import "TravelBotJourneyViewController.h"
#import "AIUtilities.h"

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
<TravelBotJourneyViewControllerDelegate>

@property (copy, nonatomic) NSString *requestUUID;
@property (strong, nonatomic) NSDictionary *countryCodeToMethod;

- (void)startSearch;
- (void)stopSearch;
- (void)onRequestCompletion:(NSNotification *)notification;
- (void)onSocketClosed;
- (void)showNetworkFailureError;

@end

@implementation TravelBotSearchViewController

@synthesize fromPlace = _fromPlace;
@synthesize toPlace = _toPlace;
@synthesize searchHeaderView = _searchHeaderView;
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
    
    AISocketManager *socketManager = [AISocketManager sharedInstance];
    if (![socketManager isConnected])
    {
        DDLogInfo(@"TravelBotSearchViewController:startSearch. Socket is not connected.");
        [self showNetworkFailureError];
        return;
    }
    // -------------------------------------------------------------------------
    
    [SVProgressHUD showWithStatus:@"Searching..."
                         maskType:SVProgressHUDMaskTypeNone];
    
    // -------------------------------------------------------------------------
    //  Make a request to the socket manager to search for journeys using
    //  the appropriate workers, depending on the country.
    // -------------------------------------------------------------------------

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
    dispatch_async(dispatch_get_main_queue(),
    ^{
        [self stopSearch];
    });
    
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
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Initialize self.searchResults on the main thread and then reload on
    //  the main thread.
    //
    //  The former is done to synchronize self.searchResults,
    //  because the main menu view controller is allowed to set it to nil.
    //
    //  The latter is done because this function is called by the notification
    //  center, and hence there is no guarantee that we're on the main thread.
    // -------------------------------------------------------------------------
    dispatch_async(dispatch_get_main_queue(),
    ^{
        self.searchResults = [NSArray arrayWithArray:journeys];
        [self.tableView reloadData];
    });
    // -------------------------------------------------------------------------

    DDLogVerbose(@"TravelBotSearchViewController:onRequestCompletion exit.");
}

#pragma mark - Table view delegate and segues.
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotSearchViewController:didSelectRowAtIndexPath entry. indexPath: %@", indexPath);
    Journey *journey = [self.searchResults $at:indexPath.row];
    DDLogVerbose(@"TravelBotSearchViewController:didSelectRowAtIndexPath. selected journey: %@", journey);
    [self performSegueWithIdentifier:@"resultsToJourney" sender:journey];
    DDLogVerbose(@"TravelBotSearchViewController:didSelectRowAtIndexPath exit");
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    DDLogVerbose(@"TravelBotSearchViewController:prepareForSegue entry. sender: %@", sender);
    
    // -------------------------------------------------------------------------
    //  Validate assumptions.
    // -------------------------------------------------------------------------
    assert($eql(segue.identifier, @"resultsToJourney"));
    // -------------------------------------------------------------------------
    
    // Set delegate and journey up.
    TravelBotJourneyViewController *controller = segue.destinationViewController;
    controller.delegate = self;
    controller.journey = sender;
    
    DDLogVerbose(@"TravelBotSearchViewController:prepareForSegue exit.");
}

#pragma mark - Table view data source.

// -----------------------------------------------------------------------------
//  Set up a custom header view here rather than in the storyboard.
// -----------------------------------------------------------------------------
- (UIView *)tableView:(UITableView *)tableView viewForHeaderInSection:(NSInteger)section
{
    return self.searchHeaderView;
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
        DDLogError(@"TravelBotSearchViewController:cellForRowAtIndexPath. cell is nil.");
    assert(cell);
    if (self.searchResults)
    {
        DDLogVerbose(@"cellForRowAtIndexPath. search results are present.");
        Journey *journey = [self.searchResults $at:indexPath.row];
        cell.journey = journey;
    } // if (self.searchResults)
    // ---------------------------------------------------------------------
    
    DDLogVerbose(@"cellForRowAtIndexPath. returning: %@.", cell);
    return cell;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return 95.0;
}

#pragma mark - View lifecycle.

- (void)viewWillAppear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotSearchViewController:viewWillAppear entry. fromPlace: %@, toPlace: %@",
                 self.fromPlace, self.toPlace);
    
    // -------------------------------------------------------------------------
    //  Set up the header of the table with from, to, when.
    // -------------------------------------------------------------------------
    self.searchHeaderView.fromValueLabel.text = self.fromPlace.name;
    self.searchHeaderView.toValueLabel.text = self.toPlace.name;
    self.searchHeaderView.whenValueLabel.text = @"Now";
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // Only perform a search if there are no search results currently available.
    // Calling view controllers must set the search results to nil if they
    // want a fresh search to be executed.
    // -------------------------------------------------------------------------
    if (!self.searchResults)
    {
        DDLogVerbose(@"TravelBotSearchViewController:viewWillAppear. self.searchResults is nil.");
        [self startSearch];
    }
    // -------------------------------------------------------------------------
    
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
    //  Update to the notification that the server socket is closed.
    //  Use this to fail searches if we lose the socket connection.
    // -------------------------------------------------------------------------
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(onSocketClosed)
                                                 name:NOTIFICATION_SOCKET_CLOSED
                                               object:nil];
    // -------------------------------------------------------------------------
    
    [super viewDidLoad];
}

- (void)viewDidUnload
{
    DDLogVerbose(@"TravelBotSarchViewController:viewDidUnload entry.");
    [self setToPlace:nil];
    [self setFromPlace:nil];
    [self setSearchHeaderView:nil];
    
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                              forKeyPath:NOTIFICATION_SOCKET_CLOSED];

    [super viewDidUnload];
}

- (void)onSocketClosed
{
    dispatch_async(dispatch_get_main_queue(),
    ^{
        DDLogVerbose(@"TravelBotSearchViewController:onSocketClosed entry.");
        [self stopSearch];
        [self showNetworkFailureError];
        DDLogVerbose(@"TravelBotSearchViewController:onSocketClosed exit.");
    });
}

- (void)showNetworkFailureError
{
    dispatch_async(dispatch_get_main_queue(),
    ^{
        UIAlertView *alert = [[UIAlertView alloc]
                              initWithTitle: @"Search failed."
                              message: @"Unable to establish network connection to server."
                              delegate:nil
                              cancelButtonTitle:@"OK"
                              otherButtonTitles:nil];
        [alert show];
    });
}

- (IBAction)backButton:(id)sender
{
    DDLogVerbose(@"TravelBotSearchViewController:backButton entry.");
    [self stopSearch];
    [self.delegate travelBotSearchViewControllerDidFinish:self];
    DDLogVerbose(@"TravelBotSearchViewController:backButton exit.");    
}

- (void)travelBotJourneyViewControllerDelegateDidFinish:(TravelBotJourneyViewController *)controller
{
    DDLogVerbose(@"TravelBotSearchViewController:travelBotJourneyViewControllerDelegateDidFinish entry.");
    [self.navigationController popViewControllerAnimated:YES];
    DDLogVerbose(@"TravelBotSearchViewController:travelBotJourneyViewControllerDelegateDidFinish exit.");
}

@end
