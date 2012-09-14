//
//  TravelBotMainMenuViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotMainMenuViewController.h"
#import "TravelBotCountriesViewController.h"
#import "TravelBotPlacesViewController.h"
#import "TravelBotSearchViewController.h"
#import "TravelBotCountry.h"
#import "TravelBotPlace.h"
#import "AISocketManager.h"
#import "AIUtilities.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;

// Tag of the UITableViewCell that contains country label view.
const int TAG_COUNTRY_CELL = 100;

// Tags of UITableViewCells that contain from/to.
const int TAG_WHERE_FROM_CELL = 200;
const int TAG_WHERE_TO_CELL = 201;

// Tag of the UITableViewCell that contains the search button.
const int TAG_SEARCH_BUTTON_CELL = 300;
// ----------------------------------------------------------------------------

@interface TravelBotMainMenuViewController ()
<TravelBotCountriesViewControllerDelegate,
 TravelBotPlacesViewControllerDelegate>

- (BOOL)isCountrySelected;
- (void)updateServerStatusLabel;

@end

@implementation TravelBotMainMenuViewController

@synthesize selectedCountry = _selectedCountry;

// IBOutlets.
@synthesize countryLabel = _countryLabel;
@synthesize fromLabelContainerCell = _fromLabelContainerCell;
@synthesize fromLabel = _fromLabel;
@synthesize toLabelContainerCell = _toLabelContainerCell;
@synthesize toLabel = _toLabel;
@synthesize searchButtonContainerCell = _searchButtonContainerCell;
@synthesize searchButton = _searchButton;
@synthesize serverStatusLabel = _serverStatusLabel;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewWillAppear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotMainMenuViewController:viewWillAppear entry.");
    BOOL isCountrySelected = [self isCountrySelected];
    DDLogVerbose(@"isCountrySelected: %@", isCountrySelected ? @"YES" : @"NO");
    
    // -------------------------------------------------------------------------
    //  Country label.
    // -------------------------------------------------------------------------
    self.countryLabel.text = isCountrySelected ?
                                 self.selectedCountry.name :
                                 @"Select a country...";
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  From label.
    // -------------------------------------------------------------------------
    self.fromLabel.enabled = isCountrySelected;
    self.fromLabelContainerCell.userInteractionEnabled = isCountrySelected;
    if (self.fromLabel.enabled && self.selectedFromPlace)
    {
        self.fromLabel.text = [NSString stringWithFormat:@"From: %@", self.selectedFromPlace.name];
    }
    else
    {
        self.fromLabel.text = @"From...";
    }
    // -------------------------------------------------------------------------
        
    // -------------------------------------------------------------------------
    //  To label.
    // -------------------------------------------------------------------------
    self.toLabel.enabled = isCountrySelected;
    self.toLabelContainerCell.userInteractionEnabled = isCountrySelected;
    if (self.toLabel.enabled && self.selectedToPlace)
    {
        self.toLabel.text = [NSString stringWithFormat:@"To: %@", self.selectedToPlace.name];
    }
    else
    {
        self.toLabel.text = @"To..";
    }
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Search button.
    // -------------------------------------------------------------------------
    if (isCountrySelected && self.selectedToPlace && self.selectedFromPlace)
    {
        self.searchButton.userInteractionEnabled = YES;
        self.searchButton.enabled = YES;
    }
    else
    {
        self.searchButton.userInteractionEnabled = NO;
        self.searchButton.enabled = NO;
    }
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  Server status cell.
    // -------------------------------------------------------------------------
    [self updateServerStatusLabel];
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Always set up the search button container cell to be transparent.
    // -------------------------------------------------------------------------
    self.searchButtonContainerCell.backgroundColor = [UIColor clearColor];
    UIView *backView = [[UIView alloc] initWithFrame:CGRectZero];
    self.searchButtonContainerCell.backgroundView = backView;
    // -------------------------------------------------------------------------
    
    [super viewWillAppear:animated];
}

// -----------------------------------------------------------------------------
//  Update the server status label on the main menu. As this may get called
//  via the notification center we may not be on the main thread, so use
//  GCD to execute this on the main thread.
// -----------------------------------------------------------------------------
- (void)updateServerStatusLabel
{
    dispatch_async(dispatch_get_main_queue(), ^{
        DDLogVerbose(@"TravelBotMainMenuViewController:updateServerStatusLabel entry.");
        self.serverStatusLabel.textLabel.text = @"Server status";
        AISocketManager *socketManager = [AISocketManager sharedInstance];
        if ([socketManager isConnected])
        {
            self.serverStatusLabel.detailTextLabel.text = @"Connected";
            self.serverStatusLabel.detailTextLabel.textColor = [AIUtilities colorWithR:50.0
                                                                                     G:205.0
                                                                                     B:50.0
                                                                                     A:1.0];

        }
        else
        {
            self.serverStatusLabel.detailTextLabel.text = @"Not connected";
            self.serverStatusLabel.detailTextLabel.textColor = [AIUtilities colorWithR:178.0
                                                                                     G:34.0
                                                                                     B:34.0
                                                                                     A:1.0];
        }
        DDLogVerbose(@"TravelBotMainMenuViewContrlller:updateServerStatusLabel exit.");
    });
}

- (void)viewDidLoad
{
    DDLogVerbose(@"TravelBotMainMenuViewController:viewDidLoad entry.");
    
    // -------------------------------------------------------------------------
    //  Update to the notification that the server socket is opened or closed.
    //  Use this to update the server status label.
    // -------------------------------------------------------------------------
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(updateServerStatusLabel)
                                                 name:NOTIFICATION_SOCKET_OPENED
                                               object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(updateServerStatusLabel)
                                                 name:NOTIFICATION_SOCKET_CLOSED
                                               object:nil];
    // -------------------------------------------------------------------------
    
    [super viewDidLoad];
}

- (void)viewDidUnload
{
    [self setSearchButton:nil];
    [self setFromLabel:nil];
    [self setToLabel:nil];
    [self setCountryLabel:nil];
    [self setSearchButtonContainerCell:nil];
    [self setFromLabelContainerCell:nil];
    [self setToLabelContainerCell:nil];
    [self setServerStatusLabel:nil];
    
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                              forKeyPath:NOTIFICATION_SOCKET_OPENED];
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                              forKeyPath:NOTIFICATION_SOCKET_CLOSED];
    
    [super viewDidUnload];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}

#pragma mark - Segues, transitions, delegate callbacks.
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    DDLogVerbose(@"TravelBotMainMenuViewController:prepareForSegue entry. segue: %@, sender: %@", segue, sender);
    if ($eql(segue.identifier, @"country"))
    {
        DDLogVerbose(@"segue to 'country'");
        TravelBotCountriesViewController *controller = segue.destinationViewController;
        controller.delegate = self;
    }
    else if ($eql(segue.identifier, @"from"))
    {
        DDLogVerbose(@"segue to 'from'");
        TravelBotPlacesViewController *controller = segue.destinationViewController;
        controller.delegate = self;
        controller.placeType = @"from";
        assert(self.selectedCountry != nil);
        controller.country = self.selectedCountry;

    }
    else if ($eql(segue.identifier, @"to"))
    {
        DDLogVerbose(@"segue to 'to'");
        TravelBotPlacesViewController *controller = segue.destinationViewController;
        controller.delegate = self;
        controller.placeType = @"to";
        assert(self.selectedCountry != nil);
        controller.country = self.selectedCountry;
    }
    else if ($eql(segue.identifier, @"search"))
    {
        DDLogVerbose(@"segue to 'search'");
        TravelBotSearchViewController *controller = segue.destinationViewController;
        assert(self.selectedFromPlace != nil);
        assert(self.selectedToPlace != nil);
        controller.fromPlace = self.selectedFromPlace;
        controller.toPlace = self.selectedToPlace;
    }
}
- (void)travelBotCountriesViewControllerDidFinish:(TravelBotCountriesViewController *)controller
                                          country:(TravelBotCountry *)country
{
    DDLogVerbose(@"TravelBotMainMenuViewController:travelBotCountriesViewControllerDidFinish entry. country: %@", country);
    if (country)
    {
        DDLogVerbose(@"setting country.");
        self.selectedCountry = country;
    }
}

- (void)travelBotPlacesViewControllerDidFinish:(TravelBotPlacesViewController *)controller
                                     placeType:(NSString *)placeType
                                         place:(TravelBotPlace *)place
{
    DDLogVerbose(@"TravelBotMainMenuViewController:travelBotPlacesViewControllerDidFinish entry. controller: %@, placeType: %@, place: %@",
                 controller, placeType, place);
    
    // -------------------------------------------------------------------------
    //  Validate inputs and assumptions.
    // -------------------------------------------------------------------------
    assert(($eql(placeType, @"from")) || $eql(placeType, @"to"));
    // -------------------------------------------------------------------------
    
    if ($eql(placeType, @"from"))
    {
        DDLogVerbose(@"TravelBotMainMenuViewController:travelBotPlacesViewControllerDidFinish: place is 'from'");
        self.selectedFromPlace = place;
    }
    else if ($eql(placeType, @"to"))
    {
        DDLogVerbose(@"TravelBotMainMenuViewController:travelBotPlacesViewControllerDidFinish: place is 'to'");
        self.selectedToPlace = place;
    }
}

#pragma mark - Private API.
- (BOOL)isCountrySelected
{
    return (self.selectedCountry != nil);
}

@end
