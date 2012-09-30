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
// ----------------------------------------------------------------------------

@interface TravelBotMainMenuViewController ()
<TravelBotCountriesViewControllerDelegate,
 TravelBotPlacesViewControllerDelegate,
 TravelBotSearchViewControllerDelegate>

- (BOOL)isCountrySelected;

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
    [super viewWillAppear:animated];
    
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
    //  Always set up the search button container cell to be transparent.
    // -------------------------------------------------------------------------
    self.searchButtonContainerCell.backgroundColor = [UIColor clearColor];
    UIView *backView = [[UIView alloc] initWithFrame:CGRectZero];
    self.searchButtonContainerCell.backgroundView = backView;
    // -------------------------------------------------------------------------
}

- (void)viewDidLoad
{
    DDLogVerbose(@"TravelBotMainMenuViewController:viewDidLoad entry.");
    [super viewDidLoad];    
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    
    [self setSearchButton:nil];
    [self setFromLabel:nil];
    [self setToLabel:nil];
    [self setCountryLabel:nil];
    [self setSearchButtonContainerCell:nil];
    [self setFromLabelContainerCell:nil];
    [self setToLabelContainerCell:nil];
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
        controller.delegate = self;
        assert(self.selectedFromPlace != nil);
        assert(self.selectedToPlace != nil);
        controller.fromPlace = self.selectedFromPlace;
        controller.toPlace = self.selectedToPlace;
        controller.searchResults = nil;
    }
}

- (void)travelBotCountriesViewControllerDidFinish:(TravelBotCountriesViewController *)controller
                                          country:(TravelBotCountry *)country
{
    DDLogVerbose(@"TravelBotMainMenuViewController:travelBotCountriesViewControllerDidFinish entry. country: %@", country);
    if (country)
    {
        DDLogVerbose(@"TravelBotMainMenuViewController:travelBotCountriesViewControllerDidFinish. setting country.");
        if (!$eql(self.selectedCountry, country))
        {
            DDLogVerbose(@"TravelBotMainMenuViewController:travelBotCountriesViewControllerDidFinish. country changed.");
            self.selectedFromPlace = nil;
            self.selectedToPlace = nil;
        }
        self.selectedCountry = country;
    }
    [self.navigationController popViewControllerAnimated:YES];
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
    
    [self.navigationController popViewControllerAnimated:YES];
}

- (void)travelBotSearchViewControllerDidFinish:(TravelBotSearchViewController *)controller
{
    DDLogVerbose(@"TravelBotMainMenuViewController:travelBotSearchViewControllerDidFinish entry.");
    [self.navigationController popViewControllerAnimated:YES];
    DDLogVerbose(@"TravelBotMainMenuViewController:travelBotSearchViewControllerDidFinish exit.");
}

#pragma mark - Private API.
- (BOOL)isCountrySelected
{
    return (self.selectedCountry != nil);
}

- (IBAction)searchButtonAction:(id)sender
{
    DDLogVerbose(@"TravelBotMainMenuViewController:searchButtonAction entry.");
    AISocketManager *socketManager = [AISocketManager sharedInstance];
    if ([socketManager isConnected])
    {
        DDLogVerbose(@"TravelBotMainMenuViewController:searchButtonAction. Socket is connected.");
        [self performSegueWithIdentifier:@"search" sender:self];
    }
    else
    {
        DDLogVerbose(@"TravelBotMainMenuViewController:searchButtonAction. Socket is not connected.");
        UIAlertView *alert = [[UIAlertView alloc]
                              initWithTitle: @"Search failed."
                              message: @"Unable to establish network connection to server. See 'Settings'."
                              delegate:nil
                              cancelButtonTitle:@"OK"
                              otherButtonTitles:nil];
        [alert show];
    }
    
    DDLogVerbose(@"TravelBotMainMenuViewController:searchButtonAction exit.");
}
@end
