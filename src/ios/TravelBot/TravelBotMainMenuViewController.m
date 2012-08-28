//
//  TravelBotMainMenuViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotMainMenuViewController.h"
#import "TravelBotCountriesViewController.h"
#import "TravelBotCountry.h"
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
<TravelBotCountriesViewControllerDelegate>

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
    BOOL isCountrySelected = [self isCountrySelected];
    DDLogVerbose(@"isCountrySelected: %@", isCountrySelected ? @"YES" : @"NO");
    
    self.countryLabel.text = isCountrySelected ?
                                 self.selectedCountry.name :
                                 @"Select a country...";
    self.toLabel.enabled = isCountrySelected;
    self.toLabelContainerCell.userInteractionEnabled = isCountrySelected;
    self.fromLabel.enabled = isCountrySelected;
    self.fromLabelContainerCell.userInteractionEnabled = isCountrySelected;
    self.searchButton.userInteractionEnabled = isCountrySelected;
    self.searchButton.enabled = isCountrySelected;
    
    // Always set up the search button container cell to be transparent.
    self.searchButtonContainerCell.backgroundColor = [UIColor clearColor];
    UIView *backView = [[UIView alloc] initWithFrame:CGRectZero];
    self.searchButtonContainerCell.backgroundView = backView;
    
    [super viewWillAppear:animated];
}

- (void)viewDidLoad
{
    DDLogVerbose(@"TravelBotMainMenuViewController:viewDidLoad entry.");
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
    [super viewDidUnload];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    DDLogVerbose(@"TravelBotMainMenuViewController:prepareForSegue entry. segue: %@, sender: %@", segue, sender);
    if ($eql(segue.identifier, @"country"))
    {
        DDLogVerbose(@"segue to 'country'");
        TravelBotCountriesViewController *controller = segue.destinationViewController;
        controller.delegate = self;
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

#pragma mark - Private API.
- (BOOL)isCountrySelected
{
    return (self.selectedCountry != nil);
}

@end
