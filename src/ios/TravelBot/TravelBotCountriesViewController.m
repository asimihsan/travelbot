//
//  TravelBotCountriesViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotCountriesViewController.h"
#import "TravelBotCountryCell.h"
#import "TravelBotCountry.h"
#import "AIConfigManager.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;

@interface TravelBotCountriesViewController ()

- (void)updateCountries;

@end

@implementation TravelBotCountriesViewController

@synthesize delegate = _delegate;

- (id)initWithStyle:(UITableViewStyle)style
{
    DDLogVerbose(@"TravelBotCountriesViewController::initWithStyle entry.");
    self = [super initWithStyle:style];
    if (self) {
        // Custom init.
    }
    return self;
}

- (id)init
{
    DDLogVerbose(@"TravelBotCountriesViewController::init entry.");
    if (!(self = [super init]))
    {
        return nil;
    }
    return self;
}

- (void)updateCountries
{
    // Note that [NSMutableArray copy] returns an immutable version of the array.
    DDLogVerbose(@"TravelBotCountriesViewController:updateCountries entry.");
    AIConfigManager *configManager = [AIConfigManager sharedInstance];
    self.countries = [configManager getCountries];
}

- (void)viewDidLoad
{
    DDLogVerbose(@"TravelBotCountriesViewController:viewDidLoad entry.");
    [self updateCountries];
    [super viewDidLoad];
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    // Return the number of sections.
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    DDLogVerbose(@"TravelBotCountriesViewController:numberOfRowsInSection entry.");
    return self.countries.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"countryCell";
    TravelBotCountryCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    TravelBotCountry *country = [self.countries $at:indexPath.row];
    [cell setCellText:country.name];
    [cell setCellImage:country.image];
    
    return cell;
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotCountriesViewController:didSelectRowAtIndexPath entry.");
    TravelBotCountry *selectedCountry = [self.countries $at:indexPath.row];
    DDLogVerbose(@"selectedCountry: %@", selectedCountry);
    [self.delegate travelBotCountriesViewControllerDidFinish:self
                                                     country:selectedCountry];
}

@end
