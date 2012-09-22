//
//  TravelBotPlacesViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotPlacesViewController.h"
#import "TravelBotCountry.h"
#import "TravelBotPlace.h"
#import "AISocketManager.h"
#import "AIDatabaseManager.h"
#import "ConciseKit/ConciseKit.h"
#import "CocoaLumberJack/DDLog.h"
#import "SVProgressHUD.h"

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;

@interface TravelBotPlacesViewController ()

@property (assign, nonatomic) NSInteger numberOfRows;
@property (assign, nonatomic) BOOL userStillTypingInSearch;

@end

@implementation TravelBotPlacesViewController

@synthesize placeType = _placeType;
@synthesize country = _country;
@synthesize delegate = _delegate;
@synthesize searchBar = _searchBar;
@synthesize tableSearchDisplayController = _tableSearchDisplayController;
@synthesize currentSearchString = _currentSearchString;
@synthesize userStillTypingInSearch = _userStillTypingInSearch;

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
    DDLogVerbose(@"TravelBotPlacesViewController:viewDidLoad entry.");
    
    // Add and configure the search bar
    self.tableView.tableHeaderView = self.searchBar;
    self.searchBar.autocorrectionType = UITextAutocorrectionTypeNo;

    [super viewDidLoad];
    DDLogVerbose(@"TravelBotPlacesViewController:viewDidLoad exit.");
}

- (void)viewWillAppear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotPlacesViewController:viewWillAppear entry.");
    self.navigationItem.title = [self.placeType capitalizedString];
}

// -----------------------------------------------------------------------------
//  When the view controller is finished drawing elements we could focus on
//  the search bar.
// -----------------------------------------------------------------------------
- (void)viewDidAppear:(BOOL)animated
{
    // Uncomment the line below to focus on the search bar. This is an unusual
    // UX choice, despite it being the main use case.
    //[self.searchBar becomeFirstResponder];
    
    [super viewDidAppear:animated];
}

- (void)viewDidUnload
{
    [self setSearchBar:nil];
    [self setSearchBar:nil];
    [self setTableSearchDisplayController:nil];
    [super viewDidUnload];
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
    NSNumber *return_value;
    AIDatabaseManager *databaseManager = [AIDatabaseManager sharedInstance];
    if ($eql(tableView, self.tableView))
    {
        DDLogVerbose(@"TravelBotPlacesViewController:numberOfRowsInSection. main table");
        return_value = [databaseManager getNumberOfPlaces:self.country.code
                                                 search:nil];
    }
    else
    {
        DDLogVerbose(@"TravelBotPlacesViewController:numberOfRowsInSection. tableSearchDisplayController. self.currentSearchString: %@", self.currentSearchString);
        if (self.userStillTypingInSearch)
        {
            DDLogVerbose(@"User still typing in search, so return zero rows.");
            return_value = [NSNumber numberWithInt:0];
        }
        else
        {
            DDLogVerbose(@"User no longer typing in search, so determine actually number of rows.");
            return_value = [databaseManager getNumberOfPlaces:self.country.code
                                                       search:self.currentSearchString];
        }
    }
    DDLogVerbose(@"TravelBotPlacesViewController:numberOfRowsInSection. returning: %@", return_value);
    return return_value.integerValue;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    static NSString *cellIdentifier = @"PlaceCell";
    NSInteger row = indexPath.row;
    AIDatabaseManager *databaseManager = [AIDatabaseManager sharedInstance];
    NSString *place;
    // -------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------
    //  Dequeue or create a cell.
    // -------------------------------------------------------------------------
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell)
    {
        DDLogVerbose(@"cellForRowAtIndexPath. cell is nil, create a new one.");
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault
                                      reuseIdentifier:cellIdentifier];
    }
    // -------------------------------------------------------------------------

    if ($eql(tableView, self.tableView))
    {
        DDLogVerbose(@"cellForRowAtIndexPath: main table");
        place = [databaseManager getPlaceWithCountryCode:self.country.code
                                                  search:nil
                                                   index:row];
    }
    else
    {
        DDLogVerbose(@"cellForRowAtIndexPath: tableSearchDisplayController.");
        place = [databaseManager getPlaceWithCountryCode:self.country.code
                                                  search:self.currentSearchString
                                                   index:row];
    }
    
    // Configure the cell...
    cell.textLabel.text = place;
    
    DDLogVerbose(@"cellForRowAtIndexPath returning: %@", cell);
    return cell;
}

- (BOOL)searchDisplayController:(UISearchDisplayController *)controller shouldReloadTableForSearchString:(NSString *)searchString
{
    DDLogVerbose(@"TravelBotPlacesViewController:shouldReloadTableForSearchString entry. searchString: %@", searchString);
    self.currentSearchString = searchString;
    self.userStillTypingInSearch = YES;
    if (![SVProgressHUD isVisible])
        [SVProgressHUD showWithStatus:@"Loading..."
                             maskType:SVProgressHUDMaskTypeClear];
    [NSObject cancelPreviousPerformRequestsWithTarget:self
                                             selector:@selector(updateSearchResults:)
                                               object:controller];
    [self performSelector:@selector(updateSearchResults:)
               withObject:controller
               afterDelay:0.2];
    return NO;
}

- (void)updateSearchResults:(id)controller
{
    DDLogVerbose(@"TravelBotPlacesViewController:updateSearchResults entry.");
    self.userStillTypingInSearch = NO;
    [((UISearchDisplayController *)controller).searchResultsTableView reloadData];
    [SVProgressHUD dismiss];
    DDLogVerbose(@"TravelBotPlacesViewController:updateSearchResults exit.");
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotCountriesViewController:didSelectRowAtIndexPath entry. indexPath: %@", indexPath);
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    NSInteger row = indexPath.row;
    AIDatabaseManager *databaseManager = [AIDatabaseManager sharedInstance];
    NSString *placeName;
    // -------------------------------------------------------------------------
    
    if ($eql(tableView, self.tableView))
    {
        DDLogVerbose(@"didSelectRowAtIndexPath: main table");
        placeName = [databaseManager getPlaceWithCountryCode:self.country.code
                                                      search:nil
                                                       index:row];

    }
    else
    {
        DDLogVerbose(@"didSelectRowAtIndexPath: tableSearchDisplayController.");
        placeName = [databaseManager getPlaceWithCountryCode:self.country.code
                                                      search:self.currentSearchString
                                                       index:row];
    }
    
    TravelBotPlace *selectedPlace = [[TravelBotPlace alloc] initWithName:placeName
                                                                 country:self.country];
    DDLogVerbose(@"TravelBotCountriesViewController:didSelectRowAtIndexPath. selectedPlace: %@", selectedPlace);
    [self.delegate travelBotPlacesViewControllerDidFinish:self
                                                placeType:self.placeType
                                                    place:selectedPlace];    
}

@end
