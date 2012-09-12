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

// ----------------------------------------------------------------------------
//  Static variables or preprocessor defines.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;

@interface TravelBotPlacesViewController ()

@property (assign, nonatomic) NSInteger numberOfRows;

@end

@implementation TravelBotPlacesViewController

@synthesize placeType = _placeType;
@synthesize country = _country;
@synthesize delegate = _delegate;
@synthesize searchBar = _searchBar;
@synthesize tableSearchDisplayController = _tableSearchDisplayController;
@synthesize currentSearchString = _currentSearchString;

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

        /*
        !!AI
         
        This is how to make requests over the socket, so very important. Howver,
        we don't request locations any more.
         
        DDLogVerbose(@"update places for Slovenia.");
        AISocketManager *socketManager = [AISocketManager sharedInstance];
        NSDictionary *request = $dict(@"1.0", @"version",
                                      @"task", @"type",
                                      @"slovenia.bus_ap.get_locations", @"method");
        NSString *request_uuid = [socketManager writeDictionary:request];
        [[NSNotificationCenter defaultCenter] addObserver:self
                                                 selector:@selector(onRequestCompletion:)
                                                     name:request_uuid
                                                   object:nil];
         */
        
        /*
         
         also need:
         
         - (void)onRequestCompletion:(NSNotification *)notification
         {
         DDLogVerbose(@"TravelBotPlacesViewController:onRequestCompletion entry.");
         [[NSNotificationCenter defaultCenter] removeObserver:self
         name:notification.name
         object:nil];
         NSArray *result = notification.object;
         DDLogVerbose(@"result.count: %d", result.count);
         DDLogVerbose(@"result[0]: %@", [result $at:0]);
         DDLogVerbose(@"result[1]: %@", [result $at:1]);
         DDLogVerbose(@"TravelBotPlacesViewController:onRequestCompletion exit.");
         }
         
        */
    [super viewDidLoad];
    DDLogVerbose(@"TravelBotPlacesViewController:viewDidLoad exit.");
}

- (void)viewWillAppear:(BOOL)animated
{
    DDLogVerbose(@"TravelBotPlacesViewController:viewWillAppear entry.");
    self.navigationItem.title = [self.placeType capitalizedString];
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
        DDLogVerbose(@"numberOfRows: main table");
        return_value = [databaseManager getNumberOfPlaces:self.country.code
                                                 search:nil];
    }
    else
    {
        DDLogVerbose(@"numberOfRows: tableSearchDisplayController. self.currentSearchString: %@", self.currentSearchString);
        return_value = [databaseManager getNumberOfPlaces:self.country.code
                                                   search:self.currentSearchString];
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
    NSNumber *row = [NSNumber numberWithInteger:indexPath.row];
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
    return YES;
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotCountriesViewController:didSelectRowAtIndexPath entry. indexPath: %@", indexPath);
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    NSNumber *row = [NSNumber numberWithInteger:indexPath.row];
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
    [self.delegate travelBotPlacesViewControllerDidFinish:self
                                                placeType:self.placeType
                                                    place:selectedPlace];
    
    // In tutorials this will read [self dismissViewControllerAnimated]. This
    // doesn't work here because, I think, I haven't embedded this in a navigation
    // hierarchy, because I want to use push segues.
    //
    // Instead, we reach into the navigation controller and ask it to pop.
    [self.navigationController popViewControllerAnimated:YES];
    
}

@end
