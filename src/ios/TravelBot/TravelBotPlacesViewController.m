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

@synthesize numberOfRows = _numberOfRows;
@synthesize placeType = _placeType;
@synthesize country = _country;
@synthesize delegate = _delegate;

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
    
    AIDatabaseManager *databaseManager = [AIDatabaseManager sharedInstance];
    NSNumber *return_value = [databaseManager getNumberOfPlaces:self.country.code];
    self.numberOfRows = return_value.integerValue;

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
    // Return the number of rows in the section.
    return self.numberOfRows;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"PlaceCell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    
    // Get the place name.
    NSNumber *row = [[NSNumber alloc] initWithInteger:indexPath.row];
    AIDatabaseManager *databaseManager = [AIDatabaseManager sharedInstance];
    NSString *place = [databaseManager getPlaceWithCountryCode:self.country.code
                                                        filter:nil
                                                         index:row];
    
    // Configure the cell...
    cell.textLabel.text = place;
    
    return cell;
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    DDLogVerbose(@"TravelBotCountriesViewController:didSelectRowAtIndexPath entry. indexPath: %@", indexPath);
    /*
    TravelBotPlace *selectedPlace =
    DDLogVerbose(@"selectedPlace: %@", selectedPlace);
    [self.delegate travelBotCountriesViewControllerDidFinish:self
                                                     place:selectedPlace];
    */
    
    // In tutorials this will read [self dismissViewControllerAnimated]. This
    // doesn't work here because, I think, I haven't embedded this in a navigation
    // hierarchy, because I want to use push segues.
    //
    // Instead, we reach into the navigation controller and ask it to pop.
    [self.navigationController popViewControllerAnimated:YES];
    
}

@end
