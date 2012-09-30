//
//  TravelBotSettingsViewController.m
//  TravelBot
//
//  Created by Asim Ihsan on 30/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotSettingsViewController.h"
#import "AIUtilities.h"
#import "AISocketManager.h"
#import "CocoaLumberJack/DDLog.h"

// ----------------------------------------------------------------------------
//  Constants.
// ----------------------------------------------------------------------------
static int ddLogLevel = LOG_LEVEL_VERBOSE;
// ----------------------------------------------------------------------------

@interface TravelBotSettingsViewController ()

- (void)updateServerStatusLabel;

@end

@implementation TravelBotSettingsViewController

@synthesize serverStatusLabel = _serverStatusLabel;

#pragma mark - View lifecycle
- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (!self)
    {
        return nil;
    }
    return self;
}

- (void)viewDidLoad
{
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
    [self setServerStatusLabel:nil];
    
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                              forKeyPath:NOTIFICATION_SOCKET_OPENED];
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                              forKeyPath:NOTIFICATION_SOCKET_CLOSED];    
    
    [super viewDidUnload];
}

- (void)viewWillAppear:(BOOL)animated
{
    // -------------------------------------------------------------------------
    //  Server status cell.
    // -------------------------------------------------------------------------
    [self updateServerStatusLabel];
    // -------------------------------------------------------------------------
    
    [super viewWillAppear:animated];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Private API.
// -----------------------------------------------------------------------------
//  Update the server status label on the main menu. As this may get called
//  via the notification center we may not be on the main thread, so use
//  GCD to execute this on the main thread.
// -----------------------------------------------------------------------------
- (void)updateServerStatusLabel
{
    dispatch_async(dispatch_get_main_queue(), ^{
        DDLogVerbose(@"TravelBotSettingsMenuViewController:updateServerStatusLabel entry.");
        AISocketManager *socketManager = [AISocketManager sharedInstance];
        if ([socketManager isConnected])
        {
            DDLogVerbose(@"TravelBotSettingsMenuViewController:updateServerStatusLabel. is connected.");
            self.serverStatusLabel.text = @"Connected";
            self.serverStatusLabel.textColor = [AIUtilities colorWithR:50.0
                                                                     G:205.0
                                                                     B:50.0
                                                                     A:1.0];
            
        }
        else
        {
            DDLogVerbose(@"TravelBotSettingsMenuViewController:updateServerStatusLabel. is not connected.");
            self.serverStatusLabel.text = @"Not connected";
            self.serverStatusLabel.textColor = [AIUtilities colorWithR:178.0
                                                                     G:34.0
                                                                     B:34.0
                                                                     A:1.0];
        }
        DDLogVerbose(@"TravelBotSettingsMenuViewController:updateServerStatusLabel exit.");
    });
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Navigation logic may go here. Create and push another view controller.
    /*
     <#DetailViewController#> *detailViewController = [[<#DetailViewController#> alloc] initWithNibName:@"<#Nib name#>" bundle:nil];
     // ...
     // Pass the selected object to the new view controller.
     [self.navigationController pushViewController:detailViewController animated:YES];
     */
}

@end
