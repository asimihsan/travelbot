//
//  TravelBotFavoriteCell.m
//  TravelBot
//
//  Created by Asim Ihsan on 01/10/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "TravelBotFavoriteCell.h"
#import "TravelBotSavedSearch.h"
#import "TravelBotPlace.h"
#import "AIUtilities.h"

@interface TravelBotSavedSearch ()

- (void)initView;

@end

@implementation TravelBotFavoriteCell

@synthesize savedSearch = _savedSearch;

- (void)setSavedSearch:(TravelBotSavedSearch *)savedSearch
{
    self.fromLabel.text = savedSearch.fromPlace.name;
    self.toLabel.text = savedSearch.toPlace.name;
    
    NSDateFormatter *dateFormatter = [AIUtilities getThreadLocalNSDateFormatter];
    dateFormatter.dateFormat = @"yyyy-MM-dd HH:mm:ss";
    self.searchDatetimeLabel.text = [dateFormatter stringFromDate:savedSearch.searchDatetime];
}

@end
