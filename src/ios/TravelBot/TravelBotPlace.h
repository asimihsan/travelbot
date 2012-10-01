//
//  TravelBotPlace.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "TravelBotCountry.h"

@interface TravelBotPlace : NSObject
<NSCoding>

@property (copy, nonatomic) NSString *name;
@property (assign, nonatomic) TravelBotCountry *country;

- (id)initWithName:(NSString *)name country:(TravelBotCountry *)country;

@end
