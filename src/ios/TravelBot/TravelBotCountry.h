//
//  TravelBotCountry.h
//  TravelBot
//
//  Created by Asim Ihsan on 28/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface TravelBotCountry : NSObject

@property (copy, nonatomic) NSString *name;
@property (copy, nonatomic) NSString *image;
@property (copy, nonatomic) NSString *code;

- (id)initWithName:(NSString *)name
             image:(NSString *)image
              code:(NSString *)code;

@end
