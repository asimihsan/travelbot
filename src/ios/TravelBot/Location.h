//
//  Location.h
//  TravelBot
//
//  Created by Asim Ihsan on 12/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Location : NSObject

@property (copy, nonatomic) NSString *name;

- (id)init:(NSDictionary *)jsonDictionary;
+ (BOOL)validateJsonDictionary:(NSDictionary *)jsonDictionary;

@end
