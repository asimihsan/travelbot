//
//  TravelBotSearchHeader.h
//  TravelBot
//
//  Created by Asim Ihsan on 13/09/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface TravelBotSearchHeader : UIView

@property (copy, nonatomic) NSString *from;
@property (copy, nonatomic) NSString *to;
@property (copy, nonatomic) NSString *when;

- (id)initWithTableView:(UIView *)tableView
                   from:(NSString *)from
                     to:(NSString *)to
                   when:(NSString *)when;

@end
