//
//  AIUtilities.h
//  TravelBot
//
//  Created by Asim Ihsan on 27/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import <Foundation/Foundation.h>

// Autorelease macros copied from FMDB.

#if ! __has_feature(objc_arc)
    #define AIAutorelease(__v) ([__v autorelease]);
    #define AIReturnAutoreleased AIAutorelease

    #define AIRetain(__v) ([__v retain]);
    #define AIReturnRetained AIRetain

    #define AIRelease(__v) ([__v release]);
#else
    // -fobjc-arc
    #define AIAutorelease(__v)
    #define AIReturnAutoreleased(__v) (__v)

    #define AIRetain(__v)
    #define AIReturnRetained(__v) (__v)

    #define AIRelease(__v)
#endif


@interface AIUtilities : NSObject

+ (NSString *)encodeBase64WithString:(NSString *)strData;
+ (NSString *)encodeBase64WithData:(NSData *)objData;
+ (NSData *)decodeBase64WithString:(NSString *)strBase64;

+ (NSData *)bzip2:(NSData *)input;
+ (NSData *)bunzip2:(NSData *)input;
+ (void)bunzip2:(NSString *)inputFilepath outputFilepath:(NSString *)outputFilepath;

+ (UIColor *)colorWithR:(CGFloat)red G:(CGFloat)green B:(CGFloat)blue A:(CGFloat)alpha;

@end
