//
//  AIUtilities.m
//  TravelBot
//
//  Created by Asim Ihsan on 27/08/2012.
//  Copyright (c) 2012 Asim Ihsan. All rights reserved.
//

#import "AIUtilities.h"
#import "bzlib.h"
#import "CocoaLumberJack/DDLog.h"

static int ddLogLevel = LOG_LEVEL_VERBOSE;

/**
 * Original location of this base64 code: https://github.com/mikeho/QSUtilities/blob/master/QSStrings.m
 *
 * Copyright (c) 2010 - 2011, Quasidea Development, LLC
 * For more information, please go to http://www.quasidea.com/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/*
 * bzip2 reference: http://www.bzip.org/1.0.3/html/low-level.html
 */

static const char _base64EncodingTable[64] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const short _base64DecodingTable[256] = {
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -1, -1, -2, -1, -1, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, 62, -2, -2, -2, 63,
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -2, -2, -2, -2, -2, -2,
	-2,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
	15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -2, -2, -2, -2, -2,
	-2, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
	41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2
};

@implementation AIUtilities

+ (NSData *)bzip2:(NSData *)input
{
    assert(input != nil);
    
    int bzret;
    const int buffer_size = 100000;
    const int compression = 9;
    
    bz_stream stream = { 0 };
    stream.next_in = (char *)input.bytes;
    stream.avail_in = input.length;

    NSMutableData *buffer = [NSMutableData dataWithLength:buffer_size];
    stream.next_out = [buffer mutableBytes];
    stream.avail_out = buffer_size;
    NSMutableData *compressed = [NSMutableData data];
    
    // Reference: http://www.bzip.org/1.0.3/html/low-level.html
    BZ2_bzCompressInit(&stream,         // bz_stream *strm
                       compression,     // int blockSize100k, 0->9
                       0,               // int verbosity, 0->4
                       0);              // int workFactor, 0->250
    
    @try
    {
        do
        {
            bzret = BZ2_bzCompress(&stream, (stream.avail_in) ? BZ_RUN : BZ_FINISH);
            if (bzret != BZ_RUN_OK && bzret != BZ_STREAM_END)
            {
                @throw [NSException exceptionWithName:@"bzip2" reason:@"BZ2_bzCompress failed" userInfo:nil];
            }
            [compressed appendBytes:[buffer bytes] length:(buffer_size - stream.avail_out)];
            stream.next_out = [buffer mutableBytes];
            stream.avail_out = buffer_size;
        } while (bzret != BZ_STREAM_END);
    }
    @finally
    {
        BZ2_bzCompressEnd(&stream);
    }
    return compressed;
}

+ (NSData *)bunzip2:(NSData *)input
{
    assert(input != nil);
    
    int bzret;
    const int buffer_size = 100000;
    
    bz_stream stream = { 0 };
    stream.next_in = (char *)input.bytes;
    stream.avail_in = input.length;
    
    NSMutableData *buffer = [NSMutableData dataWithLength:buffer_size];
    stream.next_out = [buffer mutableBytes];
    stream.avail_out = buffer_size;
    NSMutableData *decompressed = [NSMutableData data];
    
    // Reference: http://www.bzip.org/1.0.3/html/low-level.html
    BZ2_bzDecompressInit(&stream,           // bz_stream *strm
                         0,                 // int verbosity
                         YES);              // int small
    @try
    {
        do
        {
            bzret = BZ2_bzDecompress(&stream);
            if (bzret != BZ_OK && bzret != BZ_STREAM_END)
            {
                @throw [NSException exceptionWithName:@"bzip2" reason:@"BZ2_bzDecompress failed" userInfo:nil];
            }
            [decompressed appendBytes:[buffer bytes] length:(buffer_size - stream.avail_out)];
            stream.next_out = [buffer mutableBytes];
            stream.avail_out = buffer_size;
        } while(bzret != BZ_STREAM_END);
    }
    @finally
    {
        BZ2_bzDecompressEnd(&stream);
    }
    return decompressed;
}

+ (void)bunzip2:(NSString *)inputFilepath
 outputFilepath:(NSString *)outputFilepath
{
    DDLogVerbose(@"AIUtilities:bunzip2 entry. inputFilepath: %@, outputFilepath: %@",
                 inputFilepath, outputFilepath);
    
    // -------------------------------------------------------------------------
    //  Initialize local variables.
    // -------------------------------------------------------------------------
    int bzret;
    const int buffer_size = 100000;
    bz_stream stream = { 0 };
    NSError *error;
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  Open input file for reading.
    // -------------------------------------------------------------------------
    NSData *input = [NSData dataWithContentsOfFile:inputFilepath
                                           options:NSDataReadingUncached
                                             error:&error];
    if (error)
    {
        DDLogError(@"bunzip2 failed to open inputFilepath %@ with error: %@",
                   inputFilepath, [error localizedDescription]);
        return;
    }
    stream.next_in = (char *)input.bytes;
    stream.avail_in = input.length;
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //  Set up the output file and buffer.
    // -------------------------------------------------------------------------
    [[NSFileManager defaultManager] createFileAtPath:outputFilepath contents:nil attributes:nil];
    NSFileHandle *outputFileHandle = [NSFileHandle fileHandleForWritingAtPath:outputFilepath];
    
    NSMutableData *buffer = [NSMutableData dataWithLength:buffer_size];
    stream.next_out = [buffer mutableBytes];
    stream.avail_out = buffer_size;
    NSMutableData *decompressed = [NSMutableData data];
    // -------------------------------------------------------------------------
    
    // Reference: http://www.bzip.org/1.0.3/html/low-level.html
    BZ2_bzDecompressInit(&stream,           // bz_stream *strm
                         0,                 // int verbosity
                         YES);              // int small
    @try
    {
        do
        {
            bzret = BZ2_bzDecompress(&stream);
            if (bzret != BZ_OK && bzret != BZ_STREAM_END)
            {
                DDLogError(@"AIUtilities:bunzip2 BZ2_bzDecompress failed.");
                [outputFileHandle closeFile];
                @throw [NSException exceptionWithName:@"bzip2"
                                               reason:@"BZ2_bzDecompress failed"
                                             userInfo:nil];

            }
            
            // Put currently decompressed bytes into a NSData buffer, flush it
            // to the output file, and then clear the buffer.
            [decompressed appendBytes:[buffer bytes] length:(buffer_size - stream.avail_out)];
            [outputFileHandle writeData:decompressed];
            [decompressed setLength:0];

            stream.next_out = [buffer mutableBytes];
            stream.avail_out = buffer_size;
            // -----------------------------------------------------------------
        } while(bzret != BZ_STREAM_END);
    }
    @finally
    {
        BZ2_bzDecompressEnd(&stream);
    }
    [outputFileHandle closeFile];
    DDLogVerbose(@"AIUtilities:bunzip2 with file, returning.");
}

+ (NSString *)encodeBase64WithString:(NSString *)strData {
	return [AIUtilities encodeBase64WithData:[strData dataUsingEncoding:NSUTF8StringEncoding]];
}

/*
 Base64 Functions ported from PHP's Core
 
 +----------------------------------------------------------------------+
 | PHP Version 5                                                        |
 +----------------------------------------------------------------------+
 | Copyright (c) 1997-2010 The PHP Group                                |
 +----------------------------------------------------------------------+
 | This source file is subject to version 3.01 of the PHP license,      |
 | that is bundled with this package in the file LICENSE, and is        |
 | available through the world-wide-web at the following url:           |
 | http://www.php.net/license/3_01.txt                                  |
 | If you did not receive a copy of the PHP license and are unable to   |
 | obtain it through the world-wide-web, please send a note to          |
 | license@php.net so we can mail you a copy immediately.               |
 +----------------------------------------------------------------------+
 | Author: Jim Winstead <jimw@php.net>                                  |
 +----------------------------------------------------------------------+
 */

+ (NSString *)encodeBase64WithData:(NSData *)objData {
	const unsigned char * objRawData = [objData bytes];
	char * objPointer;
	char * strResult;
    
	// Get the Raw Data length and ensure we actually have data
	size_t intLength = [objData length];
	if (intLength == 0) return nil;
    
	// Setup the String-based Result placeholder and pointer within that placeholder
	strResult = (char *)calloc(((intLength + 2) / 3) * 4, sizeof(char));
	objPointer = strResult;
    
	// Iterate through everything
	while (intLength > 2) { // keep going until we have less than 24 bits
		*objPointer++ = _base64EncodingTable[objRawData[0] >> 2];
		*objPointer++ = _base64EncodingTable[((objRawData[0] & 0x03) << 4) + (objRawData[1] >> 4)];
		*objPointer++ = _base64EncodingTable[((objRawData[1] & 0x0f) << 2) + (objRawData[2] >> 6)];
		*objPointer++ = _base64EncodingTable[objRawData[2] & 0x3f];
        
		// we just handled 3 octets (24 bits) of data
		objRawData += 3;
		intLength -= 3;
	}
    
	// now deal with the tail end of things
	if (intLength != 0) {
		*objPointer++ = _base64EncodingTable[objRawData[0] >> 2];
		if (intLength > 1) {
			*objPointer++ = _base64EncodingTable[((objRawData[0] & 0x03) << 4) + (objRawData[1] >> 4)];
			*objPointer++ = _base64EncodingTable[(objRawData[1] & 0x0f) << 2];
			*objPointer++ = '=';
		} else {
			*objPointer++ = _base64EncodingTable[(objRawData[0] & 0x03) << 4];
			*objPointer++ = '=';
			*objPointer++ = '=';
		}
	}
    
	NSString *strToReturn = [[NSString alloc] initWithBytesNoCopy:strResult
                                                           length:objPointer - strResult
                                                         encoding:NSASCIIStringEncoding
                                                     freeWhenDone:YES];
    AIAutorelease(strToReturn);
	return strToReturn;
}

+ (NSData *)decodeBase64WithString:(NSString *)strBase64 {
    DDLogVerbose(@"AIUtilitites:decodeBase64WithString entry. strBase64: %@", strBase64);
    
	const char * objPointer = [strBase64 cStringUsingEncoding:NSASCIIStringEncoding];
	if (objPointer == NULL)  return nil;
	size_t intLength = strlen(objPointer);
	int intCurrent;
	int i = 0, j = 0, k;
    
	unsigned char * objResult;
	objResult = calloc(intLength, sizeof(unsigned char));
    
	// Run through the whole string, converting as we go
	while ( ((intCurrent = *objPointer++) != '\0') && (intLength-- > 0) ) {
		if (intCurrent == '=') {
			if (*objPointer != '=' && ((i % 4) == 1)) {// || (intLength > 0)) {
				// the padding character is invalid at this point -- so this entire string is invalid
                DDLogVerbose(@"the padding character %c at %d is invalid.", *objPointer, intCurrent);
				free(objResult);
				return nil;
			}
			continue;
		}
        
		intCurrent = _base64DecodingTable[intCurrent];
		if (intCurrent == -1) {
			// we're at a whitespace -- simply skip over
			continue;
		} else if (intCurrent == -2) {
			// we're at an invalid character
            DDLogVerbose(@"the character %c at %d is invalid", *objPointer, intCurrent);
			free(objResult);
			return nil;
		}
        
		switch (i % 4) {
			case 0:
				objResult[j] = intCurrent << 2;
				break;
                
			case 1:
				objResult[j++] |= intCurrent >> 4;
				objResult[j] = (intCurrent & 0x0f) << 4;
				break;
                
			case 2:
				objResult[j++] |= intCurrent >>2;
				objResult[j] = (intCurrent & 0x03) << 6;
				break;
                
			case 3:
				objResult[j++] |= intCurrent;
				break;
		}
		i++;
	}
    
	// mop things up if we ended on a boundary
	k = j;
	if (intCurrent == '=') {
		switch (i % 4) {
			case 1:
				// Invalid state
                DDLogVerbose(@"invalid state.");
				free(objResult);
				return nil;
                
			case 2:
				k++;
				// flow through
			case 3:
				objResult[k] = 0;
		}
	}
    
	// Cleanup and setup the return NSData
    NSData *return_value = [[NSData alloc] initWithBytesNoCopy:objResult
                                                        length:j];
    AIAutorelease(return_value);
    DDLogVerbose(@"returning: %@", return_value);
    return return_value;
}

@end
