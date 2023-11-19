//
//  NamedCapturer.h
//  parser
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import <Foundation/Foundation.h>
#import "NamedCaptureMatch.h"

NS_ASSUME_NONNULL_BEGIN

@interface NamedCapturer : NSObject

// TODO: How do we want to handle invalid regexes?
+ (NamedCapturer *) forPattern:(NSString *)pattern;

@property (nonatomic, strong) NSArray<NSString *> *groupNames;

- (NamedCaptureMatch *)firstMatchIn:(NSString *)haystack;


@end

NS_ASSUME_NONNULL_END
