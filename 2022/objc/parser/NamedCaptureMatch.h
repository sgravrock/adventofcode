//
//  NamedCaptureMatch.h
//  parser
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NamedCaptureMatch : NSObject

@property (nonatomic, assign) NSRange range;

+ (NamedCaptureMatch *)matchForTextCheckingResult:(NSTextCheckingResult *)result inInput:(NSString *)haystack;

- (NSString *)valueForGroup:(NSString *)groupName;

@end

NS_ASSUME_NONNULL_END
