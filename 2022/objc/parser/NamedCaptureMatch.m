//
//  NamedCaptureMatch.m
//  parser
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import "NamedCaptureMatch.h"

@interface NamedCaptureMatch()
@property (nonatomic, strong) NSTextCheckingResult *result;
@property (nonatomic, strong) NSString *haystack;
@end

@implementation NamedCaptureMatch

+ (NamedCaptureMatch *)matchForTextCheckingResult:(NSTextCheckingResult *)result inInput:(NSString *)haystack {
    NamedCaptureMatch *m = [[NamedCaptureMatch alloc] init];
    
    if (m) {
        m.range = result.range;
        m.result = result;
        m.haystack = haystack;
    }
    
    return m;
}

- (NSString *)valueForGroup:(NSString *)groupName {
    return [self.haystack substringWithRange:[self.result rangeWithName:groupName]];
}

@end
