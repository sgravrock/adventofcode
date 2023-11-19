//
//  NamedCapturer.m
//  parser
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import "NamedCapturer.h"

@interface NamedCapturer()

+ (NSArray<NSString *> *) groupNamesForPattern:(NSString *)pattern;

@property (nonatomic, strong) NSRegularExpression *regex;


@end

@implementation NamedCapturer

+ (NamedCapturer *) forPattern:(NSString *)pattern {
    // TODO: error handling
    NSRegularExpression *regex = [[NSRegularExpression alloc] initWithPattern:pattern options:0 error:nil];
    NSArray *groupNames = [self groupNamesForPattern:pattern];
    NamedCapturer *result = [[NamedCapturer alloc] init];
    result.regex = regex;
    result.groupNames = groupNames;
    return result;
}

+ (NSArray<NSString *> *) groupNamesForPattern:(NSString *)pattern {
    // Regex from https://stackoverflow.com/questions/24814974/named-capture-groups-with-nsregularexpression/48309290#48309290
    NSRegularExpression *groupNameRegex = [[NSRegularExpression alloc] initWithPattern:@"\\(\\?\\<(\\w+)\\>" options:0 error:nil];
    NSArray<NSTextCheckingResult *> *matches = [groupNameRegex matchesInString:pattern options:0 range:NSMakeRange(0, pattern.length)];    
    NSMutableArray *result = [NSMutableArray arrayWithCapacity:matches.count];
    
    for (NSTextCheckingResult *match in matches) {
        [result addObject:[pattern substringWithRange:[match rangeAtIndex:1]]];
    }
    
    return result;
}

- (NamedCaptureMatch *)firstMatchIn:(NSString *)haystack {
     NSTextCheckingResult *r = [self.regex firstMatchInString:haystack options:0 range:NSMakeRange(0, haystack.length)];
    return [NamedCaptureMatch matchForTextCheckingResult:r inInput:haystack];

    return nil;
}

@end
