//
//  parser.m
//  parser
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import "Parser.h"
#import "NamedCapturer.h"

@interface Parser()

@property (nonatomic, strong) NamedCapturer *regex;
@property (nonatomic, assign) Class resultClass;

@end

@implementation Parser

+ (Parser *) forRegex:(NSString *)pattern into:(Class)resultClass {
    Parser *p = [[Parser alloc] init];
    
    if (p) {
        p.regex = [NamedCapturer forPattern:pattern];
        p.resultClass = resultClass;
    }
    
    return p;
}

- (id) parse:(NSString *)input {
    NamedCaptureMatch *match = [self.regex firstMatchIn:input];
    
    // TODO: better error reporting
    if (match.range.location == NSNotFound) {
        return nil;
    }
    
    // TODO: can build this in advance
    NSMutableString *selStr = [[NSMutableString alloc] initWithString:@"initWith"];
    
    for (NSInteger i = 0; i < self.regex.groupNames.count; i++ ) {
        NSString *name = self.regex.groupNames[i];
        
        if (i == 0) {
            name = [name capitalizedString];
        }
        
        [selStr appendFormat:@"%@:", name];
    }
    
    SEL initSelector = NSSelectorFromString(selStr);
    NSMethodSignature *sig = [self.resultClass instanceMethodSignatureForSelector:initSelector];
    NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:sig];
    invocation.selector = initSelector;
    
    for (NSInteger i = 0; i < self.regex.groupNames.count; i++ ) {
        NSString *value = [match valueForGroup:self.regex.groupNames[i]];
        [invocation setArgument:&value atIndex:i + 2];
    }
    
    // TODO: do we need to retain and/or release this?
    id allocated = [self.resultClass alloc];
    
    if (!allocated) {
        return nil;
    }
    
    invocation.target = allocated;
    [invocation invoke];
    CFTypeRef result;
    [invocation getReturnValue:&result];
    
    if (result) {
        CFRetain(result);
    }
    
    return (__bridge_transfer id)result;
}

@end
