//
//  parserTests.m
//  parserTests
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import <XCTest/XCTest.h>
#import "Parser.h"

@interface TestDestination: NSObject
@property (nonatomic, strong) NSString *foo;
@property (nonatomic, strong) NSString *bar;
- (TestDestination *)initWithFoo:(NSString *)foo bar:(NSString *)bar;
@end

@implementation TestDestination
- (TestDestination *)initWithFoo:(NSString *)foo bar:(NSString *)bar {
    if ((self = [super init])) {
        self.foo = foo;
        self.bar = bar;
    }
    
    return self;
}
@end

@interface parserTests : XCTestCase

@end

@implementation parserTests

- (void) testParseOne {
    Parser *parser = [Parser forRegex:@"^(?<foo>[0-9]+) (?<bar>[a-z]+)" into:[TestDestination class]];
    TestDestination *result = [parser parse:@"1234 abcd"];
    XCTAssertNotNil(result);
    XCTAssertEqualObjects(result.foo, @"1234");
    XCTAssertEqualObjects(result.bar, @"abcd");
}

@end
