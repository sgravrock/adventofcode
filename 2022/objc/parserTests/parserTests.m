//
//  parserTests.m
//  parserTests
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import <XCTest/XCTest.h>
#import "Parser.h"

@interface TestDestination: NSObject

@property (nonatomic, assign) int foo;
@property (nonatomic, strong) NSString *bar;
- (TestDestination *)initWithFoo:(int)foo bar:(NSString *)bar;

@end

@implementation TestDestination

- (TestDestination *)initWithFoo:(int)foo bar:(NSString *)bar {
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
    Parser *parser = [Parser forRegex:@"^(?<foo>[0-9]+) (?<bar>[0-9]+)" into:[TestDestination class]];
    TestDestination *result = [parser parse:@"1234 5678"];
    XCTAssertNotNil(result);
    XCTAssertEqual(result.foo, 1234);
    XCTAssertEqualObjects(result.bar, @"5678");
}

@end
