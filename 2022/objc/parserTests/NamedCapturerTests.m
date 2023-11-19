//
//  NamedCapturerTests.m
//  parserTests
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import <XCTest/XCTest.h>
#import "NamedCapturer.h"

@interface NamedCapturerTests : XCTestCase

@end

@implementation NamedCapturerTests

- (void)testGroupNames {
    NSString *pattern = @"^(?<foo>fooo)\\.(?<bar>barr)\\.(?<baz>bazz)$";
    NamedCapturer *subject = [NamedCapturer forPattern:pattern];
    NSArray *expected = @[@"foo", @"bar", @"baz"];
    XCTAssertEqualObjects(subject.groupNames, expected);
}

- (void)testParseSuccess {
    NSString *pattern = @"^(?<foo>[a-z]+)\\.(?<bar>[a-z]+)";
    NSString *input = @"asdf.qwer.zxcv";
    NamedCapturer *subject = [NamedCapturer forPattern:pattern];
    
    NamedCaptureMatch *match = [subject firstMatchIn:input];
    XCTAssertNotNil(match);
    XCTAssertEqualObjects([match valueForGroup:@"foo"], @"asdf");
    XCTAssertEqualObjects([match valueForGroup:@"bar"], @"qwer");
    XCTAssertEqual(match.range.location, 0);
    XCTAssertEqual(match.range.length, @"asdf.qwer".length);
}

@end
