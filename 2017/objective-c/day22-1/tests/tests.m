#import <XCTest/XCTest.h>
#import "day22-1.h"

@interface tests : XCTestCase

@end

@implementation tests

- (void)testExamples {
    NSArray *input = @[
                       @"..#",
                       @"#..",
                       @"..."
                       ];
    XCTAssertEqual(burstsCausingInfection(input, 7), 5);
    XCTAssertEqual(burstsCausingInfection(input, 70), 41);
    XCTAssertEqual(burstsCausingInfection(input, 10000), 5587);
}


@end
