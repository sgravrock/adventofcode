#import <XCTest/XCTest.h>
#import "day22-2.h"

@interface tests : XCTestCase

@end

@implementation tests

- (void)testExamples {
    system("date");
    NSArray *input = @[
                       @"..#",
                       @"#..",
                       @"..."
                       ];
    XCTAssertEqual(burstsCausingInfection(input, 100), 26);
    XCTAssertEqual(burstsCausingInfection(input, 10000000), 2511944);
    system("date");
}


@end
