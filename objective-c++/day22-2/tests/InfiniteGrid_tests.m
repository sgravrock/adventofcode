#import <XCTest/XCTest.h>
#import "InfiniteGrid.h"

@interface InfiniteGrid_tests : XCTestCase

@end

@implementation InfiniteGrid_tests

- (void)testParse {
    NSArray *input = @[
                       @"..#",
                       @"#..",
                       @"..."
                       ];
    InfiniteGrid *subject = [InfiniteGrid parse:input];
    
    XCTAssertFalse([subject atX:-1 y:-1]);
    XCTAssertFalse([subject atX:0 y:-1]);
    XCTAssertTrue([subject atX:1 y:-1]);

    XCTAssertTrue([subject atX:-1 y:0]);
    XCTAssertFalse([subject atX:0 y:0]);
    XCTAssertFalse([subject atX:0 y:1]);

    XCTAssertFalse([subject atX:-1 y:1]);
    XCTAssertFalse([subject atX:0 y:1]);
    XCTAssertFalse([subject atX:1 y:1]);
}

- (void)testSets {
    InfiniteGrid *subject = [[InfiniteGrid alloc] init];
    [subject setState:FLAGGED atX:0 y:0];
    XCTAssertEqual([subject atX:0 y:0], FLAGGED);
}

- (void)testDefaultsToClean {
    InfiniteGrid *subject = [[InfiniteGrid alloc] init];
    XCTAssertEqual([subject atX:0 y:0], CLEAN);

}

@end
