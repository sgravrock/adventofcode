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

- (void)testSetsAndClears {
    InfiniteGrid *subject = [[InfiniteGrid alloc] init];
    XCTAssertFalse([subject atX:-1 y:1]);
    [subject setAtX:-1 y:1];
    XCTAssertTrue([subject atX:-1 y:1]);
    XCTAssertFalse([subject atX:0 y:1]);
    [subject clearAtX:-1 y:1];
    XCTAssertFalse([subject atX:-1 y:1]);
}

@end
