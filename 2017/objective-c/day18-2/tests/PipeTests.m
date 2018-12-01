#import <XCTest/XCTest.h>
#import "Pipe.h"

@interface PipeTests : XCTestCase
@end

@implementation PipeTests

- (void)testNonBlockingRead {
	Pipe *subject = [[Pipe alloc] init];
	[subject write:42];
	__block long long result = -1;
	[subject readAndThen:^(long long value) {
		result = value;
	}];
	XCTAssertEqual(42, result);
}

- (void)testBlockingRead {
	Pipe *subject = [[Pipe alloc] init];
	__block long long result = -1;
	[subject readAndThen:^(long long value) {
		result = value;
	}];
	XCTAssertEqual(-1, result);
	[subject write:17];
	XCTAssertEqual(17, result);
}

@end
