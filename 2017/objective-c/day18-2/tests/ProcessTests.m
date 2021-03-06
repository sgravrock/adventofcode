#import <XCTest/XCTest.h>
#import "Process.h"
#import "Instruction.h"

@interface ProcessTests : XCTestCase
@end

@implementation ProcessTests

- (void)testRegisterInitialization {
	Process *subject = [[Process alloc] initWithId:1];
	XCTAssertEqual([subject valueInRegister:'p'], 1);
}

- (void)testRunsToCompletion {
	Process *subject = [[Process alloc] initWithId:0];
	Program *program = parseInstructions(@[@"set a 1", @"set b 2"]);
	[subject execute:program];
	XCTAssertEqual(subject.ip, 2);
	XCTAssertEqual(subject.state, PS_DONE);
	XCTAssertEqual([subject valueInRegister:'a'], 1);
	XCTAssertEqual([subject valueInRegister:'b'], 2);

}

- (void)testRcvReads {
	Process *subject = [[Process alloc] initWithId:0];
	Pipe *pipe = [[Pipe alloc] init];
	[pipe write:42];
	subject.reader = pipe;
	Program *program = parseInstructions(@[@"rcv a"]);
	[subject execute:program];
	XCTAssertEqual(subject.state, PS_DONE);
	XCTAssertEqual([subject valueInRegister:'a'], 42);
	XCTAssertTrue(pipe.isEmpty);
}

- (void)testRcvBlocksIfReaderEmpty {
	Process *subject = [[Process alloc] initWithId:0];
	Pipe *reader = [[Pipe alloc] init];
	subject.reader = reader;
	Program *program = parseInstructions(@[@"rcv a", @"set b 1"]);
	[subject execute:program];
	
	XCTAssertEqual(subject.state, PS_BLOCKED);
	XCTAssertEqual([subject valueInRegister:'a'], 0);
	XCTAssertEqual([subject valueInRegister:'b'], 0);

	[reader write:17];

	XCTAssertEqual(subject.state, PS_DONE);
	XCTAssertEqual([subject valueInRegister:'a'], 17);
	XCTAssertEqual([subject valueInRegister:'b'], 1);
}

@end
