#import <XCTest/XCTest.h>
#import "Instruction.h"
#import "Process.h"

@interface ExampleTests : XCTestCase
@end

@implementation ExampleTests

- (void)testExample {
	NSArray<NSString *> *inputs = @[
									 @"set a 1",
									 @"add a 2",
									 @"mul a a",
									 @"mod a 5",
									 @"snd a",
									 @"set a 0",
									 @"rcv a",
									 @"jgz a -1",
									 @"set a 1",
									 @"jgz a -2"
									 ];
	NSArray<NSObject<Instruction> *> *program = parseInstructions(inputs);
	Process *process = [[Process alloc] init];
	[process execute:program];
	XCTAssertEqual(process.recoveredSound, [NSNumber numberWithLongLong:4]);
	XCTAssertEqual([process valueInRegister:'a'], 1);
}

@end
