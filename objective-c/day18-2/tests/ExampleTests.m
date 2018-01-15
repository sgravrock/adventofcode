#import <XCTest/XCTest.h>
#import "Instruction.h"
#import "Machine.h"

@interface ExampleTests : XCTestCase
@end

@implementation ExampleTests

- (void)testExample {
	NSArray<NSString *> *inputs = @[
									@"snd 1",
									@"snd 2",
									@"snd p",
									@"rcv a",
									@"rcv b",
									@"rcv c",
									@"rcv d"
									];
	NSArray<NSObject<Instruction> *> *program = parseInstructions(inputs);
	Machine *machine = [[Machine alloc] init];
	[machine execute:program];
	XCTAssertEqual(machine.process1SendCount, 3);
}

@end
