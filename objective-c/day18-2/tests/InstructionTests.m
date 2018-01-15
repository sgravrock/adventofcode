#import <XCTest/XCTest.h>
#import "Instruction.h"
#import "Process.h"

@interface InstructionTests : XCTestCase

@end

@implementation InstructionTests

- (void)testParseSndValue {
	SoundInstruction *result = (SoundInstruction *)parseInstruction(@"snd 5");
	XCTAssertTrue([result isKindOfClass:[SoundInstruction class]]);
	XCTAssertFalse(result.arg1.isRef);
	XCTAssertEqual(result.arg1.refOrValue.value, 5);
}

- (void)testParseSndRef {
	SoundInstruction *result = (SoundInstruction *)parseInstruction(@"snd x");
	XCTAssertTrue([result isKindOfClass:[SoundInstruction class]]);
	XCTAssertTrue(result.arg1.isRef);
	XCTAssertEqual(result.arg1.refOrValue.ref, 'x');
}

- (void)testParseRcv {
	ReceiveInstruction *result = (ReceiveInstruction *)parseInstruction(@"rcv 5");
	XCTAssertTrue([result isKindOfClass:[ReceiveInstruction class]]);
	XCTAssertFalse(result.arg1.isRef);
	XCTAssertEqual(result.arg1.refOrValue.value, 5);
}

- (void)testParseSetReg {
	SetInstruction *result = (SetInstruction *)parseInstruction(@"set X Y");
	XCTAssertTrue([result isKindOfClass:[SetInstruction class]]);
	XCTAssertEqual(result.arg1, 'X');
	XCTAssertTrue(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.ref, 'Y');
}

- (void)testParseSetValue {
	SetInstruction *result = (SetInstruction *)parseInstruction(@"set X 1");
	XCTAssertTrue([result isKindOfClass:[SetInstruction class]]);
	XCTAssertEqual(result.arg1, 'X');
	XCTAssertFalse(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.value, 1);
}

- (void)testParseAdd {
	AddInstruction *result = (AddInstruction *)parseInstruction(@"add X 1");
	XCTAssertTrue([result isKindOfClass:[AddInstruction class]]);
	XCTAssertEqual(result.arg1, 'X');
	XCTAssertFalse(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.value, 1);
}

- (void)testParseMul {
	MulInstruction *result = (MulInstruction *)parseInstruction(@"mul X 1");
	XCTAssertTrue([result isKindOfClass:[MulInstruction class]]);
	XCTAssertEqual(result.arg1, 'X');
	XCTAssertFalse(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.value, 1);
}

- (void)testParseMod {
	ModInstruction *result = (ModInstruction *)parseInstruction(@"mod X 1");
	XCTAssertTrue([result isKindOfClass:[ModInstruction class]]);
	XCTAssertEqual(result.arg1, 'X');
	XCTAssertFalse(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.value, 1);
}

- (void)testParseJgz1 {
	JumpInstruction *result = (JumpInstruction *)parseInstruction(@"jgz X 1");
	XCTAssertTrue([result isKindOfClass:[JumpInstruction class]]);
	XCTAssertTrue(result.arg1.isRef);
	XCTAssertEqual(result.arg1.refOrValue.ref, 'X');
	XCTAssertFalse(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.value, 1);
}

- (void)testParseJgz2 {
	JumpInstruction *result = (JumpInstruction *)parseInstruction(@"jgz 1 Y");
	XCTAssertTrue([result isKindOfClass:[JumpInstruction class]]);
	XCTAssertFalse(result.arg1.isRef);
	XCTAssertEqual(result.arg1.refOrValue.value, 1);
	XCTAssertTrue(result.arg2.isRef);
	XCTAssertEqual(result.arg2.refOrValue.ref, 'Y');
}

- (void)testParseMultiple {
	NSArray<NSObject<Instruction> *> *result = parseInstructions(@[
																   @"jgz 1 Y",
																   @"mod X n"
																   ]);
	XCTAssertEqual(result.count, 2);
	XCTAssertTrue([result[0] isKindOfClass:[JumpInstruction class]]);
	XCTAssertTrue([result[1] isKindOfClass:[ModInstruction class]]);
}

- (void)testExecuteSndValue {
	SoundInstruction *subject = (SoundInstruction *)parseInstruction(@"snd 5");
	Process *process = [[Process alloc] init];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertEqual(process.mostRecentSound, [NSNumber numberWithLongLong:5]);
	}];
}

- (void)testExecuteSndReg {
	SoundInstruction *subject = (SoundInstruction *)parseInstruction(@"snd X");
	Process *process = [[Process alloc] init];
	[process setRegister:'X' to:3];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertEqual(process.mostRecentSound, [NSNumber numberWithLongLong:3]);
	}];
}

- (void)testExecuteSet {
	SetInstruction *subject = (SetInstruction *)parseInstruction(@"set X 5");
	Process *process = [[Process alloc] init];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertEqual([process valueInRegister:'X'], 5);
	}];
}

- (void)testExecuteAdd {
	SetInstruction *subject = (SetInstruction *)parseInstruction(@"add X 2");
	Process *process = [[Process alloc] init];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertEqual([process valueInRegister:'X'], 2);
		[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset2) {
			XCTAssertEqual([process valueInRegister:'X'], 4);
		}];
	}];
}

- (void)testExecuteMul {
	MulInstruction *subject = (MulInstruction *)parseInstruction(@"mul X 2");
	Process *process = [[Process alloc] init];
	[process setRegister:'X' to:3];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertEqual([process valueInRegister:'X'], 6);
	}];
}

- (void)testExecuteMod {
	ModInstruction *subject = (ModInstruction *)parseInstruction(@"mod X 2");
	Process *process = [[Process alloc] init];
	[process setRegister:'X' to:3];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertEqual([process valueInRegister:'X'], 1);
	}];
}

- (void)testExecuteRcv {
	ReceiveInstruction *subject = (ReceiveInstruction *)parseInstruction(@"rcv X");
	Process *process = [[Process alloc] init];
	process.mostRecentSound = [NSNumber numberWithInt:42];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
		XCTAssertNil(process.recoveredSound);
		[process setRegister:'X' to:1];
		[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset) {
			XCTAssertEqual(process.recoveredSound, [NSNumber numberWithInt:42]);
		}];
	}];
}

- (void)testExecuteJgz {
	JumpInstruction *subject = (JumpInstruction *)parseInstruction(@"jgz X 5");
	Process *process = [[Process alloc] init];
	[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset1) {
		XCTAssertNil(offset1);
		[process setRegister:'X' to:-1];
		[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset2) {
			XCTAssertNil(offset2);
			[process setRegister:'X' to:1];
			[subject executeInProcess:process andThen:^(NSNumber * _Nullable offset3) {
				XCTAssertEqual(offset3, [NSNumber numberWithLongLong:5]);
			}];
		}];
	}];
}

@end
