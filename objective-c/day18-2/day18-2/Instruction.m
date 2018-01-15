#import "Instruction.h"
#import "Process.h"


static Class<Instruction> classForOpcode(NSString *opcode);
static Rvalue parseArg(NSString *s);

NSArray<NSObject<Instruction> *> *parseInstructions(NSArray<NSString *> *lines) {
	NSMutableArray<NSObject<Instruction> *> *result = [NSMutableArray array];
	
	for (NSString *line in lines) {
		if (line.length > 0) {
			[result addObject:parseInstruction(line)];
		}
	}
	
	return result;
}

NSObject<Instruction> *parseInstruction(NSString *input) {
	NSArray<NSString *> *tokens = [input componentsSeparatedByString:@" "];
	Class<Instruction> class = classForOpcode(tokens[0]);
	return [class fromTokens:tokens];
}

static Class<Instruction> classForOpcode(NSString *opcode) {
	static NSDictionary<NSString *, Class<Instruction>> *lookup = nil;
	
	if (!lookup) {
		lookup = @{
				   @"snd": [SoundInstruction class],
				   @"rcv": [ReceiveInstruction class],
				   @"set": [SetInstruction class],
				   @"add": [AddInstruction class],
				   @"mul": [MulInstruction class],
				   @"mod": [ModInstruction class],
				   @"jgz": [JumpInstruction class]
				   };
	}
	
	if (!lookup[opcode]) {
		NSString *reason = [NSString stringWithFormat:@"Unrecognized opcode: %@", opcode];
		@throw [NSException exceptionWithName:NSInvalidArgumentException
									   reason:reason
									 userInfo:nil];
	}
	
	return lookup[opcode];
}

static Rvalue parseArg(NSString *s) {
	int value = -1;
	Rvalue result;
	
	if ([[NSScanner scannerWithString:s] scanInt:&value]) {
		result.isRef = NO;
		result.refOrValue.value = value;
	} else {
		result.isRef = YES;
		result.refOrValue.ref = [s characterAtIndex:0];
	}
	
	return result;
}


@implementation UnaryInstruction
+ (instancetype)fromTokens:(NSArray<NSString *> *)tokens {
	Rvalue arg = parseArg(tokens[1]);
	return [[self alloc] initWithArg:arg];
}

- (instancetype)initWithArg:(Rvalue)arg {
	if ((self = [super init])) {
		_arg1 = arg;
	}
	
	return self;
}

- (NSNumber *)executeInProcess:(Process *)process {
	@throw [NSException exceptionWithName:NSInternalInconsistencyException
								   reason:@"UnaryInstruction subclasses must override executeInProcess:"
								 userInfo:nil];
}

@end

@implementation SoundInstruction
- (NSNumber *)executeInProcess:(Process *)process {
	long long frequency = [process evaluate:self.arg1];
	process.mostRecentSound = [NSNumber numberWithLongLong:frequency];
	return nil;
}
@end

@implementation ReceiveInstruction
- (NSNumber *)executeInProcess:(Process *)process {
	if ([process evaluate:self.arg1] != 0) {
		process.recoveredSound = process.mostRecentSound;
	}
	return nil;
}
@end

@implementation MutatingInstruction
+ (instancetype)fromTokens:(NSArray<NSString *> *)tokens {
	char dest = [tokens[1] characterAtIndex:0];
	Rvalue src = parseArg(tokens[2]);
	return [[self alloc] initWithDest:dest arg:src];
}

- (instancetype)initWithDest:(char)dest arg:(Rvalue)arg2 {
	if ((self = [super init])) {
		_arg1 = dest;
		_arg2 = arg2;
	}
	
	return self;
}

- (NSNumber *)executeInProcess:(Process *)process {
	@throw [NSException exceptionWithName:NSInternalInconsistencyException
								   reason:@"MutatingInstruction subclasses must override executeInProcess:"
								 userInfo:nil];
}
@end

@implementation SetInstruction
- (NSNumber *)executeInProcess:(Process *)process {
	[process setRegister:self.arg1 to:[process evaluate:self.arg2]];
	return nil;
}
@end

@implementation AddInstruction
- (NSNumber *)executeInProcess:(Process *)process {
	long long value = [process valueInRegister:self.arg1] + [process evaluate:self.arg2];
	[process setRegister:self.arg1 to:value];
	return nil;
}
@end

@implementation MulInstruction
- (NSNumber *)executeInProcess:(Process *)process {
	long long value = [process valueInRegister:self.arg1] * [process evaluate:self.arg2];
	[process setRegister:self.arg1 to:value];
	return nil;
}
@end

@implementation ModInstruction
- (NSNumber *)executeInProcess:(Process *)process {
	long long value = [process valueInRegister:self.arg1] % [process evaluate:self.arg2];
	[process setRegister:self.arg1 to:value];
	return nil;
}
@end

@implementation JumpInstruction
+ (instancetype)fromTokens:(NSArray<NSString *> *)tokens {
	return [[self alloc] initWithArg:parseArg(tokens[1])
								 arg:parseArg(tokens[2])];
}

- (instancetype)initWithArg:(Rvalue)arg1 arg:(Rvalue)arg2 {
	if ((self = [super init])) {
		_arg1 = arg1;
		_arg2 = arg2;
	}
	
	return self;
}

- (NSNumber *)executeInProcess:(Process *)process {
	if ([process evaluate:self.arg1] > 0) {
		return [NSNumber numberWithLongLong:[process evaluate:self.arg2]];
	}
	
	return nil;
}
@end
