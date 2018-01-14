#import "Instruction.h"


static Class<Instruction> classForOpcode(NSString *opcode);
static Argument parseArg(NSString *s);

NSArray<NSObject<Instruction> *> *parseInstructions(NSString *input) {
	NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
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

static Argument parseArg(NSString *s) {
	int value = -1;
	Argument result;
	
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
	Argument arg = parseArg(tokens[1]);
	return [[self alloc] initWithArg:arg];
}

- (instancetype)initWithArg:(Argument)arg {
	if ((self = [super init])) {
		_arg1 = arg;
	}
	
	return self;
}
@end

@implementation SoundInstruction
@end

@implementation ReceiveInstruction
@end

@implementation MutatingInstruction
+ (instancetype)fromTokens:(NSArray<NSString *> *)tokens {
	char dest = [tokens[1] characterAtIndex:0];
	Argument src = parseArg(tokens[2]);
	return [[self alloc] initWithDest:dest arg:src];
}

- (instancetype)initWithDest:(char)dest arg:(Argument)arg2 {
	if ((self = [super init])) {
		_arg1 = dest;
		_arg2 = arg2;
	}
	
	return self;
}
@end

@implementation SetInstruction
@end

@implementation AddInstruction
@end

@implementation MulInstruction
@end

@implementation ModInstruction
@end

@implementation JumpInstruction
+ (instancetype)fromTokens:(NSArray<NSString *> *)tokens {
	return [[self alloc] initWithArg:parseArg(tokens[1])
								 arg:parseArg(tokens[2])];
}

- (instancetype)initWithArg:(Argument)arg1 arg:(Argument)arg2 {
	if ((self = [super init])) {
		_arg1 = arg1;
		_arg2 = arg2;
	}
	
	return self;
}
@end
