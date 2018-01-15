#import "Process.h"
#import "Instruction.h"

@interface Process()
@property (nonatomic, readonly, strong, nonnull) NSMutableDictionary<NSString *, NSNumber *>* registers;
@end

@implementation Process

- (instancetype)init {
	if ((self = [super init])) {
		_registers = [NSMutableDictionary dictionary];
	}
	
	return self;
}

- (void)execute:(NSArray<NSObject<Instruction> *> *)instructions {
	int ip = 0;
	
	while (ip >= 0 && ip < instructions.count && self.recoveredSound == nil) {
		NSNumber *delta = [instructions[ip] executeInProcess:self];
		ip += (delta == nil ? 1 : [delta intValue]);
	}
}

- (NSString *)keyForRegister:(char)name {
	return [NSString stringWithFormat:@"%c", name];
}

- (long long)valueInRegister:(char)name {
	return [self.registers[[self keyForRegister:name]] longLongValue];
}

- (void)setRegister:(char) name to:(long long)value {
	self.registers[[self keyForRegister:name]] = [NSNumber numberWithLongLong:value];
}

- (long long)evaluate:(Rvalue)rvalue {
	if (rvalue.isRef) {
		return [self valueInRegister:rvalue.refOrValue.ref];
	} else {
		return rvalue.refOrValue.value;
	}
}

- (void)playSoundWithFrequency:(int)frequency {
	self.mostRecentSound = [NSNumber numberWithInt:frequency];
}

@end
