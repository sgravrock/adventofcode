#import "Process.h"
#import "Instruction.h"
NS_ASSUME_NONNULL_BEGIN

@interface Process()
@property (nonatomic, readonly, strong) NSMutableDictionary<NSString *, NSNumber *>* registers;
@property (nonatomic, strong) NSArray<NSObject<Instruction> *> *instructions;
@property (nonatomic, assign) int ip;
@end

@implementation Process

- (instancetype)init {
	if ((self = [super init])) {
		_registers = [NSMutableDictionary dictionary];
	}
	
	return self;
}

- (void)execute:(NSArray<NSObject<Instruction> *> *)instructions andThen:(void (^)())callback {
	self.instructions = instructions;
	self.ip = 0;
	[self resumeAndThen:callback];
}

- (void)resumeAndThen:(void (^)())callback {
	if (self.ip >= 0 && self.ip < self.instructions.count && self.recoveredSound == nil) {
		[self.instructions[self.ip] executeInProcess:self andThen:^(NSNumber * _Nullable offset) {
			self.ip += (offset == nil ? 1 : [offset intValue]);
			[self resumeAndThen:callback];
		}];
	} else {
		callback();
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
NS_ASSUME_NONNULL_END
