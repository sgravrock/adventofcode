#import "Process.h"
#import "Instruction.h"
NS_ASSUME_NONNULL_BEGIN

@interface Process()
@property (nonatomic, readonly, strong) NSMutableDictionary<NSString *, NSNumber *>* registers;
@property (nonatomic, strong) Program *instructions;
@property (nonatomic, assign) int ip;

@end

@implementation Process

- (instancetype)init {
	return [self initWithId:-1];
}

- (instancetype)initWithId:(int)pid {
	if ((self = [super init])) {
		_registers = [NSMutableDictionary dictionary];
		[self setRegister:'p' to:pid];
	}
	
	return self;
}

- (void)execute:(NSArray<NSObject<Instruction> *> *)instructions andThen:(void (^)())callback {
	self.instructions = instructions;
	self.ip = 0;
	[self resumeAndThen:callback];
}

- (void)resumeAndThen:(void (^)())callback {
	if (self.ip >= 0 && self.ip < self.instructions.count) {
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

- (void)sendValue:(long long)value {
	[self.writer write:value];
}

- (void)receiveValueAndDo:(void (^)(long long))callback {
	[self.reader readAndThen:callback];
}

- (long long)evaluate:(Rvalue)rvalue {
	if (rvalue.isRef) {
		return [self valueInRegister:rvalue.refOrValue.ref];
	} else {
		return rvalue.refOrValue.value;
	}
}

@end
NS_ASSUME_NONNULL_END
