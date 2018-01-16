#import "Process.h"
#import "Instruction.h"
NS_ASSUME_NONNULL_BEGIN

@interface Process()
@property (nonatomic, readonly, strong) NSMutableDictionary<NSString *, NSNumber *>* registers;
@property (nonatomic, strong) Program *instructions;
@property (nonatomic, assign) int ip;
@property (nonatomic, assign) ProcessState state;
@end

@implementation Process

- (instancetype)init {
	return [self initWithId:-1];
}

- (instancetype)initWithId:(int)pid {
	if ((self = [super init])) {
		_registers = [NSMutableDictionary dictionary];
		_state = PS_DONE;
		[self setRegister:'p' to:pid];
	}
	
	return self;
}

- (void)execute:(NSArray<NSObject<Instruction> *> *)instructions {
	self.instructions = instructions;
	self.ip = 0;
	[self resume];
}

- (void)resume {
	self.state = PS_RUNNING;
	
	while (self.ip >= 0 && self.ip < self.instructions.count) {
		__block BOOL wentAsync = NO;
		__block BOOL completedSynchronously = NO;
		
		[self.instructions[self.ip] executeInProcess:self andThen:^(NSNumber * _Nullable offset) {
			self.ip += (offset == nil ? 1 : [offset intValue]);
			
			if (wentAsync) {
				[self resume];
			} else {
				completedSynchronously = YES;
			}
		}];
		
		if (!completedSynchronously) {
			wentAsync = YES;
			self.state = PS_BLOCKED;
			return;
		}
	}
	
	self.state = PS_DONE;
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
