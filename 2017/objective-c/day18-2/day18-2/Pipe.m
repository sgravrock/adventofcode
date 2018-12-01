#import "Pipe.h"
NS_ASSUME_NONNULL_BEGIN

@interface Pipe()
@property (nonatomic, assign) int numWrites;
@property (nonatomic, readonly, strong) NSMutableArray *buf;
@property (nonatomic, strong, nullable) void (^pendingReadCallback)(long long);
@end

@implementation Pipe

- (instancetype)init {
	if ((self = [super init])) {
		_buf = [NSMutableArray array];
		_numWrites = 0;
	}
	
	return self;
}

- (void)readAndThen:(void (^)(long long))callback {
	self.pendingReadCallback = callback;
	
	if (self.buf.count > 0) {
		[self completePendingRead];
	}
}

- (void)completePendingRead {
	NSNumber *ret = self.buf[0];
	[self.buf removeObjectAtIndex:0];
	void (^cb)(long long) = self.pendingReadCallback;
	self.pendingReadCallback = nil;
	cb([ret longLongValue]);
}

- (void)write:(long long)value {
	self.numWrites++;
	[self.buf addObject:[NSNumber numberWithLongLong:value]];
	
	if (self.pendingReadCallback) {
		[self completePendingRead];
	}
}

- (BOOL)isEmpty {
	return self.buf.count == 0;
}

@end
NS_ASSUME_NONNULL_END
