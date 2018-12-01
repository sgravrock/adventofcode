#import "MockProcess.h"

@interface MockProcess()
@end

@implementation MockProcess

- (instancetype)init {
	if ((self = [super initWithId:0])) {
		_sentValues = [NSMutableArray array];
	}
	
	return self;
}

- (void)sendValue:(long long)value {
	[self.sentValues addObject:@(value)];
}

@end
