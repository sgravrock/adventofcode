#import "Machine.h"
#import "Process.h"

@interface Machine()
@property (nonatomic, assign) int process1SendCount;
@end

@implementation Machine

- (void)execute:(NSArray<NSObject<Instruction> *>*)program {
	Process *proc0 = [[Process alloc] initWithId:0];
	Process *proc1 = [[Process alloc] initWithId:1];
	Pipe *from0to1 = [[Pipe alloc] init];
	proc0.writer = from0to1;
	proc1.reader = from0to1;
	Pipe *from1to0 = [[Pipe alloc] init];
	proc1.writer = from1to0;
	proc0.reader = from1to0;
	
	[proc0 execute:program andThen:^{
	}];
	
	[proc1 execute:program andThen:^{
	}];
	
	self.process1SendCount = from1to0.numWrites;
}

@end
