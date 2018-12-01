#import <Foundation/Foundation.h>
@protocol Instruction;

@interface Machine : NSObject

@property (nonatomic, readonly, assign) int process1SendCount;

- (void)execute:(NSArray<NSObject<Instruction> *>*)program;

@end
