#import <Foundation/Foundation.h>
#import "rvalue.h"
#import "Pipe.h"
NS_ASSUME_NONNULL_BEGIN
@protocol Instruction;

typedef enum {
	PS_RUNNING,
	PS_DONE,
	PS_BLOCKED
} ProcessState;

@interface Process : NSObject

@property (nonatomic, weak, nullable) id<Readable> reader;
@property (nonatomic, weak, nullable) id<Writeable> writer;
@property (nonatomic, readonly, assign) int ip;
@property (nonatomic, readonly, assign) ProcessState state;


- (instancetype)initWithId:(int)pid;
- (void)execute:(NSArray<NSObject<Instruction> *> * _Nonnull)instructions;
- (long long)valueInRegister:(char)name;
- (void)setRegister:(char) name to:(long long)value;
- (void)sendValue:(long long)value;
- (void)receiveValueAndDo:(void (^)(long long))callback;
- (long long)evaluate:(Rvalue)rvalue;

@end

NS_ASSUME_NONNULL_END
