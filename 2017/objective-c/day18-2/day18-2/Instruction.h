#import <Foundation/Foundation.h>
#import "rvalue.h"
@class Process;

NS_ASSUME_NONNULL_BEGIN

typedef enum {
	Play,
	Set,
	Add,
	Multiply,
	Receive,
	Jump
} InstructionType;

@protocol Instruction
+ (instancetype)fromTokens:(NSArray<NSString *> *)tokens;
- (void)executeInProcess:(Process *)process andThen:(void (^)(NSNumber * _Nullable offset))callback;
@end

typedef NSArray<NSObject<Instruction> *> Program;

@interface UnaryInstruction: NSObject<Instruction>
- (instancetype)initWithArg:(Rvalue)arg;
@property (nonatomic, readonly, assign) Rvalue arg1;
@end

@interface SendInstruction : UnaryInstruction
@end

@interface ReceiveInstruction : NSObject<Instruction>
- (instancetype)initWithDest:(char)arg;
@property (nonatomic, readonly, assign) char arg;
@end

@interface MutatingInstruction : NSObject<Instruction>
- (instancetype)initWithDest:(char)arg1 arg:(Rvalue)arg2;
@property (nonatomic, readonly, assign) char arg1;
@property (nonatomic, readonly, assign) Rvalue arg2;
@end

@interface SetInstruction : MutatingInstruction
@end

@interface AddInstruction : MutatingInstruction
@end

@interface MulInstruction : MutatingInstruction
@end

@interface ModInstruction : MutatingInstruction
@end

@interface JumpInstruction: NSObject<Instruction>
- (instancetype)initWithArg:(Rvalue)arg1 arg:(Rvalue)arg2;
@property (nonatomic, readonly, assign) Rvalue arg1;
@property (nonatomic, readonly, assign) Rvalue arg2;
@end


NSObject<Instruction> *parseInstruction(NSString *input);
Program *parseInstructions(NSArray<NSString *> *lines);

NS_ASSUME_NONNULL_END
