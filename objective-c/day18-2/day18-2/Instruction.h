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
- (NSNumber *)executeInProcess:(Process *)process;
@end

@interface UnaryInstruction: NSObject<Instruction>
- (instancetype)initWithArg:(Rvalue)arg;
@property (nonatomic, readonly, assign) Rvalue arg1;
@end

@interface SoundInstruction : UnaryInstruction
@end

@interface ReceiveInstruction : UnaryInstruction
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
NSArray<NSObject<Instruction> *> *parseInstructions(NSArray<NSString *> *lines);

NS_ASSUME_NONNULL_END
