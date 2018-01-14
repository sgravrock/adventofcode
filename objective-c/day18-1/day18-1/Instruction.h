#import <Foundation/Foundation.h>
NS_ASSUME_NONNULL_BEGIN

typedef struct Argument {
	BOOL isRef;
	union {
		int value;
		char ref;
	} refOrValue;
} Argument;

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
@end

@interface UnaryInstruction: NSObject<Instruction>
- (instancetype)initWithArg:(Argument)arg;
@property (nonatomic, readonly, assign) Argument arg1;
@end

@interface SoundInstruction : UnaryInstruction
@end

@interface ReceiveInstruction : UnaryInstruction
@end

@interface MutatingInstruction : NSObject<Instruction>
- (instancetype)initWithDest:(char)arg1 arg:(Argument)arg2;
@property (nonatomic, readonly, assign) char arg1;
@property (nonatomic, readonly, assign) Argument arg2;
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
- (instancetype)initWithArg:(Argument)arg1 arg:(Argument)arg2;
@property (nonatomic, readonly, assign) Argument arg1;
@property (nonatomic, readonly, assign) Argument arg2;
@end


NSObject<Instruction> *parseInstruction(NSString *input);
NSArray<NSObject<Instruction> *> *parseInstructions(NSString *input);

NS_ASSUME_NONNULL_END
