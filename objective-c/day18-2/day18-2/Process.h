#import <Foundation/Foundation.h>
#import "rvalue.h"
#import "Pipe.h"
NS_ASSUME_NONNULL_BEGIN
@protocol Instruction;

@interface Process : NSObject

@property (nonatomic, weak, nullable) id<Readable> reader;
@property (nonatomic, weak, nullable) id<Writeable> writer;

- (instancetype)initWithId:(int)pid;
- (void)execute:(NSArray<NSObject<Instruction> *> * _Nonnull)instructions
		andThen:(void (^)())callback;
- (long long)valueInRegister:(char)name;
- (void)setRegister:(char) name to:(long long)value;
- (void)sendValue:(long long)value;
- (void)receiveValueAndDo:(void (^)(long long))callback;
- (long long)evaluate:(Rvalue)rvalue;

@end

NS_ASSUME_NONNULL_END
