#import <Foundation/Foundation.h>
#import "rvalue.h"
NS_ASSUME_NONNULL_BEGIN
@protocol Instruction;

@interface Process : NSObject

@property (nonatomic, strong, nullable) NSNumber *mostRecentSound;
@property (nonatomic, strong, nullable) NSNumber *recoveredSound;

- (void)execute:(NSArray<NSObject<Instruction> *> * _Nonnull)instructions
		andThen:(void (^)())callback;
- (long long)valueInRegister:(char)name;
- (void)setRegister:(char) name to:(long long)value;
- (long long)evaluate:(Rvalue)rvalue;

@end
NS_ASSUME_NONNULL_END
