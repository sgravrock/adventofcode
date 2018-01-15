#import <Foundation/Foundation.h>
#import "rvalue.h"
@protocol Instruction;

@interface Machine : NSObject

@property (nonatomic, strong, nullable) NSNumber *mostRecentSound;
@property (nonatomic, strong, nullable) NSNumber *recoveredSound;

- (void)execute:(NSArray<NSObject<Instruction> *> * _Nonnull)instructions;
- (long long)valueInRegister:(char)name;
- (void)setRegister:(char) name to:(long long)value;
- (long long)evaluate:(Rvalue)rvalue;

@end
