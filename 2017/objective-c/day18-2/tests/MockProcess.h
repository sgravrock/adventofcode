#import <Foundation/Foundation.h>
#import "Process.h"

@interface MockProcess : Process
@property (nonatomic, readonly, strong) NSMutableArray *sentValues;
@end
