#import <Foundation/Foundation.h>

@interface InfiniteGrid : NSObject

+ (instancetype)parse:(NSArray<NSString *>*)lines;

- (BOOL)atX:(int)x y:(int)y;
- (void)setAtX:(int)x y:(int)y;
- (void)clearAtX:(int)x y:(int)y;

@end
