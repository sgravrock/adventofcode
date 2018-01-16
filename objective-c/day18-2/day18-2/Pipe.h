#import <Foundation/Foundation.h>
NS_ASSUME_NONNULL_BEGIN

@protocol Readable
- (void)readAndThen:(void (^)(long long value))callback;
@end

@protocol Writeable
- (void)write:(long long)value;
@end

@interface Pipe: NSObject<Readable, Writeable>
@property (nonatomic, readonly, assign) int numWrites;
- (BOOL)isEmpty;
@end

NS_ASSUME_NONNULL_END
