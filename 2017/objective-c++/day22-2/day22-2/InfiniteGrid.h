#import <Foundation/Foundation.h>

typedef enum {
    CLEAN,
    WEAKENED,
    INFECTED,
    FLAGGED
} NodeState;

typedef struct Coord {
    int x;
    int y;
    
#ifdef __cplusplus
    bool operator <(const Coord &other) const {
        return y < other.y || (y == other.y && x < other.x);
    }
#endif
} Coord;


@interface InfiniteGrid : NSObject

+ (instancetype)parse:(NSArray<NSString *>*)lines;

- (NodeState)atX:(int)x y:(int)y;
- (void)setState:(NodeState)state atX:(int)x y:(int)y;

@end
