#import "InfiniteGrid.h"


@interface InfiniteGrid()
@property (nonatomic, strong) NSMutableSet<NSString *> *coords;
@end

@implementation InfiniteGrid

+ (instancetype)parse:(NSArray<NSString *>*)lines {
    InfiniteGrid *grid = [[InfiniteGrid alloc] init];
    int yOffset = (int)lines.count / 2;

    
    for (int y = 0; y < lines.count; y++) {
        NSString *line = lines[y];
        int xOffset = (int)line.length / 2;
        
        for (int x = 0; x < line.length; x++) {
            if ([line characterAtIndex:x] == '#') {
                [grid setAtX:x - xOffset y:y - yOffset];
            }
        }
    }
    
    return grid;
}

- (instancetype)init {
    if ((self = [super init])) {
        self.coords = [NSMutableSet set];
    }
    
    return self;
}

- (BOOL)atX:(int)x y:(int)y {
    return [self.coords containsObject:[self keyForX:x y:y]];
}

- (void)setAtX:(int)x y:(int)y {
    [self.coords addObject:[self keyForX:x y:y]];
}

- (void)clearAtX:(int)x y:(int)y {
    [self.coords removeObject:[self keyForX:x y:y]];
}

- (NSString *)keyForX:(int)x y:(int) y {
    return [NSString stringWithFormat:@"%d,%d", x, y];
}

@end
