#import "InfiniteGrid.h"
#include <map>


@interface InfiniteGrid()
@property (nonatomic, assign) std::map<Coord, NodeState> *nodes;
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
                [grid setState:INFECTED atX:x - xOffset y:y - yOffset];
            }
        }
    }
    
    return grid;
}

- (instancetype)init {
    if ((self = [super init])) {
        self.nodes = new std::map<Coord, NodeState>;
    }
    
    return self;
}

- (void)dealloc {
    delete _nodes;
    _nodes = nullptr;
}

- (NodeState)atX:(int)x y:(int)y {
    Coord c = {.x = x, .y = y};
    auto iter = self.nodes->find(c);
    
    if (iter == self.nodes->end()) {
        return CLEAN;
    }  else {
        return iter->second;
    }
}

- (void)setState:(NodeState)state atX:(int)x y:(int)y {
    Coord c = {.x = x, .y = y};
    (*self.nodes)[c] = state;
}


@end
