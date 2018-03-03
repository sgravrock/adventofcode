#import "day22-1.h"
#import "InfiniteGrid.h"

typedef enum {
    NORTH = 0,
    EAST = 1,
    SOUTH = 2,
    WEST = 3
} Orientation;

typedef struct {
    int x;
    int y;
} Coord;

Orientation left(Orientation o);
Orientation right(Orientation o);
Coord advance(Coord c, Orientation o);


int burstsCausingInfection(NSArray<NSString *>*input, int maxIterations) {
    InfiniteGrid *grid = [InfiniteGrid parse:input];
    int result = 0;
    Coord coord = {.x = 0, .y = 0};
    Orientation orientation = NORTH;
    
    
    for (int i = 0; i < maxIterations; i++) {
        if ([grid atX:coord.x y:coord.y]) {
            orientation = right(orientation);
            [grid clearAtX:coord.x y:coord.y];
        } else {
            result++;
            orientation = left(orientation);
            [grid setAtX:coord.x y:coord.y];
        }
        
        coord = advance(coord, orientation);
    }
    
    return result;
}

Orientation left(Orientation o) {
    return o == 0 ? 3 : o - 1;
}

Orientation right(Orientation o) {
    return o == 3 ? 0 : o + 1;

}

Coord advance(Coord c, Orientation o) {
    switch (o) {
        case NORTH:
            c.y--;
            break;
        case SOUTH:
            c.y++;
            break;
        case EAST:
            c.x++;
            break;
        case WEST:
            c.x--;
            break;
    }
    
    return c;
}
