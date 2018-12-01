#import "day22-2.h"
#import "InfiniteGrid.h"

typedef enum {
    NORTH = 0,
    EAST = 1,
    SOUTH = 2,
    WEST = 3
} Orientation;

Orientation left(Orientation o);
Orientation right(Orientation o);
Orientation reverse(Orientation o);
Coord advance(Coord c, Orientation o);


int burstsCausingInfection(NSArray<NSString *>*input, int maxIterations) {
    InfiniteGrid *grid = [InfiniteGrid parse:input];
    int result = 0;
    Coord coord = {.x = 0, .y = 0};
    Orientation orientation = NORTH;
    
    
    for (int i = 0; i < maxIterations; i++) {
        NodeState state = [grid atX:coord.x y:coord.y];
        NodeState newState;
        
        switch (state) {
            case CLEAN:
                orientation = left(orientation);
                newState = WEAKENED;
                break;
            case WEAKENED:
                newState = INFECTED;
                break;
            case INFECTED:
                orientation = right(orientation);
                newState = FLAGGED;
                break;
            case FLAGGED:
                orientation = reverse(orientation);
                newState = CLEAN;
                break;
        }
        
        [grid setState:newState atX:coord.x y:coord.y];
        
        if (newState == INFECTED) {
            result++;
        }
        
        coord = advance(coord, orientation);
//        NSLog(@"=> %d", newState);
    }
    
    return result;
}

Orientation left(Orientation o) {
    return (Orientation)(o == 0 ? 3 : o - 1);
}

Orientation right(Orientation o) {
    return (Orientation)(o == 3 ? 0 : o + 1);
}

Orientation reverse(Orientation o) {
    switch (o) {
        case NORTH: return SOUTH;
        case SOUTH: return NORTH;
        case EAST: return WEST;
        case WEST: return EAST;
    }
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
