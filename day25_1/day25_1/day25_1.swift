func codeAt(position: (x: Int, y: Int), start: Int64) -> Int64 {
    var code = start
    
    for _ in (1...distance(to: position)) {
        code = nextCode(code)
    }
    
    return code
}

func distance(to: (x: Int, y: Int)) -> Int {
    /* TODO: There's probably a closed-form solution to this...
    
        Diagonal index                      Distance
        |  1   2   3   4   5   6       | 1   2   3   4   5   6
     ---|----+---+---+------------  ---+---+---+---+---+---+---+
     1  |  1   2   3   4   5   6     1 |  1   3   6  10  15  21
     2  |  2   3   4   5   6   7     2 |  2   5   9  14  20
     3  |  3   4   5   6   7   8     3 |  4   8  13  19
     4  |  4   5   6   7   8   9     4 |  7  12  18
     5  |  5   6   7   8   9  10     5 | 11  17
     6  |  6   7   8   9  10  11     6 | 16
     Diagonals:
     x + y - 1?
     1 + 1 - 1 = 1
     1 + 2 - 1 = 2
     4 + 4 - 1 = 7
     3 + 4 - 1 = 6
     
     Distance:
     x=3, y=3, diag=5, d=13
     x=1, y=5, diag=5, d=11
     x=5, y=2, diag=6, d=20
    */
    
    var i = 0
    var pos = (x: 1, y: 1)
    
    while pos.x != to.x || pos.y != to.y {
        if pos.y == 1 {
            pos = (x: 1, y: pos.x + 1)
        } else {
            pos = (x: pos.x + 1, y: pos.y - 1)
        }
        
        i += 1
    }
    
    return i
}

func nextCode(_ n: Int64) -> Int64 {
    return (n * 252533) % 33554393
}
