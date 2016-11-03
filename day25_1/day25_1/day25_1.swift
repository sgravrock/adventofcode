func codeAt(position: (x: Int, y: Int), start: Int64) -> Int64 {
    var code = start
    
    for _ in (1...distance(to: position)) {
        code = nextCode(code)
    }
    
    return code
}

func distance(to: (x: Int, y: Int)) -> Int {
    let diagonal = to.x + to.y - 1
    let sumOfPreviousDiags = diagonal * (diagonal + 1) / 2 - diagonal
    return sumOfPreviousDiags + to.x - 1
}

func nextCode(_ n: Int64) -> Int64 {
    return (n * 252533) % 33554393
}
