fun main(args: Array<String>) {
    println(mostPowerfulGrid(2568))
}

data class Coord(val x: Int, val y: Int) {
    operator fun plus(other: Coord): Coord {
        return Coord(x + other.x, y + other.y)
    }
}

data class Grid(val x: Int, val y: Int, val size: Int)

fun mostPowerfulGrid(serial: Int): Grid {
    return (1..300)
            .flatMap { size ->
                val topLeftRange = (1..(300 - size + 1))
                topLeftRange.flatMap { x ->
                    topLeftRange.map { y -> Grid(x, y, size)}
                }
            }
            .maxBy { g -> powerOfGrid(serial, g) }!!
}

fun powerOfGrid(serial: Int, grid: Grid): Int {
    return (0..grid.size - 1)
            .flatMap { x ->
                (0..grid.size - 1)
                        .map { y -> powerOfCell(serial, Coord(grid.x, grid.y) + Coord(x, y))}
            }
            .sum()
}

fun powerOfCell(serial: Int, cell: Coord): Int {
    val rackId = cell.x + 10
    return hundredsDigit((rackId * cell.y + serial) * rackId) - 5
}

fun hundredsDigit(i: Int): Int {
    return (i / 100) % 10
}
