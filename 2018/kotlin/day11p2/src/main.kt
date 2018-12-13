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
    var maxPower = Int.MIN_VALUE
    var bestGrid: Grid? = null

    for (size in 1 until 300) {
        for (x in 1 until 300 - size + 1) {
            for (y in 1 until 300 - size + 1) {
                val p = powerOfGrid(serial, x, y, size)

                if (p > maxPower) {
                    maxPower = p
                    bestGrid = Grid(x, y, size)
                }
            }
        }
    }

    return bestGrid!!
}

fun powerOfGrid(serial: Int, topLeftX: Int, topLeftY: Int, size: Int): Int {
    var result = 0

    for (x in 0 until size) {
        for (y in 0 until size) {
            result += powerOfCell(serial, topLeftX + x, topLeftY + y)
        }
    }

    return result
}

fun powerOfCell(serial: Int, x: Int, y: Int): Int {
    val rackId = x + 10
    return hundredsDigit((rackId * y + serial) * rackId) - 5
}

fun hundredsDigit(i: Int): Int {
    return (i / 100) % 10
}
