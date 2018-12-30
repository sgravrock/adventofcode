import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    println(mostPowerful3x3(2568))
    println("in ${Date().time - start.time}ms")

}

data class Coord(val x: Int, val y: Int) {
    operator fun plus(other: Coord): Coord {
        return Coord(x + other.x, y + other.y)
    }
}

fun mostPowerful3x3(serial: Int): Coord {
    return (1..300)
            .flatMap { x ->
                (1..300).map {y -> Coord(x, y)}
            }
            .maxBy { c -> powerOf3x3(serial, c) }!!
}

fun powerOf3x3(serial: Int, topLeft: Coord): Int {
    return listOf(0, 1, 2)
            .flatMap { x ->
                listOf(0, 1, 2)
                        .map { y -> powerOfCell(serial, topLeft + Coord(x, y))}
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
