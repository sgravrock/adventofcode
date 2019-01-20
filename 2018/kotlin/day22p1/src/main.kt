import java.lang.Exception
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val millis = measureTimeMillis {
        println(riskLevel(9171, 7, 721))
    }
    println("in ${millis}ms")
}

data class Coord(val x: Int, val y: Int)
enum class Region { Rocky, Narrow, Wet }

fun riskLevel(depth: Int, targetX: Int, targetY: Int): Int {
    val memo = mutableMapOf<Coord, Int>()
    var sum = 0

    for (x in 0..targetX) {
        for (y in 0..targetY) {
            sum += if ((x == 0 && y == 0) || (x == targetX && y == targetY)) {
                0
            } else when (regionType(depth, x, y, memo)) {
                Region.Narrow -> 2
                Region.Wet -> 1
                else -> 0
            }
        }
    }

    return sum
}

private fun regionType(
    depth: Int, x: Int, y: Int,
    memo: MutableMap<Coord, Int>
): Region {
    return when (erosionLevel(depth, x, y, memo) % 3) {
        0 -> Region.Rocky
        1 -> Region.Wet
        2 -> Region.Narrow
        else -> throw Exception("Can't happen")
    }
}

private fun erosionLevel(
    depth: Int, x: Int, y: Int,
    memo: MutableMap<Coord, Int>
): Int {
    return memo.getOrPut(Coord(x, y), {
        (geologicIndex(depth, x, y, memo) + depth) % 20183
    })
}

private fun geologicIndex(
    depth: Int, x: Int, y: Int,
    memo: MutableMap<Coord, Int>
): Int {
    return if (x == 0) {
        y * 48271
    } else if (y == 0) {
        x * 16807
    } else {
        erosionLevel(depth, x - 1, y, memo) * erosionLevel(depth, x, y - 1, memo)
    }
}