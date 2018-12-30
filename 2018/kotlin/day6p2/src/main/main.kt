import java.util.*
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

data class Coord(val x: Int, val y: Int)

fun main(args: Array<String>) {
    val start = Date()
    val input = "292, 73\n" +
            "204, 176\n" +
            "106, 197\n" +
            "155, 265\n" +
            "195, 59\n" +
            "185, 136\n" +
            "54, 82\n" +
            "209, 149\n" +
            "298, 209\n" +
            "274, 157\n" +
            "349, 196\n" +
            "168, 353\n" +
            "193, 129\n" +
            "94, 137\n" +
            "177, 143\n" +
            "196, 357\n" +
            "272, 312\n" +
            "351, 340\n" +
            "253, 115\n" +
            "109, 183\n" +
            "252, 232\n" +
            "193, 258\n" +
            "242, 151\n" +
            "220, 345\n" +
            "336, 348\n" +
            "196, 203\n" +
            "122, 245\n" +
            "265, 189\n" +
            "124, 57\n" +
            "276, 204\n" +
            "309, 125\n" +
            "46, 324\n" +
            "345, 228\n" +
            "251, 134\n" +
            "231, 117\n" +
            "88, 112\n" +
            "256, 229\n" +
            "49, 201\n" +
            "142, 108\n" +
            "150, 337\n" +
            "134, 109\n" +
            "288, 67\n" +
            "297, 231\n" +
            "310, 131\n" +
            "208, 255\n" +
            "246, 132\n" +
            "232, 45\n" +
            "356, 93\n" +
            "356, 207\n" +
            "83, 97"
    println(regionWithinDistance(parseInput(input), 9999).size)
    println("in ${Date().time - start.time}ms")
}

fun regionWithinDistance(coords: List<Coord>, maxDistance: Int): Set<Coord> {
    val distances = mutableMapOf<Coord, Int>()
    val pending = mutableSetOf(center(coords))

    while (!pending.isEmpty()) {
        val p = pending.first()
        pending.remove(p)

        for (c in coords) {
            val tmp = distanceBetween(c, p)
            distances[p] = (distances[p] ?: 0) + tmp
        }

        if (distances[p]!! < maxDistance) {
            val nextPoints = listOf(
                    Coord(p.x - 1, p.y),
                    Coord(p.x + 1, p.y),
                    Coord(p.x, p.y - 1),
                    Coord(p.x, p.y + 1)
            )
                    .filter { !distances.containsKey(it) }

            for (np in nextPoints) {
                pending.add(np)
            }
        }
    }

    return distances
            .filter { kv -> kv.value <= maxDistance }
            .map { kv -> kv.key }
            .toSet()
}

fun center(points: List<Coord>): Coord {
    var minX = Int.MAX_VALUE
    var maxX = Int.MIN_VALUE
    var minY = Int.MAX_VALUE
    var maxY = Int.MIN_VALUE

    for (p in points) {
        minX = min(minX, p.x)
        maxX = max(maxY, p.x)
        minY = min(minY, p.y)
        maxY = max(maxY, p.y)
    }

    return Coord(
            (maxX - minX) / 2 + minX,
            (maxY - minY) / 2 + minY
    )
}

fun distanceBetween(a: Coord, b: Coord): Int {
    return abs(a.x - b.x) + abs(a.y - b.y)
}


fun parseInput(input: String): List<Coord> {
    return input.split('\n')
            .map { line ->
                val nums = line.split(", ")
                        .map { t -> t.toInt() }
                Coord(nums[0], nums[1])
            }
}