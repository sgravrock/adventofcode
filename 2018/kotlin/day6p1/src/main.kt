import java.util.*
import kotlin.math.abs

data class Coord(val x: Int, val y: Int)

sealed class Area {
    object Infinite : Area()
    data class Finite(val coords: Set<Coord>) : Area()
}

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
    println(largestFiniteArea(parseInput(input)))
    println("in ${Date().time - start.time}ms")
}

fun parseInput(input: String): List<Coord> {
    return input.split('\n')
            .map { line ->
                val nums = line.split(", ")
                        .map { t -> t.toInt() }
                Coord(nums[0], nums[1])
            }
}

fun largestFiniteArea(srcCoords: List<Coord>): Int? {
    return findAreas(srcCoords)
            .filter {
                when (it) {
                    is Area.Infinite -> false
                    is Area.Finite -> true
                }
            }
            .map { (it as Area.Finite).coords.size }
            .max()
}

fun findAreas(srcCoords: List<Coord>): List<Area> {
    val visited = mutableSetOf<Coord>()
    val pendingPoints = srcCoords
            .map { PendingPoint(it, null) }
            .toMutableList()
    val builders = srcCoords.map { AreaBuilder() }

    while (!pendingPoints.isEmpty()) {
        val p = pendingPoints.removeAt(pendingPoints.size - 1);
        val distances = srcCoords
                .mapIndexed { i, s -> DistanceFrom(i, distanceBetween(s, p.point)) }
                .sortedBy { it.distance }

        if (distances[0].distance != distances[1].distance) {
            val region = distances[0].srcIx
            visited.add(p.point)
            builders[region].coords.add(p.point)
            val nextPoints = listOf(
                    Coord(p.point.x - 1, p.point.y),
                    Coord(p.point.x + 1, p.point.y),
                    Coord(p.point.x, p.point.y - 1),
                    Coord(p.point.x, p.point.y + 1)
            )
                    .filter { !visited.contains(it) }

            for (np in nextPoints) {
                if (p.predecessorDistances != null && allGreater(distances, p.predecessorDistances)) {
                    builders[region].isFinite = false
                } else {
                    pendingPoints.add(PendingPoint(np, distances))
                }
            }
        }
    }

    return builders.map { it.build() }
}

fun distanceBetween(a: Coord, b: Coord): Int {
    return abs(a.x - b.x) + abs(a.y - b.y)
}

fun allGreater(a: List<DistanceFrom>, b: List<DistanceFrom>): Boolean {
    return a.all { ae -> ae.distance > b.find { be -> be.srcIx == ae.srcIx }!!.distance }
}

data class PendingPoint(val point: Coord, val predecessorDistances: List<DistanceFrom>?)
data class DistanceFrom(val srcIx: Int, val distance: Int)

class AreaBuilder {
    val coords: MutableSet<Coord> = HashSet()
    var isFinite = true

    fun build(): Area {
        return if (isFinite) {
            Area.Finite(coords)
        } else {
            Area.Infinite
        }
    }
}