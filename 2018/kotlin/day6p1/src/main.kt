import java.util.HashSet
import kotlin.math.abs

data class Coord(val x: Int, val y: Int)

sealed class Area {
    object Infinite : Area()

    data class Finite(val coords: Set<Coord>) : Area()
}

fun main(args: Array<String>) {
    println("hi!")
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
                if (np.x < 0 || np.y < 0 || np.x > 9 || np.y > 9) {  // TODO: detect infinity smarter
                    builders[region].isFinite = false
                } else {
                    pendingPoints.add(PendingPoint(np, p.point))
                }
            }
        }
    }

    return builders.map { it.build() }
}

fun distanceBetween(a: Coord, b: Coord): Int {
    return abs(a.x - b.x) + abs(a.y - b.y)
}

data class PendingPoint(val point: Coord, val predecessor: Coord?)
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