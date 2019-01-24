import kotlin.math.abs
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val ms = measureTimeMillis {
        val classLoader = Point::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        println(constellations(parsePoints(input)).size)
    }
    println("in ${ms}ms")
}

data class Point(val x: Int, val y: Int, val z: Int, val t: Int) {
    fun distanceFrom(other: Point): Int {
        return abs(x - other.x) + abs(y - other.y) + abs(z - other.z) + abs(t - other.t)
    }
}

fun constellations(points: List<Point>): List<Set<Point>> {
    val result = mutableListOf<MutableSet<Point>>()

    for (a in points) {
        for (b in points) {
            if (a != b && a.distanceFrom(b) <= 3) {
                val matches = result.filter { c -> a in c || b in c }

                if (matches.isEmpty()) {
                    result.add(mutableSetOf(a, b))
                } else {
                    result.removeAll(matches)
                    val combination = matches.flatMap { it }.toMutableSet()
                    combination.add(a)
                    combination.add(b)
                    result.add(combination)
                }
            }
        }
    }

    for (p in points) {
        if (!result.any { c -> p in c }) {
            result.add(mutableSetOf(p))
        }
    }

    return result
}

fun parsePoints(input: String): List<Point> {
    return input.lines()
        .map { line ->
            val coords = line.split(",").map { it.toInt() }
            Point(coords[0], coords[1], coords[2], coords[3])
        }
}