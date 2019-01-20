import java.lang.Exception
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val millis = measureTimeMillis {
        val cave = Cave.build(9171, Coord(7, 721))
        println(cave.riskLevel())
    }
    println("in ${millis}ms")

}

data class Coord(val x: Int, val y: Int)

enum class Region {
    Mouth,
    Rocky,
    Narrow,
    Wet,
    Target
}

data class Cave(val regions: Map<Coord, Region>) {
    fun riskLevel(): Int {
        val target = regions.asSequence()
            .first { kv -> kv.value == Region.Target }
            .key
        return (0..target.x).asSequence()
            .flatMap { x ->
                (0..target.y).asSequence()
                    .map { y ->
                        when (regions[Coord(x, y)]) {
                            Region.Narrow -> 2
                            Region.Wet -> 1
                            else -> 0
                        }
                    }
            }
            .sum()
    }

    override fun toString(): String {
        val range = regions.coordinateRange()
        return range.y.asSequence()
            .map { y ->
                range.x.asSequence()
                    .map { x ->
                        when (regions[Coord(x, y)]) {
                            Region.Mouth -> 'M'
                            Region.Rocky -> '.'
                            Region.Narrow -> '|'
                            Region.Wet -> '='
                            Region.Target -> 'T'
                            null -> '*'
                        }
                    }.joinToString("")
            }
            .joinToString("\n")
    }

    companion object {
        fun parse(input: String): Cave {
            val grid = mutableMapOf<Coord, Region>()

            input.lineSequence().forEachIndexed { y, line ->
                line.toCharArray().forEachIndexed { x, c ->
                    grid[Coord(x, y)] = when (c) {
                        'M' -> Region.Mouth
                        '.' -> Region.Rocky
                        '|' -> Region.Narrow
                        '=' -> Region.Wet
                        'T' -> Region.Target
                        else -> throw Exception("Unexpected '$c'")
                    }
                }
            }

            return Cave(grid)
        }

        fun build(depth: Int, target: Coord): Cave {
            val mouth = Coord(0, 0)
            val erosionLevels = mutableMapOf<Coord, Int>()
            val regions = mutableMapOf(
                mouth to Region.Mouth,
                target to Region.Target
            )


            for (x in 0..target.x) {
                for (y in 0..target.y) {
                    val c = Coord(x, y)
                    erosionLevels[c] = if (c == mouth || c == target) {
                        0
                    } else if (x == 0) {
                        giToEl(y * 48271, depth)
                    } else if (y == 0) {
                        giToEl(x * 16807, depth)
                    } else {
                        giToEl(
                            erosionLevels[Coord(x-1, y)]!! * erosionLevels[Coord(x, y-1)]!!,
                            depth
                        )
                    }

                    if (c != mouth && c != target) {
                        regions[c] = when (erosionLevels[c]!! % 3) {
                            0 -> Region.Rocky
                            1 -> Region.Wet
                            2 -> Region.Narrow
                            else -> throw Exception("Can't happen")
                        }
                    }
                }
            }

            return Cave(regions)
        }

        private fun giToEl(geologicIndex: Int, caveDepth: Int): Int {
            return (geologicIndex + caveDepth) % 20183
        }
    }
}

class RangePair(val x: IntRange, val y: IntRange)

fun <V> Map<Coord, V>.coordinateRange(): RangePair {
    val xs = this.asSequence().map { it.key.x }
    val ys = this.asSequence().map { it.key.y }
    return RangePair(
        xs.min()!!..xs.max()!!,
        ys.min()!!..ys.max()!!
    )
}