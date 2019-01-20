import java.lang.Exception
import kotlin.math.min
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val millis = measureTimeMillis {
        println(Cave(9171, 7, 721).fewestMinutesToTarget())
    }
    println("in ${millis}ms")
}

data class Coord(val x: Int, val y: Int) {
    fun neighbors(): Sequence<Coord> {
        return sequenceOf(
                Coord(x - 1, y),
                Coord(x + 1, y),
                Coord(x, y - 1),
                Coord(x, y + 1)
        )
    }
}

enum class Region { Rocky, Narrow, Wet }
enum class Tool { Torch, ClimbingGear, None }

interface ICave {
    val depth: Int
    fun regionType(pos: Coord): Region;
}

class Cave(override val depth: Int, val targetX: Int, val targetY: Int) : ICave {
    private val memo = mutableMapOf<Coord, Int>()

    fun fewestMinutesToTarget(): Int {
        var bestToTarget = Int.MAX_VALUE
        val best = mutableMapOf<SearchState, Int>()
        val pending = mutableMapOf<SearchState, Int>()
        pending[SearchState(Coord(0, 0), Tool.Torch)] = 0

        while (pending.isNotEmpty()) {
            val (cur, curMinutes) = pending.asSequence().first()
            pending.remove(cur)

            if (best.getOrDefault(cur, Int.MAX_VALUE) <= curMinutes ||
                    bestToTarget <= curMinutes) {
                continue
            }

            best[cur] = curMinutes

            if (cur.pos.x == targetX && cur.pos.y == targetY) {
                val toolChangeMinutes = if (cur.tool == Tool.Torch) 0 else 7
                bestToTarget = min(bestToTarget, curMinutes + toolChangeMinutes)
                continue
            }

            for (n in cur.neighbors(this)) {
                val toolChangeMinutes = if (n.tool == cur.tool) 0 else 7
                val minutes = curMinutes + toolChangeMinutes + 1

                if (minutes < pending.getOrDefault(n, Int.MAX_VALUE)) {
                    pending[n] = minutes
                }
            }
        }

        assert(bestToTarget != Int.MAX_VALUE)
        return bestToTarget
    }

    override fun regionType(pos: Coord): Region {
        if (pos.x == targetX && pos.y == targetY) {
            return Region.Rocky
        }

        return when (erosionLevel(pos) % 3) {
            0 -> Region.Rocky
            1 -> Region.Wet
            2 -> Region.Narrow
            else -> throw Exception("Can't happen")
        }
    }

    private fun erosionLevel(pos: Coord): Int {
        return memo.getOrPut(pos, {
            (geologicIndex(pos) + depth) % 20183
        })
    }

    private fun geologicIndex(pos: Coord): Int {
        return if (pos.x == 0) {
            pos.y * 48271
        } else if (pos.y == 0) {
            pos.x * 16807
        } else {
            erosionLevel(Coord(pos.x - 1, pos.y)) *
                    erosionLevel(Coord(pos.x, pos.y - 1))
        }
    }

    fun printToWidth(width: Int) {
        for (y in 0..(depth - 1)) {
            for (x in 0..(width - 1)) {
                if (x == 0 && y == 0) {
                    print('M')
                } else if (x == targetX && y == targetY) {
                    print('T')
                } else {
                    print(when (regionType(Coord(x, y))) {
                        Region.Rocky -> '.'
                        Region.Narrow -> '|'
                        Region.Wet -> '='
                    })
                }
            }

            println()
        }
    }
}

data class SearchState(val pos: Coord, val tool: Tool) {
    fun neighbors(cave: ICave): Sequence<SearchState> {
        val region = cave.regionType(pos)
        return pos.neighbors()
                .filter { n -> n.x >= 0 && n.y >= 0 && n.y < cave.depth }
                .flatMap { n ->
                    Tool.values().asSequence()
                            .filter {
                                canUseTool(it, region) &&
                                        canUseTool(it, cave.regionType(n))
                            }
                            .map { SearchState(n, it) }
                }
    }
}

fun canUseTool(t: Tool, r: Region): Boolean {
    return t != when (r) {
        Region.Rocky -> Tool.None
        Region.Wet -> Tool.Torch
        Region.Narrow -> Tool.ClimbingGear
    }
}