import java.lang.Exception
import kotlin.math.min
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val millis = measureTimeMillis {
        println(Cave(9171, 7, 721).fewestMinutesToTarget())
    }
    println("in ${millis}ms")
}

data class Coord(val x: Int, val y: Int)
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
}

data class SearchState(val pos: Coord, val tool: Tool) {
    fun neighbors(cave: ICave): List<SearchState> {
        val region = cave.regionType(pos)
        val result = mutableListOf<SearchState>()
        val tools = Tool.values()

        for (x in -1..1) {
            for (y in -1..1) {
                if ((x == 0) xor (y == 0)) {
                    val n = Coord(pos.x + x, pos.y + y)

                    if (n.x >= 0 && n.y >= 0 && n.y < cave.depth) {
                        for (t in tools) {
                            if (canUseTool(t, region) && canUseTool(t, cave.regionType(n))) {
                                result.add(SearchState(n, t))
                            }
                        }
                    }
                }
            }
        }

        return result
    }
}

fun canUseTool(t: Tool, r: Region): Boolean {
    return t != when (r) {
        Region.Rocky -> Tool.None
        Region.Wet -> Tool.Torch
        Region.Narrow -> Tool.ClimbingGear
    }
}