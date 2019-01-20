import java.lang.Exception
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

class Cave(val depth: Int, val targetX: Int, val targetY: Int) {
    private val memo = mutableMapOf<Coord, Int>()

    fun fewestMinutesToTarget(): Int {
        data class SearchState(val pos: Coord, val tool: Tool)

        val best = mutableMapOf<Coord, Int>()
        val pending = mutableMapOf<SearchState, Int>()
        pending[SearchState(Coord(0, 0), Tool.Torch)] = 0

        while (pending.isNotEmpty()) {
            val (cur, curMinutes) = pending.asSequence().first()
            pending.remove(cur)

            if (best.getOrDefault(cur.pos, Int.MAX_VALUE) <= curMinutes ||
                    best.getOrDefault(Coord(targetX, targetY), Int.MAX_VALUE) <= curMinutes) {
                continue
            }

            best[cur.pos] = curMinutes

            if (cur.pos.x == targetX && cur.pos.y == targetY) {
                continue
            }

            for (n in cur.pos.neighbors()) {
                if (n.x >= 0 && n.y >= 0 && n.y < depth) {
                    for (t in Tool.values()) {
                        if (canUseTool(t, cur.pos) && canUseTool(t, n)) {
                            val toolChangeMinutes = if (t == cur.tool) 0 else 7
                            val minutes = curMinutes + toolChangeMinutes + 1
                            val s = SearchState(n, t)

//                            if (n.x == targetX && n.y == targetY) {
//                                println("Considering target-reaching move p=${cur.pos} t=${cur.tool} m=$curMinutes -> p=$n t=$t")
//                            }

                            if (minutes < pending.getOrDefault(s, Int.MAX_VALUE)) {
                                pending[s] = minutes
                            }
                        }
                    }
                }
            }
        }

//        for (e in best) {
//            println(e)
//        }
        return best[Coord(targetX, targetY)]
                ?: throw Exception("Never reached target")
    }

    private fun canUseTool(t: Tool, pos: Coord): Boolean {
        return t != when (regionType(pos)) {
            Region.Rocky -> Tool.None
            Region.Wet -> Tool.Torch
            Region.Narrow -> Tool.ClimbingGear
        }
    }

    private fun regionType(pos: Coord): Region {
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