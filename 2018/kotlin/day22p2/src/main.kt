import java.lang.Exception
import kotlin.math.abs
import kotlin.math.roundToInt
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
        val start = PathNode(Coord(0, 0), Tool.Torch)
        val target = PathNode(Coord(targetX, targetY), Tool.Torch)
        val closedSet = mutableSetOf<PathNode>()
        val openSet = mutableSetOf(start)
        val gScoreMap = mutableMapOf(start to 0)
        val fScoreMap = mutableMapOf(
                start to heuristicCostEstimate(start, target)
        )

        fun gScore(n: PathNode): Int = gScoreMap.getOrDefault(n, Int.MAX_VALUE)
        fun fScore(n: PathNode): Int = fScoreMap.getOrDefault(n, Int.MAX_VALUE)

        while (openSet.isNotEmpty()) {
            val curr = openSet.minBy { fScore(it) }!!

            if (curr == target) {
                return gScoreMap[target]!!
            }

            openSet.remove(curr)
            closedSet.add(curr)

            for (n in curr.neighbors(this)) {
                if (closedSet.contains(n)) {
                    continue
                }

                val tentativeGScore = gScore(curr) + timeBetweenNeigbors(curr, n)

                if (!openSet.contains(n)) {
                    openSet.add(n)
                } else if (tentativeGScore >= gScore(n)) {
                    continue
                }

                gScoreMap[n] = tentativeGScore
                fScoreMap[n] = tentativeGScore + heuristicCostEstimate(n, target)
            }
        }

        throw Error("Did not find target")
    }

    private fun timeBetweenNeigbors(src: PathNode, dest: PathNode): Int {
        if (src.tool != dest.tool) {
            return 7
        } else {
            val dx = abs(src.pos.x - dest.pos.x)
            val dy = abs(src.pos.y - dest.pos.y)
            assert((dx == 0 && dy == 1) || (dx == 1 && dy == 0))
            return 1
        }
    }

    private fun heuristicCostEstimate(start: PathNode, goal: PathNode): Int {
        val dist = abs(start.pos.x - goal.pos.x) + abs(start.pos.x - goal.pos.x)
//        val toolChanges = (dist * 0.36).roundToInt() + if (start.tool != goal.tool) 1 else 0
//        return dist + toolChanges * 7
        return dist + if (start.tool == goal.tool) 0 else 1
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

data class PathNode(val pos: Coord, val tool: Tool) {
    fun neighbors(cave: ICave): List<PathNode> {
        val region = cave.regionType(pos)
        val result = pos.neighbors()
                .filter { n ->
                    n.x >= 0 && n.y >= 0 && n.y < cave.depth &&
                            canUseTool(tool, cave.regionType(n))
                }
                .map { n -> PathNode(n, tool) }
                .toMutableList()
        result.addAll(
                Tool.values()
                        .filter { t -> t != tool && canUseTool(t, region) }
                        .map { t -> PathNode(pos, t) }
        )
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