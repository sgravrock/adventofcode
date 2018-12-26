import java.util.*
import kotlin.math.max
import kotlin.math.min

fun main(args: Array<String>) {
    val classLoader = GroundMap::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    val map = GroundMap.parse(input)
    println(map.spacesReachable())
}

data class GroundMap(val spaces: MutableMap<Coord, Space>) {
    fun spacesReachable(): Int {
        while (advance()) {/*println("${this}\n")*/}

//        println(this)
        return spaces.count { it.value == Space.WetSand }
    }

    fun getSpace(c: Coord): Space {
        return spaces[c] ?: Space.DrySand
    }

    fun advance(): Boolean {
        // TODO: can probably save state across calls to advance()
        val pending = Stack<Move>()
        val visited = mutableSetOf<Coord>()
        val ranges = range() // TODO this is probably expensive to do in a tight loop

        fun pushIfVisitable(move: Move) {
            val visitable = move.dest.x in ranges.x &&
                    move.dest.y in ranges.y &&
                    !visited.contains(move.dest)
            
            if (visitable) {
                pending.push(move)
            }
        }

        pending.push(Move.Vert(Coord(500, 1)))

        while (!pending.isEmpty()) {
            val move = pending.pop()
            visited.add(move.dest)

            when (getSpace(move.dest)) {
                Space.DrySand -> {
                    if (canWet(move)) {
                        spaces[move.dest] = Space.WetSand
                        return true
                    } else {
                        println(this)
                        println("Pruning move $move")

                    }
                }
                Space.WetSand -> {
                    val below = Coord(move.dest.x, move.dest.y + 1)
                    pushIfVisitable(Move.Horiz(
                        Coord(move.dest.x - 1, move.dest.y),
                        below
                    ))
                    pushIfVisitable(Move.Horiz(
                        Coord(move.dest.x + 1, move.dest.y),
                        below
                    ))
                    pushIfVisitable(Move.Vert(below))
                }
                Space.Clay -> {}
                Space.Spring -> throw Error("Can't happen: visited the spring")
            }
        }

        return false
    }

    override fun toString(): String {
        val r = range()
        return r.y.map { y ->
            r.x.map { x ->
                when (getSpace(Coord(x, y))) {
                    Space.Clay -> "#"
                    Space.DrySand -> "."
                    Space.WetSand -> "~"
                    Space.Spring -> "+"
                }
            }.joinToString("")
        }.joinToString("\n")
    }

    private fun canWet(move: Move): Boolean {
        return when (move) {
            is Move.Vert -> true
            is Move.Horiz -> {
                val belowPredecessor = getSpace(move.belowPredecessor)
                return belowPredecessor == Space.Clay || hasClayBelow(move.dest)
            }
        }
    }

    private fun hasClayBelow(c: Coord): Boolean {
        val ymin = c.y + 1
        val ymax = range().y.last
        return (ymin..ymax)
            .any { y -> getSpace(Coord(c.x, y)) == Space.Clay }
    }


    private fun range(): Ranges {
        var xmin = Int.MAX_VALUE
        var xmax = Int.MIN_VALUE
        var ymin = Int.MAX_VALUE
        var ymax = Int.MIN_VALUE

        for (c in spaces.keys) {
            xmin = min(xmin, c.x)
            xmax = max(xmax, c.x)
            ymin = min(ymin, c.y)
            ymax = max(ymax, c.y)
        }

        return Ranges(x = xmin..xmax, y = ymin..ymax)
    }

    companion object {
        fun parse(input: String): GroundMap {
            val spaces = mutableMapOf(Coord(500, 0) to Space.Spring)
            val re = Regex("([xy])=([0-9]+), ([xy])=([0-9]+(\\.\\.[0-9]+)?)")

            for (line in input.lines()) {
                val m = re.matchEntire(line)
                    ?: throw Error("Can't parse line: $line")
                var xrange: IntRange
                var yrange: IntRange

                if (m.groupValues[1] == "x") {
                    xrange = parseRange(m.groupValues[2])
                    yrange = parseRange(m.groupValues[4])
                } else {
                    assert(m.groupValues[1] == "y")
                    xrange = parseRange(m.groupValues[4])
                    yrange = parseRange(m.groupValues[2])
                }

                for (x in xrange) {
                    for (y in yrange) {
                        spaces[Coord(x, y)] = Space.Clay
                    }
                }
            }

            return GroundMap(spaces)
        }

        private fun parseRange(input: String): IntRange {
            val parts = input.split("..")
            assert(parts.size in 1..2)
            val a = parts[0].toInt()

            return if (parts.size == 1) {
                a..a
            } else {
                val b = parts[1].toInt()
                a..b
            }
        }
    }
}

data class Coord(val x: Int, val y: Int)

sealed class Move() {
    abstract val dest: Coord

    data class Vert(override val dest: Coord): Move()
    data class Horiz(override val dest: Coord, val belowPredecessor: Coord): Move()
}

data class Ranges(val x: IntRange, val y: IntRange)

enum class Space {
    Spring,
    Clay,
    DrySand,
    WetSand
}