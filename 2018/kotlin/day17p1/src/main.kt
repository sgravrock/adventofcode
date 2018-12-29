import kotlin.math.max
import kotlin.math.min

fun main(args: Array<String>) {
    val classLoader = GroundMap::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    val map = GroundMap.parse(input)
    println(map.spacesReachable())
    println(map)
    // 31928 is too low
}

data class GroundMap(val spaces: MutableMap<Coord, Space>) {
    fun spacesReachable(): Int {
        while (advance()) {/*println("${this}\n")*/}

//        println(this)
        return wettedSpaces()
    }

    fun wettedSpaces(): Int {
        return spaces.count {
            it.value == Space.StandingWater || it.value == Space.FlowingWater
        }

    }

    fun getSpace(c: Coord): Space {
        return spaces[c] ?: Space.DrySand
    }

    override fun toString(): String {
        return range.y.map { y ->
            range.x.map { x ->
                when (getSpace(Coord(x, y))) {
                    Space.Clay -> "#"
                    Space.DrySand -> "."
                    Space.StandingWater -> "~"
                    Space.FlowingWater -> "|"
                    Space.Spring -> "+"
                }
            }.joinToString("")
        }.joinToString("\n")
    }

    fun advance(): Boolean {
        // TODO: some state can and should be shared across calls
        val numBefore = wettedSpaces()
        val emitters = mutableListOf(Coord(500, 0))

        while (!emitters.isEmpty()) {
            val e = emitters.removeAt(emitters.lastIndex)
            dropFrom(e, emitters)
        }

        return wettedSpaces() != numBefore
    }

    private fun dropFrom(origin: Coord, emitters: MutableList<Coord>) {
        for (y in (origin.y + 1)..range.y.last) {
            val c = Coord(origin.x, y)
            val below = getSpace(Coord(origin.x, y + 1))

            when (below) {
                Space.StandingWater, Space.Clay -> {
                    flowAcross(c, emitters)
                    return
                }
                Space.DrySand -> spaces[c] = Space.FlowingWater
                Space.FlowingWater -> {}
                Space.Spring -> {
                    throw Error("Can't happen: flowed down to spring")
                }
            }
        }
    }

    fun flowAcross(origin: Coord, emitters: MutableList<Coord>) {
        val left = findSpillEnd(origin, true)
        val right = findSpillEnd(origin, false)
        val fill = if (left is Spill.Bounded && right is Spill.Bounded) {
            Space.StandingWater
        } else {
            Space.FlowingWater
        }
        
        for (i in left.x..right.x) {
            spaces[Coord(i, origin.y)] = fill
        }

        if (left is Spill.Drop) emitters.add(Coord(left.x, origin.y))
        if (right is Spill.Drop) emitters.add(Coord(right.x, origin.y))
    }

    private fun findSpillEnd(origin: Coord, toLeft: Boolean): Spill {
        val r = if (toLeft) {
            origin.x downTo range.x.first
        } else {
            origin.x..range.x.last
        }

        for (x in r) {
            val below = getSpace(Coord(x, origin.y + 1))
            if (below == Space.DrySand || below == Space.FlowingWater) {
                return Spill.Drop(x)
            }

            val next = Coord(x + if (toLeft) -1 else 1, origin.y)
            if (getSpace(next) == Space.Clay) {
                return Spill.Bounded(x)
            }
        }

//        println(this) // TODO remove
        return Spill.Unbounded(r.last)
//        throw Error("Reached the edge while spilling horizontally from $origin")
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
        val ymax = range.y.last
        return (ymin..ymax)
            .any { y -> getSpace(Coord(c.x, y)) == Space.Clay }
    }


    val range: Ranges by lazy {
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

        Ranges(x = xmin..xmax, y = ymin..ymax)
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
    StandingWater,
    FlowingWater
}

sealed class Spill {
    abstract val x: Int

    data class Bounded(override val x: Int): Spill()
    data class Unbounded(override val x: Int): Spill()
    data class Drop(override val x: Int): Spill()
}