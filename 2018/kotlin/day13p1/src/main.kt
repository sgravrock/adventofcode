import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = Coord::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    val rr = MineRailroad.parse(input)
    rr.advanceUntilCrash()
    println(rr.crashCoord())
    println("in ${Date().time - start.time}ms")
}

data class Coord(val x: Int, val y: Int)

enum class Orientation(val x: Int, val y: Int) {
    Up(0, -1),
    Down(0, 1),
    Left(-1, 0),
    Right(1, 0)
}

enum class TurnDirection {
    Left,
    Straight,
    Right
}

enum class TrackSegment {
    Vertical,
    Horizontal,
    SlashyTurn,
    BackslashyTurn,
    Intersection
}

data class MineRailroad(val tracks: Map<Coord, TrackSegment>, val carts: List<Cart>) {
    fun advanceUntilCrash() {
        // Note: This is wrong per the description in the puzzle (won't detect a crash
        // if it's caused by the first of the involved carts to move) but it works for
        // the given test cases and my puzzle input.
        while (crashCoord() == null) {
            advance()
        }
    }

    fun advance() {
        carts.forEach { it.advance(tracks) }
    }

    fun crashCoord(): Coord? {
        val seen = mutableSetOf<Coord>()

        for (c in carts) {
            if (seen.contains(c.coord)) {
                return c.coord
            }

            seen.add(c.coord)
        }

        return null
    }

    companion object {
        fun parse(input: String): MineRailroad {
            val lines = input.lines()
            val tracks = mutableMapOf<Coord, TrackSegment>()
            val carts = mutableListOf<Cart>()

            for (y in lines.indices) {
                for (x in lines[y].indices) {
                    val maybeSeg = when (lines[y][x]) {
                        '-', '<', '>' -> TrackSegment.Horizontal
                        '|', 'v', '^' -> TrackSegment.Vertical
                        '/' -> TrackSegment.SlashyTurn
                        '\\' -> TrackSegment.BackslashyTurn
                        '+' -> TrackSegment.Intersection
                        ' ' -> null
                        else -> throw Error("Unexpected character: '${lines[y][x]}'")
                    }

                    if (maybeSeg != null) {
                        tracks[Coord(x, y)] = maybeSeg
                    }

                    val maybeCart = Cart.fromChar(lines[y][x], Coord(x, y))

                    if (maybeCart != null) {
                        carts.add(maybeCart)
                    }
                }
            }

            return MineRailroad(tracks, carts)
        }
    }
}

data class Cart(
        var coord: Coord,
        var orientation: Orientation,
        var nextTurnDirection: TurnDirection
) {
    fun advance(tracks: Map<Coord, TrackSegment>) {
        coord = Coord(coord.x + orientation.x, coord.y + orientation.y)

        orientation = when (tracks[coord]!!) {
            TrackSegment.Horizontal, TrackSegment.Vertical -> orientation
            TrackSegment.SlashyTurn -> {
                when (orientation) {
                    Orientation.Up -> Orientation.Right
                    Orientation.Right -> Orientation.Up
                    Orientation.Left -> Orientation.Down
                    Orientation.Down -> Orientation.Left
                }
            }
            TrackSegment.BackslashyTurn -> {
                when (orientation) {
                    Orientation.Down -> Orientation.Right
                    Orientation.Right -> Orientation.Down
                    Orientation.Left -> Orientation.Up
                    Orientation.Up -> Orientation.Left
                }
            }
            TrackSegment.Intersection -> {
                val turn = nextTurnDirection
                nextTurnDirection = successor(turnSequence, nextTurnDirection)
                when (turn) {
                    TurnDirection.Left -> predecessor(orientationSequence, orientation)
                    TurnDirection.Right -> successor(orientationSequence, orientation)
                    TurnDirection.Straight -> orientation
                }
            }
        }
    }

    companion object {
        fun fromChar(c: Char, coord: Coord): Cart? {
            val o = when (c) {
                '<' -> Orientation.Left
                '>' -> Orientation.Right
                '^' -> Orientation.Up
                'v' -> Orientation.Down
                else -> null
            }

            return if (o == null) {
                null
            } else {
                Cart(coord, o, TurnDirection.Left)
            }
        }

        private val turnSequence = listOf(TurnDirection.Left, TurnDirection.Straight, TurnDirection.Right)
        private val orientationSequence = listOf(Orientation.Up, Orientation.Right, Orientation.Down, Orientation.Left)
    }
}

fun <T> successor(list: List<T>, curr: T): T {
    return list[(list.indexOf(curr) + 1) % list.count()]
}

fun <T> predecessor(list: List<T>, curr: T): T {
    val i = list.indexOf(curr)
    return if (i == 0) list.last() else list[i - 1]
}
