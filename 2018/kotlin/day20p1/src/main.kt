import java.util.*
import kotlin.math.min

fun main(args: Array<String>) {
    val start = Date()
    println(answerForPuzzleInput())
    println("in ${Date().time - start.time}ms")
}

fun answerForPuzzleInput(): Int {
    val classLoader = Coord::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    return shortestPathToFarthestRoom(input)
}

data class Coord(val x: Int, val y: Int) {
    fun neighbor(dir: Dir): Coord {
        return when (dir) {
            Dir.N -> Coord(x, y - 1)
            Dir.S -> Coord(x, y + 1)
            Dir.W -> Coord(x - 1, y)
            Dir.E -> Coord(x + 1, y)
        }
    }
}

enum class Dir { N, E, W, S }

fun shortestPathToFarthestRoom(input: String): Int {
    val world = World(parseRoomEx(input))
    val distancesToRooms = mutableMapOf<Coord, Int>()

    world.paths { dest, len ->
        distancesToRooms[dest] = min(
            len,
            distancesToRooms.getOrDefault(dest, Int.MAX_VALUE)
        )
    }

    return distancesToRooms.values.max()!!
}

enum class Tile {
    Room,
    Wall,
    Hdoor,
    Vdoor
}

class World(private val grid: Map<Coord, Tile>) {
    fun paths(emit: (Coord, Int) -> Unit) {
        dfs(mutableListOf(Coord(0, 0)), emit)
    }

    private fun dfs(rooms: MutableList<Coord>, emit: (Coord, Int) -> Unit) {
        for (d in Dir.values()) {
            val doorCoord = rooms.last().neighbor(d)

            when (grid.getOrDefault(doorCoord, Tile.Wall)) {
                Tile.Hdoor, Tile.Vdoor -> {
                    val nextRoomCoord = doorCoord.neighbor(d)

                    if (!rooms.contains(nextRoomCoord)) {
                        rooms.add(nextRoomCoord)
                        emit(rooms.last(), rooms.size - 1)
                        dfs(rooms, emit)
                        rooms.removeAt(rooms.size - 1)
                    }
                }
                Tile.Wall -> {}
                Tile.Room -> throw Exception("Can't happen: two adjacent rooms")
            }
        }
    }

    override fun toString(): String {
        val minX = grid.keys.asSequence().map { it.x }.min()!! - 1
        val maxX = grid.keys.asSequence().map { it.x }.max()!! + 1
        val minY = grid.keys.asSequence().map { it.y }.min()!! - 1
        val maxY = grid.keys.asSequence().map { it.y }.max()!! + 1

        return (minY..maxY).map { y ->
            (minX..maxX).map { x ->
                when (grid.getOrDefault(Coord(x, y), Tile.Wall)) {
                    Tile.Room -> if (y == 0 && x == 0) 'X' else '.'
                    Tile.Wall -> '#'
                    Tile.Vdoor -> '|'
                    Tile.Hdoor -> '-'
                }
            }.joinToString("")
        }.joinToString("\n")
    }
}

fun parseRoomEx(input: String): Map<Coord, Tile> {
    val tiles = mutableMapOf(Coord(0, 0) to Tile.Room)
    val contextStack = mutableListOf<ParsingContext>(
        ParsingContext.NonOptionGroup(listOf(Coord(0, 0)))
    )

    require(input, 0, '^')

    for (i in 1..(input.length - 2)) {
        val c = input[i];
        val top = contextStack.last()

        when (c) {
            'N', 'E', 'W', 'S' -> {
                val dir = when (c) {
                    'N' -> Dir.N
                    'E' -> Dir.E
                    'W' -> Dir.W
                    'S' -> Dir.S
                    else -> throw Error("Can't happen")
                }
                top.advance(dir)
                val door = when (c) {
                    'N', 'S' -> Tile.Hdoor
                    'E', 'W' -> Tile.Vdoor
                    else -> throw Error("Can't happen")
                }
                top.markPos(door, tiles)
                top.advance(dir)
                top.markPos(Tile.Room, tiles)
            }
            '(' -> {
                val initialPoses = when (input[i - 1]) {
                    '(' -> {
                        assert(contextStack.size > 1)
                        contextStack[contextStack.size - 2].positions()
                    }
                    else -> when (top) {
                        is ParsingContext.NonOptionGroup -> top.positions()
                        is ParsingContext.OptionGroup -> top.options.last()
                    }
                }
                contextStack.add(ParsingContext.OptionGroup(initialPoses))
            }
            '|' -> {
                when (top) {
                    is ParsingContext.OptionGroup -> {
                        assert(contextStack.size > 1)
                        val nextDown = contextStack[contextStack.size - 2]
                        top.options.add(nextDown.positions())
                    }
                    is ParsingContext.NonOptionGroup -> {
                        throw Error("Unexpected | when not in an option group")
                    }
                }
            }
            ')' -> {
                if (top is ParsingContext.NonOptionGroup) {
                    throw Error("Unexpected ) when not in an option group")
                }

                val positions = top.positions()
                contextStack.removeAt(contextStack.size - 1)
                contextStack.last().receiveNestedPositions(positions)
            }
            else -> throw Error("Unexpected: $c")
        }
    }


    if (contextStack.size != 1) {
        throw Error("Expected )")
    }

    require(input, input.length - 1, '$')
    return tiles
}

fun require(input: String, i: Int, expected: Char) {
    if (input[i] != expected) {
        throw Exception("Expected $expected but got ${input[i]}.")
    }
}

sealed class ParsingContext {
    abstract fun positions(): List<Coord>
    abstract fun markPos(tile: Tile, tiles: MutableMap<Coord, Tile>)
    abstract fun advance(dir: Dir)
    abstract fun receiveNestedPositions(nested: List<Coord>)

    data class NonOptionGroup(var positions: List<Coord>): ParsingContext() {
        override fun positions(): List<Coord> {
            return positions
        }

        override fun markPos(tile: Tile, tiles: MutableMap<Coord, Tile>) {
            for (pos in positions) {
                tiles[pos] = tile
            }
        }

        override fun advance(dir: Dir) {
            positions = positions.map { it.neighbor(dir) }
        }

        override fun receiveNestedPositions(nested: List<Coord>) {
            positions = nested
        }
    }

    data class OptionGroup(val predecessorPositions: List<Coord>): ParsingContext() {
        val options: MutableList<List<Coord>>

        init {
            options = mutableListOf(predecessorPositions)
        }

        override fun positions(): List<Coord> {
            return options.flatMap { it }
        }

        override fun markPos(tile: Tile, tiles: MutableMap<Coord, Tile>) {
            for (pos in options.last()) {
                tiles[pos] = tile
            }
        }

        override fun advance(dir: Dir) {
            options[options.size - 1] = options[options.size - 1].map {
                it.neighbor(dir)
            }
        }

        override fun receiveNestedPositions(nested: List<Coord>) {
            options[options.size - 1] = nested
        }
    }
}