import java.util.*
import kotlin.math.min

fun main(args: Array<String>) {
    val start = Date()
    println(answerForPuzzleInput())
    println("in ${Date().time - start.time}ms")
}

fun answerForPuzzleInput(): Int {
    val classLoader = RoomExParser::class.java.classLoader
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

enum class Dir {
    N, E, W, S;

    companion object {
        fun fromChar(c: Char): Dir {
            return when(c) {
                'N' -> Dir.N
                'E' -> Dir.E
                'S' -> Dir.S
                'W' -> Dir.W
                else -> throw Exception("Invalid direction char: $c")
            }

        }
    }
}

fun shortestPathToFarthestRoom(input: String): Int {
    val world = RoomExParser.parse(input)
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

class RoomExParser(private val input: String) {
    companion object {
        fun parse(input: String): World {
            val parser = RoomExParser(input)
            parser.require('^')
            parser.parse(setOf(Coord(0, 0)))
            parser.require('$')
            return World(parser.tiles)
        }
    }

    private var i = 0
    private val tiles = mutableMapOf(Coord(0, 0) to Tile.Room)

    private fun parse(starts: Set<Coord>): Set<Coord> {
        var positions = starts
        val previousOptions = mutableSetOf<Coord>()

        loop@ while (input[i] != '$') {
            val c = input[i++]

            when (c) {
                'N', 'S', 'E', 'W' -> {
                    val dir = Dir.fromChar(c)
                    val doorType = when (dir) {
                        Dir.N, Dir.S -> Tile.Hdoor
                        Dir.E, Dir.W -> Tile.Vdoor
                    }
                    positions = positions.map { pos ->
                        val doorCoord = pos.neighbor(dir)
                        tiles[doorCoord] = doorType
                        val roomCoord = doorCoord.neighbor(dir)
                        tiles[roomCoord] = Tile.Room
                        roomCoord
                    }.toSet()
                }
                '(' -> {
                    positions = parse(positions)
                }
                '|' -> {
                    previousOptions.addAll(positions)
                    positions = starts
                }
                ')' -> {
                    previousOptions.addAll(positions)
                    positions = previousOptions
                    break@loop
                }
                else -> throw Error("Unexpected '$c'")
            }
        }

        return positions
    }

    private fun require(expected: Char) {
        val actual = input[i++]

        if (actual != expected) {
            throw Error("Expected $expected but got $actual")
        }
    }
}