import java.util.*
import kotlin.math.min

fun main(args: Array<String>) {
    val start = Date()
    println(answerForPuzzleInput())
    println("in ${Date().time - start.time}ms")
}

fun answerForPuzzleInput(): Int {
    val classLoader = RoomEx::class.java.classLoader
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
    val world = World.build(RoomExParser.parse(input))
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

    companion object {
        fun build(input: RoomEx): World {
            val origin = Coord(0, 0)
            val tiles = mutableMapOf(origin to Tile.Room)
            input.walk(origin, { c, t -> tiles[c] = t })
            return World(tiles)
        }
    }
}

interface RoomEx {
    fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord>
}

data class Expression(val els: List<RoomEx>) : RoomEx {
    override fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord> {
        return els.fold(setOf(start), { prevDests, el ->
            prevDests
                .flatMap { prev -> el.walk(prev, visit) }
                .toSet()
        })
    }
}

data class AtomList(val els: List<Dir>): RoomEx {
    override fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord> {
        val dest = els.fold(start, { prev, dir ->
            val door = prev.neighbor(dir)
            visit(door, when(dir) {
                Dir.N, Dir.S -> Tile.Hdoor
                Dir.W, Dir.E -> Tile.Vdoor
            })
            val room = door.neighbor(dir)
            visit(room, Tile.Room)
            room
        })

        return setOf(dest)
    }
}

data class Options(val opts: List<RoomEx>) : RoomEx {
    override fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord> {
        return opts.flatMap { it.walk(start, visit) }.toSet()
    }
}

class RoomExParser(private val input: String) {
    companion object {
        fun parse(input: String): RoomEx {
            return RoomExParser(input).parse()
        }
    }
    var i: Int = 0

    // RoomEx: BEGIN expression END
    fun parse(): RoomEx {
        require('^')
        val result = parseExpression()
        require('$')
        return result
    }

    // Expression: term expression | nothing
    private fun parseExpression(): RoomEx {
        val terms = mutableListOf<RoomEx>()
        var t = parseTerm()

        while (t != null) {
            terms.add(t)
            t = parseTerm()
        }

        if (terms.size == 1) {
            return terms[0] // elide unnecessary non-terminals
        } else {
            return Expression(terms)
        }
    }

    // Term: atomList | lparen options
    private fun parseTerm(): RoomEx? {
        var token = input[i++]
        return when (token) {
            'N', 'E', 'W', 'S' -> {
                val atoms = mutableListOf<Dir>()
                while (token in listOf('N', 'E', 'W', 'S')) {
                    atoms.add(dirFromChar(token))
                    token = input[i++]
                }
                i--
                AtomList(atoms)
            }
            '(' -> parseOptions()
            else -> {
                i--
                null
            }
        }
    }

    // options: expression pipe options | rparen
    private fun parseOptions(): Options {
        val expressions = mutableListOf<RoomEx>()

        while (true) {
            expressions.add(parseExpression())
            val t = input[i++]

            if (t == ')') {
                break
            } else if (t != '|') {
                throw Error("Expected Rparen or Pipe but got $t")
            }
        }

        return Options(expressions)
    }

    private fun require(expected: Char) {
        val actual = input[i++]

        if (actual != expected) {
            throw Error("Expected $expected but got $actual")
        }
    }

    private fun dirFromChar(c: Char): Dir {
        return when(c) {
            'N' -> Dir.N
            'E' -> Dir.E
            'S' -> Dir.S
            'W' -> Dir.W
            else -> throw Exception("Invalid direction char: $c")
        }
    }
}