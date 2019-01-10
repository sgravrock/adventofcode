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
    fun add(other: Coord): Coord {
        return Coord(x + other.x, y + other.y)
    }

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
data class Path(val dest: Coord, val len: Int) {
    fun append(other: Path): Path {
        return Path(dest.add(other.dest), len + other.len)
    }

    companion object {
        fun crossProduct(a: Set<Path>, b: Set<Path>): Set<Path> {
            return a.flatMap { aE -> b.map { bE -> aE.append(bE)} }
                .toSet()
        }
    }
}

fun shortestPathToFarthestRoom(input: String): Int {
    val world = World.build(RoomEx.parse(input))
    val distancesToRooms = mutableMapOf<Coord, Int>()
    var nPaths = 0

    world.paths { dest, len ->
        nPaths++
        distancesToRooms[dest] = min(
            len,
            distancesToRooms.getOrDefault(dest, Int.MAX_VALUE)
        )
    }

    println("Found ${nPaths} paths")
    println("Found ${distancesToRooms.size} rooms")
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


sealed class RoomEx {
    abstract fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord>

    data class Expression(val els: List<RoomEx>) : RoomEx() {
        override fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord> {
            return els.fold(setOf(start), { prevDests, el ->
                prevDests
                    .flatMap { prev -> el.walk(prev, visit) }
                    .toSet()
            })
        }
    }

    data class AtomList(val els: List<Dir>): RoomEx() {
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

    data class Options(val opts: List<RoomEx>) : RoomEx() {
        override fun walk(start: Coord, visit: (Coord, Tile) -> Unit): Set<Coord> {
            return opts.flatMap { it.walk(start, visit) }.toSet()
        }
    }

    companion object {
        // RoomEx: BEGIN expression END
        fun parse(input: String): RoomEx {
            val lexer = Lexer(input)
            lexer.require(Token.Begin)
            val result = parseExpression(lexer)
            lexer.require(Token.End)
            return result
        }

        // Expression: term expression | nothing
        private fun parseExpression(lexer: Lexer): RoomEx {
            val terms = mutableListOf<RoomEx>()
            var t = parseTerm(lexer)

            while (t != null) {
                terms.add(t)
                t = parseTerm(lexer)
            }

            if (terms.size == 1) {
                return terms[0] // elide unnecessary non-terminals
            } else {
                return Expression(terms)
            }
        }

        // Term: atomList | lparen options
        private fun parseTerm(lexer: Lexer): RoomEx? {
            var token = lexer.get()
            return when (token) {
                is Token.Atom -> {
                    val atoms = mutableListOf<Dir>()
                    while (token is Token.Atom) {
                        atoms.add(token.dir)
                        token = lexer.get()
                    }
                    lexer.unget()
                    AtomList(atoms)
                }
                Token.Lparen -> parseOptions(lexer)
                else -> {
                    lexer.unget()
                    null
                }
            }
        }

        // options: expression pipe options | rparen
        private fun parseOptions(lexer: Lexer): Options {
            val expressions = mutableListOf<RoomEx>()

            while (true) {
                expressions.add(parseExpression(lexer))
                val t = lexer.get()

                if (t == Token.Rparen) {
                    break
                } else if (t != Token.Pipe) {
                    throw Error("Expected Rparen or Pipe but got $t")
                }
            }

            return Options(expressions)
        }
    }
}

class Lexer(private val input: String) {
    private var i = 0

    fun get(): Token {
        val c = input[i++]
        return when (c) {
            '^' -> Token.Begin
            '$' -> Token.End
            '|' -> Token.Pipe
            '(' -> Token.Lparen
            ')' -> Token.Rparen
            'N' -> Token.Atom(Dir.N)
            'E' -> Token.Atom(Dir.E)
            'W' -> Token.Atom(Dir.W)
            'S' -> Token.Atom(Dir.S)
            else -> throw Exception("Unrecognized: $c")
        }
    }

    fun unget() {
        i--
    }

    fun require(expected: Token) {
        val actual = get()
        if (actual != expected) {
            throw Error("Expected $expected but got $actual")
        }
    }
}

sealed class Token {
    object Begin : Token()
    object End : Token()
    object Pipe : Token()
    object Lparen : Token()
    object Rparen : Token()
    data class Atom(val dir: Dir) : Token()
}