import java.util.*
import kotlin.math.min

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = RoomEx::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    println(shortestPathToFarthestRoom(input))
    println("in ${Date().time - start.time}ms")
    // 3964 is too low
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
    val distancesToRooms = mutableMapOf<Coord, Int>()
    val paths = RoomEx.parse(input).paths()
    println("Found ${paths.size} paths")

    for (path in paths) {
        distancesToRooms[path.dest] = min(
            path.len,
            distancesToRooms.getOrDefault(path.dest, Int.MAX_VALUE)
        )
    }

    println("Found ${distancesToRooms.size} rooms")
    return distancesToRooms.values.max()!!
}


sealed class RoomEx {
    abstract fun paths(): Set<Path>
    data class Expression(val els: List<RoomEx>) : RoomEx() {
        override fun paths(): Set<Path> {
            val start = setOf(Path(Coord(0, 0), 0))
            return els.fold(start, { prev, el ->
                Path.crossProduct(prev, el.paths())
            })
        }
    }

    data class AtomList(val els: List<Dir>): RoomEx() {
        override fun paths(): Set<Path> {
            val dest = els.fold(Coord(0, 0), { prev, el -> prev.neighbor(el)})
            return setOf(Path(dest, els.size))
        }
    }

    data class Options(val opts: List<RoomEx>) : RoomEx() {
        override fun paths(): Set<Path> {
            val result = mutableSetOf<Path>()
            opts.forEach { result.addAll(it.paths()) }
            return result
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