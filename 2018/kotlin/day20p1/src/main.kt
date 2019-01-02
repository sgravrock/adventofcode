import java.util.*
import kotlin.math.min

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = RoomEx::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    println(shortestPathToFarthestRoom(input))
    println("in ${Date().time - start.time}ms")
}

data class Coord(val x: Int, val y: Int)
enum class Dir { N, E, W, S }

fun shortestPathToFarthestRoom(input: String): Int {
    var farthestDistance = Int.MIN_VALUE
    var shortestDistance = Int.MAX_VALUE
    var farthestRoom: Coord? = null
    var i = 0
    RoomEx.parse(input).paths(null, {
        i++
        if (i % 1000 == 0) {
            println(i)
        }
        // TODO: is conversion to list really useful here?
        val path = it.toList()
        val dest = destination(path)

        if (path.size > farthestDistance) {
            farthestDistance = path.size
            shortestDistance = path.size
            farthestRoom = dest
        } else if (dest == farthestRoom) {
            shortestDistance = min(shortestDistance, path.size)
        }
    })
    println("Considered ${i} paths")
    assert(farthestRoom != null)
    return shortestDistance
}

fun destination(path: List<Dir>): Coord {
    return path.fold(Coord(0, 0), { acc, dir ->
        when (dir) {
            Dir.N -> Coord(acc.x, acc.y - 1)
            Dir.S -> Coord(acc.x, acc.y + 1)
            Dir.W -> Coord(acc.x - 1, acc.y)
            Dir.E -> Coord(acc.x + 1, acc.y)
        }
    })
}

sealed class RoomEx {
    abstract fun paths(
        prefix: PersistentList<Dir>?, emit: (PersistentList<Dir>)->Unit)

    data class Expression(val els: List<RoomEx>) : RoomEx() {
        override fun paths(prefix: PersistentList<Dir>?, emit: (PersistentList<Dir>)->Unit) {
            pathsForSublist(prefix, 0, emit)
        }

        private fun pathsForSublist(prefix: PersistentList<Dir>?, start: Int, emit: (PersistentList<Dir>) -> Unit) {
            if (start in els.indices) {
                els[start].paths(prefix, { pathsForSublist(it, start + 1, emit) })
            } else if (prefix != null) {
                emit(prefix)
            }
        }
    }

    data class AtomList(val els: List<Dir>): RoomEx() {
        override fun paths(prefix: PersistentList<Dir>?, emit: (PersistentList<Dir>) -> Unit) {
            val path = els.fold(prefix, {acc: PersistentList<Dir>?, el: Dir ->
                PersistentList(el, acc)
            })!!
            emit(path)
        }
    }

    data class Options(val opts: List<RoomEx>) : RoomEx() {
        override fun paths(prefix: PersistentList<Dir>?, emit: (PersistentList<Dir>)->Unit) {
            for (option in opts) {
                option.paths(prefix, emit)
            }
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

data class PersistentList<T>(val el: T, val prev: PersistentList<T>?) {
    fun toList(): MutableList<T> {
        val result = when (prev) {
            null -> mutableListOf()
            else -> prev.toList()
        }
        result.add(el)
        return result
    }
}