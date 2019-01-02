import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = RoomEx::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    println("in ${Date().time - start.time}ms")
}

enum class Dir { N, E, W, S }

sealed class RoomEx {
    abstract fun paths(): Set<List<Dir>>

    data class Expression(val els: List<Term>) : RoomEx() {
        override fun paths(): Set<List<Dir>> {
            return pathsForSubList(els, 0)
        }

        private fun pathsForSubList(l: List<Term>, start: Int): Set<List<Dir>> {
            if (!(start in l.indices)) {
                return setOf(emptyList())
            }

            val prefixes = l[start].paths()
            assert(!prefixes.isEmpty())
            val suffixes = pathsForSubList(l, start + 1)
            assert(!suffixes.isEmpty())
            return crossProduct(prefixes, suffixes)
        }
    }

    sealed class Term : RoomEx() {
        data class Atom(val d: Dir) : Term() {
            override fun paths(): Set<List<Dir>> {
                return setOf(listOf(d))
            }
        }

        data class Options(val opts: List<Expression>) : Term() {
            override fun paths(): Set<List<Dir>> {
                val result = mutableSetOf<List<Dir>>()
                opts.forEach { result.addAll(it.paths()) }
                return result
            }
        }
    }

    companion object {
        // RoomEx: BEGIN expression END
        fun parse(input: String): RoomEx.Expression {
            val lexer = Lexer(input)
            lexer.require(Token.Begin)
            val result = parseExpression(lexer)
            lexer.require(Token.End)
            return result
        }

        // Expression: term expression | nothing
        private fun parseExpression(lexer: Lexer): Expression {
            val terms = mutableListOf<Term>()
            var t = parseTerm(lexer)

            while (t != null) {
                terms.add(t)
                t = parseTerm(lexer)
            }

            return Expression(terms)
        }

        // Term: atom | lparen options
        private fun parseTerm(lexer: Lexer): Term? {
            val token = lexer.get()
            return when (token) {
                is Token.Atom -> Term.Atom(token.dir)
                Token.Lparen -> parseOptions(lexer)
                else -> {
                    lexer.unget()
                    null
                }
            }
        }

        // options: expression pipe options | rparen
        private fun parseOptions(lexer: Lexer): Term.Options {
            val expressions = mutableListOf<Expression>()

            while (true) {
                expressions.add(parseExpression(lexer))
                val t = lexer.get()

                if (t == Token.Rparen) {
                    break
                } else if (t != Token.Pipe) {
                    throw Error("Expected Rparen or Pipe but got $t")
                }
            }

            return Term.Options(expressions)
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


fun <T> crossProduct(a: Set<List<T>>, b: Set<List<T>>): Set<List<T>> {
    val result = mutableSetOf<List<T>>()

    for (aEl in a) {
        for (bEl in b) {
            result.add(concat(aEl, bEl))
        }
    }

    return result
}

fun <T> concat(a: List<T>, b: List<T>): List<T> {
    val r = a.toMutableList()
    r.addAll(b)
    return r

}
