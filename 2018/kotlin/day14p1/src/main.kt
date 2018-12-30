import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    println(next10ScoresAfter(920831))
    println("in ${Date().time - start.time}ms")
}

fun next10ScoresAfter(numRecipes: Int): String {
    val initialState = State(scores = listOf(3, 7), elf1 = 0, elf2 = 1)
    return initialState.take(10 + numRecipes).drop(numRecipes).joinToString("")
}

class State(val scores: List<Int>, val elf1: Int, val elf2: Int) : Iterable<Int> {
    override fun iterator(): Iterator<Int> {
        return ScoresIterator(scores.toMutableList(), this.elf1, this.elf2)
    }
}

class ScoresIterator(
        private var scores: MutableList<Int>,
        private var elf1: Int,
        private var elf2: Int
) : Iterator<Int> {
    private var lastIndex = -1
    override fun hasNext() = true

    override fun next(): Int {
        val i = ++lastIndex

        while (i >= scores.count()) {
            generateNextScore()
        }

        return scores[i]
    }

    private fun generateNextScore() {
        scores.addAll(digits(scores[elf1] + scores[elf2]))
        elf1 = (elf1 + 1 + scores[elf1]) % scores.count()
        elf2 = (elf2 + 1 + scores[elf2]) % scores.count()
    }
}

fun digits(n: Int): List<Int> {
    if (n == 0) {
        return listOf(0)
    }

    val result = mutableListOf<Int>()
    var num = n

    while (num >= 1) {
        result.add(num % 10)
        num /= 10
    }

    result.reverse()
    return result
}