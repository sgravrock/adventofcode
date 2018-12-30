import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    println(firstIndexOf(listOf(9, 2, 0, 8, 3, 1)))
    println("in ${Date().time - start.time}ms")
}

fun firstIndexOf(digits: List<Int>): Int {
    val state = State(scores = mutableListOf(3, 7), elf1 = 0, elf2 = 1)
    val searcher = GrowOnlyListSearcher(state.scores, digits)

    while (true) {
        val i = searcher.search()

        if (i != null) {
            return i
        }

        state.generateNextScore()
    }
}

class GrowOnlyListSearcher<T>(val haystack: List<T>, val needle: List<T>) {
    private var lastStart = 0;

    fun search(): Int? {
        for (i in lastStart until haystack.size - needle.size - 1) {
            lastStart = i

            if (sublistMatches(i)) {
                return i
            }
        }

        return null
    }

    fun sublistMatches(offset: Int): Boolean {
        for (i in 0 until needle.size) {
            if (haystack[i + offset] != needle[i]) {
                return false
            }
        }

        return true
    }
}

class State(val scores: MutableList<Int>, var elf1: Int, var elf2: Int) {
    fun generateNextScore() {
        scores.addAll(digits(scores[elf1] + scores[elf2]))
        elf1 = (elf1 + 1 + scores[elf1]) % scores.size
        elf2 = (elf2 + 1 + scores[elf2]) % scores.size
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