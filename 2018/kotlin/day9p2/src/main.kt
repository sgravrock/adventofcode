import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    println(findHighScore(476, 7165700))
    println("in ${Date().time - start.time}ms")
}

// A fully constructed Node should always have non-null prev and next,
// but the fields need to be nullable so we can construct self-referential nodes.
data class Node<T>(val value: T, var prev: Node<T>?, var next: Node<T>?)

class Ring<T> {
    private var current: Node<T>

    constructor(initialValue: T) {
        current = Node(initialValue, null, null)
        current.prev = current
        current.next = current
    }

    fun currentValue(): T {
        return current.value
    }

    fun toList(): List<T> {
        var p = current
        val result = mutableListOf<T>()

        do {
            result.add(p.value)
            p = p.next!!
        } while (p != current)

        return result
    }

    fun debug(startValue: T): String {
        var p = current

        while (p.value != startValue) {
            p = p.next!!
        }

        val start = p
        val tokens = mutableListOf<String>()

        do {
            tokens.add(if (p == current) "(${p.value})" else  "${p.value}")
            p = p.prev!!
        } while (p != start)

        return tokens.joinToString(" ")
    }

    fun moveCW() {
        current = current.prev!!
    }

    fun moveCCW() {
        current = current.next!!
    }

    fun append(value: T) {
        val newNode = Node(value, current.prev, current)
        newNode.prev!!.next = newNode
        newNode.next!!.prev = newNode
        current = newNode
    }

    fun removeCurrent(): T {
        val toRemove = current
        current = toRemove.prev!!
        current.next = toRemove.next
        current.next!!.prev = current
        return toRemove.value
    }
}

fun findHighScore(numPlayers: Int, highestMarble: Int, debug: Boolean = false): Long {
    val playerScores = initScores(numPlayers)
    val ring = Ring(0)
    var currentPlayer = 0
    if (debug) println("[-] ${ring.debug(0)}")

    for (marble in 1..highestMarble) {
        if (marble % 23 == 0) {
            for (i in 1..7) {
                ring.moveCCW()
            }

            val s = playerScores[currentPlayer]
            playerScores[currentPlayer] = s!! + marble + ring.removeCurrent()
        } else {
            ring.moveCW()
            ring.append(marble)
        }

        if (debug) println("[${currentPlayer + 1}] ${ring.debug(0)}")
        currentPlayer = (currentPlayer + 1) % numPlayers
    }

    return playerScores.values.max()!!
}

fun initScores(numPlayers: Int): MutableMap<Int, Long> {
    val playerScores = mutableMapOf<Int, Long>()

    for (player in 0 until numPlayers) {
        playerScores[player] = 0
    }

    return playerScores
}