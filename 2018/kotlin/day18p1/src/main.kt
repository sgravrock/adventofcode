fun main(args: Array<String>) {
    val classLoader = World::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    val result = scoreAfter10(World.parse(input))
    println(result)
}

enum class Acre { Open, Wooded, LumberYard }

fun scoreAfter10(world: World): Int {
    val endState = (1..10).fold(world, { prev, ignored -> prev.next() })
    val wooded = endState.cells().count { it == Acre.Wooded }
    val lumberYards = endState.cells().count { it == Acre.LumberYard }
    return wooded * lumberYards
}

data class Coord(val x: Int, val y: Int)

data class World(val data: List<List<Acre>>) {
    fun next(): World {
        val result = data.mapIndexed { y, row ->
            row.mapIndexed { x, acre ->
                when (acre) {
                    Acre.Open -> {
                        if (numNeighbors(x, y, Acre.Wooded) >= 3) {
                            Acre.Wooded
                        } else {
                            Acre.Open
                        }
                    }
                    Acre.Wooded -> {
                        if (numNeighbors(x, y, Acre.LumberYard) >= 3) {
                            Acre.LumberYard
                        } else {
                            Acre.Wooded
                        }
                    }
                    Acre.LumberYard -> {
                        if (
                            numNeighbors(x, y, Acre.LumberYard) >= 1 &&
                            numNeighbors(x, y, Acre.Wooded) >= 1
                        ) {
                            Acre.LumberYard
                        } else {
                            Acre.Open
                        }
                    }
                }
            }
        }
        return World(result)
    }

    fun cells(): Sequence<Acre> {
        return data.asSequence().flatMap { it.asSequence() }
    }

    private fun numNeighbors(x: Int, y: Int, type: Acre): Int {
        val candidates = listOf(
            Coord(x - 1, y - 1),
            Coord(x, y - 1),
            Coord(x + 1, y - 1),
            Coord(x - 1, y),
            Coord(x + 1, y),
            Coord(x - 1, y + 1),
            Coord(x, y + 1),
            Coord(x + 1, y + 1)
        )
        return candidates.count { c -> at(c) == type }
    }

    private fun at(c: Coord): Acre? {
        val row = data.getOrNull(c.y)
            ?: return null
        return row.getOrNull(c.x)
    }

    override fun toString(): String {
        return data
            .map { row ->
                row.map {
                    when (it) {
                        Acre.Open -> '.'
                        Acre.Wooded -> '|'
                        Acre.LumberYard -> '#'
                    }
                }.joinToString("")
            }.joinToString("\n")
    }

    companion object {
        fun parse(input: String): World {
            val result = input.lines()
                .map { line ->
                    line.toCharArray()
                        .map { c ->
                            when (c) {
                                '.' -> Acre.Open
                                '|' -> Acre.Wooded
                                '#' -> Acre.LumberYard
                                else -> throw Error("Unrecognized character: $c")
                            }
                        }
                }
            return World(result)
        }
    }
}