import java.lang.Exception
import java.util.*

fun main(args: Array<String>) {

}

enum class Race { Goblin, Elf }
data class Combatant(val race: Race, var hitPoints: Int)

data class Coord(val x: Int, val y: Int) : Comparable<Coord> {
    fun neighborsInOrder(): List<Coord> {
        return listOf(
            Coord(x - 1, y),
            Coord(x, y - 1),
            Coord(x, y + 1),
            Coord(x + 1, y)
        )
    }

    override fun compareTo(other: Coord): Int {
        return if (y == other.y) {
            x.compareTo(other.x)
        } else {
            y.compareTo(other.y)
        }
    }
}

sealed class Space {
    object Wall : Space()
    data class Occupied(val combatant: Combatant) : Space()
}

interface IWorld {
    fun combatantsInOrder(): Sequence<Pair<Coord, Combatant>>
    fun advance(combatant: Coord): Boolean
    fun fight(attacker: Coord): Boolean
}

data class Path(val length: Int, val start: Coord)

data class World(val grid: MutableMap<Coord, Space>) : IWorld {
    override fun combatantsInOrder(): Sequence<Pair<Coord, Combatant>> {
        return grid.asSequence()
            .filter { it.value is Space.Occupied }
            .map { Pair(it.key, (it.value as Space.Occupied).combatant) }
            .sortedBy { it.first }
    }

    override fun advance(combatant: Coord): Boolean {
        val combatantRace = (grid[combatant] as Space.Occupied).combatant.race

        if (hasEnemyInRange(combatant, combatantRace)) {
            return false
        }

        // TODO: Is this right? Might need to consider all squares adjacent
        // to combatants, not the combatants themselves.
        val paths = combatantsInOrder()
            .filter { it.second.race != combatantRace }
            .map { shortestPath(combatant, it.first) }
            .filter { it != null }
            .sortedBy { it!!.length }

        if (!paths.any()) {
            return false
        }

        val minLength = paths.map { it!!.length }.min()!!

        val path = paths
            .filter { it!!.length == minLength }
            .minBy { it!!.start }!!

        if (grid[path.start] is Space.Occupied) {
            return false
        }

        assert(!grid.containsKey(path.start))
        grid[path.start] = grid.remove(combatant)!!
        return true
    }

    override fun fight(attacker: Coord): Boolean {
        val attackerRace = (grid[attacker] as Space.Occupied).combatant.race
        val target = attacker.neighborsInOrder().firstOrNull {
            val space = grid[it]
            space is Space.Occupied && space.combatant.race != attackerRace
        } ?: return false

        val opponent = (grid[target] as Space.Occupied).combatant
        opponent.hitPoints -= 3

        if (opponent.hitPoints <= 0) {
            grid.remove(target)
        }

        return true
    }

    fun shortestPath(src: Coord, dest: Coord): Path? {
        data class PathStub(val length: Int, val start: Coord, val pos: Coord)

        val queue: Queue<PathStub> = LinkedList<PathStub>()
        val added = mutableSetOf(src)

        fun maybeEnqueue(p: PathStub) {
            if (!added.contains(p.pos)) {
                queue.add(p)
                added.add(p.pos)
            }
        }

        for (c in src.neighborsInOrder()) {
            maybeEnqueue(PathStub(1, c, c))
        }

        while (!queue.isEmpty()) {
            val p = queue.remove()

            if (p.pos == dest) {
                return Path(p.length, p.start)
            } else if (!grid.containsKey(p.pos)) {
                for (c in p.pos.neighborsInOrder()) {
                    maybeEnqueue(PathStub(p.length + 1, p.start, c))
                }
            }
        }

        return null
    }

    private fun hasEnemyInRange(combatant: Coord, combatantRace: Race): Boolean {
        return combatantsInOrder()
            .filter { it.second.race != combatantRace }
            .any { it.first in combatant.neighborsInOrder() }
    }

    private fun gridRange(): RangePair {
        val xs = grid.asSequence().map { it.key.x }
        val ys = grid.asSequence().map { it.key.y }
        return RangePair(
            xs.min()!!..xs.max()!!,
            ys.min()!!..ys.max()!!
        )
    }

    override fun toString(): String {
        val r = gridRange()
        return r.y.asSequence()
            .map { y ->
                val gridLine = r.x.asSequence()
                    .map { x ->
                        val s = grid[Coord(x, y)]
                        when (s) {
                            null -> '.'
                            is Space.Wall -> '#'
                            is Space.Occupied -> {
                                when (s.combatant.race) {
                                    Race.Elf -> 'E'
                                    Race.Goblin -> 'G'
                                }
                            }
                        }
                    }
                    .joinToString("")
                val hitPoints = r.x.asSequence()
                    .map { x -> grid[Coord(x, y)] }
                    .filter { it is Space.Occupied }
                    .map { (it as Space.Occupied).combatant.hitPoints }
                    .joinToString(",")

                if (hitPoints == "") {
                    gridLine
                } else {
                    "$gridLine   $hitPoints"
                }
            }
            .joinToString("\n")
    }

    companion object {
        fun parse(input: String): World {
            val grid = mutableMapOf<Coord, Space>()

            input.lineSequence().forEachIndexed { y, line ->
                val chunks = line.split("   ")
                val hitPoints = if (chunks.size > 1) {
                    chunks[1].split(",").map { it.toInt() }
                } else {
                    emptyList()
                }
                var nextCombatant = 0

                fun getHp(): Int {
                    return hitPoints.getOrNull(nextCombatant++) ?: 200
                }

                chunks[0].forEachIndexed { x, c ->
                    val coord = Coord(x, y)
                    when (c) {
                        '#' -> grid[coord] = Space.Wall
                        'G' -> grid[coord] = Space.Occupied(
                            Combatant(Race.Goblin, getHp())
                        )
                        'E' -> grid[coord] = Space.Occupied(
                            Combatant(Race.Elf, getHp())
                        )
                        '.' -> {
                        }
                        else -> throw Exception("Unexpected '${c}'")
                    }
                }
            }

            return World(grid)
        }
    }
}

class RangePair(val x: IntRange, val y: IntRange)

fun runGame(world: IWorld): Int {
    for (i in 0..Int.MAX_VALUE) {
        val advanced = world.combatantsInOrder().all { world.advance(it.first) }
        val fought = world.combatantsInOrder().all {
            // May have died since combatantsInOrder() was called
            it.second.hitPoints > 0 && world.fight(it.first)
        }

        if (!advanced && !fought) {
            return i
        }
    }

    throw Error("Game failed to end")
}