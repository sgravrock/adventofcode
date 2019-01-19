import java.lang.Exception
import java.util.*

fun main(args: Array<String>) {
    val input = """
        ################################
        ###################........#####
        ###################..G..#G..####
        ####################........####
        ##..G###############G......#####
        ###..G###############.....######
        #####.######..######....G##..###
        #####.........####............##
        #########...#####.............##
        #########...####..............##
        #########E#####.......GE......##
        #########............E...G...###
        ######.###....#####..G........##
        #.G#....##...#######.........###
        ##.#....##GG#########.........##
        #....G#....E#########....#....##
        #...........#########.......####
        #####..G....#########...##....##
        #####....G..#########.#.......##
        #######...G..#######G.....#...##
        ######....E...#####............#
        ######...GG.......E......#...E.#
        #######.G...#....#..#...#.....##
        #######..........#####..####.###
        ########.......E################
        #######..........###############
        ########.............###########
        #########...#...##....##########
        #########.....#.#..E..##########
        ################.....###########
        ################.##E.###########
        ################################
    """.trimIndent()
    println(battleOutcome(World.parse(input)))
    // 186124 is too low
    // 187984 is too low
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
    fun takeTurn(combatant: Coord): Boolean
    fun advance(combatant: Coord): Coord
    fun fight(attacker: Coord)
}

data class Path(val length: Int, val start: Coord)

data class World(val grid: MutableMap<Coord, Space>) : IWorld {
    override fun combatantsInOrder(): Sequence<Pair<Coord, Combatant>> {
        return grid.asSequence()
            .filter { it.value is Space.Occupied }
            .map { Pair(it.key, (it.value as Space.Occupied).combatant) }
            .sortedBy { it.first }
    }

    override fun takeTurn(combatant: Coord): Boolean {
        if (!anyOpponents(combatant)) {
            return false
        }

        fight(advance(combatant))
        return true
    }

    private fun anyOpponents(combatant: Coord): Boolean {
        val combatantRace = (grid[combatant] as Space.Occupied).combatant.race
        return grid.any {
            val space = it.value
            space is Space.Occupied && space.combatant.race != combatantRace
        }
    }

    override fun advance(combatant: Coord): Coord {
        val combatantRace = (grid[combatant] as Space.Occupied).combatant.race

        if (hasEnemyInRange(combatant, combatantRace)) {
            println("Not advancing $combatant because an enemy is in range")
            return combatant
        }

        // TODO: Is this right? Might need to consider all squares adjacent
        // to combatants, not the combatants themselves.
        val paths = combatantsInOrder()
            .filter { it.second.race != combatantRace }
            .map { shortestPath(combatant, it.first) }
            .filter { it != null }
            .sortedBy { it!!.length }

        if (!paths.any()) {
            println("Not advancing $combatant because no paths were found")
            return combatant
        }

        val minLength = paths.map { it!!.length }.min()!!

        val path = paths
            .filter { it!!.length == minLength }
            .minBy { it!!.start }!!

        if (grid[path.start] is Space.Occupied) {
            println("Not advancing $combatant because ${path.start} was occupied")
            return combatant
        }

        assert(!grid.containsKey(path.start))
        grid[path.start] = grid.remove(combatant)!!
        return path.start
    }

    override fun fight(attacker: Coord) {
        val attackerRace = (grid[attacker] as Space.Occupied).combatant.race
        val target = attacker.neighborsInOrder()
            .filter {
                val space = grid[it]
                space is Space.Occupied && space.combatant.race != attackerRace
            }
            .minBy { (grid[it] as Space.Occupied).combatant.hitPoints }
            ?: return

        val opponent = (grid[target] as Space.Occupied).combatant
        opponent.hitPoints -= 3

        if (opponent.hitPoints <= 0) {
            grid.remove(target)
        }
    }

    // TODO: rename to shortestPathToNeighbor?
    fun shortestPath(src: Coord, dest: Coord): Path? {
//        println("shortestPath($src, $dest)")
        data class PathStub(val length: Int, val start: Coord, val pos: Coord)

        val queue: Queue<PathStub> = LinkedList<PathStub>()
        val added = mutableSetOf(src)
        val candidates = mutableListOf<Path>()

        fun maybeEnqueue(p: PathStub) {
            if (grid.containsKey(p.pos)) {
//                println("Not enqueueing $p because ${p.pos} is not vacant")
            } else if (added.add(p.pos)) {
//                println("Enqueueing $p")
                queue.add(p)
            } else {
//                println("Not enqueueing $p because ${p.pos} was already added")
            }
        }

        for (c in src.neighborsInOrder()) {
            maybeEnqueue(PathStub(1, c, c))
        }

        while (!queue.isEmpty()) {
            val p = queue.remove()
//            println("Visiting ${p}")

            // TODO: optimize?
            if (p.pos.neighborsInOrder().contains(dest)) {
                // TODO: May be able to exit early if longer.
                if (candidates.size == 0 || p.length == candidates[0].length) {
                    candidates.add(Path(p.length, p.start))
                }
            } else {
                for (c in p.pos.neighborsInOrder()) {
                    maybeEnqueue(PathStub(p.length + 1, p.start, c))
                }
            }
        }

        return candidates.minBy { it.start }
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
        if (!doRound(world)) return i

        println("After turn ${i}:\n$world")
    }

    throw Error("Game failed to end")
}

fun doRound(world: IWorld): Boolean {
    val allFoundTargets = world.combatantsInOrder()
        .filter { it.second.hitPoints > 0 }
        .all { world.takeTurn(it.first) }
    return allFoundTargets
}

fun battleOutcome(world: IWorld): Int {
    val nTurns = runGame(world)
    val hitPoints = world.combatantsInOrder().sumBy { it.second.hitPoints }
    return nTurns * hitPoints
}