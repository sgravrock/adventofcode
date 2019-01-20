import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class Tests {
    @Test
    fun battleOutcomeForPuzzleInput() {
        assertEquals(189000, battleOutcome(World.parse(puzzleInput)))
    }

    @Test
    fun runGameReturnsTurnsUntilCompletion() {
        val world = World.parse(
            """
            ####
            #EG#   200,6
            ####
        """.trimIndent()
        )
        assertEquals(2, runGame(world))
    }

    @Test
    fun runGameDoesNotFightDeadCombatants() {
        val world = World.parse(
            """
            ####
            #GE#
            ####
        """.trimIndent()
        )
        (world.grid[Coord(2, 1)] as Space.Occupied).combatant.hitPoints = 3
        runGame(world)
        val expected = World.parse(
            """
            ####
            #G.#   200
            ####
        """.trimIndent()
        )
        assertEquals(expected, world)
    }

    @Test
    fun runGame_example_rightNumberOfTurns() {
        val world = World.parse(
            """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """.trimIndent()
        )
        assertEquals(47, runGame(world))
    }

    @Test
    fun runGame_example_rightEndState() {
        val world = World.parse(
            """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """.trimIndent()
        )
        runGame(world)
        val expected = World.parse(
            """
            #######
            #G....#   200
            #.G...#   131
            #.#.#G#   59
            #...#.#
            #....G#   200
            #######
        """.trimIndent()
        )
        assertEquals(expected, world)
    }

    @Test
    fun runGame_anotherExample() {
        val world = World.parse(
            """
            ####
            ##E#
            #GG#
            ####
        """.trimIndent()
        )
        assertEquals(67, runGame(world))
    }

    @Test
    fun runGame_stillAnotherExample() {
        val world = World.parse(
            """
            #####
            #GG##
            #.###
            #..E#
            #.#G#
            #.E##
            #####
        """.trimIndent()
        )
        assertEquals(71, runGame(world))
    }

    @Test
    fun battleOutcome_example() {
        val world = World.parse(
            """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """.trimIndent()
        )
        assertEquals(27730, battleOutcome(world))
    }

    @Test
    fun battleOutcome_anotherExample() {
        val world = World.parse(
            """
            ####
            ##E#
            #GG#
            ####
        """.trimIndent()
        )
        assertEquals(67 * 200, battleOutcome(world))
    }

    @Test
    fun battleOutcome_stillAnotherExample() {
        val world = World.parse(
            """
            #####
            #GG##
            #.###
            #..E#
            #.#G#
            #.E##
            #####
        """.trimIndent()
        )
        assertEquals(71 * 197, battleOutcome(world))
    }

    @Test
    fun battleOutcome_evenMoreExamples() {
        val world1 = World.parse(
            """
            ################
            #.......G......#
            #G.............#
            #..............#
            #....###########
            #....###########
            #.......EG.....#
            ################
        """.trimIndent()
        )
        assertEquals(38 * 486, battleOutcome(world1))

        val world2 = World.parse(
            """
            ######################
            #...................E#
            #.####################
            #....................#
            ####################.#
            #....................#
            #.####################
            #....................#
            ###.##################
            #EG.#................#
            ######################
        """.trimIndent()
        )
        assertEquals(66 * 202, battleOutcome(world2))
    }


    @Test
    fun doRound_example() {
        val world = World.parse(
            """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """.trimIndent()
        )
        val expected1 = World.parse(
            """
            #######
            #..G..#   200
            #...EG#   197,197
            #.#G#G#   200,197
            #...#E#   197
            #.....#
            #######
        """.trimIndent()
        )
        val expected2 = World.parse(
            """
            #######
            #...G.#   200
            #..GEG#   200,188,194
            #.#.#G#   194
            #...#E#   194
            #.....#
            #######
        """.trimIndent()
        )
        doRound(world)
        assertEquals(expected1, world)
        doRound(world)
        assertEquals(expected2, world)
    }

    @Test
    fun doRound_movesThatOneGoblinFromMyPuzzleInputCorrectly() {
        val world = World.parse(puzzleInput)
        doRound(world)
        assertEquals(false, world.grid.containsKey(Coord(24, 11)))
        assertEquals(
            Space.Occupied(Combatant(Race.Goblin, 200)),
            world.grid[Coord(25, 10)]
        )
    }

    @Test
    fun testFilterMap() {
        val actual = sequenceOf("a", "b", "c")
            .filterMap {
                if (it == "a") {
                    1
                } else if (it == "c") {
                    2
                } else {
                    null
                }
            }
            .toList()
        assertEquals(listOf(1, 2), actual)
    }
}

class WorldTests {
    @Test
    fun combatantsInOrderReturnsReadingOrder() {
        val subject = World(
            mutableMapOf(
                Coord(0, 0) to Space.Occupied(arbitraryCombatant),
                Coord(0, 1) to Space.Occupied(arbitraryCombatant),
                Coord(1, 0) to Space.Occupied(arbitraryCombatant),
                Coord(1, 1) to Space.Wall
            )
        )
        val expected = listOf(
            Pair(Coord(0, 0), arbitraryCombatant),
            Pair(Coord(1, 0), arbitraryCombatant),
            Pair(Coord(0, 1), arbitraryCombatant)
        )
        assertEquals(expected, subject.combatantsInOrder().toList())
    }

    @Test
    fun advanceDoesNothingIfAlreadyInRange() {
        val input = """
            #####
            #G.E#
            #E..#
            #####
        """.trimIndent()
        val subject = World.parse(input)
        assertEquals(Coord(1, 1), subject.advance(Coord(1, 1)))
        assertEquals(World.parse(input), subject)
    }

    @Test
    fun advanceMovesTowardNearestEnemy() {
        val subject = World.parse(
            """
            ######
            #G..E#
            #....#
            #E...#
            ######
        """.trimIndent()
        )
        assertEquals(Coord(1, 2), subject.advance(Coord(1, 1)))
        val expected = World.parse(
            """
            ######
            #...E#
            #G...#
            #E...#
            ######
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun advanceChoosesShortestPathToTarget() {
        val subject = World.parse(
            """
            ########
            #E.....#
            #.####.#
            #..G...#
            ########
        """.trimIndent()
        )
        assertEquals(Coord(1, 2), subject.advance(Coord(1, 1)))
        val expected = World.parse(
            """
            ########
            #......#
            #E####.#
            #..G...#
            ########
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun advanceBreaksTiesBetweenOpponentsInReadingOrder() {
        val subject = World.parse(
            """
            #####
            #E.G#
            #...#
            #G..#
            #####
        """.trimIndent()
        )
        assertEquals(Coord(2, 1), subject.advance(Coord(1, 1)))
        val expected = World.parse(
            """
            #####
            #.EG#
            #...#
            #G..#
            #####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun advanceBreaksTiesBetweenSameOpponentPathsInReadingOrder() {
        val subject = World.parse(
            """
            ####
            #E.#
            #.G#
            ####
        """.trimIndent()
        )
        assertEquals(Coord(2, 1), subject.advance(Coord(1, 1)))
        val expected = World.parse(
            """
            ####
            #.E#
            #.G#
            ####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun advanceBreaksTiesByDestNotFirstMove() {
        val subject = World.parse(
            """
            #######
            #.E..G#
            #.#####
            #G#####
            #######
        """.trimIndent()
        )
        assertEquals(Coord(3, 1), subject.advance(Coord(2, 1)))
        val expected = World.parse(
            """
            #######
            #..E.G#
            #.#####
            #G#####
            #######
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun advanceIgnoresSameRace() {
        val subject = World.parse(
            """
            ####
            #G.#
            #G.#
            #..#
            #E.#
            ####
        """.trimIndent()
        )
        assertEquals(Coord(1, 3), subject.advance(Coord(1, 2)))
        val expected = World.parse(
            """
            ####
            #G.#
            #..#
            #G.#
            #E.#
            ####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun advanceExample() {
        val subject = World.parse(
            """
            #######
            #G...#
            #..EG#
            #....#
            #....#
            #E...#
            #######
        """.trimIndent()
        )
        val expected = World.parse(
            """
            #######
            #.G..#
            #..EG#
            #....#
            #....#
            #E...#
            #######
        """.trimIndent()
        )
        assertEquals(Coord(2, 1), subject.advance(Coord(1, 1)))
        assertEquals(expected, subject)
    }

    @Test
    fun advanceMovesThatOneGoblinFromMyPuzzleInputCorrectly() {
        val subject = World.parse(
            """
            #####
            #E..#
            #..G#
            #####
        """.trimIndent()
        )
        val expected = World.parse(
            """
            #####
            #E.G#
            #...#
            #####
        """.trimIndent()
        )
        subject.advance(Coord(3, 2))
        assertEquals(expected, subject)
    }

    @Test
    fun fightDoesNothingIfNoTargetInRange() {
        val input = """
            #####
            #G.E#
            #####
        """.trimIndent()
        val subject = World.parse(input)
        val expected = World.parse(input)
        subject.fight(Coord(1, 1))
        assertEquals(expected, subject)
    }

    @Test
    fun fightAttacksTargetInRange() {
        val subject = World.parse(
            """
            #####
            #GEE#   200,200,100
            #####
        """.trimIndent()
        )
        subject.fight(Coord(1, 1))
        val expected = World.parse(
            """
            #####
            #GEE#   200,197,100
            #####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun fightAttacksWeakestTargetInRange() {
        val subject = World.parse(
            """
            #####
            #GEG#   200,200,199
            #####
        """.trimIndent()
        )
        subject.fight(Coord(2, 1))
        val expected = World.parse(
            """
            #####
            #GEG#   200,200,196
            #####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun fightIgnoresSameRace() {
        val subject = World.parse(
            """
            #####
            #GGE#
            #####
        """.trimIndent()
        )
        subject.fight(Coord(1, 1))
        subject.fight(Coord(2, 1))
        val expected = World.parse(
            """
            #####
            #GGE#   200,200,197
            #####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun fightRemovesDeadCombatant() {
        val subject = World.parse(
            """
            ####
            #EG#
            ####
        """.trimIndent()
        )
        (subject.grid[Coord(2, 1)] as Space.Occupied).combatant.hitPoints = 3
        subject.fight(Coord(1, 1))
        val expected = World.parse(
            """
            ####
            #E.#
            ####
        """.trimIndent()
        )
        assertEquals(expected, subject)
    }

    @Test
    fun shortestPath_reachable() {
        val subject = World.parse(
            """
            #####
            #E..#
            #.#.#
            #.G.#
            #####
        """.trimIndent()
        )
        assertEquals(
            Path(2, Coord(1, 2), Coord(1, 4)),
            subject.shortestPathToNeighbor(Coord(1, 1), Coord(1, 4))
        )
    }

    @Test
    fun shortestPath_unreachable() {
        val subject = World.parse(
            """
            #####
            #E#.#
            #####
        """.trimIndent()
        )
        assertNull(subject.shortestPathToNeighbor(Coord(1, 1), Coord(3, 1)))
    }

    @Test
    fun parse() {
        val input = """
            ####
            #G.#
            #.E#
            ####
        """.trimIndent()
        val expected = World(
            mutableMapOf(
                Coord(0, 0) to Space.Wall,
                Coord(1, 0) to Space.Wall,
                Coord(2, 0) to Space.Wall,
                Coord(3, 0) to Space.Wall,
                Coord(0, 1) to Space.Wall,
                Coord(1, 1) to Space.Occupied(Combatant(Race.Goblin, 200)),
                Coord(3, 1) to Space.Wall,
                Coord(0, 2) to Space.Wall,
                Coord(2, 2) to Space.Occupied(Combatant(Race.Elf, 200)),
                Coord(3, 2) to Space.Wall,
                Coord(0, 3) to Space.Wall,
                Coord(1, 3) to Space.Wall,
                Coord(2, 3) to Space.Wall,
                Coord(3, 3) to Space.Wall
            )
        )
        assertEquals(expected, World.parse(input))
    }

    @Test
    fun parseWithHitPoints() {
        val input = """
            #####
            #G.E#   100,150
            #####
        """.trimIndent()
        val expected = World(
            mutableMapOf(
                Coord(0, 0) to Space.Wall,
                Coord(1, 0) to Space.Wall,
                Coord(2, 0) to Space.Wall,
                Coord(3, 0) to Space.Wall,
                Coord(4, 0) to Space.Wall,
                Coord(0, 1) to Space.Wall,
                Coord(1, 1) to Space.Occupied(Combatant(Race.Goblin, 100)),
                Coord(3, 1) to Space.Occupied(Combatant(Race.Elf, 150)),
                Coord(4, 1) to Space.Wall,
                Coord(0, 2) to Space.Wall,
                Coord(1, 2) to Space.Wall,
                Coord(2, 2) to Space.Wall,
                Coord(3, 2) to Space.Wall,
                Coord(4, 2) to Space.Wall
            )
        )
        assertEquals(expected, World.parse(input))
    }

    @Test
    fun testToString() {
        val expected = """
            #####
            #G..#   200
            #.EG#   100,150
            #####
        """.trimIndent()
        val subject = World(
            mutableMapOf(
                Coord(0, 0) to Space.Wall,
                Coord(1, 0) to Space.Wall,
                Coord(2, 0) to Space.Wall,
                Coord(3, 0) to Space.Wall,
                Coord(4, 0) to Space.Wall,
                Coord(0, 1) to Space.Wall,
                Coord(1, 1) to Space.Occupied(Combatant(Race.Goblin, 200)),
                Coord(4, 1) to Space.Wall,
                Coord(0, 2) to Space.Wall,
                Coord(2, 2) to Space.Occupied(Combatant(Race.Elf, 100)),
                Coord(3, 2) to Space.Occupied(Combatant(Race.Goblin, 150)),
                Coord(4, 2) to Space.Wall,
                Coord(0, 3) to Space.Wall,
                Coord(1, 3) to Space.Wall,
                Coord(2, 3) to Space.Wall,
                Coord(3, 3) to Space.Wall,
                Coord(4, 3) to Space.Wall
            )
        )
        assertEquals(expected, subject.toString())
    }
}

class CoordTests {
    @Test
    fun comparesInReadingOrder() {
        val input = listOf(Coord(0, 0), Coord(0, 1), Coord(1, 1), Coord(1, 0))
        val expected = listOf(Coord(0, 0), Coord(1, 0), Coord(0, 1), Coord(1, 1))
        assertEquals(expected, input.sorted())
        assertEquals(Coord(0, 0), input.min())
        assertEquals(Coord(1, 1), input.max())
    }

    @Test
    fun comparesInReadingOrder2() {
        val input = listOf(Coord(1, 2), Coord(2, 1))
        val expected = listOf(Coord(2, 1), Coord(1, 2))
        assertEquals(expected, input.sorted())
        assertEquals(Coord(2, 1), input.minBy { it })
    }
}

val arbitraryCoord = Coord(-1, -1)
val arbitraryCombatant = Combatant(Race.Goblin, Int.MAX_VALUE)