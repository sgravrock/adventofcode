import com.nhaarman.mockitokotlin2.any
import com.nhaarman.mockitokotlin2.mock
import com.nhaarman.mockitokotlin2.whenever
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class Tests {
    @Test
    fun runGameReturnsTurnsUntilCompletion() {
        var i = 0
        val world1 = mock<IWorld>()
        val combatants = sequenceOf(Pair(arbitraryCoord, arbitraryCombatant))
        whenever(world1.combatantsInOrder()).thenReturn(combatants)
        whenever(world1.advance(any())).then { i++ == 0 }
        whenever(world1.fight(any())).thenReturn(false)
        assertEquals(1, runGame(world1))

        i = 0
        val world2 = mock<IWorld>()
        whenever(world2.combatantsInOrder()).thenReturn(combatants)
        whenever(world2.advance(any())).thenReturn(false)
        whenever(world2.fight(any())).then { i++ <= 1 }
        assertEquals(2, runGame(world2))
    }

    @Test
    fun runGameDoesNotFightDeadCombatants() {
        val world = World.parse("""
            ####
            #GE#
            ####
        """.trimIndent())
        (world.grid[Coord(2, 1)] as Space.Occupied).combatant.hitPoints = 3
        runGame(world)
        assertEquals(200, (world.grid[Coord(1, 1)] as Space.Occupied).combatant.hitPoints)
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
        assertEquals(false, subject.advance(Coord(1, 1)))
        assertEquals(World.parse(input), subject)
    }

    @Test
    fun advanceMovesTowardNearestEnemy() {
        val subject = World.parse("""
            ######
            #G..E#
            #....#
            #E...#
            ######
        """.trimIndent())
        assertEquals(true, subject.advance(Coord(1, 1)))
        val expected = World.parse("""
            ######
            #...E#
            #G...#
            #E...#
            ######
        """.trimIndent())
        assertEquals(expected, subject)
    }

    @Test
    fun advanceChoosesShortestPathToTarget() {
        val subject = World.parse("""
            ########
            #E.....#
            #.####.#
            #..G...#
            ########
        """.trimIndent())
        assertEquals(true, subject.advance(Coord(1, 1)))
        val expected = World.parse("""
            ########
            #......#
            #E####.#
            #..G...#
            ########
        """.trimIndent())
        assertEquals(expected, subject)
    }

    @Test
    fun advanceBreaksTiesInReadingOrder() {
        val subject = World.parse("""
            #####
            #E.G#
            #...#
            #G..#
            #####
        """.trimIndent())
        assertEquals(true, subject.advance(Coord(1, 1)))
        val expected = World.parse("""
            #####
            #.EG#
            #...#
            #G..#
            #####
        """.trimIndent())
        assertEquals(expected, subject)
    }

    @Test
    fun advanceIgnoresSameRace() {
        val subject = World.parse("""
            ####
            #G.#
            #G.#
            #..#
            #E.#
            ####
        """.trimIndent())
        assertEquals(true, subject.advance(Coord(1, 2)))
        val expected = World.parse("""
            ####
            #G.#
            #..#
            #G.#
            #E.#
            ####
        """.trimIndent())
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
        assertEquals(false, subject.fight(Coord(1, 1)))
        assertEquals(expected, subject)
    }

    @Test
    fun fightAttacksTargetInRange() {
        val subject = World.parse("""
            #####
            #GEE#
            #####
        """.trimIndent())
        assertEquals(true, subject.fight(Coord(1, 1)))
        assertEquals(
            197,
            (subject.grid[Coord(2, 1)] as Space.Occupied).combatant.hitPoints
        )
        assertEquals(
            200,
            (subject.grid[Coord(3, 1)] as Space.Occupied).combatant.hitPoints
        )
    }

    @Test
    fun fightIgnoresSameRace() {
        val subject = World.parse("""
            #####
            #GGE#
            #####
        """.trimIndent())
        assertEquals(false, subject.fight(Coord(1, 1)))
        assertEquals(true, subject.fight(Coord(2, 1)))
        assertEquals(
            197,
            (subject.grid[Coord(3, 1)] as Space.Occupied).combatant.hitPoints
        )
        assertEquals(
            200,
            (subject.grid[Coord(1, 1)] as Space.Occupied).combatant.hitPoints
        )
    }

    @Test
    fun fightRemovesDeadCombatant() {
        val subject = World.parse("""
            ####
            #EG#
            ####
        """.trimIndent())
        (subject.grid[Coord(2, 1)] as Space.Occupied).combatant.hitPoints = 3
        subject.fight(Coord(1, 1))
        val expected = World.parse("""
            ####
            #E.#
            ####
        """.trimIndent())
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
            Path(3, Coord(1, 2)),
            subject.shortestPath(Coord(1, 1), Coord(1, 4))
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
        assertNull(subject.shortestPath(Coord(1, 1), Coord(3, 1)))
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
    fun testToString() {
        val expected = """
            ####
            #G.#
            #.E#
            ####
        """.trimIndent()
        val subject = World(
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
val arbitraryGoblin = Combatant(Race.Goblin, Int.MAX_VALUE)
val arbitraryElf = Combatant(Race.Elf, Int.MAX_VALUE)