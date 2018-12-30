import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testScoreAfter() {
        val input = """
            |.#.#...|#.
            |.....#|##|
            |.|..|...#.
            |..|#.....#
            |#.#|||#|#|
            |...#.||...
            |.|....|...
            |||...#|.#|
            ||.||||..|.
            |...#.|..|.
        """.trimMargin()
        assertEquals(1147, unoptimizedScoreAfter(10, World.parse(input)))
    }

    @Test
    fun testWorldParse() {
        val input = """
            |.|
            |#.
        """.trimMargin()
        val expected = World(listOf(
            listOf(Acre.Open, Acre.Wooded),
            listOf(Acre.LumberYard, Acre.Open)
        ))
        assertEquals(expected, World.parse(input))
    }

    @Test
    fun testToString() {
        val input = """
            |.|
            |#.
        """.trimMargin()
        assertEquals(input, World.parse(input).toString())
    }

    @Test
    fun testNext() {
        val input = World.parse("""
            |.#.#...|#.
            |.....#|##|
            |.|..|...#.
            |..|#.....#
            |#.#|||#|#|
            |...#.||...
            |.|....|...
            |||...#|.#|
            ||.||||..|.
            |...#.|..|.
        """.trimMargin())
        val expected = World.parse("""
            |.......##.
            |......|###
            |.|..|...#.
            |..|#||...#
            |..##||.|#|
            |...#||||..
            |||...|||..
            ||||||.||.|
            |||||||||||
            |....||..|.
        """.trimMargin())
        assertEquals(expected, input.next())
    }

    @Test
    fun testCycleOptimization() {
        val start = puzzleInput()

        assertEquals(unoptimizedScoreAfter(431, start), unoptimizedScoreAfter(466, start))
        assertEquals(unoptimizedScoreAfter(432, start), unoptimizedScoreAfter(467, start))

        for (i in 0..36) {
            val expected = unoptimizedScoreAfter(431 + i, start)
            val unoptimized = unoptimizedScoreAfter(466 + i, start)
            val optimized = scoreAfter(466 + i, start)
            assertEquals(expected, unoptimized);
            assertEquals(unoptimized, optimized);
        }
    }
}