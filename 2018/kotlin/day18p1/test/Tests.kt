import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testScoreAfter10() {
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
        assertEquals(1147, scoreAfter10(World.parse(input)))
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
}