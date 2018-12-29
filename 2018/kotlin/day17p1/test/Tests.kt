import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testSpacesReachable() {
        val input = """x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504"""
        assertEquals(57, GroundMap.parse(input).spacesReachable())
    }

    @Test
    fun testParse() {
        val input = """x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4"""
        val expected = mapOf(
            Coord(495, 2) to Space.Clay,
            Coord(495, 3) to Space.Clay,
            Coord(495, 4) to Space.Clay,
            Coord(495, 5) to Space.Clay,
            Coord(495, 6) to Space.Clay,
            Coord(495, 7) to Space.Clay,
            Coord(496, 7) to Space.Clay,
            Coord(497, 7) to Space.Clay,
            Coord(498, 2) to Space.Clay,
            Coord(498, 3) to Space.Clay,
            Coord(498, 4) to Space.Clay,
            Coord(498, 7) to Space.Clay,
            Coord(499, 7) to Space.Clay,
            Coord(500, 7) to Space.Clay,
            Coord(501, 7) to Space.Clay,
            Coord(500, 0) to Space.Spring,
            Coord(501, 3) to Space.Clay,
            Coord(501, 4) to Space.Clay,
            Coord(501, 5) to Space.Clay,
            Coord(501, 6) to Space.Clay,
            Coord(501, 6) to Space.Clay
        )
        assertEquals(expected, GroundMap.parse(input).spaces)
    }

    @Test
    fun testToString() {
        val input = """x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504"""
        val expected = """.....+......
...........#
#..#.......#
#..#..#.....
#..#..#.....
#.....#.....
#.....#.....
#######.....
............
............
...#.....#..
...#.....#..
...#.....#..
...#######.."""
        assertEquals(expected, GroundMap.parse(input).toString())
    }

    // Note: This test is more diagnostic than descriptive. It exercises implementation
    // details rather than any well-defined external behavior. If the other tests pass
    // and this one fails, then this one is probably "wrong".
    @Test
    fun testAdvance() {
        val input = """x=497, y=1..5
x=501, y=1..5
x=499, y=2..3
y=5, x=497..500"""
        val state = GroundMap.parse(input)
        state.advance()
        assertEquals("""...+.
#..|#
#.#|#
#.#|#
#~~~#
#####""", state.toString())
        state.advance()
        assertEquals("""...+.
#..|#
#.#|#
#.#~#
#~~~#
#####""", state.toString())
        state.advance()
        assertEquals("""...+.
#..|#
#.#~#
#.#~#
#~~~#
#####""", state.toString())
        state.advance()
        assertEquals("""...+.
#|||#
#|#~#
#~#~#
#~~~#
#####""", state.toString())
        state.advance()
        assertEquals("""...+.
#|||#
#~#~#
#~#~#
#~~~#
#####""", state.toString())
        state.advance()
        assertEquals("""...+.
#~~~#
#~#~#
#~#~#
#~~~#
#####""", state.toString())
    }

    @Test
    fun testAdvance_doesNotSpreadAcrossBottom() {
        val input = """
            |x=499, y=2..2
            |x=501, y=2..2
            |y=3, x=499..501
            |x=497, y=2..2
            |x=503, y=2..2
        """.trimMargin()
        val state = GroundMap.parse(input)

        assertEquals("""
            |...+...
            |.......
            |#.#.#.#
            |..###..
        """.trimMargin(), state.toString())

        state.advance()
        state.advance()

        assertEquals("""
            |...+...
            |.|||||.
            |#|#~#|#
            |.|###|.
        """.trimMargin(), state.toString())
    }

    @Test
    fun testAdvance_doesNotSpreadAcrossBottom_moreComplexCase() {
        val input = """
            |x=496, y=2..2
            |x=504, y=2..2
            |x=499, y=2..2
            |x=501, y=2..2
            |y=3, x=499..501
        """.trimMargin()
        val state = GroundMap.parse(input)

        assertEquals("""
            |....+....
            |.........
            |#..#.#..#
            |...###...
        """.trimMargin(), state.toString())

        state.advance()
        state.advance()

        assertEquals("""
            |....+....
            |..|||||..
            |#.|#~#|.#
            |..|###|..
        """.trimMargin(), state.toString())
    }
}