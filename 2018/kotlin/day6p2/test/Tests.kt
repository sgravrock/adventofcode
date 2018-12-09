import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testRegionWithinDistance() {
        val input = listOf(
                Coord(1, 1),
                Coord(1, 6),
                Coord(8, 3),
                Coord(3, 4),
                Coord(5, 5),
                Coord(8, 9)
        )

        val expected = setOf(
                Coord(3, 3),
                Coord(4, 3),
                Coord(5, 3),
                Coord(2, 4),
                Coord(3, 4),
                Coord(4, 4),
                Coord(5, 4),
                Coord(6, 4),
                Coord(2, 5),
                Coord(3, 5),
                Coord(4, 5),
                Coord(5, 5),
                Coord(6, 5),
                Coord(3, 6),
                Coord(4, 6),
                Coord(5, 6)
        )

        assertEquals(expected, regionWithinDistance(input, 31))
    }

    @Test
    fun TestParseInput() {
        val input = "292, 73\n204, 176"
        val expected = listOf(
                Coord(292, 73),
                Coord(204, 176)
        )
        assertEquals(expected, parseInput(input))
    }
}