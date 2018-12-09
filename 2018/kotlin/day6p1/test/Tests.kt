import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun TestFindAreas() {
        val input = listOf(
                Coord(1, 1),
                Coord(1, 6),
                Coord(8, 3),
                Coord(3, 4),
                Coord(5, 5),
                Coord(8, 9)
        )
        val expected = listOf(
                Area.Infinite,
                Area.Infinite,
                Area.Infinite,
                Area.Finite(setOf(
                        Coord(3, 2),
                        Coord(4, 2),
                        Coord(2, 3),
                        Coord(3, 3),
                        Coord(4, 3),
                        Coord(2, 4),
                        Coord(3, 4),
                        Coord(4, 4),
                        Coord(3, 5)
                )),
                Area.Finite(setOf(
                        Coord(5, 2),
                        Coord(5, 3),
                        Coord(5, 4),
                        Coord(6, 4),
                        Coord(4, 5),
                        Coord(5, 5),
                        Coord(6, 5),
                        Coord(7, 5),
                        Coord(4, 6),
                        Coord(5, 6),
                        Coord(6, 6),
                        Coord(7, 6),
                        Coord(4, 7),
                        Coord(5, 7),
                        Coord(6, 7),
                        Coord(4, 8),
                        Coord(5, 8)
                )),
                Area.Infinite
        )
        assertEquals(expected, findAreas(input))
    }

    @Test
    fun TestLargestFiniteArea() {
        val input = listOf(
                Coord(1, 1),
                Coord(1, 6),
                Coord(8, 3),
                Coord(3, 4),
                Coord(5, 5),
                Coord(8, 9)
        )

        assertEquals(17, largestFiniteArea(input))
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