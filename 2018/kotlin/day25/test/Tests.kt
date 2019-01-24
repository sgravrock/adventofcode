import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun constellations_puzzleInput() {
        val classLoader = Point::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        assertEquals(386, constellations(parsePoints(input)).size)
    }

    @Test
    fun constellations_example1() {
        val points = listOf(
            Point(0, 0, 0, 0),
            Point(3, 0, 0, 0),
            Point(0, 3, 0, 0),
            Point(0, 0, 3, 0),
            Point(0, 0, 0, 3),
            Point(0, 0, 0, 6),
            Point(9, 0, 0, 0),
            Point(12, 0, 0, 0)
        )
        val expected = listOf(
            setOf(
                Point(0, 0, 0, 0),
                Point(3, 0, 0, 0),
                Point(0, 3, 0, 0),
                Point(0, 0, 3, 0),
                Point(0, 0, 0, 3),
                Point(0, 0, 0, 6)
            ),
            setOf(
                Point(9, 0, 0, 0),
                Point(12, 0, 0, 0)
            )
        )
        assertEquals(expected, constellations(points))
    }

    @Test
    fun constellations_example2() {
        val points = listOf(
            Point(0, 0, 0, 0),
            Point(3, 0, 0, 0),
            Point(0, 3, 0, 0),
            Point(0, 0, 3, 0),
            Point(0, 0, 0, 3),
            Point(0, 0, 0, 6),
            Point(9, 0, 0, 0),
            Point(12, 0, 0, 0),
            Point(6, 0, 0, 0)
        )
        assertEquals(listOf(points.toSet()), constellations(points))
    }

    @Test
    fun constellations_example3() {
        val points = listOf(
            Point(-1, 2, 2, 0),
            Point(0, 0, 2, -2),
            Point(0, 0, 0, -2),
            Point(-1, 2, 0, 0),
            Point(-2, -2, -2, 2),
            Point(3, 0, 2, -1),
            Point(-1, 3, 2, 2),
            Point(-1, 0, -1, 0),
            Point(0, 2, 1, -2),
            Point(3, 0, 0, 0)
        )
        val expected = listOf(
            setOf(
                Point(-1, 0, -1, 0),
                Point(-1, 2, 2, 0),
                Point(-1, 2, 0, 0),
                Point(-1, 3, 2, 2)
            ),
            setOf(
                Point(0, 2, 1, -2),
                Point(0, 0, 2, -2),
                Point(0, 0, 0, -2)
            ),
            setOf(
                Point(3, 0, 0, 0),
                Point(3, 0, 2, -1)
            ),
            setOf(
                Point(-2, -2, -2, 2)
            )
        )
        assertEquals(expected, constellations(points))
    }

    @Test
    fun constellations_example4() {
        val points = listOf(
            Point(1, -1, 0, 1),
            Point(2, 0, -1, 0),
            Point(3, 2, -1, 0),
            Point(0, 0, 3, 1),
            Point(0, 0, -1, -1),
            Point(2, 3, -2, 0),
            Point(-2, 2, 0, 0),
            Point(2, -2, 0, -1),
            Point(1, -1, 0, -1),
            Point(3, 2, 0, 2)
        )
        assertEquals(3, constellations(points).size)
    }

    @Test
    fun constellations_example5() {
        val points = listOf(
            Point(1, -1, -1, -2),
            Point(-2, -2, 0, 1),
            Point(0, 2, 1, 3),
            Point(-2, 3, -2, 1),
            Point(0, 2, 3, -2),
            Point(-1, -1, 1, -2),
            Point(0, -2, -1, 0),
            Point(-2, 2, 3, -1),
            Point(1, 2, 2, 0),
            Point(-1, -2, 0, -2)
        )
        assertEquals(8, constellations(points).size)
    }

    @Test
    fun parsePoints() {
        val input = """
            -1,2,2,0
            0,0,0,-2
            3,0,2,-1
        """.trimIndent()
        val expected = listOf(
            Point(-1, 2, 2, 0),
            Point(0, 0, 0, -2),
            Point(3, 0, 2, -1)
        )
        assertEquals(expected, parsePoints(input))
    }
}