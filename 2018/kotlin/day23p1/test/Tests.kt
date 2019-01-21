import org.junit.jupiter.api.Test
import kotlin.test.assertEquals


class NanobotTests {
    @Test
    fun inRangeOfStrongest() {
        val nanobots = Nanobot.parseMany("""
            pos=<0,0,0>, r=4
            pos=<1,0,0>, r=1
            pos=<4,0,0>, r=3
            pos=<0,2,0>, r=1
            pos=<0,5,0>, r=3
            pos=<0,0,3>, r=1
            pos=<1,1,1>, r=1
            pos=<1,1,2>, r=1
            pos=<1,3,1>, r=1
        """.trimIndent())
        val expected = listOf(
            nanobots[0], nanobots[1], nanobots[2], nanobots[3],
            nanobots[5], nanobots[6], nanobots[7]
        )
        assertEquals(expected, Nanobot.inRangeOfStrongest(nanobots))
    }

    @Test
    fun parseMany() {
        val input = """
            pos=<0,0,0>, r=4
            pos=<4,0,0>, r=3
            pos=<-1,3,1>, r=1
        """.trimIndent()
        val expected = listOf(
            Nanobot(Coord(0, 0, 0), 4),
            Nanobot(Coord(4, 0, 0), 3),
            Nanobot(Coord(-1, 3, 1), 1)
        )
        assertEquals(expected, Nanobot.parseMany(input))
    }

    @Test
    fun testToString() {
        assertEquals("pos=<1,3,1>, r=1", Nanobot(Coord(1, 3, 1), 1).toString())
    }
}