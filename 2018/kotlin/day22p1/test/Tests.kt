import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CaveTests {
    @Test
    fun build() {
        val expected = Cave.parse(
            """
            M=.|=.|.|=.
            .|=|=|||..|
            .==|....||=
            =.|....|.==
            =|..==...=.
            =||.=.=||=|
            |.=.===|||.
            |..==||=.|=
            .=..===..=|
            .======|||=
            .===|=|===T
        """.trimIndent()
        )
        assertEquals(expected, Cave.build(510, Coord(10, 10)))
    }

    @Test
    fun riskLevel() {
        val subject = Cave.build(510, Coord(10, 10))
        assertEquals(114, subject.riskLevel())
    }

    @Test
    fun riskLevelForPuzzleInput() {
        val subject = Cave.build(9171, Coord(7, 721))
        assertEquals(5786, subject.riskLevel())
    }

    @Test
    fun testToString() {
        val subject = Cave(
            mapOf(
                Coord(0, 0) to Region.Mouth,
                Coord(1, 0) to Region.Rocky,
                Coord(2, 0) to Region.Narrow,
                Coord(0, 1) to Region.Wet,
                Coord(1, 1) to Region.Target,
                Coord(2, 1) to Region.Rocky
            )
        )
        val expected = """
            M.|
            =T.
        """.trimIndent()
        assertEquals(expected, subject.toString())
    }

    @Test
    fun parse() {
        val expected = Cave(
            mapOf(
                Coord(0, 0) to Region.Mouth,
                Coord(1, 0) to Region.Rocky,
                Coord(2, 0) to Region.Narrow,
                Coord(0, 1) to Region.Wet,
                Coord(1, 1) to Region.Target,
                Coord(2, 1) to Region.Rocky
            )
        )
        val actual = Cave.parse(
            """
            M.|
            =T.
        """.trimIndent()
        )
        assertEquals(expected, actual)
    }
}