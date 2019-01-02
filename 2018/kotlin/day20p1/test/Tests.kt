import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testShortestPathToFarthestRoom() {
        assertEquals(3, shortestPathToFarthestRoom("^WNE\$"))
        assertEquals(10, shortestPathToFarthestRoom("^ENWWW(NEEE|SSE(EE|N))\$"))
        assertEquals(18, shortestPathToFarthestRoom("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN\$"))
        assertEquals(23, shortestPathToFarthestRoom("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))\$"))
        assertEquals(31, shortestPathToFarthestRoom("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))\$"))
    }

    @Test
    fun testRoomExParse_simple() {
        val input = "^NSEW\$"
        val expected = RoomEx.AtomList(
            listOf(Dir.N, Dir.S, Dir.E, Dir.W)
        )
        assertEquals(expected, RoomEx.parse(input))
    }

    @Test
    fun testRoomExParse_option() {
        val input = "^NS(E|W)\$"
        val expected = RoomEx.Expression(
            listOf(
                RoomEx.AtomList(listOf(Dir.N, Dir.S)),
                RoomEx.Options(listOf(
                    RoomEx.AtomList(listOf(Dir.E)),
                    RoomEx.AtomList(listOf(Dir.W))
                ))
            )
        )
        assertEquals(expected, RoomEx.parse(input))
    }


    @Test
    fun testRoomExParse_nested() {
        val input = "^EN(NW|S(E|))\$"
        val expected = RoomEx.Expression(listOf(
            RoomEx.AtomList(listOf(Dir.E, Dir.N)),
            RoomEx.Options(listOf(
                RoomEx.AtomList(listOf(Dir.N, Dir.W)),
                RoomEx.Expression(listOf(
                    RoomEx.AtomList(listOf(Dir.S)),
                    RoomEx.Options(listOf(
                        RoomEx.AtomList(listOf(Dir.E)),
                        RoomEx.Expression(emptyList())
                    ))
                ))
            ))
        ))
        assertEquals(expected, RoomEx.parse(input))
    }

    @Test
    fun testRoomExPaths() {
        val subject = RoomEx.parse("^EN(NW|S(E|))\$")
        val expected = setOf(
            listOf(Dir.E, Dir.N, Dir.N, Dir.W),
            listOf(Dir.E, Dir.N, Dir.S, Dir.E),
            listOf(Dir.E, Dir.N, Dir.S)
        )
        val actual = mutableSetOf<List<Dir>>()
        subject.paths(null, { actual.add(it!!.toList()) })
        assertEquals(expected, actual)
    }

    @Test
    fun testExpressionPaths_basic() {
        val subject = RoomEx.parse("^ENS\$")
        val expected = setOf(
            listOf(Dir.E, Dir.N, Dir.S)
        )
        val actual = mutableSetOf<List<Dir>>()
        subject.paths(null, { actual.add(it.toList()) })
        assertEquals(expected, actual)
    }
}