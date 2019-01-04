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
    fun testRoomExPaths_nested() {
        val subject = RoomEx.parse("^EN(NW|S(E|))\$")
        val expected = setOf(
            Path(Coord(0, -2), 4),
            Path(Coord(2, 0), 4),
            Path(Coord(1, 0), 3)
        )
        assertEquals(expected, subject.paths())
    }

    @Test
    fun testRoomExPaths_moreAfterOptions() {
        val subject = RoomEx.parse("^EN(NW|S(E|))E\$")
        val expected = setOf(
            Path(Coord(1, -2), 5),
            Path(Coord(3, 0), 5),
            Path(Coord(2, 0), 4)
        )
        assertEquals(expected, subject.paths())
    }


    @Test
    fun testRoomExPaths_basic() {
        val subject = RoomEx.parse("^ENS\$")
        val expected = setOf(
            Path(Coord(1, 0), 3)
        )
        assertEquals(expected, subject.paths())
    }

    @Test
    fun testOptionsPaths_basic() {
        val subject = RoomEx.Options(listOf(
            RoomEx.AtomList(listOf(Dir.N, Dir.W)),
            RoomEx.AtomList(listOf(Dir.S))
        ))
        val expected = setOf(
            Path(Coord(-1, -1), 2),
            Path(Coord(0, 1), 1)
        )
        assertEquals(expected, subject.paths())
    }

    @Test
    fun testCrossProduct() {
        val a = setOf(
            Path(Coord(0, 0), 0),
            Path(Coord(1, 1), 1)
        )
        val b = setOf(
            Path(Coord(3, 3), 3),
            Path(Coord(5, 5), 5)
        )
        val expected = setOf(
            Path(Coord(3, 3), 3),
            Path(Coord(5, 5), 5),
            Path(Coord(4, 4), 4),
            Path(Coord(6, 6), 6)

        )
        assertEquals(expected, Path.crossProduct(a, b))
    }
}