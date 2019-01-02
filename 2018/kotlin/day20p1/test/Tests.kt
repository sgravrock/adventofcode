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
        val expected = RoomEx.Expression(
            listOf(
                RoomEx.Term.Atom(Dir.N),
                RoomEx.Term.Atom(Dir.S),
                RoomEx.Term.Atom(Dir.E),
                RoomEx.Term.Atom(Dir.W)
            )
        )
        assertEquals(expected, RoomEx.parse(input))
    }

    @Test
    fun testRoomExParse_nested() {
        val input = "^EN(NW|S(E|))\$"
        val expected = RoomEx.Expression(listOf(
            RoomEx.Term.Atom(Dir.E),
            RoomEx.Term.Atom(Dir.N),
            RoomEx.Term.Options(listOf(
                RoomEx.Expression(listOf(
                    RoomEx.Term.Atom(Dir.N),
                    RoomEx.Term.Atom(Dir.W)
                )),
                RoomEx.Expression(listOf(
                    RoomEx.Term.Atom(Dir.S),
                    RoomEx.Term.Options(listOf(
                        RoomEx.Expression(listOf(RoomEx.Term.Atom(Dir.E))),
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
        assertEquals(expected, subject.paths())
    }

    @Test
    fun testOptionsPaths_handlesEmpty() {
        val subject = RoomEx.Term.Options(listOf(
            RoomEx.Expression(listOf(RoomEx.Term.Atom(Dir.E))),
            RoomEx.Expression(emptyList())
        ))
        val expected = setOf(
            listOf(Dir.E),
            listOf()
        )
        assertEquals(expected, subject.paths())
    }

    @Test
    fun testCrossProduct() {
        val a = setOf(listOf(1, 2), listOf(3))
        val b = setOf(listOf(4), listOf(), listOf(5, 6))
        val expected = setOf(
            listOf(1, 2, 4),
            listOf(1, 2),
            listOf(1, 2, 5, 6),
            listOf(3, 4),
            listOf(3),
            listOf(3, 5, 6)
        )
        assertEquals(expected, crossProduct(a, b))
    }
}