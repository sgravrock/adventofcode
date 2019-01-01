import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
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
}