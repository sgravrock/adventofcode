import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testMineRailroadAdvance() {
        val subject = MineRailroad.parse(
                ">->\\\n" +
                "   |"
        )
        val expectedCarts1 = setOf(
                Cart(Coord(1, 0), Orientation.Right, TurnDirection.Left),
                Cart(Coord(3, 0), Orientation.Down, TurnDirection.Left)
        )
        val expectedCarts2 = setOf(
                Cart(Coord(2, 0), Orientation.Right, TurnDirection.Left),
                Cart(Coord(3, 1), Orientation.Down, TurnDirection.Left)
        )
        subject.advance()
        assertEquals(expectedCarts1, subject.carts.toSet())
        subject.advance()
        assertEquals(expectedCarts2, subject.carts.toSet())
    }

    @Test
    fun testMineRailroadAdvanceUntilOneCart() {
        val subject = MineRailroad.parse(
                "/>-<\\  \n" +
                "|   |  \n" +
                "| /<+-\\\n" +
                "| | | v\n" +
                "\\>+</ |\n" +
                "  |   ^\n" +
                "  \\<->/"
        )
        subject.advanceUntilOneCart()
        assertEquals(Coord(6, 4), subject.remainingCartCoord())
    }

    @Test
    fun testMineRailroadAdvance_findsMidTickCrash() {
        val subject = MineRailroad.parse(">>>-")
        subject.advance()
        val expectedCarts = listOf(
                Cart(Coord(3, 0), Orientation.Right, TurnDirection.Left)
        )
        assertEquals(expectedCarts, subject.carts)
    }

    @Test
    fun testMineRailroadParse() {
        val actual = MineRailroad.parse(
                "/-<>--\\\n" +
                "^     |\n" +
                "v  /--+"
        )
        val expected = MineRailroad(
                tracks = mapOf(
                        Coord(0, 0) to TrackSegment.SlashyTurn,
                        Coord(1, 0) to TrackSegment.Horizontal,
                        Coord(2, 0) to TrackSegment.Horizontal,
                        Coord(3, 0) to TrackSegment.Horizontal,
                        Coord(4, 0) to TrackSegment.Horizontal,
                        Coord(5, 0) to TrackSegment.Horizontal,
                        Coord(6, 0) to TrackSegment.BackslashyTurn,
                        Coord(0, 1) to TrackSegment.Vertical,
                        Coord(6, 1) to TrackSegment.Vertical,
                        Coord(0, 2) to TrackSegment.Vertical,
                        Coord(3, 2) to TrackSegment.SlashyTurn,
                        Coord(4, 2) to TrackSegment.Horizontal,
                        Coord(5, 2) to TrackSegment.Horizontal,
                        Coord(6, 2) to TrackSegment.Intersection
                ),
                carts = mutableListOf(
                        Cart(Coord(2, 0), Orientation.Left, TurnDirection.Left),
                        Cart(Coord(3, 0), Orientation.Right, TurnDirection.Left),
                        Cart(Coord(0, 1), Orientation.Up, TurnDirection.Left),
                        Cart(Coord(0, 2), Orientation.Down, TurnDirection.Left)
                )
        )
        assertEquals(expected, actual)
    }

    @Test
    fun testCartAdvanceLeft() {
        val subject = Cart(Coord(1, 0), Orientation.Left, TurnDirection.Left)
        val tracks = mapOf(Coord(0, 0) to TrackSegment.Horizontal)
        subject.advance(tracks)
        assertEquals(Coord(0, 0), subject.coord)
    }

    @Test
    fun testCartAdvanceRight() {
        val subject = Cart(Coord(0, 0), Orientation.Right, TurnDirection.Left)
        val tracks = mapOf(Coord(1, 0) to TrackSegment.Horizontal)
        subject.advance(tracks)
        assertEquals(Coord(1, 0), subject.coord)
    }

    @Test
    fun testCartAdvanceUp() {
        val subject = Cart(Coord(0, 1), Orientation.Up, TurnDirection.Left)
        val tracks = mapOf(Coord(0, 0) to TrackSegment.Vertical)
        subject.advance(tracks)
        assertEquals(Coord(0, 0), subject.coord)
    }

    @Test
    fun testCartAdvanceDown() {
        val subject = Cart(Coord(0, 0), Orientation.Down, TurnDirection.Left)
        val tracks = mapOf(Coord(0, 1) to TrackSegment.Vertical)
        subject.advance(tracks)
        assertEquals(Coord(0, 1), subject.coord)
    }

    @Test
    fun testCartAdvanceSlashyTurn() {
        val tracks = mapOf(Coord(0, 0) to TrackSegment.SlashyTurn)
        val fromRight = Cart(Coord(1, 0), Orientation.Left, TurnDirection.Left)
        fromRight.advance(tracks)
        assertEquals(Cart(Coord(0, 0), Orientation.Down, TurnDirection.Left), fromRight)

        val fromLeft = Cart(Coord(-1, 0), Orientation.Right, TurnDirection.Left)
        fromLeft.advance(tracks)
        assertEquals(Cart(Coord(0, 0), Orientation.Up, TurnDirection.Left), fromLeft)

        val fromBelow = Cart(Coord(0, 1), Orientation.Up, TurnDirection.Left)
        fromBelow.advance(tracks)
        assertEquals(Cart(Coord(0, 0), Orientation.Right, TurnDirection.Left), fromBelow)

        val fromAbove = Cart(Coord(0, -1), Orientation.Down, TurnDirection.Left)
        fromAbove.advance(tracks)
        assertEquals(Cart(Coord(0, 0), Orientation.Left, TurnDirection.Left), fromAbove)
    }

    @Test
    fun testCartAdvanceBackslashyTurn() {
        val tracks = mapOf(Coord(1, 1) to TrackSegment.BackslashyTurn)
        val fromLeft = Cart(Coord(0, 1), Orientation.Right, TurnDirection.Left)
        fromLeft.advance(tracks)
        assertEquals(Cart(Coord(1, 1), Orientation.Down, TurnDirection.Left), fromLeft)

        val fromRight = Cart(Coord(2, 1), Orientation.Left, TurnDirection.Left)
        fromRight.advance(tracks)
        assertEquals(Cart(Coord(1, 1), Orientation.Up, TurnDirection.Left), fromRight)

        val fromAbove = Cart(Coord(1, 0), Orientation.Down, TurnDirection.Left)
        fromAbove.advance(tracks)
        assertEquals(Cart(Coord(1, 1), Orientation.Right, TurnDirection.Left), fromAbove)

        val fromBelow = Cart(Coord(1, 2), Orientation.Up, TurnDirection.Left)
        fromBelow.advance(tracks)
        assertEquals(Cart(Coord(1, 1), Orientation.Left, TurnDirection.Left), fromBelow)
    }

    @Test
    fun testCartAdvanceIntersection() {
        val subject = Cart(Coord(0, 0), Orientation.Right, TurnDirection.Left)
        subject.advance(mapOf(Coord(1, 0) to TrackSegment.Intersection))
        assertEquals(Cart(Coord(1, 0), Orientation.Up, TurnDirection.Straight), subject)

        subject.advance(mapOf(Coord(1, -1) to TrackSegment.Intersection))
        assertEquals(Cart(Coord(1, -1), Orientation.Up, TurnDirection.Right), subject)

        subject.advance(mapOf(Coord(1, -2) to TrackSegment.Intersection))
        assertEquals(Cart(Coord(1, -2), Orientation.Right, TurnDirection.Left), subject)

        subject.advance(mapOf(Coord(2, -2) to TrackSegment.Intersection))
        assertEquals(Cart(Coord(2, -2), Orientation.Up, TurnDirection.Straight), subject)
    }
}