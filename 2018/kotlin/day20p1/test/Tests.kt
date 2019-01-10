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
    fun testAnswerForPuzzleInput() {
        assertEquals(3966, answerForPuzzleInput())
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
    fun testRoomExWalk_nested() {
        val subject = RoomEx.parse("^E(N|S(E|))\$")
        val expectedTiles = mapOf(
            Coord(1, 0) to Tile.Vdoor,
            Coord(2, 0) to Tile.Room,
            Coord(2, -1) to Tile.Hdoor,
            Coord(2, -2) to Tile.Room,
            Coord(2, 1) to Tile.Hdoor,
            Coord(2, 2) to Tile.Room,
            Coord(3, 2) to Tile.Vdoor,
            Coord(4, 2) to Tile.Room
        )
        val expectedDests = setOf(
            Coord(2, -2),
            Coord(2, 2),
            Coord(4, 2)
        )
        val actualTiles = mutableMapOf<Coord, Tile>()
        val actualDests = subject.walk(Coord(0, 0), { c, t -> actualTiles[c] = t })
        assertEquals(expectedTiles, actualTiles)
        assertEquals(expectedDests, actualDests)
    }

    @Test
    fun testRoomExWalk_moreAfterOptions() {
        val subject = RoomEx.parse("^E(N|S)W$")
        val expectedTiles = mapOf(
            Coord(1, 0) to Tile.Vdoor,
            Coord(2, 0) to Tile.Room,
            Coord(2, -1) to Tile.Hdoor,
            Coord(2, -2) to Tile.Room,
            Coord(1, -2) to Tile.Vdoor,
            Coord(0, -2) to Tile.Room,
            Coord(2, 1) to Tile.Hdoor,
            Coord(2, 2) to Tile.Room,
            Coord(1, 2) to Tile.Vdoor,
            Coord(0, 2) to Tile.Room
        )
        val expectedDests = setOf(
            Coord(0, -2),
            Coord(0, 2)
        )
        val actualTiles = mutableMapOf<Coord, Tile>()
        val actualDests = subject.walk(Coord(0, 0), { c, t -> actualTiles[c] = t })
        assertEquals(expectedTiles, actualTiles)
        assertEquals(expectedDests, actualDests)
    }

    @Test
    fun testAtomListWalk() {
        val subject = RoomEx.parse("^EN\$")
        val expectedTiles = mapOf(
            Coord(1, 0) to Tile.Vdoor,
            Coord(2, 0) to Tile.Room,
            Coord(2, -1) to Tile.Hdoor,
            Coord(2, -2) to Tile.Room
        )
        val expectedDests = setOf(Coord(2, -2))
        val actualTiles = mutableMapOf<Coord, Tile>()
        val actualDests = subject.walk(Coord(0, 0), { c, t -> actualTiles[c] = t })
        assertEquals(expectedTiles, actualTiles)
        assertEquals(expectedDests, actualDests)
    }

    @Test
    fun testExpressionWalk() {
        val subject = RoomEx.Expression(listOf(
            RoomEx.AtomList(listOf(Dir.E)),
            RoomEx.AtomList(listOf(Dir.N))
        ))
        val expectedTiles = mapOf(
            Coord(1, 0) to Tile.Vdoor,
            Coord(2, 0) to Tile.Room,
            Coord(2, -1) to Tile.Hdoor,
            Coord(2, -2) to Tile.Room
        )
        val expectedDests = setOf(Coord(2, -2))
        val actualTiles = mutableMapOf<Coord, Tile>()
        val actualDests = subject.walk(Coord(0, 0), { c, t -> actualTiles[c] = t })
        assertEquals(expectedTiles, actualTiles)
        assertEquals(expectedDests, actualDests)
    }

    @Test
    fun testOptionsWalk_basic() {
        val subject = RoomEx.Options(listOf(
            RoomEx.AtomList(listOf(Dir.N, Dir.W)),
            RoomEx.AtomList(listOf(Dir.S))
        ))
        val expectedTiles = mapOf(
            Coord(0, -1) to Tile.Hdoor,
            Coord(0, -2) to Tile.Room,
            Coord(-1, -2) to Tile.Vdoor,
            Coord(-2, -2) to Tile.Room,
            Coord(0, 1) to Tile.Hdoor,
            Coord(0, 2) to Tile.Room
        )
        val expectedDests = setOf(
            Coord(-2, -2),
            Coord(0, 2)
        )
        val actualTiles = mutableMapOf<Coord, Tile>()
        val actualDests = subject.walk(Coord(0, 0), { c, t -> actualTiles[c] = t })
        assertEquals(expectedTiles, actualTiles)
        assertEquals(expectedDests, actualDests)
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

    @Test
    fun testWorldToString() {
        val input = mapOf(
            Coord(0, 0) to Tile.Room,
            Coord(1, 0) to Tile.Vdoor,
            Coord(2, 0) to Tile.Room,
            Coord(0, 1) to Tile.Hdoor,
            Coord(1, 1) to Tile.Wall,
            Coord(2, 1) to Tile.Wall,
            Coord(0, 2) to Tile.Room,
            Coord(1, 2) to Tile.Wall,
            Coord(2, 2) to Tile.Wall
        )
        val expected = """
            #####
            #X|.#
            #-###
            #.###
            #####
        """.trimIndent()
        assertEquals(expected, World(input).toString())
    }

    @Test
    fun testWorldBuild() {
        val input = RoomEx.parse("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))\$")
        val expected = """
            #############
            #.|.|.|.|.|.#
            #-#####-###-#
            #.#.|.#.#.#.#
            #-#-###-#-#-#
            #.#.#.|.#.|.#
            #-#-#-#####-#
            #.#.#.#X|.#.#
            #-#-#-###-#-#
            #.|.#.|.#.#.#
            ###-#-###-#-#
            #.|.#.|.|.#.#
            #############
        """.trimIndent()
        assertEquals(expected, World.build(input).toString())
    }
}