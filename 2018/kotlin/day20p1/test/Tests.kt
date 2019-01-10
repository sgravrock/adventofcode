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
    fun testRoomExParserParse_simple() {
        val input = "^NSEW\$"
        val expected = AtomList(
            listOf(Dir.N, Dir.S, Dir.E, Dir.W)
        )
        assertEquals(expected, RoomExParser.parse(input))
    }

    @Test
    fun ttestRoomExParserParse_option() {
        val input = "^NS(E|W)\$"
        val expected = Expression(
            listOf(
                AtomList(listOf(Dir.N, Dir.S)),
                Options(listOf(
                    AtomList(listOf(Dir.E)),
                    AtomList(listOf(Dir.W))
                ))
            )
        )
        assertEquals(expected, RoomExParser.parse(input))
    }


    @Test
    fun testRoomExParserParse_nested() {
        val input = "^EN(NW|S(E|))\$"
        val expected = Expression(listOf(
            AtomList(listOf(Dir.E, Dir.N)),
            Options(listOf(
                AtomList(listOf(Dir.N, Dir.W)),
                Expression(listOf(
                    AtomList(listOf(Dir.S)),
                    Options(listOf(
                        AtomList(listOf(Dir.E)),
                        Expression(emptyList())
                    ))
                ))
            ))
        ))
        assertEquals(expected, RoomExParser.parse(input))
    }

    @Test
    fun testRoomExWalk_nested() {
        val subject = RoomExParser.parse("^E(N|S(E|))\$")
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
        val subject = RoomExParser.parse("^E(N|S)W$")
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
        val subject = RoomExParser.parse("^EN\$")
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
        val subject = Expression(listOf(
            AtomList(listOf(Dir.E)),
            AtomList(listOf(Dir.N))
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
        val subject = Options(listOf(
            AtomList(listOf(Dir.N, Dir.W)),
            AtomList(listOf(Dir.S))
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
        val input = RoomExParser.parse("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))\$")
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