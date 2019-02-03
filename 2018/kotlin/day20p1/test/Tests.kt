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
        val expected = """
            #####
            #.###
            #-###
            #X|.#
            #-###
            #.###
            #####
        """.trimIndent()
        val actual = RoomExParser.parse("^NSEWS\$")
        assertEquals(expected, actual.toString())
    }

    @Test
    fun testRoomExParserParse_option() {
        val expected = """
            #######
            #.|.###
            ###-###
            ###X|.#
            #######
        """.trimIndent()
        val actual = RoomExParser.parse("^(NW|E)$")
        assertEquals(expected, actual.toString())
    }

    @Test
    fun testRoomExParserParse_moreAfterOption() {
        val expected = """
            #######
            #.|.###
            #-#-###
            #.#X|.#
            #####-#
            #####.#
            #######
        """.trimIndent()
        val actual = RoomExParser.parse("^(NW|E)S$")
        assertEquals(expected, actual.toString())
    }

    @Test
    fun testRoomExParserParse_emptyOption() {
        val expected = """
            #####
            #.|X#
            #-#-#
            #.#.#
            #####
        """.trimIndent()
        val actual = RoomExParser.parse("^(W|)S$")
        assertEquals(expected, actual.toString())
    }

    @Test
    fun testRoomExParserParse_nestedOptions() {
        val expected = """
            #######
            ###.###
            ###-###
            #X|.###
            ###-###
            ###.|.#
            #######
        """.trimIndent()
        val actual = RoomExParser.parse("^E(N|S(E|))$")
        assertEquals(expected, actual.toString())
    }

    @Test
    fun testRoomExParserParse_example() {
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
        val actual = RoomExParser.parse("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))\$")
        assertEquals(expected, actual.toString())
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
}