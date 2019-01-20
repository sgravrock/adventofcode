import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CaveTests {
    @Test
    fun fewestMinutes() {
        assertEquals(45, Cave(510, 10, 10).fewestMinutesToTarget())
    }

    @Test
    fun fewestMinutes_tinyExample() {
        val subject = Cave(3, 1, 1)
        subject.printToWidth(5)
        assertEquals(2, subject.fewestMinutesToTarget())
    }
}
/*
  012
0 M=.
1 .T.
2 ..=

00T -> 01T(1) -> 11T(2)
00T -> 01T(1) -> 11C
00T -> 01T(1) -> 02
00T -> 10N(8) -> 20C(16) PRUNE
00T -> 10N(8) -> 20T(16) PRUNE
00T -> 10N(8) -> 11C(16) PRUNE
00T -> 10N(8) -> 11T(16)
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 11C(11) +7=18
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 11T(18)
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 22C(11)
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 22N(18) PRUNE
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 11C(25) PRUNE
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 11T(18)
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 22C(25) PRUNE
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 22N(18) PRUNE
00T -> 10C(8) -> 20T(16) -> 21C(24) PRUNE
00T -> 10C(8) -> 20T(16) -> 21T(17) -> 11C(25) PRUNE
00T -> 10C(8) -> 20T(16) -> 21T(17) -> 11T(18)
00T -> 10C(8) -> 20T(16) -> 21T(17) -> 22*(>=18) PRUNE
00T -> 10C(8) -> 11C(9) + 7 = 16
00T -> 10C(8) -> 20C(9) -> 21 PRUNE
00T -> 10C(8) -> 20T(15) -> 21C(22) PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 22N(23) PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 22C(23) PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 11C PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 11T(17)
00T -> 10C(8) -> 11C(9) + 7 = 16
00T -> 10C(8) -> 11T(16)
00T -> 01C(8)


best:
    11* -> 16
 */

class SearchStateTests {
    @Test
    fun neighbors_allFourDirections() {
        val cave = StubCave.fill(3, 3, Region.Rocky)
        val subject = SearchState(Coord(1, 1), Tool.Torch)
        val expected = setOf(
                SearchState(Coord(1, 0), Tool.Torch),
                SearchState(Coord(0, 1), Tool.Torch),
                SearchState(Coord(2, 1), Tool.Torch),
                SearchState(Coord(1, 2), Tool.Torch),
                SearchState(Coord(1, 0), Tool.ClimbingGear),
                SearchState(Coord(0, 1), Tool.ClimbingGear),
                SearchState(Coord(2, 1), Tool.ClimbingGear),
                SearchState(Coord(1, 2), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_excludesLessThanZero() {
        val cave = StubCave.fill(2, 2, Region.Rocky)
        val subject = SearchState(Coord(0, 0), Tool.Torch)
        val expected = setOf(
                SearchState(Coord(1, 0), Tool.Torch),
                SearchState(Coord(0, 1), Tool.Torch),
                SearchState(Coord(1, 0), Tool.ClimbingGear),
                SearchState(Coord(0, 1), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_excludesBelowDepth() {
        val cave = StubCave.fill(2, 2, Region.Rocky)
        val subject = SearchState(Coord(0, 1), Tool.Torch)
        val expected = setOf(
                SearchState(Coord(0, 0), Tool.Torch),
                SearchState(Coord(1, 1), Tool.Torch),
                SearchState(Coord(0, 0), Tool.ClimbingGear),
                SearchState(Coord(1, 1), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_rockyToRocky() {
        testNeighborsByType(
                Region.Rocky, Region.Rocky,
                setOf(Tool.Torch, Tool.ClimbingGear)
        )
    }

    @Test
    fun neighbors_rockyToNarrow() {
        testNeighborsByType(Region.Rocky, Region.Narrow, setOf(Tool.Torch))
    }

    @Test
    fun neighbors_rockyToWet() {
        testNeighborsByType(Region.Rocky, Region.Wet, setOf(Tool.ClimbingGear))
    }

    @Test
    fun neighbors_wetToWet() {
        testNeighborsByType(
                Region.Wet, Region.Wet,
                setOf(Tool.None, Tool.ClimbingGear)
        )
    }

    @Test
    fun neighbors_wetToRocky() {
        testNeighborsByType(Region.Wet, Region.Rocky, setOf(Tool.ClimbingGear))
    }

    @Test
    fun neighbors_wetToNarrow() {
        testNeighborsByType(Region.Wet, Region.Narrow, setOf(Tool.None))
    }

    @Test
    fun neighbors_narrowToNarrow() {
        testNeighborsByType(
                Region.Narrow, Region.Narrow,
                setOf(Tool.None, Tool.Torch)
        )
    }

    @Test
    fun neighbors_narrowToWet() {
        testNeighborsByType(Region.Narrow, Region.Wet, setOf(Tool.None))
    }

    @Test
    fun neighbors_narrowToRocky() {
        testNeighborsByType(Region.Narrow, Region.Rocky, setOf(Tool.Torch))
    }

    private fun testNeighborsByType(src: Region, dest: Region, expected: Set<Tool>) {
        val cave = StubCave(1, mapOf(
                Coord(0, 0) to src,
                Coord(1, 0) to dest
        ))
        val subject = SearchState(Coord(0, 0), Tool.Torch)
        val actual = subject.neighbors(cave).map { it.tool }.toSet()
        assertEquals(expected, actual)
    }
}

class StubCave(override val depth: Int, val regions: Map<Coord, Region>) : ICave {
    override fun regionType(pos: Coord): Region {
        return regions[pos]!!
    }

    companion object {
        fun fill(depth: Int, width: Int, region: Region): StubCave {
            val regions = mutableMapOf<Coord, Region>()

            for (y in 0..(depth - 1)) {
                for (x in 0..(width - 1)) {
                    regions[Coord(x, y)] = region
                }
            }

            return StubCave(depth, regions)
        }
    }
}