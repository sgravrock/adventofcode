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
        assertEquals(2, subject.fewestMinutesToTarget())
    }
}

class PathNodeTests {
    @Test
    fun neighbors_allFourDirections() {
        val cave = StubCave.fill(3, 3, Region.Rocky)
        val subject = PathNode(Coord(1, 1), Tool.Torch)
        val expected = setOf(
                PathNode(Coord(1, 0), Tool.Torch),
                PathNode(Coord(0, 1), Tool.Torch),
                PathNode(Coord(2, 1), Tool.Torch),
                PathNode(Coord(1, 2), Tool.Torch),
                PathNode(Coord(1, 1), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_excludesLessThanZero() {
        val cave = StubCave.fill(2, 2, Region.Rocky)
        val subject = PathNode(Coord(0, 0), Tool.Torch)
        val expected = setOf(
                PathNode(Coord(1, 0), Tool.Torch),
                PathNode(Coord(0, 1), Tool.Torch),
                PathNode(Coord(0, 0), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_excludesBelowDepth() {
        val cave = StubCave.fill(2, 2, Region.Rocky)
        val subject = PathNode(Coord(0, 1), Tool.Torch)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.Torch),
                PathNode(Coord(1, 1), Tool.Torch),
                PathNode(Coord(0, 1), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_rockyWithTorch() {
        val cave = StubCave.fill(1, 2, Region.Rocky)
        val subject = PathNode(Coord(0, 0), Tool.Torch)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.ClimbingGear),
                PathNode(Coord(1, 0), Tool.Torch)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_rockyWithClimbingGear() {
        val cave = StubCave.fill(1, 2, Region.Rocky)
        val subject = PathNode(Coord(0, 0), Tool.ClimbingGear)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.Torch),
                PathNode(Coord(1, 0), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_wetWithNone() {
        val cave = StubCave.fill(1, 2, Region.Wet)
        val subject = PathNode(Coord(0, 0), Tool.None)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.ClimbingGear),
                PathNode(Coord(1, 0), Tool.None)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_wetWithClimbingGear() {
        val cave = StubCave.fill(1, 2, Region.Wet)
        val subject = PathNode(Coord(0, 0), Tool.ClimbingGear)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.None),
                PathNode(Coord(1, 0), Tool.ClimbingGear)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_narrowWithTorch() {
        val cave = StubCave.fill(1, 2, Region.Narrow)
        val subject = PathNode(Coord(0, 0), Tool.Torch)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.None),
                PathNode(Coord(1, 0), Tool.Torch)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
    }

    @Test
    fun neighbors_narrowWithNone() {
        val cave = StubCave.fill(1, 2, Region.Narrow)
        val subject = PathNode(Coord(0, 0), Tool.None)
        val expected = setOf(
                PathNode(Coord(0, 0), Tool.Torch),
                PathNode(Coord(1, 0), Tool.None)
        )
        assertEquals(expected, subject.neighbors(cave).toSet())
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