import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class GridTests {
    @Test
    fun testParse() {
        val expected = Grid(listOf(
                Particle(position = XY(9, 1), velocity = XY(0, 2)),
                Particle(position = XY(-7, -1), velocity = XY(-1, -2)),
                Particle(position = XY(6, 10), velocity = XY(-2, -1))
        ))
        val actual = Grid.parse("position=< 9,  1> velocity=< 0,  2>\n" +
                "position=<-7, -1> velocity=<-1, -2>\n" +
                "position=< 6, 10> velocity=<-2, -1>"
        )
        assertEquals(expected, actual)
    }

    @Test
    fun testToString() {
        val subject = Grid.parse("position=< 9,  1> velocity=< 0,  2>\n" +
                "position=< 7,  0> velocity=<-1,  0>\n" +
                "position=< 3, -2> velocity=<-1,  1>\n" +
                "position=< 6, 10> velocity=<-2, -1>\n" +
                "position=< 2, -4> velocity=< 2,  2>\n" +
                "position=<-6, 10> velocity=< 2, -2>\n" +
                "position=< 1,  8> velocity=< 1, -1>\n" +
                "position=< 1,  7> velocity=< 1,  0>\n" +
                "position=<-3, 11> velocity=< 1, -2>\n" +
                "position=< 7,  6> velocity=<-1, -1>\n" +
                "position=<-2,  3> velocity=< 1,  0>\n" +
                "position=<-4,  3> velocity=< 2,  0>\n" +
                "position=<10, -3> velocity=<-1,  1>\n" +
                "position=< 5, 11> velocity=< 1, -2>\n" +
                "position=< 4,  7> velocity=< 0, -1>\n" +
                "position=< 8, -2> velocity=< 0,  1>\n" +
                "position=<15,  0> velocity=<-2,  0>\n" +
                "position=< 1,  6> velocity=< 1,  0>\n" +
                "position=< 8,  9> velocity=< 0, -1>\n" +
                "position=< 3,  3> velocity=<-1,  1>\n" +
                "position=< 0,  5> velocity=< 0, -1>\n" +
                "position=<-2,  2> velocity=< 2,  0>\n" +
                "position=< 5, -2> velocity=< 1,  2>\n" +
                "position=< 1,  4> velocity=< 2,  1>\n" +
                "position=<-2,  7> velocity=< 2, -2>\n" +
                "position=< 3,  6> velocity=<-1, -1>\n" +
                "position=< 5,  0> velocity=< 1,  0>\n" +
                "position=<-6,  0> velocity=< 2,  0>\n" +
                "position=< 5,  9> velocity=< 1, -2>\n" +
                "position=<14,  7> velocity=<-2,  0>\n" +
                "position=<-3,  6> velocity=< 2, -1>"
        )
        val expected = "\n" +
                "........#.............\n" +
                "................#.....\n" +
                ".........#.#..#.......\n" +
                "......................\n" +
                "#..........#.#.......#\n" +
                "...............#......\n" +
                "....#.................\n" +
                "..#.#....#............\n" +
                ".......#..............\n" +
                "......#...............\n" +
                "...#...#.#...#........\n" +
                "....#..#..#.........#.\n" +
                ".......#..............\n" +
                "...........#..#.......\n" +
                "#...........#.........\n" +
                "...#.......#..........\n"
        assertEquals(expected, subject.toString())
    }

    @Test
    fun testAdvance() {
        val initial = Grid.parse("position=< 9,  1> velocity=< 0,  2>\n" +
                "position=< 7,  0> velocity=<-1,  0>\n" +
                "position=< 3, -2> velocity=<-1,  1>\n" +
                "position=< 6, 10> velocity=<-2, -1>\n" +
                "position=< 2, -4> velocity=< 2,  2>\n" +
                "position=<-6, 10> velocity=< 2, -2>\n" +
                "position=< 1,  8> velocity=< 1, -1>\n" +
                "position=< 1,  7> velocity=< 1,  0>\n" +
                "position=<-3, 11> velocity=< 1, -2>\n" +
                "position=< 7,  6> velocity=<-1, -1>\n" +
                "position=<-2,  3> velocity=< 1,  0>\n" +
                "position=<-4,  3> velocity=< 2,  0>\n" +
                "position=<10, -3> velocity=<-1,  1>\n" +
                "position=< 5, 11> velocity=< 1, -2>\n" +
                "position=< 4,  7> velocity=< 0, -1>\n" +
                "position=< 8, -2> velocity=< 0,  1>\n" +
                "position=<15,  0> velocity=<-2,  0>\n" +
                "position=< 1,  6> velocity=< 1,  0>\n" +
                "position=< 8,  9> velocity=< 0, -1>\n" +
                "position=< 3,  3> velocity=<-1,  1>\n" +
                "position=< 0,  5> velocity=< 0, -1>\n" +
                "position=<-2,  2> velocity=< 2,  0>\n" +
                "position=< 5, -2> velocity=< 1,  2>\n" +
                "position=< 1,  4> velocity=< 2,  1>\n" +
                "position=<-2,  7> velocity=< 2, -2>\n" +
                "position=< 3,  6> velocity=<-1, -1>\n" +
                "position=< 5,  0> velocity=< 1,  0>\n" +
                "position=<-6,  0> velocity=< 2,  0>\n" +
                "position=< 5,  9> velocity=< 1, -2>\n" +
                "position=<14,  7> velocity=<-2,  0>\n" +
                "position=<-3,  6> velocity=< 2, -1>"
        )
        val expected1 = "\n" +
                "........#....#....\n" +
                "......#.....#.....\n" +
                "#.........#......#\n" +
                "..................\n" +
                "....#.............\n" +
                "..##.........#....\n" +
                "....#.#...........\n" +
                "...##.##..#.......\n" +
                "......#.#.........\n" +
                "......#...#.....#.\n" +
                "#...........#.....\n" +
                "..#.....#.#.......\n"
        val actual1 = initial.advance()

        assertEquals(expected1, actual1.toString())
    }
}