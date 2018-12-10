import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testParseInput() {
        val input = "[1518-11-01 00:00] Guard #10 begins shift\n" +
                "[1518-11-01 00:05] falls asleep\n" +
                "[1518-11-01 00:25] wakes up\n" +
                "[1518-11-01 00:30] falls asleep\n" +
                "[1518-11-01 00:55] wakes up\n" +
                "[1518-11-03 00:29] wakes up\n" +
                "[1518-11-04 00:02] Guard #99 begins shift\n" +
                "[1518-11-04 00:36] falls asleep\n" +
                "[1518-11-01 23:58] Guard #99 begins shift\n" +
                "[1518-11-02 00:40] falls asleep\n" +
                "[1518-11-02 00:50] wakes up\n" +
                "[1518-11-03 00:05] Guard #10 begins shift\n" +
                "[1518-11-03 00:24] falls asleep\n" +
                "[1518-11-04 00:46] wakes up\n" +
                "[1518-11-05 00:03] Guard #99 begins shift\n" +
                "[1518-11-05 00:45] falls asleep\n" +
                "[1518-11-05 00:55] wakes up"
        val expected = mapOf(
                10 to listOf(
                        TimeRange(Time(0, 5), Time(0, 25)),
                        TimeRange(Time(0, 30), Time(0, 55)),
                        TimeRange(Time(0, 24), Time(0, 29))
                ),
                99 to listOf(
                        TimeRange(Time(0, 40), Time(0, 50)),
                        TimeRange(Time(0, 36), Time(0, 46)),
                        TimeRange(Time(0, 45), Time(0, 55))
                )
        )
        assertEquals(expected, parseInput(input))
    }

    @Test
    fun testSleepiestGuard() {
        val input = mapOf(
                10 to listOf(
                        TimeRange(Time(0, 5), Time(0, 25)),
                        TimeRange(Time(0, 30), Time(0, 55)),
                        TimeRange(Time(0, 24), Time(0, 29))
                ),
                99 to listOf(
                        TimeRange(Time(0, 0), Time(0, 50)),
                        TimeRange(Time(0, 24), Time(0, 29))
                )
        )
        assertEquals(99, sleepiestGuard(input))
    }

    @Test
    fun testSleepiestMinute() {
        val input = listOf(
                TimeRange(Time(0, 5), Time(0, 25)),
                TimeRange(Time(0, 30), Time(0, 55)),
                TimeRange(Time(0, 24), Time(0, 29))
        )
        assertEquals(24, sleepiestMinute(input))
    }
}