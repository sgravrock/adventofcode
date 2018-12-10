import java.lang.Exception

fun main(args: Array<String>) {
    val classLoader = Time::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    println(puzzleSolution(parseInput(input)))
}

data class Time(val hour: Int, val minute: Int) {
    fun toMinutes() = hour * 60 + minute
}

data class TimeRange(val start: Time, val end: Time)

fun sleepiestGuard(sleepsByGuard: Map<Int, List<TimeRange>>): Int {
    return sleepsByGuard.entries
            .sortedBy { totalMinutes(it.value) }
            .last().key
}

fun sleepiestMinute(sleeps: List<TimeRange>): Int {
    return (0..59)
            .sortedBy { minute ->
                sleeps.filter {
                    it.start.hour == 0 &&
                            it.end.hour == 0 &&
                            it.start.minute <= minute &&
                            it.end.minute > minute
                }.count()
            }
            .last()
}

fun totalMinutes(ranges: List<TimeRange>): Int {
    // Offer void where prohibited or if any sleep spans midnight.
    // Do not fold, spindle, or mutilate.
    return ranges.map { it.end.toMinutes() - it.start.toMinutes() }.sum()
}

fun parseInput(input: String): Map<Int, List<TimeRange>> {
    val result = mutableMapOf<Int, MutableList<TimeRange>>()
    var curGuard: Int? = null
    var curSleepStart: Time? = null
    val shiftStartRe = Regex("Guard #([\\d]+) begins shift")
    val sleepStartRe = Regex("([\\d]{2}):([\\d]{2})\\] falls asleep")
    val sleepEndRe = Regex("([\\d]{2}):([\\d]{2})\\] wakes up")

    for (line in input.lines().sorted()) {
        val shiftMatch = shiftStartRe.find(line)
        val sleepStartMatch = sleepStartRe.find(line)
        val sleepEndMatch = sleepEndRe.find(line)

        if (shiftMatch != null) {
            curGuard = shiftMatch.groupValues[1].toInt()
            if (!result.containsKey(curGuard)) {
                result[curGuard] = mutableListOf()
            }
        } else if (sleepStartMatch != null) {
            curSleepStart = Time(
                    sleepStartMatch.groupValues[1].toInt(),
                    sleepStartMatch.groupValues[2].toInt()
            )
        } else if (sleepEndMatch != null) {
            result[curGuard]!!.add(TimeRange(
                    curSleepStart!!,
                    Time(
                            sleepEndMatch.groupValues[1].toInt(),
                            sleepEndMatch.groupValues[2].toInt()
                    )
            ))
        } else {
            throw Exception("Parse error: ${line}")
        }
    }

    return result
}

fun puzzleSolution(sleepsByGuard: Map<Int, List<TimeRange>>): Int {
    val guard = sleepiestGuard(sleepsByGuard)
    return guard * sleepiestMinute(sleepsByGuard[guard]!!)
}