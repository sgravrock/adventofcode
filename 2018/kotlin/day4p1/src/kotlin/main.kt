import java.io.File
import java.lang.Exception

fun main(args: Array<String>) {
    val classLoader = ClassForFindingClassloader::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    println(puzzleSolution(parseInput(input)))
}

class ClassForFindingClassloader {}

data class Time(val hour: Int, val minute: Int)
data class TimeRange(val start: Time, val end: Time)
data class Shift(val guard: Int, val sleeps: List<TimeRange>)

class ShiftsBuilder {
    val shifts = mutableListOf<Shift>()
    var curGuard: Int? = null
    var curFinishedSleeps = mutableListOf<TimeRange>()
    var curSleepStart: Time? = null

    fun startShift(guard: Int) {
        finalizeShift()
        curGuard = guard
    }

    fun finalizeShift() {
        if (curGuard == null) {
            return
        }

        if (curSleepStart != null) {
            throw Exception("Can't finalize a shift mid-sleep")
        }

        shifts.add(Shift(curGuard!!, curFinishedSleeps))
        curGuard = null
        curFinishedSleeps = mutableListOf()
        curSleepStart = null
    }

    fun startSleep(time: Time) {
        curSleepStart = time
    }

    fun endSleep(time: Time) {
        curFinishedSleeps.add(TimeRange(curSleepStart!!, time))
        curSleepStart = null
    }
}

fun parseInput(input: String): List<Shift> {
    val builder = ShiftsBuilder()

    val shiftStartRe = Regex("Guard #([\\d]+) begins shift")
    val sleepStartRe = Regex("([\\d]{2}):([\\d]{2})\\] falls asleep")
    val sleepEndRe = Regex("([\\d]{2}):([\\d]{2})\\] wakes up")

    for (line in input.lines().sorted()) {
        val shiftMatch = shiftStartRe.find(line)
        val sleepStartMatch = sleepStartRe.find(line)
        val sleepEndMatch = sleepEndRe.find(line)

        if (shiftMatch != null) {
            builder.startShift(shiftMatch.groupValues[1].toInt())
        } else if (sleepStartMatch != null) {
            builder.startSleep(Time(
                    sleepStartMatch.groupValues[1].toInt(),
                    sleepStartMatch.groupValues[2].toInt()
            ))
        } else if (sleepEndMatch != null) {
            builder.endSleep(Time(
                    sleepEndMatch.groupValues[1].toInt(),
                    sleepEndMatch.groupValues[2].toInt()
            ))
        } else {
            throw Exception("Parse error: ${line}")
        }
    }

    builder.finalizeShift()
    return builder.shifts
}

fun puzzleSolution(shifts: List<Shift>): Int {
    return -1
}