import kotlin.math.abs
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val millis = measureTimeMillis {
        val classLoader = Nanobot::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        val nanobots = Nanobot.parseMany(input)
        println(Nanobot.inRangeOfStrongest(nanobots).size)
    }
    println("in ${millis}ms")
}

data class Coord(val x: Int, val y: Int, val z: Int) {
    fun distanceFrom(other: Coord): Int {
        return abs(x - other.x) + abs(y - other.y) + abs(z - other.z)
    }

    override fun toString(): String {
        return "<$x,$y,$z>"
    }
}

data class Nanobot(val pos: Coord, val range: Int) {
    override fun toString(): String {
        return "pos=$pos, r=$range"
    }

    companion object {
        fun inRangeOfStrongest(nanobots: List<Nanobot>): List<Nanobot> {
            val strongest = nanobots.maxBy { it.range }!!
            return nanobots.filter {
                it.pos.distanceFrom(strongest.pos) <= strongest.range
            }
        }

        fun parseMany(input: String): List<Nanobot> {
            return input.lines().map { parseOne(it) }
        }

        fun parseOne(input: String): Nanobot {
            val match = re.matchEntire(input)
                ?: throw Exception("Parse error at $input")

            return Nanobot(
                pos = Coord(
                    x = match.groupValues[1].toInt(),
                    y = match.groupValues[2].toInt(),
                    z = match.groupValues[3].toInt()
                ),
                range = match.groupValues[4].toInt()
            )
        }

        private val re = Regex("^pos=<([0-9-]+),([0-9-]+),([0-9-]+)>, r=([0-9-]+)$")
    }
}