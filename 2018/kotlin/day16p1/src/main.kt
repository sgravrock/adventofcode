fun main(args: Array<String>) {
    val classLoader = Opcode::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    val instructions = Instruction.parse(input)
    val matches = instructions.filter { it.behavesLike().size >= 3 }
    println(matches.size)
}

enum class Opcode {
    addr,
    addi,
    mulr,
    muli,
    banr,
    bani,
    borr,
    bori,
    setr,
    seti,
    gtir,
    gtri,
    gtrr,
    eqir,
    eqri,
    eqrr
}

fun evaluate(opcode: Opcode, registers: List<Int>, i1: Int, i2: Int, o: Int): List<Int> {
    val result = registers.toMutableList()
    result[o] = when (opcode) {
        Opcode.addr -> registers[i1] + registers[i2]
        Opcode.addi -> registers[i1] + i2
        Opcode.mulr -> registers[i1] * registers[i2]
        Opcode.muli -> registers[i1] * i2
        Opcode.banr -> (registers[i1].toUInt() and registers[i2].toUInt()).toInt()
        Opcode.bani -> (registers[i1].toUInt() and i2.toUInt()).toInt()
        Opcode.borr -> (registers[i1].toUInt() or registers[i2].toUInt()).toInt()
        Opcode.bori -> (registers[i1].toUInt() or i2.toUInt()).toInt()
        Opcode.setr -> registers[i1]
        Opcode.seti -> i1
        Opcode.gtir -> if (i1 > registers[i2]) 1 else 0
        Opcode.gtri -> if (registers[i1] > i2) 1 else 0
        Opcode.gtrr -> if (registers[i1] > registers[i2]) 1 else 0
        Opcode.eqir -> if (i1 == registers[i2]) 1 else 0
        Opcode.eqri -> if (registers[i1] == i2) 1 else 0
        Opcode.eqrr -> if (registers[i1] == registers[i2]) 1 else 0
    }

    return result
}

data class Instruction(val input: List<Int>, val codes: List<Int>, val output: List<Int>) {
    fun behavesLike(): List<Opcode> {
        return Opcode.values().filter { opcode ->
            evaluate(opcode, input, codes[1], codes[2], codes[3]) == output
        }
    }

    companion object {
        fun parse(input: String): List<Instruction> {
            return input.lines()
                .filter { it != "" }
                .chunked(3)
                .map { chunk ->
                    Instruction(
                        input = extractList(chunk[0]),
                        codes = extractList(chunk[1]),
                        output = extractList(chunk[2])
                    )
                }
        }
    }
}

fun extractList(s: String): List<Int> {
    return s
        .replace(Regex("[^\\d ]"), "")
        .replace(Regex("^ *"), "")
        .split(" ")
        .map { it.toInt() }
}