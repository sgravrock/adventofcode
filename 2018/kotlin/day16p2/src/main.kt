import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = Opcode::class.java.classLoader
    val puzzle = PuzzleInput.parse(classLoader.getResource("input.txt").readText())
    val opcodeMap = findOpcodes(puzzle.hints)
    val result = execute(puzzle.program, opcodeMap)
    println(result[0])
    println("in ${Date().time - start.time}ms")
}

fun findOpcodes(instructionHints: List<InstructionHint>): Map<Int, Opcode> {
    val found = mutableMapOf<Int, Opcode>()
    val pending = instructionHints.toMutableList()

    while (pending.size > 0) {
        for (ins in pending) {
            val candidates = ins.behavesLike().filter { !found.containsValue(it) }
            assert(candidates.size > 0)

            if (candidates.size == 1) {
                found[ins.codes[0]] = candidates[0]
            }
        }

        if (!pending.removeIf { found.containsKey(it.codes[0]) }) {
            throw Error("Stalled at ${found.size} found")
        }
    }

    return found
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
        Opcode.banr -> registers[i1] and registers[i2]
        Opcode.bani -> registers[i1] and i2
        Opcode.borr -> registers[i1] or registers[i2]
        Opcode.bori -> registers[i1] or i2
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

data class InstructionHint(
        val input: List<Int>,
        val codes: List<Int>,
        val output: List<Int>
) {
    fun behavesLike(): List<Opcode> {
        return Opcode.values().filter { opcode ->
            evaluate(opcode, input, codes[1], codes[2], codes[3]) == output
        }
    }

    companion object {
        fun parse(input: String): List<InstructionHint> {
            return input.lines()
                .filter { it != "" }
                .chunked(3)
                .map { chunk ->
                    InstructionHint(
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

data class PuzzleInput(val hints: List<InstructionHint>, val program: List<List<Int>>) {
    companion object {
        fun parse(input: String): PuzzleInput {
            val parts = input.split("\n\n\n\n")
            assert(parts.size == 2)
            return PuzzleInput(
                    InstructionHint.parse(parts[0]),
                    parts[1].lines()
                            .filter { line -> line != "" }
                            .map { line -> line.split(" ").map { it.toInt()} }
            )

        }
    }
}

fun execute(program: List<List<Int>>, opcodeMap: Map<Int, Opcode>): List<Int> {
    var registers = listOf(0, 0, 0, 0)

    for (instruction in program) {
        val opcode = opcodeMap[instruction[0]]!!
        registers = evaluate(opcode, registers,
                instruction[1], instruction[2], instruction[3]
        )
    }

    return registers
}