import java.lang.Exception
import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = Opcode::class.java.classLoader
    val input = classLoader.getResource("input.txt").readText()
    println("in ${Date().time - start.time}ms")
}


data class Program(val ipReg: Int, val instructions: List<Instruction>) {
    companion object {
        private val instructionRe = Regex("^([a-z]+) ([0-9]+) ([0-9]+) ([0-9]+)$")

        fun parse(input: String): Program {
            val lines = input.lines()
            return Program(
                    parseIp(lines[0]),
                    lines.drop(1).map { parseInstruction(it) }
            )
        }

        private fun parseIp(line: String): Int {
            val m = Regex("^#ip ([0-5])$").matchEntire(line)
                    ?: throw Error("Parse error: expected IP assignment but got $line")
            return m.groupValues[1].toInt()
        }

        private fun parseInstruction(line: String): Instruction {
            val m = instructionRe.matchEntire(line)
                    ?: throw Error("Parse error: expected instruction but got $line")
            val opcode = when (m.groupValues[1]) {
                "addr" -> Opcode.addr
                "addi" -> Opcode.addi
                "mulr" -> Opcode.mulr
                "muli" -> Opcode.muli
                "banr" -> Opcode.banr
                "bani" -> Opcode.bani
                "borr" -> Opcode.borr
                "bori" -> Opcode.bori
                "setr" -> Opcode.setr
                "seti" -> Opcode.seti
                "gtir" -> Opcode.gtir
                "gtri" -> Opcode.gtri
                "gtrr" -> Opcode.gtrr
                "eqir" -> Opcode.eqir
                "eqri" -> Opcode.eqri
                "eqrr" -> Opcode.eqrr
                else -> throw Exception("No such opcode: ${m.groupValues[1]}")
            }
            return Instruction(
                    opcode,
                    Triple(
                            m.groupValues[2].toInt(),
                            m.groupValues[3].toInt(),
                            m.groupValues[4].toInt()
                    )
            )
        }
    }
}

data class Instruction(val opcode: Opcode, val operands: Triple<Int, Int, Int>)

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