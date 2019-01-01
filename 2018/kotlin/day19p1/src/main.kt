import java.lang.Exception
import java.util.*

fun main(args: Array<String>) {
    val start = Date()
    val classLoader = Opcode::class.java.classLoader
    val program = Program.parse(classLoader.getResource("input.txt").readText())
    val machine = Machine()
    machine.execute(program)
    println(machine.registers[0])
    println("in ${Date().time - start.time}ms")
}

class Machine() {
    val registers = mutableListOf(0, 0, 0, 0, 0, 0)

    fun execute(program: Program) {
        registers[program.ipReg] = 0

        while (registers[program.ipReg] in program.instructions.indices) {
            evaluate(program.instructions[registers[program.ipReg]])
            registers[program.ipReg]++
        }
    }

    fun evaluate(ins: Instruction) {
        val i1 = ins.operands.first
        val i2 = ins.operands.second
        registers[ins.operands.third] = when (ins.opcode) {
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
    }
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