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
        registers[ins.o] = when (ins.opcode) {
            Opcode.addr -> registers[ins.i1] + registers[ins.i2]
            Opcode.addi -> registers[ins.i1] + ins.i2
            Opcode.mulr -> registers[ins.i1] * registers[ins.i2]
            Opcode.muli -> registers[ins.i1] * ins.i2
            Opcode.banr -> registers[ins.i1] and registers[ins.i2]
            Opcode.bani -> registers[ins.i1] and ins.i2
            Opcode.borr -> registers[ins.i1] or registers[ins.i2]
            Opcode.bori -> registers[ins.i1] or ins.i2
            Opcode.setr -> registers[ins.i1]
            Opcode.seti -> ins.i1
            Opcode.gtir -> if (ins.i1 > registers[ins.i2]) 1 else 0
            Opcode.gtri -> if (registers[ins.i1] > ins.i2) 1 else 0
            Opcode.gtrr -> if (registers[ins.i1] > registers[ins.i2]) 1 else 0
            Opcode.eqir -> if (ins.i1 == registers[ins.i2]) 1 else 0
            Opcode.eqri -> if (registers[ins.i1] == ins.i2) 1 else 0
            Opcode.eqrr -> if (registers[ins.i1] == registers[ins.i2]) 1 else 0
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
                    m.groupValues[2].toInt(),
                    m.groupValues[3].toInt(),
                    m.groupValues[4].toInt()
            )
        }
    }
}

data class Instruction(val opcode: Opcode, val i1: Int, val i2: Int, val o: Int)

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