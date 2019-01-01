import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testProgramParse() {
        val input = """
            #ip 4
            addi 4 16 4
            seti 1 3 5
        """.trimIndent()
        val expected = Program(
                ipReg = 4,
                instructions = listOf(
                        Instruction(Opcode.addi, 4, 16, 4),
                        Instruction(Opcode.seti, 1, 3, 5)
                )
        )
        assertEquals(expected, Program.parse(input))
    }

    @Test
    fun testMachineExecute() {
        val input = """
            #ip 0
            seti 5 0 1
            seti 6 0 2
            addi 0 1 0
            addr 1 2 3
            setr 1 0 0
            seti 8 0 4
            seti 9 0 5
        """.trimIndent()
        val subject = Machine()
        subject.execute(Program.parse(input))
        assertEquals(listOf(7, 5, 6, 0, 0, 9), subject.registers)
    }

    @Test
    fun testAddr() {
        assertEquals(
                listOf(0, 2, 1, 3, 0, 0),
                evaluate(Instruction(Opcode.addr, 2, 1, 3), listOf(0, 2, 1, 0))
        )
    }

    @Test
    fun testAddi() {
        assertEquals(
                listOf(0, 0, 1, 4, 0, 0),
                evaluate(Instruction(Opcode.addi, 2, 3, 3), listOf(0, 0, 1, 0))
        )
    }

    @Test
    fun testMulr() {
        assertEquals(
                listOf(0, 2, 1, 2, 0, 0),
                evaluate(Instruction(Opcode.mulr, 2, 1, 3), listOf(0, 2, 1, 0))
        )
    }

    @Test
    fun testMuli() {
        assertEquals(
                listOf(0, 0, 1, 3, 0, 0),
                evaluate(Instruction(Opcode.muli, 2, 3, 3), listOf(0, 0, 1, 0))
        )
    }

    @Test
    fun testBanr() {
        assertEquals(
                listOf(0, 1, 3, 1, 0, 0),
                evaluate(Instruction(Opcode.banr, 2, 1, 3), listOf(0, 1, 3, 0))
        )
    }

    @Test
    fun testBani() {
        assertEquals(
                listOf(0, 0, 3, 1, 0, 0),
                evaluate(Instruction(Opcode.bani, 2, 1, 3), listOf(0, 0, 3, 0))
        )
    }

    @Test
    fun testBorr() {
        assertEquals(
                listOf(0, 1, 2, 3, 0, 0),
                evaluate(Instruction(Opcode.borr, 2, 1, 3), listOf(0, 1, 2, 0))
        )
    }


    @Test
    fun testBori() {
        assertEquals(
                listOf(0, 0, 2, 3, 0, 0),
                evaluate(Instruction(Opcode.bori, 2, 1, 3), listOf(0, 0, 2, 0))
        )
    }

    @Test
    fun testSetr() {
        assertEquals(
                listOf(0, 0, 1, 1, 0, 0),
                evaluate(Instruction(Opcode.setr, 2, -1, 3), listOf(0, 0, 1, 0))
        )
    }

    @Test
    fun testSeti() {
        assertEquals(
                listOf(0, 0, 0, 2, 0, 0),
                evaluate(Instruction(Opcode.seti, 2, -1, 3), listOf(0, 0, 0, 0))
        )
    }

    @Test
    fun testGtir() {
        assertEquals(
                listOf(0, 1, 0, 1, 0, 0),
                evaluate(Instruction(Opcode.gtir, 2, 1, 3), listOf(0, 1, 0, 0))
        )
        assertEquals(
                listOf(0, 1, 0, 0, 0, 0),
                evaluate(Instruction(Opcode.gtir, 1, 1, 3), listOf(0, 1, 0, 0))
        )
    }

    @Test
    fun testGtri() {
        assertEquals(
                listOf(0, 2, 0, 1, 0, 0),
                evaluate(Instruction(Opcode.gtri, 1, 1, 3), listOf(0, 2, 0, 0))
        )
        assertEquals(
                listOf(0, 1, 0, 0, 0, 0),
                evaluate(Instruction(Opcode.gtri, 1, 1, 3), listOf(0, 1, 0, 0))
        )
    }

    @Test
    fun testGtrr() {
        assertEquals(
                listOf(0, 2, 1, 1, 0, 0),
                evaluate(Instruction(Opcode.gtrr, 1, 2, 3), listOf(0, 2, 1, 0))
        )
        assertEquals(
                listOf(0, 1, 2, 0, 0, 0),
                evaluate(Instruction(Opcode.gtrr, 1, 2, 3), listOf(0, 1, 2, 0))
        )
    }

    @Test
    fun testEqir() {
        assertEquals(
                listOf(0, 0, 1, 1, 0, 0),
                evaluate(Instruction(Opcode.eqir, 1, 2, 3), listOf(0, 0, 1, 0))
        )
        assertEquals(
                listOf(0, 0, 1, 0, 0, 0),
                evaluate(Instruction(Opcode.eqir, 2, 2, 3), listOf(0, 0, 1, 0))
        )
    }

    @Test
    fun testEqri() {
        assertEquals(
                listOf(0, 0, 1, 1, 0, 0),
                evaluate(Instruction(Opcode.eqri, 2, 1, 3), listOf(0, 0, 1, 0))
        )
        assertEquals(
                listOf(0, 0, 1, 0, 0, 0),
                evaluate(Instruction(Opcode.eqri, 2, 2, 3), listOf(0, 0, 1, 0))
        )
    }

    @Test
    fun testEqrr() {
        assertEquals(
                listOf(0, 1, 1, 1, 0, 0),
                evaluate(Instruction(Opcode.eqrr, 2, 1, 3), listOf(0, 1, 1, 0))
        )
        assertEquals(
                listOf(0, 0, 1, 0, 0, 0),
                evaluate(Instruction(Opcode.eqrr, 2, 1, 3), listOf(0, 0, 1, 0))
        )
    }

    fun evaluate(
            instruction: Instruction,
            regsBefore: List<Int>
    ): List<Int> {
        val subject = Machine()
        regsBefore.indices.forEach { i -> subject.registers[i] = regsBefore[i] }
        subject.evaluate(instruction)
        return subject.registers
    }
}