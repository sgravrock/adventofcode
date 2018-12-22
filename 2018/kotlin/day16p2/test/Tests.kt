import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testBehavesLike() {
        val input = """Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]"""
        val subject = InstructionHint.parse(input)[0]
        assertEquals(
                listOf(Opcode.addi, Opcode.mulr, Opcode.seti),
                subject.behavesLike()
        )
    }

    @Test
    fun testFindOpcodes_tmp() {
        val input = """Before: [0, 2, 1, 1]
4 3 0 3
After:  [0, 2, 1, 3]
Before: [3, 2, 5, 1]
9 1 2 2
After:  [3, 2, 10, 1]"""
        val instructions = InstructionHint.parse(input)
        assertEquals(
                mapOf(4 to Opcode.seti, 9 to Opcode.mulr),
                findOpcodes(instructions)
        )
    }


    @Test
    fun testFindOpcodes() {
        val input = """Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
Before: [0, 2, 1, 1]
4 3 0 3
After:  [0, 2, 1, 3]
Before: [3, 2, 5, 1]
9 1 2 2
After:  [3, 2, 10, 1]"""
        val instructions = InstructionHint.parse(input)
        assertEquals(
                mapOf(4 to Opcode.seti, 9 to Opcode.mulr),
                findOpcodes(instructions)
        )
    }

    @Test
    fun testInstructionHintParse() {
        val input = """Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

Before: [1, 2, 1, 1]
1 2 1 2
After:  [1, 2, 2, 1]
"""
        val expected = listOf(
                InstructionHint(
                        input = listOf(3, 2, 1, 1),
                        codes = listOf(9, 2, 1, 2),
                        output = listOf(3, 2, 2, 1)
                ),
                InstructionHint(
                        input = listOf(1, 2, 1, 1),
                        codes = listOf(1, 2, 1, 2),
                        output = listOf(1, 2, 2, 1)
                )
        )
        assertEquals(expected, InstructionHint.parse(input))
    }

    @Test
    fun testParsePuzzleInput() {
        val input = """Before: [0, 2, 0, 2]
5 0 3 1
After:  [0, 0, 0, 2]

Before: [0, 3, 2, 1]
5 0 2 2
After:  [0, 3, 0, 1]



5 2 0 2
10 2 0 2
"""
        val expected = PuzzleInput(
                hints = listOf(
                        InstructionHint(
                                input = listOf(0, 2, 0, 2),
                                codes = listOf(5, 0, 3, 1),
                                output = listOf(0, 0, 0, 2)
                        ),
                        InstructionHint(
                                input = listOf(0, 3, 2, 1),
                                codes = listOf(5, 0, 2, 2),
                                output = listOf(0, 3, 0, 1)
                        )
                ),
                program = listOf(
                        listOf(5, 2, 0, 2),
                        listOf(10, 2, 0, 2)
                )
        )
        assertEquals(expected, PuzzleInput.parse(input))
    }

    @Test
    fun testAddr() {
        assertEquals(
                listOf(0, 2, 1, 3),
                evaluate(Opcode.addr, listOf(0, 2, 1, 0), 2, 1, 3)
        )
    }

    @Test
    fun testAddi() {
        assertEquals(
                listOf(0, 0, 1, 4),
                evaluate(Opcode.addi, listOf(0, 0, 1, 0), 2, 3, 3)
        )
    }

    @Test
    fun testMulr() {
        assertEquals(
                listOf(0, 2, 1, 2),
                evaluate(Opcode.mulr, listOf(0, 2, 1, 0), 2, 1, 3)
        )
    }

    @Test
    fun testMuli() {
        assertEquals(
                listOf(0, 0, 1, 3),
                evaluate(Opcode.muli, listOf(0, 0, 1, 0), 2, 3, 3)
        )
    }

    @Test
    fun testBanr() {
        assertEquals(
                listOf(0, 1, 3, 1),
                evaluate(Opcode.banr, listOf(0, 1, 3, 0), 2, 1, 3)
        )
    }

    @Test
    fun testBani() {
        assertEquals(
                listOf(0, 0, 3, 1),
                evaluate(Opcode.bani, listOf(0, 0, 3, 0), 2, 1, 3)
        )
    }

    @Test
    fun testBorr() {
        assertEquals(
                listOf(0, 1, 2, 3),
                evaluate(Opcode.borr, listOf(0, 1, 2, 0), 2, 1, 3)
        )
    }


    @Test
    fun testBori() {
        assertEquals(
                listOf(0, 0, 2, 3),
                evaluate(Opcode.bori, listOf(0, 0, 2, 0), 2, 1, 3)
        )
    }

    @Test
    fun testSetr() {
        assertEquals(
                listOf(0, 0, 1, 1),
                evaluate(Opcode.setr, listOf(0, 0, 1, 0), 2, -1, 3)
        )
    }

    @Test
    fun testSeti() {
        assertEquals(
                listOf(0, 0, 0, 2),
                evaluate(Opcode.seti, listOf(0, 0, 0, 0), 2, -1, 3)
        )
    }

    @Test
    fun testGtir() {
        assertEquals(
                listOf(0, 1, 0, 1),
                evaluate(Opcode.gtir, listOf(0, 1, 0, 0), 2, 1, 3)
        )
        assertEquals(
                listOf(0, 1, 0, 0),
                evaluate(Opcode.gtir, listOf(0, 1, 0, 0), 1, 1, 3)
        )
    }

    @Test
    fun testGtri() {
        assertEquals(
                listOf(0, 2, 0, 1),
                evaluate(Opcode.gtri, listOf(0, 2, 0, 0), 1, 1, 3)
        )
        assertEquals(
                listOf(0, 1, 0, 0),
                evaluate(Opcode.gtri, listOf(0, 1, 0, 0), 1, 1, 3)
        )
    }

    @Test
    fun testGtrr() {
        assertEquals(
                listOf(0, 2, 1, 1),
                evaluate(Opcode.gtrr, listOf(0, 2, 1, 0), 1, 2, 3)
        )
        assertEquals(
                listOf(0, 1, 2, 0),
                evaluate(Opcode.gtrr, listOf(0, 1, 2, 0), 1, 2, 3)
        )
    }

    @Test
    fun testEqir() {
        assertEquals(
                listOf(0, 0, 1, 1),
                evaluate(Opcode.eqir, listOf(0, 0, 1, 0), 1, 2, 3)
        )
        assertEquals(
                listOf(0, 0, 1, 0),
                evaluate(Opcode.eqir, listOf(0, 0, 1, 0), 2, 2, 3)
        )
    }

    @Test
    fun testEqri() {
        assertEquals(
                listOf(0, 0, 1, 1),
                evaluate(Opcode.eqri, listOf(0, 0, 1, 0), 2, 1, 3)
        )
        assertEquals(
                listOf(0, 0, 1, 0),
                evaluate(Opcode.eqri, listOf(0, 0, 1, 0), 2, 2, 3)
        )
    }

    @Test
    fun testEqrr() {
        assertEquals(
                listOf(0, 1, 1, 1),
                evaluate(Opcode.eqrr, listOf(0, 1, 1, 0), 2, 1, 3)
        )
        assertEquals(
                listOf(0, 0, 1, 0),
                evaluate(Opcode.eqrr, listOf(0, 0, 1, 0), 2, 1, 3)
        )
    }
}