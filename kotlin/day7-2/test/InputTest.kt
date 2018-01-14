import org.junit.Test
import kotlin.test.assertEquals

class InputTest {
	@Test
	fun testParseInput() {
		val input = """fwft (72) -> ktlj, cntj, xhth
qoyq (66)
cntj (57)
"""
		val expected = listOf(
				NodeSpec("fwft", 72, listOf("ktlj", "cntj", "xhth")),
				NodeSpec("qoyq", 66, emptyList()),
				NodeSpec("cntj", 57, emptyList())

		)
		assertEquals(expected, parseInput(input))
	}
}