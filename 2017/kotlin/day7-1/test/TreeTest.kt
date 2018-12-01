import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.assertFails

class TreeTest {
	@Test
	fun testBuildTree_parentFirst() {
		val input = listOf(
				NodeSpec("a", listOf("b")),
				NodeSpec("b", emptyList())
		)
		val expected = Node("a", setOf(Node("b")))
		assertEquals(expected, buildTree(input))
	}

	@Test
	fun testBuildTree_childFirst() {
		val input = listOf(
				NodeSpec("b", emptyList()),
				NodeSpec("a", listOf("b"))
		)
		val expected = Node("a", setOf(Node("b")))
		assertEquals(expected, buildTree(input))
	}

	@Test
	fun testBuildTree_grandchildren() {
		val input = listOf(
				NodeSpec("c", emptyList()),
				NodeSpec("a", listOf("b")),
				NodeSpec("b", listOf("c"))
		)
		val expected = Node(
				"a",
				setOf(
						Node(
								"b",
								setOf(
										Node("c")
								)
						)
				)
		)
		assertEquals(expected, buildTree(input))
	}

	@Test(timeout = 1000)
	fun testBuildTree_bogus() {
		val input = listOf(
				NodeSpec("a", emptyList()),
				NodeSpec("b", emptyList())
		)
		assertFails("Found more than one root", { buildTree(input) })
	}

	@Test
	fun testBuildTree_example() {
		val input = parseInput("""pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)""")

		val expected = Node(
				"tknk",
				setOf(
						Node(
								"ugml",
								setOf(
										Node("gyxo"),
										Node("ebii"),
										Node("jptl")
								)
						),
						Node(
								"padx",
								setOf(
										Node("pbga"),
										Node("havc"),
										Node("qoyq")

								)
						),
						Node(
								"fwft",
								setOf(
										Node("ktlj"),
										Node("cntj"),
										Node("xhth")

								)
						)
				)
		)

		assertEquals(expected, buildTree(input))
	}
}