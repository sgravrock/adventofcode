import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.assertFails

class NodeBuilderTest {
	val arbitraryWeight = 0

	@Test
	fun testBuildTree_parentFirst() {
		val input = listOf(
				NodeSpec("a", arbitraryWeight, listOf("b")),
				NodeSpec("b", arbitraryWeight, emptyList())
		)
		val expected = Node("a", arbitraryWeight, setOf(
				Node("b", arbitraryWeight)
		))
		assertEquals(expected, NodeBuilder.buildTree(input))
	}

	@Test
	fun testBuildTree_childFirst() {
		val input = listOf(
				NodeSpec("b", arbitraryWeight, emptyList()),
				NodeSpec("a", arbitraryWeight, listOf("b"))
		)
		val expected = Node("a", arbitraryWeight, setOf(
				Node("b", arbitraryWeight)
		))
		assertEquals(expected, NodeBuilder.buildTree(input))
	}

	@Test
	fun testBuildTree_grandchildren() {
		val input = listOf(
				NodeSpec("c", arbitraryWeight, emptyList()),
				NodeSpec("a", arbitraryWeight, listOf("b")),
				NodeSpec("b", arbitraryWeight, listOf("c"))
		)
		val expected = Node(
				"a",
				arbitraryWeight,
				setOf(
						Node(
								"b",
								arbitraryWeight,
								setOf(
										Node("c", arbitraryWeight)
								)
						)
				)
		)
		assertEquals(expected, NodeBuilder.buildTree(input))
	}

	@Test(timeout = 1000)
	fun testBuildTree_bogus() {
		val input = listOf(
				NodeSpec("a", arbitraryWeight, emptyList()),
				NodeSpec("b", arbitraryWeight, emptyList())
		)
		assertFails("Found more than one root",
				{ NodeBuilder.buildTree(input) })
	}

	@Test
	fun testBuildTree_example() {
		val input = parseInput("""pbga ($arbitraryWeight)
xhth ($arbitraryWeight)
ebii ($arbitraryWeight)
havc ($arbitraryWeight)
ktlj ($arbitraryWeight)
fwft ($arbitraryWeight) -> ktlj, cntj, xhth
qoyq ($arbitraryWeight)
padx ($arbitraryWeight) -> pbga, havc, qoyq
tknk ($arbitraryWeight) -> ugml, padx, fwft
jptl ($arbitraryWeight)
ugml ($arbitraryWeight) -> gyxo, ebii, jptl
gyxo ($arbitraryWeight)
cntj ($arbitraryWeight)""")

		val expected = Node(
				"tknk",
				arbitraryWeight,
				setOf(
						Node(
								"ugml",
								arbitraryWeight,
								setOf(
										Node("gyxo", arbitraryWeight),
										Node("ebii", arbitraryWeight),
										Node("jptl", arbitraryWeight)
								)
						),
						Node(
								"padx",
								arbitraryWeight,
								setOf(
										Node("pbga", arbitraryWeight),
										Node("havc", arbitraryWeight),
										Node("qoyq", arbitraryWeight)

								)
						),
						Node(
								"fwft",
								arbitraryWeight,
								setOf(
										Node("ktlj", arbitraryWeight),
										Node("cntj", arbitraryWeight),
										Node("xhth", arbitraryWeight)

								)
						)
				)
		)

		assertEquals(expected, NodeBuilder.buildTree(input))
	}
}