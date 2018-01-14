import org.junit.Assert.*
import org.junit.Test

class NodeTest {
	@Test
	fun testFindBalancingChange_balanced() {
		val tree = Node(
				"ugml",
				68,
				setOf(
						Node("gyxo", 61),
						Node("ebii", 61),
						Node("jptl", 61)
				)
		)
		assertEquals(null, tree.findBalancingChange())
	}

	@Test
	fun testFindBalancingChange_leafMost() {
		val tree = Node("root", 0, setOf(
				Node("a", 1, setOf(
						Node("aa", 1, emptySet()),
						Node("ab", 2, emptySet())
				)),
				Node("b", 5, emptySet())
		))
		assertEquals(
				BalancingChange("aa", 2),
				tree.findBalancingChange()
		)
	}

	@Test
	fun testFindBalancingChange_example() {
		val tree = Node(
				"tknk",
				41,
				setOf(
						Node(
								"ugml",
								68,
								setOf(
										Node("gyxo", 61),
										Node("ebii", 61),
										Node("jptl", 61)
								)
						),
						Node(
								"padx",
								45,
								setOf(
										Node("pbga", 66),
										Node("havc", 66),
										Node("qoyq", 66)

								)
						),
						Node(
								"fwft",
								72,
								setOf(
										Node("ktlj", 57),
										Node("cntj", 57),
										Node("xhth", 57)

								)
						)
				)
		)
		assertEquals(
				BalancingChange("ugml", 60),
				tree.findBalancingChange()
		)
	}

}