data class Node(
		val name: String,
		val ownWeight: Int,
		val children: Set<Node> = emptySet()
) {
	fun totalWeight(): Int {
		return ownWeight + childWeights()
	}

	fun findBalancingChange(): BalancingChange? {
		for (node in children) {
			val c = node.findBalancingChange()

			if (c != null) {
				return c
			}
		}

		val groups = groupByWeight(children).toList()

		if (groups.size > 2) {
			throw Exception("Can't balance")
		} else if (groups.size < 2) {
			return null
		} else {
			val a = groups[0]
			val b = groups[1]

			if (a.size == 1) {
				return makeBalancingChange(a[0], b[0])
			} else if (b.size == 1) {
				return makeBalancingChange(b[0], a[0])
			} else {
				throw Exception("Can't balance")
			}
		}
	}

	private fun makeBalancingChange(nodeToChange: Node, example: Node): BalancingChange {
		return BalancingChange(
				nodeToChange.name,
				example.totalWeight() - nodeToChange.childWeights()
		)
	}

	private fun groupByWeight(nodes: Iterable<Node>): List<List<Node>> {
		val result: MutableMap<Int, MutableList<Node>> = mutableMapOf()

		for (node in nodes) {
			val weight = node.totalWeight()
			val group = result.get(weight)

			if (group == null) {
				result.put(weight, mutableListOf(node))
			} else {
				group.add(node)
			}
		}

		return result.values.toList()
	}

	private fun childWeights(): Int {
		return children.map { it.totalWeight() }.sum()
	}
}

data class BalancingChange(
		val nodeName: String,
		val weight: Int
)