data class Node(
		val name: String,
		val children: Set<Node> = emptySet()
)

fun buildTree(input: List<NodeSpec>): Node {
	val roots = input.map { NodeBuilder.fromSpec(it) }.toMutableList()

	while (roots.size > 1) {
		val toEmplace = roots.removeAt(0)

		if (!emplace(toEmplace, roots)) {
			throw Exception("Found more than one root")
		}
	}

	if (roots.size == 0) {
		throw Exception("Didn't find any roots")
	}

	return roots[0].build()
}

fun emplace(toEmplace: NodeBuilder, roots: MutableList<NodeBuilder>): Boolean {
	for ((i, root) in roots.withIndex()) {
		val newRoot = if (emplaceOne(toEmplace, root)) {
			root
		} else if (emplaceOne(root, toEmplace)) {
			toEmplace
		} else {
			null
		}

		if (newRoot != null) {
			roots[i] = newRoot
			return true
		}
	}

	return false
}

fun emplaceOne(toEmplace: NodeBuilder, root: NodeBuilder): Boolean {
	if (root.spec.childNames.contains(toEmplace.spec.name)) {
		root.children.add(toEmplace)
		return true
	}

	return emplace(toEmplace, root.children)
}

class NodeBuilder(
		val spec: NodeSpec,
		val children: MutableList<NodeBuilder>
) {
	companion object {
		fun fromSpec(ns: NodeSpec): NodeBuilder {
			return NodeBuilder(ns, mutableListOf())
		}
	}

	fun build(): Node {
		return Node(spec.name, children.map { it.build() }.toSet())
	}
}