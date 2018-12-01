class NodeBuilder(
		val spec: NodeSpec,
		val children: MutableList<NodeBuilder>
) {
	fun build(): Node {
		return Node(spec.name, spec.weight, children.map { it.build() }.toSet())
	}

	companion object {
		fun fromSpec(ns: NodeSpec): NodeBuilder {
			return NodeBuilder(ns, mutableListOf())
		}

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

		private fun emplace(
				toEmplace: NodeBuilder,
				roots: MutableList<NodeBuilder>
		): Boolean {
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

		private fun emplaceOne(toEmplace: NodeBuilder, root: NodeBuilder): Boolean {
			if (root.spec.childNames.contains(toEmplace.spec.name)) {
				root.children.add(toEmplace)
				return true
			}

			return emplace(toEmplace, root.children)
		}
	}
}