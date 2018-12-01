fun parseInput(input: String): List<NodeSpec> {
	return input.split('\n')
			.filter { it.length > 0 }
			.map { parseLine(it) }
}

fun parseLine(line: String): NodeSpec {
	val tokens = line.replace(",", "")
			.split(' ')
	val childNames = if (tokens.size > 4) {
		tokens.subList(3, tokens.size)
	} else {
		emptyList()
	}

	return NodeSpec(tokens[0], childNames)
}

data class NodeSpec(
		val name: String,
		val childNames: List<String>
)