fun parseInput(input: String): List<NodeSpec> {
	return input.split('\n')
			.filter { it.length > 0 }
			.map { parseLine(it) }
}

fun parseLine(line: String): NodeSpec {
	val tokens = line.replace(",", "")
			.split(' ')
	val weight = tokens[1].replace(Regex("[\\(\\)]"), "").toInt()
	val childNames = if (tokens.size > 4) {
		tokens.subList(3, tokens.size)
	} else {
		emptyList()
	}

	return NodeSpec(tokens[0], weight, childNames)
}

data class NodeSpec(
		val name: String,
		val weight: Int,
		val childNames: List<String>
)