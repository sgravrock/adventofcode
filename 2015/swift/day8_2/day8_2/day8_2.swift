func delta(lines: [String]) -> Int {
	return lines.map { delta($0) }.reduce(0, +)
}

func delta(_ unescaped: String) -> Int {
	return escape(unescaped).characters.count - unescaped.characters.count
}

func escape(_ unescaped: String) -> String {
	return "\"\(escapeQuotes(escapeBackslashes(unescaped)))\""
}

func escapeQuotes(_ unescaped: String) -> String {
	return unescaped.replacingOccurrences(of: "\"", with: "\\\"")
}

func escapeBackslashes(_ unescaped: String) -> String {
	return unescaped.replacingOccurrences(of: "\\", with: "\\\\")
}
