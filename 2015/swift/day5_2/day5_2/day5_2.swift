func numNice(lines: [String]) -> Int {
	return lines.filter(isNice).count
}

func isNice(_ s: String) -> Bool {
	return hasTriplet(s) && hasRepeatingPair(s)
}

func hasTriplet(_ s: String) -> Bool {
	let match = s.range(of: "(.).\\1", options: .regularExpression)
	return match != nil
}

func hasRepeatingPair(_ s: String) -> Bool {
	let match = s.range(of: "(..).*\\1", options: .regularExpression)
	return match != nil
}
