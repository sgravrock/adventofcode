func lookAndSay(_ input: String) -> String {
	return findGroups(input).map(say).reduce("", +)
}

func lookAndSay(_ input: String, times: Int) -> String {
	var s = input
	
	for _ in 1...times {
		s = lookAndSay(s)
	}
	
	return s
}

typealias Group = (c: Character, n: Int)

func findGroups(_ input: String) -> [String] {
	var result: [String] = []
	var current = ""
	
	for c in input.characters {
		if current == "" || current.characters.first == c {
			current.append(c)
		} else {
			result.append(current)
			current = "\(c)"
		}
	}
	
	if current != "" {
		result.append(current)
	}
	
	return result
}

func say(_ group: String) -> String {
	return "\(group.characters.count)\(group.characters.first!)"
}
