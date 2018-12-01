import Foundation

struct Replacement : Equatable {
	var input: String
	var output: String
	
	static func ==(lhs: Replacement, rhs: Replacement) -> Bool {
		return lhs.input == rhs.input && lhs.output == rhs.output
	}
}

struct Problem : Equatable {
	let config: [Replacement]
	let input: String
	
	static func ==(lhs: Problem, rhs: Problem) -> Bool {
		return lhs.config == rhs.config && lhs.input == rhs.input
	}
}

func parseInput(_ lines: [String]) -> Problem {
	var config: [Replacement] = []
	var foundSeparator = false
	
	for line in lines {
		if line == "" {
			foundSeparator = true
		} else if foundSeparator {
			return Problem(config: config, input: line)
		} else {
			config.append(parseReplacement(line))
		}
	}
	
	return Problem(config: config, input: "")
}

func parseReplacement(_ line: String) -> Replacement {
	let tokens = line.components(separatedBy: " => ")
	return Replacement(input: tokens[0], output: tokens[1])
}

func possibleMolecules(_ problem: Problem) throws -> Set<String> {
	var result = Set<String>()
	
	for repl in problem.config {
		try replaceEach(repl, inString: problem.input, into: &result)
	}
	
	return result
}

func replaceEach(_ repl: Replacement, inString: String, into: inout Set<String>) throws {
	let regex = try NSRegularExpression(pattern: repl.input)
	let matches = regex.matches(in: inString,
		range:NSRange(location: 0, length: inString.characters.count))

	for match in matches {
		var copy = inString
		let range = copy.index(copy.startIndex, offsetBy: match.range.location)..<copy.index(copy.startIndex, offsetBy: match.range.location + match.range.length)
		copy.replaceSubrange(range, with: repl.output)
		into.insert(copy)
	}
}

func regexMap(input: String,
              pattern: String,
              options: NSRegularExpression.Options,
              convertedBy: (String) -> String) throws -> String {
	
	var output = input
	var offset = 0
	let re = try NSRegularExpression(pattern: pattern, options: options)
	let matches = re.matches(in: input, options: [], range: NSMakeRange(0, input.characters.count))
	
	for match in matches {
		// OMGWTF... there has to be a better way than this.
		let inputRange = input.index(input.startIndex, offsetBy: match.range.location)..<input.index(input.startIndex, offsetBy: match.range.location + match.range.length)
		let outputRange = output.index(output.startIndex, offsetBy:match.range.location + offset)..<output.index(output.startIndex, offsetBy:match.range.location + offset + match.range.length)
		let replacement = convertedBy(input.substring(with: inputRange))
		output = output.replacingCharacters(in: outputRange, with: replacement)
		offset += replacement.characters.count - match.range.length
	}
	
	return output
}
