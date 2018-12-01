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
    let target: String
    
    static func ==(lhs: Problem, rhs: Problem) -> Bool {
        return lhs.config == rhs.config && lhs.target == rhs.target
    }
}

func parseInput(_ lines: [String]) -> Problem {
    var config: [Replacement] = []
    var foundSeparator = false
    
    for line in lines {
        if line == "" {
            foundSeparator = true
        } else if foundSeparator {
            return Problem(config: config, target: line)
        } else {
            config.append(parseReplacement(line))
        }
    }
    
    return Problem(config: config, target: "")
}

func parseReplacement(_ line: String) -> Replacement {
    let tokens = line.components(separatedBy: " => ")
    return Replacement(input: tokens[0], output: tokens[1])
}

func iterationsToGenerate(_ target: String, config: [Replacement], input: String) -> Int? {
    var steps = 0
    var current = target
    
    while current != input {
        guard let s = replaceFirst(of: config, inString: current) else {
            return nil
        }
        
        current = s
		steps += 1
    }
    
    return steps
}

func replaceFirst(of: [Replacement], inString: String) -> String? {
    let haystack = inString
    let needles = of
	
	for needle in needles {
		if let match = haystack.range(of: needle.output) {
			return haystack.replacingCharacters(in: match, with: needle.input)
		}
	}
	
	return nil
}
