import Foundation
import Darwin

func delta(_ escaped: String) throws -> Int {
	return try escaped.characters.count - unescape(escaped).characters.count
}

func delta(lines: [String]) throws -> Int {
	return try lines.map { try delta($0) }.reduce(0, +)
}

func unescape(_ escaped: String) throws -> String {
	return try expandHexEscapes(simpleUnescapes(stripEndQuotes(escaped)))
}

func stripEndQuotes(_ escaped: String) -> String {
	return escaped.replacingOccurrences(of: "(^\")|(\"$)",
	                                    with: "",
	                                    options: .regularExpression)
}

func simpleUnescapes(_ escaped: String) -> String {
	return escaped
		.replacingOccurrences(of: "\\\"", with: "\"")
		.replacingOccurrences(of: "\\\\", with: "\\")
}

func expandHexEscapes(_ escaped: String) throws -> String {
	return try regexMap(input: escaped,
	                pattern: "\\\\x[0-9a-f]{2}",
	                options: .caseInsensitive,
	                convertedBy: { (m) -> String in
						let ss = m.substring(from: m.index(m.startIndex, offsetBy: 2))
						return hexToString(ss)
					})
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

func hexToString(_ hex: String) -> String {
	let n = UInt8(strtoul(hex, nil, 16))
	return String(UnicodeScalar(n))
}

