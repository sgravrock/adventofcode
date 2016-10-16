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
	var output = escaped
	var offset = 0
	let re = try NSRegularExpression(pattern: "\\\\x[0-9a-f]{2}", options: .caseInsensitive)
	let matches = re.matches(in: escaped, options: [], range: NSMakeRange(0, escaped.characters.count))
	
	for match in matches {
		// OMGWTF... there has to be a better way than this.
		let inputRange = escaped.index(escaped.startIndex, offsetBy: match.range.location + 2)..<escaped.index(escaped.startIndex, offsetBy: match.range.location + 4)
		let outputRange = output.index(output.startIndex, offsetBy:match.range.location + offset)..<output.index(output.startIndex, offsetBy:match.range.location + offset + match.range.length)
		let replacement = hexToString(escaped.substring(with: inputRange))
		output = output.replacingCharacters(in: outputRange, with: replacement)
		offset += replacement.characters.count - match.range.length
	}
	
	return output
}

func hexToString(_ hex: String) -> String {
	let n = UInt8(strtoul(hex, nil, 16))
	return String(UnicodeScalar(n))
}

