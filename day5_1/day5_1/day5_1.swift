import Foundation

func numNice(lines: [String]) -> Int {
	return lines.filter(isNice).count
}

func isNice(_ s: String) -> Bool {
	return hasDoubles(s) && !hasBanned(s) && hasEnoughVowels(s)
}

func hasDoubles(_ s: String) -> Bool {
	let match = s.range(of: "(.)\\1", options: .regularExpression)
	return match != nil
}

func hasBanned(_ s: String) -> Bool {
	let banned = ["ab", "cd", "pq", "xy"]
	return banned.filter({ s.range(of: $0) != nil }).count > 0
}

func hasEnoughVowels(_ s: String) -> Bool {
	let match = s.range(of: "([aeiou].*){3}", options: .regularExpression)
	return match != nil
}
