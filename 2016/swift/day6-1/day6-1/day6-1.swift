import Foundation


func find_message(_ input: [String]) -> String {
	let cols = to_columns(input)
	let chars = cols.map(most_frequent)
	return String(chars)
}

func to_columns(_ input: [String]) -> [[Character]] {
	return (0..<input[0].characters.count).map({ (i: Int) -> [Character] in
		return input.map({ (s: String) -> Character in
			return s[s.index(s.startIndex, offsetBy: i)]
		})
	})
}

func most_frequent(_ chars: [Character]) -> Character {
	let freqs = to_frequency(chars)
	let sorted = freqs.map({ $0 })
		.sorted(by: { (a: (key: Character, value: Int), b: (key: Character, value: Int)) -> Bool in
			return a.value > b.value || (a.value == b.value && a.key < b.key)
		})
		.map { $0.key }
	return sorted[0]
}

func to_frequency(_ input: [Character]) -> [Character:Int] {
	var freqs: [Character:Int] = [:]
	
	for c in input {
		if c == "-" {
			continue
		}
		
		if freqs[c] == nil {
			freqs[c] = 1
		} else {
			freqs[c]! += 1
		}
	}
	
	return freqs
}
