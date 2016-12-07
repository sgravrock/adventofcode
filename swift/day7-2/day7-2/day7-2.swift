import Foundation

typealias Triplet = (outer: Character, inner: Character, bracketed: Bool)

func supports_ssl(_ ip: String) -> Bool {
	var triplets = find_triplets(ip)
	let pivot = triplets.partition { $0.bracketed }
	
	for i in 0..<pivot {
		for j in pivot..<triplets.count {
			if triplets[i].outer == triplets[j].inner && triplets[i].inner == triplets[j].outer {
				return true
			}
		}
	}
	
	return false
}

func find_triplets(_ ip: String) -> [Triplet] {
	var result: [Triplet] = []
	let chars = ip.characters.map { $0 }
	var maybe_in_brackets = false
	var pending: [(outer: Character, inner: Character)] = []
	
	for i in 0..<chars.count {
		if chars[i] == "[" {
			maybe_in_brackets = true
		} else if chars[i] == "]" {
			maybe_in_brackets = false
			result += pending.map { Triplet(outer: $0.outer, inner: $0.inner, bracketed: true) }
			pending = []
		} else if is_aba(chars, at: i) {
			if maybe_in_brackets {
				pending.append((outer: chars[i], inner: chars[i + 1]))
			} else {
				result.append(Triplet(outer: chars[i], inner: chars[i + 1], bracketed: false))
			}
		}
	}
	
	result += pending.map { Triplet(outer: $0.outer, inner: $0.inner, bracketed: false) }
	return result
}

func is_aba(_ chars: [Character], at: Int) -> Bool {
	return at + 2 < chars.count &&
		chars[at] == chars[at + 2] &&
		chars[at] != chars[at + 1]
}
