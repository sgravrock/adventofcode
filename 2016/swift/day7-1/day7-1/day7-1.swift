import Foundation

func supports_tls(_ ip: String)-> Bool {
	// Regexes in Swift make me sad, and backreferences inside character classes
	// (needed to make this really clean) don't appear to work anyway.
	let chars = ip.characters.map { $0 }
	var maybe_in_brackets = false
	var abba_maybe_in_brackets = false
	var found_abba = false
	
	for i in 0..<chars.count {
		if chars[i] == "[" {
			maybe_in_brackets = true
		} else if chars[i] == "]" {
			if abba_maybe_in_brackets {
				return false // badabba
			}
			
			maybe_in_brackets = false
		} else if is_abba(chars, at: i) {
			found_abba = true
			abba_maybe_in_brackets = maybe_in_brackets
		}
	}
	
	return found_abba
}

func is_abba(_ chars: [Character], at: Int) -> Bool {
	return at + 3 < chars.count &&
		chars[at] == chars[at + 3] &&
		chars[at + 1] == chars[at + 2] &&
		chars[at] != chars[at + 1]
}
