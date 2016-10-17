import Foundation

func isValid(_ s: String) -> Bool {
	return hasStraight(s) && hasMultiplePairs(s) && !hasBadChars(s)
}

func nextValidPassword(_ current: String) -> String? {
    var p: String? = current
    
    while p != nil {
        p = nextPassword(p!)
        
        if let p2 = p, isValid(p2) {
            return p
        }
    }
    
    return nil
}

func allZs(_ s: String) -> Bool {
    return s.range(of: "[^z]", options: .regularExpression) == nil
}

func nextPassword(_ current: String) -> String? {
	var chars = Array(current.characters)
	
	for i in (0..<chars.count).reversed() {
		if chars[i] == "z" {
            if i == 0 {
                return nil
            }
            
			chars[i] = "a"
		} else {
			chars[i] = nextChar(chars[i])
            break
		}
	}
    
    return String(chars)
}

func nextChar(_ c: Character) -> Character {
    return Character(UnicodeScalar(unicodeScalar(ofCharacter: c).value + 1)!)
}

func unicodeScalar(ofCharacter: Character) -> UnicodeScalar {
    return String(ofCharacter).unicodeScalars.first!
}

func hasStraight(_ s: String) -> Bool {
	let scalars = Array(s.unicodeScalars)
	
	for i in 2..<scalars.count {
		let a = scalars[scalars.index(scalars.startIndex, offsetBy: i - 2)]
		let b = scalars[scalars.index(scalars.startIndex, offsetBy: i - 1)]
		let c = scalars[scalars.index(scalars.startIndex, offsetBy: i)]
		
		if a.value == c.value - 2 && b.value == c.value - 1 {
			return true
		}
	}
	
	return false

}

func hasMultiplePairs(_ s: String) -> Bool {
	return s.range(of: "(.)\\1.*(.)\\2", options: .regularExpression) != nil
}

func hasBadChars(_ s: String) -> Bool {
	return s.rangeOfCharacter(from: CharacterSet(charactersIn: "iol")) != nil
}
