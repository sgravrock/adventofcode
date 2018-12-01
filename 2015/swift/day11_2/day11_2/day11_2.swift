import Foundation

func isValid(_ s: String) -> Bool {
    return isValid(chars: Array(s.characters))
}

func isValid(chars: [Character]) -> Bool {
    return hasStraight(chars) && hasMultiplePairs(chars) && !hasBadChars(chars)
}

func nextValidPassword(_ current: String) -> String? {
    var chars = Array(current.characters)
    
    while true {
        if !toNextPassword(&chars) {
            return nil
        }
        
        if isValid(chars: chars) {
            return String(chars)
        }
    }
}

func nextPassword(_ current: String) -> String? {
    var chars = Array(current.characters)
    
    if toNextPassword(&chars) {
        return String(chars)
    }
    
    return nil
}

func toNextPassword( _ chars: inout [Character]) -> Bool {
    for i in (0..<chars.count).reversed() {
        if chars[i] == "z" {
            if i == 0 {
                return false
            }
            
            chars[i] = "a"
        } else {
            chars[i] = nextChar(chars[i])
            return true
        }
    }
    
    assertionFailure("toNextPassword() reached unreachable code")
    return false
}

func nextChar(_ c: Character) -> Character {
    return Character(UnicodeScalar(unicodeScalar(ofCharacter: c).value + 1)!)
}

func unicodeScalar(ofCharacter: Character) -> UnicodeScalar {
    return String(ofCharacter).unicodeScalars.first!
}

func hasStraight(_ chars: [Character]) -> Bool {
    for i in 2..<chars.count {
        // Assumes ASCII-like properties.
        // If input is EBCDIC you get to keep both pieces.
        let a = unicodeScalar(ofCharacter: chars[i - 2]).value
        let b = unicodeScalar(ofCharacter: chars[i - 1]).value
        let c = unicodeScalar(ofCharacter: chars[i]).value
        
        if a == c - 2 && b == c - 1 {
            return true
        }
    }
    
    return false
    
}

func hasMultiplePairs(_ chars: [Character]) -> Bool {
    var firstPairEnd: Int? = nil
    
    for i in 1..<chars.count {
        if chars[i] == chars[i - 1] {
            if let f = firstPairEnd {
                if i - 1 > f {
                    return true
                }
            } else {
                firstPairEnd = i
            }
        }
    }
    
    return false
}

func hasBadChars(_ chars: [Character]) -> Bool {
    return chars.contains(where: { (c: Character) -> Bool in
        return c == "i" || c == "o" || c == "l"
    })
}
