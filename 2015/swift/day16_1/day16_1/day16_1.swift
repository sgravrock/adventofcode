import Foundation

func whichSue(sues: [SueSpec], sample: Dictionary<String, Int>) -> Int? {
    for sue in sues {
        if isMatch(sue: sue, sample: sample) {
            return sue.number
        }
    }
    
    return nil
}

func isMatch(sue: SueSpec, sample: Dictionary<String, Int>) -> Bool {
    for kv in sample {
        let sueval = sue.compounds[kv.key]
        if sueval != nil && sueval != kv.value {
            return false
        }
    }
    
    return true
}

func parseLine(line: String) -> SueSpec {
    // "Sue 1: goldfish: 6, trees: 9, akitas: 0"
    let tokens = line.components(separatedBy: CharacterSet(charactersIn: ",: ")).filter { $0 != "" }
    let num = Int(tokens[1])!
    var compounds = Dictionary<String, Int>()
    
    for i in stride(from: 2, to: tokens.count - 1, by: 2) {
        compounds[tokens[i]] = Int(tokens[i + 1])!
    }
    
    return SueSpec(number: num, compounds: compounds)
}

struct SueSpec : Equatable {
    let number: Int
    let compounds: Dictionary<String, Int>
    
    static func ==(lhs: SueSpec, rhs: SueSpec) -> Bool {
        return lhs.number == rhs.number && equals(dict1: lhs.compounds, dict2: rhs.compounds)
    }
}

func equals<TKey, TValue>(dict1: Dictionary<TKey, TValue>, dict2: Dictionary<TKey, TValue>) -> Bool
    where TKey: Hashable, TValue: Hashable {
        
        if dict1.count != dict2.count {
            return false
        }
        
        for kv in dict1 {
            if dict2[kv.key] != kv.value {
                return false
            }
        }
        
        return true
}
