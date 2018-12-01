import Foundation

struct Preference : Equatable {
    let person: String
    let nextTo: String
    let delta: Int
    
    static func ==(lhs: Preference, rhs: Preference) -> Bool {
        return lhs.person == rhs.person &&
            lhs.nextTo == rhs.nextTo &&
            lhs.delta == rhs.delta
    }
}

func addSelf(input: [String]) -> [Preference] {
    var prefs = input.map(parseLine)
    let names = Set(prefs.map { $0.person })
    
    for name in names {
        prefs.append(Preference(person: "Self", nextTo: name, delta: 0))
        prefs.append(Preference(person: name, nextTo: "Self", delta: 0))
    }
    
    return prefs
}

func bestHappiness(input: [String]) -> Int {
    let prefs = input.map(parseLine)
    return bestHappiness(prefs: prefs)
}

func bestHappiness(prefs: [Preference]) -> Int {
    let names = Array(Set(prefs.map { $0.person }))
    let arrangements = permute(names)
    return arrangements
        .map { totalHappiness(prefs: prefs, arrangement: $0) }
        .sorted()
        .last!
}

func totalHappiness(input: [String], arrangement: [String]) -> Int {
    let prefs = input.map(parseLine)
    return totalHappiness(prefs: prefs, arrangement: arrangement)
}

func totalHappiness(prefs: [Preference], arrangement: [String]) -> Int {
    if arrangement.count < 2 {
        return 0
    }
    
    var sum = 0
    
    for i in 0..<arrangement.count {
        let a = arrangement[i]
        let b = i == 0 ? arrangement.last! : arrangement[i - 1]
        sum += happiness(prefs: prefs, person: a, nextTo: b)
        sum += happiness(prefs: prefs, person: b, nextTo: a)
    }
    
    return sum
}

func happiness(prefs: [Preference], person: String, nextTo: String) -> Int {
    let pref = prefs.first(where: { $0.person == person && $0.nextTo == nextTo })!
    return pref.delta
}

func parseLine(_ line: String) -> Preference {
    let tokens = line.components(separatedBy: " ")
    let n = Int(tokens[3])!
    let delta = tokens[2] == "gain" ? n : -n
    let nextTo = tokens[10].replacingOccurrences(of: ".", with: "")
    return Preference(person: tokens[0], nextTo: nextTo, delta: delta)
}

// TODO: Can this be made generic using the Collection protocol?
func permute(_ elems: [String]) -> [[String]] {
    if elems.count == 0 {
        return []
    } else if elems.count == 1 {
        return [elems]
    }
    
    return elems.map({ (first: String) -> [[String]] in
        return permute2(elems, first: first)
    }).reduce([], +)
}

func permute2(_ elems: [String], first: String) -> [[String]] {
    let rest = elems.filter({ $0 != first })
    let subresult = permute(rest)
    return subresult.map({ [first] + $0 })
}
