import Foundation

func minQE(packages: [Int]) -> Int? {
	let totalWeight = sum(packages)
	
	for i in 1..<packages.count - 2 {
		if let m = minQE(packages: packages, totalWeight: totalWeight, passengerCount: i) {
			return m
		}
	}
	
	return nil
}

func minQE(packages: [Int], totalWeight: Int, passengerCount: Int ) -> Int? {
	return autoreleasepool {
		print("Checking with \(passengerCount) in passenger")

		let combos = combinations(packages, length: passengerCount).sorted { (a: [Int], b: [Int]) -> Bool in
			return quantumEntanglement(a) < quantumEntanglement(b)
		}
		
		print("Checking \(combos.count) combos")
		
		for combo in combos {
			if isValid(packages: packages, totalWeight: totalWeight, passesngerCombo: combo) {
				return quantumEntanglement(combo)
			}
		}
		
		return nil
	}
}

func isValid(packages: [Int], totalWeight: Int, passesngerCombo: [Int]) -> Bool {
	return autoreleasepool {
		let weight = sum(passesngerCombo)
		
		if weight != totalWeight / 3 {
			return false
		}
		
		let notInPassenger = packages.filter { !passesngerCombo.contains($0) }
		
		for combo in combinations(notInPassenger, withSum: weight) {
			let ok = autoreleasepool(invoking: { () -> Bool in
				let rest = notInPassenger.filter { !combo.contains($0) }
				return sum(rest) == weight
			})
			
			if ok {
				return true
			}
		}
		
		return false
	}
}

func quantumEntanglement(_ a: [Int]) -> Int {
	var p = 1
	
	for n in a {
		p *= n
	}
	
	return p
}

func combinations(_ things: [Int], withSum: Int) -> [[Int]] {
    return combinations(things).filter { sum($0) == withSum }
}

func sum(_ a: [Int]) -> Int {
    return a.reduce(0, +)
}

func combinations<T>(_ things: [T]) -> [[T]] {
    var result: [[T]] = []
	
	if things.count == 0 {
		return result
	}
    
    for i in 1...things.count {
        let subresult = combinations(length: i, outOf: things.count, startingAt: 0)
        for sr in subresult {
            result.append(sr.map { things[$0] })
        }
    }
    
    return result
}

func combinations<T>(_ things: [T], length: Int) -> [[T]] {
	let indexedCombos = combinations(length: length, outOf: things.count, startingAt: 0)
	return indexedCombos.map({ (indices: [Int]) -> [T] in
		return indices.map { things[$0] }
	})
}

func combinations(length: Int, outOf: Int, startingAt: Int) -> [[Int]] {
    assert(length >= 1)
    assert(length <= outOf - startingAt)
    
    var wip: [Int] = []
    var result: [[Int]] = []
    
    func generateSub(length: Int, startingAt: Int) {
        if wip.count == length {
            result.append(wip)
            return
        }
        
        for i in startingAt..<outOf {
            wip.append(i)
            generateSub(length: length, startingAt: i + 1)
            wip.removeLast()
        }
    }
    
    generateSub(length: length, startingAt: startingAt)
    return result
}
