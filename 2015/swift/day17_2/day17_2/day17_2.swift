func minimalCombinations(_ things: [Int], withSum: Int) -> [[Int]] {
	let validCombos = combinations(things).filter { sum($0) == withSum }
	let minLength = validCombos.reduce(Int.max) { (prev: Int, it: [Int]) -> Int in
		return min(prev, it.count)
	}
	return validCombos.filter { $0.count == minLength }
}

func combinations(_ things: [Int], withSum: Int) -> [[Int]] {
	return combinations(things).filter { sum($0) == withSum }
}

func sum(_ a: [Int]) -> Int {
	return a.reduce(0, +)
}

func combinations<T>(_ things: [T]) -> [[T]] {
	var result: [[T]] = []
	
	for i in 1...things.count {
		let subresult = combinations(length: i, outOf: things.count, startingAt: 0)
		for sr in subresult {
			result.append(sr.map { things[$0] })
		}
	}
	
	return result
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
