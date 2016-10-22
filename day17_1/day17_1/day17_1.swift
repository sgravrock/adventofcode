func combinations(_ things: [Int], withSum: Int) -> [[Int]] {
	return combinations(things).filter { sum($0) == withSum }
}

func sum(_ a: [Int]) -> Int {
	return a.reduce(0, +)
}

func combinations<T>(_ things: [T]) -> [[T]] {
    var resultIndices = Set<Set<Int>>()
    combine(things, into: &resultIndices)

	return resultIndices.map { (result: Set<Int>) -> [T] in
		return result.map { things[$0] }
	}
}

func combine<T>(_ things: [T], into: inout Set<Set<Int>>) {
	for i in 1...things.count {
		combine(things, length: i, into: &into)
	}
}

func combine<T>(_ things: [T], length: Int, into: inout Set<Set<Int>>) {
	assert(length > 0)
	assert(things.count >= length)
	
	if length == things.count {
		into.insert(Set<Int>(0..<things.count))
	} else if length == 1 {
		for i in 0..<things.count {
			into.insert(Set<Int>([i]))
		}
	} else {
		for i in 0..<things.count {
			var rest = things
			rest.remove(at: i)
			var subresult = Set<Set<Int>>()
			combine(rest, length: length - 1, into: &subresult)
			
			for var sr in subresult {
				sr.insert(i)
				into.insert(sr)
			}
		}
	}
}
