import Foundation

func minQE(packages: [Int]) -> Int? {
	let weight = sum(packages) / 3
	
	for i in 1..<packages.count - 2 {
		if let m = minQE(packages: packages, weight: weight, passengerCount: i) {
			return m
		}
	}
	
	return nil
}

func minQE(packages: [Int], weight: Int, passengerCount: Int ) -> Int? {
	return autoreleasepool {
		print("Checking with \(passengerCount) in passenger")
        let allCombos = combinations(packages, length: passengerCount)
        let combosWithMinWeight: LazyFilterSequence<LazyMapSequence<IndexCombinations, [Int]> > = allCombos.filter { sum($0) == weight }
        let combos = combosWithMinWeight.sorted { (a: [Int], b: [Int]) -> Bool in
				return quantumEntanglement(a) < quantumEntanglement(b)
			}
		
		print("Checking \(combos.count) combos")
		
		for combo in combos {
			if isValid(packages: packages, weight: weight, passesngerCombo: combo) {
				return quantumEntanglement(combo)
			}
		}
		
		return nil
	}
}

func isValid(packages: [Int], weight: Int, passesngerCombo: [Int]) -> Bool {
	return autoreleasepool {
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

// Basically the same as the std lib LazySequence's lazy flatMap overload,
// except that I can figure out how to write the type signature of a method that returns it.
struct PotentiallyLazyFlatMapSequence<TInputSequence, TChunk>: Sequence where TInputSequence: Sequence, TChunk: Sequence {
    var input: TInputSequence
    let transform: (TInputSequence.Iterator.Element) -> TChunk
    
    init(input: TInputSequence, transform: @escaping (TInputSequence.Iterator.Element) -> TChunk) {
        self.input = input
        self.transform = transform
    }
    
    func makeIterator() -> AnyIterator<TChunk.Iterator.Element> {
        var inputIter = input.makeIterator()
        var chunkIter: TChunk.Iterator? = nil
        
        return AnyIterator<TChunk.Iterator.Element> {
            while true {
                if chunkIter == nil {
                    guard let inputEl = inputIter.next() else {
                        return nil
                    }
                    
                    let chunk = self.transform(inputEl)
                    chunkIter = chunk.makeIterator()
                }
                
                if let result = chunkIter?.next() {
                    return result
                } else {
                    chunkIter = nil
                }
            }
        }
    }
}

func combinations(_ things: [Int]) -> PotentiallyLazyFlatMapSequence<[Int], LazyMapSequence<IndexCombinations, [Int]>> {
    let sizes = Array(1...things.count)
    return PotentiallyLazyFlatMapSequence(input: sizes, transform: { (size: Int) -> LazyMapSequence<IndexCombinations, [Int]> in
        return combinations(things, length: size)
    })
}

func combinations(_ things: [Int], length: Int) -> LazyMapSequence<IndexCombinations, [Int]> {
	let indexedCombos = lazyIndexCombinations(length: length, outOf: things.count, startingAt: 0)
    let result: LazyMapSequence<IndexCombinations, [Int]> = indexedCombos.map({ (indices: [Int]) -> [Int] in
        return indices.map { things[$0] }
    })
    return result
}

func lazyIndexCombinations(length: Int, outOf: Int, startingAt: Int) -> LazySequence<IndexCombinations> {
    return IndexCombinations(length: length, outOf: outOf, startingAt: startingAt).lazy
}

struct IndexCombinations : Sequence {
    let comboSize: Int
    let inputSize: Int
    let startIndex: Int
    
    init(length: Int, outOf: Int, startingAt: Int) {
        assert(length >= 1)
        assert(length <= outOf - startingAt)
        comboSize = length
        inputSize = outOf
        startIndex = startingAt
    }
    
    func makeIterator() -> AnyIterator<[Int]> {
        var wip: [Int] = []
        var result: [[Int]] = []
        
        // TODO: moar lazy
        func generateSub(length: Int, startingAt: Int) {
            if wip.count == length {
                result.append(wip)
                return
            }
            
            for i in startingAt..<self.inputSize {
                wip.append(i)
                generateSub(length: length, startingAt: i + 1)
                wip.removeLast()
            }
        }
        
        generateSub(length: self.comboSize, startingAt: self.startIndex)
        var i = 0

        return AnyIterator<[Int]> {
            guard i < result.count else {
                return nil
            }
            
            let ret = result[i]
            i += 1
            return ret
        }
    }
}
