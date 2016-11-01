func idealConfigurations(packages: [Int]) -> Set<Config> {
	var currentMin = Int.max
	var result = Set<Config>()
	
	for config in configsWithMinPassenger(packages: packages) {
		let qe = config.quantumEntanglement
		
		if qe < currentMin {
			currentMin = qe
			result = Set<Config>()
		}
		
		if qe == currentMin {
			result.insert(config)
		}
	}
	
	return result
}

func configsWithMinPassenger(packages: [Int]) -> Set<Config> {
	var currentMin = Int.max
	var result = Set<Config>()
	
	for config in configsWithEqualWeight(packages: packages) {
		if config.passenger.count < currentMin {
			currentMin = config.passenger.count
			result = Set<Config>()
		}
		
		if config.passenger.count == currentMin {
			result.insert(config)
		}
	}
	
	return result
}

func configsWithEqualWeight(packages: [Int]) -> Set<Config> {
	var result = Set<Config>()
	
	for passenger in combinations(packages) {
		let weight = sum(passenger)
		let notInPassenger = packages.filter { !passenger.contains($0) }
		
		for left in combinations(notInPassenger, withSum: weight) {
			let right = notInPassenger.filter { !left.contains($0) }
			
			if sum(right) == weight {
				result.insert(Config(passenger: passenger, left: left, right: right))
			}
		}
	}
	return result
}

struct Config : Equatable, Hashable {
    let passenger: Set<Int>
    let left: Set<Int>
    let right: Set<Int>
	
	init(passenger: [Int], left: [Int], right: [Int]) {
		self.passenger = Set(passenger)
		self.left = Set(left)
		self.right = Set(right)
	}
	
	var quantumEntanglement: Int {
		return passenger.reduce(1, *)
	}
    
    var hashValue: Int {
        let hashes = [passenger, left, right].map { $0.hashValue }
        return hash(array: hashes)
    }
    
    static func ==(lhs: Config, rhs: Config) -> Bool {
        return lhs.passenger == rhs.passenger && lhs.left == rhs.left && lhs.right == rhs.right
    }
}

func hash<T>(array: [T]) -> Int where T: Hashable {
    // DJB hash algorithm
    var hash = 5381
    
    for v in array {
        hash = ((hash << 5) &+ hash) &+ v.hashValue
    }
    
    return hash
}

func combinations(_ things: [Int], withSum: Int) -> [[Int]] {
    return combinations(things).filter { sum($0) == withSum }
}

func sum(_ a: [Int]) -> Int {
    return a.reduce(0, +)
}

func sum(_ s: Set<Int>) -> Int {
	return s.reduce(0, +)
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
