func combinations(_ things: [Int], withSum: Int) -> [[Int]] {
    let candidates = combinations(things)
    return candidates.filter({ (candidate: [Int]) -> Bool in
        let sum = candidate.reduce(0, +)
        return sum == withSum
    })
}

func combinations<T>(_ things: [T]) -> [[T]] where T: Hashable {
    let boxedThings = things.map { IdentityBox($0) }
    var boxedResult = Set<ArrayWrapper<IdentityBox<T>>>()
    combine(boxedThings, into: &boxedResult)
    return boxedResult.map { $0.a.map { $0.it }}
}

func combine<T>(_ things: [T], into: inout Set<ArrayWrapper<T>>) where T: Hashable, T: Equatable {
	into.insert(ArrayWrapper(things))

	if things.count <= 1 {
		return
	}
	
	for i in 0..<things.count {
		var copy = things
		copy.remove(at: i)
		combine(copy, into: &into)
	}
}

class IdentityBox<T>: Equatable, Hashable where T: Hashable {
    let it: T
    
    init(_ value: T) {
        it = value
    }
    
    static func ==(lhs: IdentityBox<T>, rhs: IdentityBox<T>) -> Bool {
        return lhs === rhs
    }
    
    var hashValue: Int {
        // This will generate collisions whenever two boxes have the same value,
        // but it should be good enough for our purposes.
        return it.hashValue
    }
}

struct ArrayWrapper<T>: Equatable, Hashable where T: Equatable, T: Hashable {
	let a: [T]
	
	init(_ arr: [T]) {
		a = arr
	}
	
	static func ==(lhs: ArrayWrapper<T>, rhs: ArrayWrapper<T>) -> Bool {
		return equals(array1: lhs.a, array2: rhs.a)
	}
	
	var hashValue: Int {
		return hash(array: a)
	}
}

func equals<T>(array1: [T], array2: [T]) -> Bool where T: Equatable {
	if array1.count != array2.count {
		return false
	}
	
	for i in 0..<array1.count {
		if array1[i] != array2[i] {
			return false
		}
	}
	
	return true
}

func hash<T>(array: Array<T>) -> Int where T: Hashable {
	// DJB hash algorighm
	var hash = 5381
	
	for x in array {
		hash = ((hash << 5) &+ hash) &+ x.hashValue
	}
	
	return hash
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
