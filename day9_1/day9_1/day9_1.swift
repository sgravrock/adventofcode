import Foundation

struct Leg {
	let start: String
	let end: String
	let distance: Int
	
	func isBetween(_ a: String, _ b: String) -> Bool {
		return (a == start && b == end) || (a == end && b == start)
	}
}

func shortestPath(input: [String]) -> Int {
	let legs = input.map(parseLeg)
	let cities = distinct(legs.map { $0.start} + legs.map { $0.end })
	let routes = permute(cities)
	let weighted = routes.map { (route: $0, distance: findDistance(route: $0, legs: legs)) }
	let sorted = weighted.sorted { $1.distance > $0.distance }
	return sorted.first!.distance
}

func findDistance(route: [String], legs:[Leg]) -> Int {
	var distance = 0
	
	for i in 1..<route.count {
		let leg = legs.first(where: { $0.isBetween(route[i - 1], route[i]) })
		distance += leg!.distance
	}
	
	return distance
}

func distinct(_ things: [String]) -> [String] {
	return Array<String>(Set<String>(things))
}

func parseLeg(_ input: String) -> Leg {
	// e.g. London to Dublin = 464
	let tokens = input.components(separatedBy: " ")
	return Leg(start: tokens[0], end: tokens[2], distance: Int(tokens[4])!)
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
/*
func permute<T: Collection>(_ elems: T) -> [[T.Iterator.Element]] {
	if elems.count == 0 {
		return []
	} else if elems.count == 1 {
		return [[elems.first!]]
	}
	
	return elems.map({ (first) in
		return permute2(elems, first: first)
	}).reduce([], +)
}

func permute2<T: Collection>(_ elems: T, first: T.Iterator.Element) -> [[T.Iterator.Element]] {
	let rest = elems.filter({ $0 != first })
	let subresult = permute(rest as AnyCol)
	return subresult.map({ [first] + $0 })
}*/
