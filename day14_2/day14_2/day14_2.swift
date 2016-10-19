struct Score : Equatable {
	let reindeer: String
	let score: Int
	
	static func ==(lhs: Score, rhs: Score) -> Bool {
		return lhs.reindeer == rhs.reindeer && lhs.score == rhs.score
	}
}

func race(reindeer: [String], seconds: Int) -> [Score] {
	// Brute force... not the most efficient or elegant approach
	// but lets us re-use part 1 with minimal changes
	let configs = reindeer.map(parseLine)
	var scores = Dictionary<String, Int>()
	for config in configs {
		scores[config.name] = 0
	}
	
	for i in 1...seconds {
		for name in farthestReindeer(reindeer: configs, seconds: i) {
			scores[name]! += 1
		}
	}
	
	return scores.map { Score(reindeer: $0, score: $1) }
		.sorted(by: { $0.score < $1.score })
		.reversed()
}

struct Reindeer: Equatable {
	let name: String
	let speed: Int
	let duration: Int
	let rest: Int
	
	static func ==(lhs: Reindeer, rhs: Reindeer) -> Bool {
		return lhs.name == rhs.name &&
			lhs.speed == rhs.speed &&
			lhs.duration == rhs.duration &&
			lhs.rest == rhs.rest
	}
}

func farthestReindeer(reindeer: [Reindeer], seconds: Int) -> [String] {
	let ranked = reindeer
		.map { (name: $0.name, distance: distanceTraveled(reindeer: $0, seconds: seconds)) }
		.sorted(by: { $0.distance > $1.distance })
	return ranked
		.filter { $0.distance == ranked[0].distance }
		.map { $0.name }
}

func distanceTraveled(reindeer: Reindeer, seconds: Int) -> Int {
	var secondsLeft = seconds
	var resting = false
	var distance = 0
	
	while secondsLeft > 0 {
		if resting {
			secondsLeft -= reindeer.rest
		} else {
			let duration = min(secondsLeft, reindeer.duration)
			distance += duration * reindeer.speed
			secondsLeft -= duration
		}
		
		resting = !resting
	}
	
	return distance
}

func parseLine(_ line: String) -> Reindeer {
	let tokens = line.components(separatedBy: " ")
	return Reindeer(name: tokens[0],
	                speed: Int(tokens[3])!,
	                duration: Int(tokens[6])!,
	                rest: Int(tokens[13])!)
}
