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

func bestDistanceTraveled(reindeer: [String], seconds: Int) -> Int {
    return reindeer
        .map { distanceTraveled(reindeer: parseLine($0), seconds: seconds) }
        .sorted()
        .last!
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
