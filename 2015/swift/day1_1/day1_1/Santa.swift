class Santa {
	func navigate(directions: String) -> Int {
		var floor = 0
		for c in directions.characters {
			if c == "(" {
				floor += 1
			} else if c == ")" {
				floor -= 1
			}
		}
		
		return floor
	}
}
