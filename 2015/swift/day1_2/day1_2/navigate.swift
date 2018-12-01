func navigate(toFloor: Int, directions: String) -> Int {
	var i = 0
	var floor = 0
	
	for c in directions.characters {
		i += 1
		
		if c == "(" {
			floor += 1
		} else if c == ")" {
			floor -= 1
		}
		
		if floor == toFloor {
			return i
		}
	}
	
	return 0
}
