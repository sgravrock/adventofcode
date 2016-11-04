func lowestHouseNumberWithPresents(_ nPresents: Int) -> Int {
	return lowestHouseNumberWithPresents(nPresents, limit: Int.max)!
}

// The limit is to allow unit tests to fail in constant time.
func lowestHouseNumberWithPresents(_ nPresents: Int, limit: Int) -> Int? {
	for houseNum in (1...limit) {
		let p = presentsAtHouse(houseNum)
		
		if p >= nPresents {
			return houseNum
		}
	}
	
	return nil
}

func presentsAtHouse(_ houseNum: Int) -> Int {
	var nPresents = houseNum * 11
	
	if houseNum >= 2 {
		for elfNum in 1...houseNum / 2 {
			if houseNum % elfNum == 0 && houseNum / elfNum <= 50 {
				nPresents += elfNum * 11
			}
		}
	}
	
	return nPresents
}
