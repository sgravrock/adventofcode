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
	var nPresents = 0

	for elfNum in 1...houseNum {
		if houseNum % elfNum == 0 {
			nPresents += elfNum * 10
		}
	}
	
	return nPresents
}
