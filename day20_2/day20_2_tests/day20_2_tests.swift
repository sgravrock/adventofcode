import XCTest

class day20_2_tests: XCTestCase {
	func test_lowestHouseNumberWithPresents() {
		XCTAssertEqual(4 as Int?, lowestHouseNumberWithPresents(77, limit: 4))
		XCTAssertEqual(3 as Int?, lowestHouseNumberWithPresents(34, limit: 3))
	}
	
	func test_presentsAtHouse_basic() {
		let expected = [11, 33, 44, 77, 66, 132, 88, 165, 143]
		
		for i in (0..<expected.count) {
			XCTAssertEqual(expected[i], presentsAtHouse(i + 1))
		}
	}
	
	func test_presentsAtHouse_elvesGiveUpAfter50() {
		XCTAssertEqual(781, presentsAtHouse(51)) // would be 792 but elf 1 has quit
	}
}
