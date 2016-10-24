import XCTest

class day20_1_tests: XCTestCase {
	func test_lowestHouseNumberWithPresents() {
		XCTAssertEqual(4 as Int?, lowestHouseNumberWithPresents(70, limit: 4))
		XCTAssertEqual(3 as Int?, lowestHouseNumberWithPresents(31, limit: 3))
	}
	
	func test_presentsAtHouse() {
		let expected = [10, 30, 40, 70, 60, 120, 80, 150, 130]
		
		for i in (0..<expected.count) {
			XCTAssertEqual(expected[i], presentsAtHouse(i + 1))
		}
	}
	
	func test_presentsPerHouse_performance() {
		measure {
			for i in 1...10000 {
				_ = presentsAtHouse(i)
			}
		}
	}
}
