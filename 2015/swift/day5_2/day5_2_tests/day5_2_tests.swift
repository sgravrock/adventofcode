import XCTest

class day5_2_tests: XCTestCase {
	func test_isNice() {
		XCTAssertTrue(isNice("qjhvhtzxzqqjkmpb"))
		XCTAssertTrue(isNice("xxyxx"))
		XCTAssertFalse(isNice("qjhvhtzxzqqkmpb"))
		XCTAssertFalse(isNice("uurcxstgmygtbstg"))
		XCTAssertFalse(isNice("ieodomkazucvgmuy"))
	}
}
