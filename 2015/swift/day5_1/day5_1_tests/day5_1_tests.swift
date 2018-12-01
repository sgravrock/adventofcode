import XCTest

class day5_1_tests: XCTestCase {
	func test_isNice() {
		XCTAssertTrue(isNice("ugknbfddgicrmopn"))
		XCTAssertTrue(isNice("aaa"))
		XCTAssertFalse(isNice("jchzalrnumimnmhp"))
		XCTAssertFalse(isNice("haegwjzuvuyypxyu"))
		XCTAssertFalse(isNice("dvszwmarrgswjxmb"))
	}
	
	func test_numNice() {
		let input = ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp"]
		XCTAssertEqual(2, numNice(lines: input))
	}
}
