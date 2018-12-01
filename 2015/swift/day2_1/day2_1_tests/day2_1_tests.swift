import XCTest

class day2_1_tests: XCTestCase {
	func test_paperNeeded() {
		XCTAssertEqual(58, paperNeeded(package: Package(l: 2, w: 3, h: 4)))
		XCTAssertEqual(43, paperNeeded(package: Package(l: 1, w: 1, h: 10)))
	}
	
	func test_paperNeededForPackages() {
		let input = "2x3x4\n1x1x10"
		XCTAssertEqual(101, paperNeededForPackages(input))
	}
	
	func test_parsePackages() {
		let input = "29x13x26\n11x11x14"
		let actual = parsePackages(input)
		let expected = [Package(l: 29, w: 13, h:26), Package(l: 11, w: 11, h: 14)]
		XCTAssertEqual(expected, actual)
	}
}
