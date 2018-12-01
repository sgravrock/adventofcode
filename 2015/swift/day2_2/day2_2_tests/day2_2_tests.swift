import XCTest

class day2_2_tests: XCTestCase {
	func test_ribbonNeeded() {
		XCTAssertEqual(34, ribbonNeeded(package: Package(l: 2, w: 3, h: 4)))
		XCTAssertEqual(14, ribbonNeeded(package: Package(l: 1, w: 1, h: 10)))
	}
	
	func test_ribbonNeededForPackages() {
		let input = "2x3x4\n1x1x10"
		XCTAssertEqual(48, ribbonNeededForPackages(input))
	}
}
