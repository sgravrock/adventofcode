import XCTest

class day3_2_tests: XCTestCase {
	func testDeliver() {
		XCTAssertEqual(3, deliver("^v"))
		XCTAssertEqual(3, deliver("^>v<"))
		XCTAssertEqual(11, deliver("^v^v^v^v^v"))
	}
}
