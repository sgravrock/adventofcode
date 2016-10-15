import XCTest

class day4_1_tests: XCTestCase {
	func test_hash() {
		XCTAssertEqual("000001dbbfa3a5c83a2d506429c7b00e", makeHash(key: "abcdef", n: 609043))
	}
	
	func test_mine() {
		XCTAssertEqual(609043, mine(key: "abcdef"))
	}
}
