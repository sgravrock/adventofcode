import XCTest

class day7_2_tests: XCTestCase {
	func test_supports_ssl() {
		XCTAssertEqual(true, supports_ssl("aba[bab]xyz"))
		XCTAssertEqual(false, supports_ssl("xyx[xyx]xyx"))
		XCTAssertEqual(true, supports_ssl("aaa[kek]eke"))
		XCTAssertEqual(true, supports_ssl("zazbz[bzb]cdb"))
	}
}
