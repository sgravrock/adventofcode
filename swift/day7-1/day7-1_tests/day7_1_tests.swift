import XCTest

class day7_1_tests: XCTestCase {
	func test_supports_tls() {
		XCTAssertEqual(true, supports_tls("abba[mnop]qrst"))
		XCTAssertEqual(true, supports_tls("[mnop]abba"))
		XCTAssertEqual(true, supports_tls("abcd[bddbxyyx"))
		XCTAssertEqual(false, supports_tls("abcd[bddb]xyyx"))
		XCTAssertEqual(false, supports_tls("aaaa[qwer]tyui"))
		XCTAssertEqual(true, supports_tls("ioxxoj[asdfgh]zxcvbn"))
		XCTAssertEqual(false, supports_tls("abba[abba]"))
	}
}
