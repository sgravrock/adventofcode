import XCTest
@testable import day1_1

class Santa_tests: XCTestCase {
	func testNavigate() {
		let subject = Santa()
		XCTAssertEqual(0, subject.navigate(directions: "(())"))
		XCTAssertEqual(0, subject.navigate(directions: "()()"))
		XCTAssertEqual(3, subject.navigate(directions: "((("))
		XCTAssertEqual(3, subject.navigate(directions: "(()(()("))
		XCTAssertEqual(-1, subject.navigate(directions: "())"))
		XCTAssertEqual(-1, subject.navigate(directions: "))("))
		XCTAssertEqual(-3, subject.navigate(directions: ")))"))
		XCTAssertEqual(-3, subject.navigate(directions: ")())())"))
	}
}
