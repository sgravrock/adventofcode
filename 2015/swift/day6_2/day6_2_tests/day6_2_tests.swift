import XCTest

class day6_2_tests: XCTestCase {
	func test_handleInstruction() throws {
		var subject = LightDisplay()
		try subject.handle(instruction: "turn on 0,0 through 99,99")
		XCTAssertEqual(10000, subject.totalBrightness)
		try subject.handle(instruction: "turn on 0,0 through 1,1")
		XCTAssertEqual(10004, subject.totalBrightness)

		subject = LightDisplay()
		try subject.handle(instruction: "turn on 0,0 through 0,9")
		try subject.handle(instruction: "turn on 0,0 through 0,9")
		XCTAssertEqual(20, subject.totalBrightness)
		try subject.handle(instruction: "turn off 0,0 through 0,9")
		XCTAssertEqual(10, subject.totalBrightness)
		try subject.handle(instruction: "turn off 0,0 through 0,9")
		XCTAssertEqual(0, subject.totalBrightness)
		try subject.handle(instruction: "turn off 0,0 through 0,9")
		XCTAssertEqual(0, subject.totalBrightness)
	}
}
