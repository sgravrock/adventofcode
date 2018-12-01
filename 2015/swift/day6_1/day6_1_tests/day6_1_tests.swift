import XCTest

class day6_1_tests: XCTestCase {
	func test_handleInstruction() throws {
		let subject = LightDisplay()
		try subject.handle(instruction: "turn on 0,0 through 99,99")
		XCTAssertEqual(10000, subject.numLit)
		try subject.handle(instruction: "turn off 0,0 through 50,0")
		XCTAssertEqual(9949, subject.numLit)
		try subject.handle(instruction: "toggle 0,0 through 99,0")
		XCTAssertEqual(9951, subject.numLit)
	}
	
	func test_handleInstructions() throws {
		let subject = LightDisplay()
		let instructions = [
			"turn on 0,0 through 99,99",
			"turn off 0,0 through 50,0",
			"toggle 0,0 through 99,0"
		]
		try subject.handle(instructions: instructions)
		XCTAssertEqual(9951, subject.numLit)
	}
}
