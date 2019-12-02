import XCTest

class ProgramTests: XCTestCase {
	
	func testParse() {
		let input = "1,9,10,3,2,3,11,0,99,30,40,50"
		let expected = [1,9,10,3,2,3,11,0,99,30,40,50]
		XCTAssertEqual(Program.parse(input), expected)
	}
	
	func testExecute_1() throws {
		var program = [1,9,10,3,2,3,11,0,99,30,40,50]
		let expected = [3500,9,10,70,2,3,11,0,99,30,40,50]
		try Program.execute(&program)
		XCTAssertEqual(program, expected)
	}
	
	func testExecute_2() throws {
		var program = [1,0,0,0,99]
		let expected = [2,0,0,0,99]
		try Program.execute(&program)
		XCTAssertEqual(program, expected)
	}
	
	func testExecute_3() throws {
		var program = [2,3,0,3,99]
		let expected = [2,3,0,6,99]
		try Program.execute(&program)
		XCTAssertEqual(program, expected)
	}
	
	func testExecute_4() throws {
		var program = [2,4,4,5,99,0]
		let expected = [2,4,4,5,99,9801]
		try Program.execute(&program)
		XCTAssertEqual(program, expected)
	}

	func testExecute_5() throws {
		var program = [1,1,1,4,99,5,6,0,99]
		let expected = [30,1,1,4,2,5,6,0,99]
		try Program.execute(&program)
		XCTAssertEqual(program, expected)
	}
}
