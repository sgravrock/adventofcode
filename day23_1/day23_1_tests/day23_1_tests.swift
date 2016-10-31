import XCTest

class day23_1_tests: XCTestCase {
	func test_interpret() {
		let instructions = [
			"inc a",
			"jio a, +2",
			"tpl a",
			"inc a"
		]
		let result = interpret(instructions: instructions)
		XCTAssertEqual(2, result["a"])
	}
	
	func test_initialRegisterValues() {
		let result = interpret(instructions: [])
		XCTAssertEqual(["a": 0, "b": 0], result)
	}
	
	func test_inc() {
		let result = interpret(instructions: ["inc a"])
		XCTAssertEqual(["a": 1, "b": 0], result)
	}
	
	func test_hlf() {
		let result = interpret(instructions: ["inc b", "inc b", "hlf b"])
		XCTAssertEqual(["a": 0, "b": 1], result)
	}
	
	func test_tpl() {
		let result = interpret(instructions: ["inc b", "tpl b"])
		XCTAssertEqual(["a": 0, "b": 3], result)
	}
	
	func test_jmp() {
		let result = interpret(instructions: ["inc a", "jmp 2", "inc b", "inc a"])
		XCTAssertEqual(["a": 2, "b": 0], result)
	}
	
	func test_jie_yes() {
		let result = interpret(instructions: ["jie a, 2", "inc b", "inc a"])
		XCTAssertEqual(["a": 1, "b": 0], result)
	}
	
	func test_jie_no() {
		let result = interpret(instructions: ["inc a", "jie a, 2", "inc b", "inc a"])
		XCTAssertEqual(["a": 2, "b": 1], result)
	}
	
	func test_jio_yes() {
		let result = interpret(instructions: ["inc a", "jio a, 2", "inc b", "inc a"])
		XCTAssertEqual(["a": 2, "b": 0], result)
	}
	
	func test_jio_no() {
		let result = interpret(instructions: ["inc a", "inc a", "inc a", "jio a, 2", "inc b"])
		XCTAssertEqual(["a": 3, "b": 1], result)
	}
	
	func test_jumpsCanBeBackward() {
		let result = interpret(instructions: ["inc a", "jie a, 3", "inc b", "jmp -3"])
		XCTAssertEqual(["a": 2, "b": 1], result)
	}
}
