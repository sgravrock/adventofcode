import XCTest

class day7_1_tests: XCTestCase {
	var subject: Circuit = Circuit()
	
	override func setUp() {
		subject = Circuit()
	}
	
	func testAssignment() {
		subject.configure(line: "2 -> x")
		XCTAssertEqual(2, subject.valueOf(wire: "x"))
		subject.configure(line: "x -> y")
		XCTAssertEqual(2, subject.valueOf(wire: "y"))
	}
	
	func testAnd() {
		subject.configure(line: "2 -> x")
		subject.configure(line: "3 -> y")
		subject.configure(line: "x AND y -> z")
		XCTAssertEqual(2, subject.valueOf(wire: "z"))
	}
	
	func testOr() {
		subject.configure(line: "5 -> x")
		subject.configure(line: "3 -> y")
		subject.configure(line: "x OR y -> z")
		XCTAssertEqual(7, subject.valueOf(wire: "z"))
	}
	
	func testLshift() {
		subject.configure(line: "1 -> x")
		subject.configure(line: "3 -> y")
		subject.configure(line: "x LSHIFT y -> z")
		XCTAssertEqual(8, subject.valueOf(wire: "z"))
	}
	
	func testRshift() {
		subject.configure(line: "8 -> x")
		subject.configure(line: "3 -> y")
		subject.configure(line: "x RSHIFT y -> z")
		XCTAssertEqual(1, subject.valueOf(wire: "z"))
	}
	
	func testNot() {
		subject.configure(line: "5 -> x")
		subject.configure(line: "NOT x -> z")
		XCTAssertEqual(65530, subject.valueOf(wire: "z"))
	}
	
	func test_allowsDependenciesToBeConfiguredLater() {
		subject.configure(line: "NOT x -> z")
		subject.configure(line: "5 -> x")
		XCTAssertEqual(65530, subject.valueOf(wire: "z"))
	}
	
	func testExample() {
		let input = ["123 -> x",
		             "456 -> y",
		             "x AND y -> d",
		             "x OR y -> e",
		             "x LSHIFT 2 -> f",
		             "y RSHIFT 2 -> g",
		             "NOT x -> h",
		             "NOT y -> i"]
		for line in input {
			subject.configure(line: line)
		}
		
		XCTAssertEqual(subject.valueOf(wire: "d"), 72)
		XCTAssertEqual(subject.valueOf(wire: "e"), 507)
		XCTAssertEqual(subject.valueOf(wire: "f"), 492)
		XCTAssertEqual(subject.valueOf(wire: "g"), 114)
		XCTAssertEqual(subject.valueOf(wire: "h"), 65412)
		XCTAssertEqual(subject.valueOf(wire: "i"), 65079)
		XCTAssertEqual(subject.valueOf(wire: "x"), 123)
		XCTAssertEqual(subject.valueOf(wire: "y"), 456)
	}

	func testValuesAreMemoized() {
		let handle = PropertyHandle(prop: nil)
		var timesComputed = 0
		handle.prop = Property(lhs: nil, rhs: nil, compute: { (_, _) -> UInt16? in
			timesComputed += 1
			return 0
		})
		let _ = handle.value()
		let _ = handle.value()
		XCTAssertEqual(1, timesComputed)
	}
}
