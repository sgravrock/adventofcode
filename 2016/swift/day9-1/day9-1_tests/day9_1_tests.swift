import XCTest

class day9_1_tests: XCTestCase {
	func test_decompress() {
		XCTAssertEqual("ADVENT", decompress("ADVENT"));
		XCTAssertEqual("ABBBBBC", decompress("A(1x5)BC"));
		XCTAssertEqual("XYZXYZXYZ", decompress("(3x3)XYZ"));
		XCTAssertEqual("ABCBCDEFEFG", decompress("A(2x2)BCD(2x2)EFG"));
		XCTAssertEqual("(1x3)A", decompress("(6x1)(1x3)A"));
		XCTAssertEqual("X(3x3)ABC(3x3)ABCY", decompress("X(8x2)(3x3)ABCY"));
	}
	
	func test_scanner() {
		let scanner = Scanner(string: "asdf(1")
		var s: NSString?
		scanner.scanUpTo("(", into: &s)
		XCTAssertEqual(s, "asdf")
		scanner.scanString("(", into: nil)
		scanner.scanUpToCharacters(from: CharacterSet(), into: &s)
		XCTAssertEqual(s, "1")

		
		let scanner2 = Scanner(string: "asdf")
		XCTAssertTrue(scanner2.scanUpTo("(", into: &s))
		XCTAssertEqual("asdf", s)
	}
}
