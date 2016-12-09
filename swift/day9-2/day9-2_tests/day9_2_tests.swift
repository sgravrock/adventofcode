import XCTest

class day9_2_tests: XCTestCase {
	func test_decompressed_length() {
		XCTAssertEqual("ADVENT".characters.count, decompressed_length("ADVENT"));
		XCTAssertEqual("ABBBBBC".characters.count, decompressed_length("A(1x5)BC"));
		XCTAssertEqual("XYZXYZXYZ".characters.count, decompressed_length("(3x3)XYZ"));
		XCTAssertEqual("ABCBCDEFEFG".characters.count, decompressed_length("A(2x2)BCD(2x2)EFG"));
		XCTAssertEqual("XABCABCABCABCABCABCY".characters.count, decompressed_length("X(8x2)(3x3)ABCY"));
		XCTAssertEqual(241920,
		               decompressed_length("(27x12)(20x12)(13x14)(7x10)(1x12)A"));
		XCTAssertEqual(445,
		               decompressed_length("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"));
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
