import XCTest

class day5_2_tests: XCTestCase {
	func test_findPassword() {
		XCTAssertEqual("05ace8e3", findPassword(doorId: "abc"))
	}
	
	func test_next_character() {
		XCTAssertEqual(NextCharResult(hash_num: 3231929, pos: 1, c: "5"), next_character(doorId: "abc", start: 0));
		XCTAssertEqual(NextCharResult(hash_num: 5357525, pos: 4, c: "e"), next_character(doorId: "abc", start: 3231930));
	}
	
	func test_hash() {
		XCTAssertEqual("000001dbbfa3a5c83a2d506429c7b00e",
		               toHexString(bytes: md5("abcdef609043")))
	}
	
	func test_isValid() {
		let maxValid: [UInt8] = [0x0, 0x0, 0x0f, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0]
		let minInvalid: [UInt8] = [0x0, 0x0, 0x10, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0]
		let otherDigit: [UInt8] = [0x0, 0x01, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0]
		XCTAssertTrue(isValid(hash: maxValid))
		XCTAssertFalse(isValid(hash: minInvalid))
		XCTAssertFalse(isValid(hash: otherDigit))
	}
}
