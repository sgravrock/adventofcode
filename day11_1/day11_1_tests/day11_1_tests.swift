import XCTest

class day11_1_tests: XCTestCase {
	func test_isValid() {
		XCTAssertTrue(isValid("abcdffaa"))
	}
	
	func test_isValid_forbids_i() {
		XCTAssertFalse(isValid("hijkmmn"))
	}
	
	func test_isValid_forbids_o() {
		XCTAssertFalse(isValid("hojkmmn"))
	}
	
	func test_isValid_forbids_l() {
		XCTAssertFalse(isValid("hljkmmn"))
	}
	
	func test_isValid_requiresStraight() {
		XCTAssertFalse(isValid("abbceffg"))
	}
	
	func test_isValid_requiresMultiplePairs() {
		XCTAssertFalse(isValid("abcdffa"))
	}
	
	func test_nextPassword() {
		XCTAssertEqual("az", nextPassword("ay"))
        XCTAssertEqual("ba", nextPassword("az"))
        XCTAssertNil(nextPassword("zz"))

    }
	
	func test_nextValidPassword() {
		XCTAssertEqual("abcdffaa", nextValidPassword("abcdefgh"))
		XCTAssertEqual("ghjaabcc", nextValidPassword("ghijklmn"))
        
	}
}
