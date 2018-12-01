import XCTest

class day12_1_tests: XCTestCase {
    func test_sumNumbers() {
        XCTAssertEqual(6, try sumNumbers(json: "[1, 2, 3]"))
        XCTAssertEqual(6, try sumNumbers(json: "{\"a\":2,\"b\":4}"))
        XCTAssertEqual(3, try sumNumbers(json: "[[[3]]]"))
        XCTAssertEqual(3, try sumNumbers(json: "{\"a\":{\"b\":4},\"c\":-1}"))
    }
}
