import XCTest

class day12_2_tests: XCTestCase {
    func test_sumNumbers() {
        XCTAssertEqual(6, try sumNumbers(json: "[1, 2, 3]"))
        XCTAssertEqual(6, try sumNumbers(json: "{\"a\":2,\"b\":4}"))
        XCTAssertEqual(3, try sumNumbers(json: "[[[3]]]"))
        XCTAssertEqual(3, try sumNumbers(json: "{\"a\":{\"b\":4},\"c\":-1}"))
    }
  
    func test_ignoresRedInObjects() {
        XCTAssertEqual(4, try sumNumbers(json: "[1,{\"c\":\"red\",\"b\":2},3]"))
        XCTAssertEqual(0, try sumNumbers(json: "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"))
        XCTAssertEqual(6, try sumNumbers(json: "[1,\"red\",5]"))
    }
}
