import XCTest

class day25_1_tests: XCTestCase {
    func test_distance() {
        XCTAssertEqual(0, distance(to: (x: 1, y: 1)))
        XCTAssertEqual(5, distance(to: (x: 3, y: 1)))
        XCTAssertEqual(8, distance(to: (x: 3, y: 2)))
        XCTAssertEqual(20, distance(to: (x: 6, y: 1)))
    }
    
    func test_nextCode() {
        XCTAssertEqual(31916031, nextCode(20151125))
        XCTAssertEqual(18749137, nextCode(31916031))
        XCTAssertEqual(77061, nextCode(30943339))
    }
    
    func test_codeAt() {
        XCTAssertEqual(27995004, codeAt(position: (x: 6, y: 6), start: 20151125))
    }
}
