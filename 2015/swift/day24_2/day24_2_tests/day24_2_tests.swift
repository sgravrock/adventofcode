import XCTest

class day24_2_tests: XCTestCase {
    func test_minQE() {
        let packages = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]
        let result = minQE(packages: packages)
        XCTAssertEqual(44, result)
    }
    
    func test_quantumEntanglement() {
        XCTAssertEqual(99, quantumEntanglement([11, 9]))
        XCTAssertEqual(90, quantumEntanglement([10, 9, 1]))
    }
    
    func test_combinations_1() {
        let things = [5]
        let actual = Array(combinations(things, length: 1))
        XCTAssertEqual(1, actual.count)
        XCTAssertEqual(1, actual.first?.count)
        XCTAssertEqual(5, actual.first?.first)
    }
    
    func test_combinations_2() {
        let things = [4, 5]
        let expected2 = [
            [4, 5]
        ]
        let expected1 = [
            [4],
            [5]
        ]
        let actual1 = Array(combinations(things, length: 1))
        let actual2 = Array(combinations(things, length: 2))
        XCTAssertEqual(expected1.count, actual1.count)
        
        for i in 0..<expected1.count {
            XCTAssertTrue(matchExists(expected: expected1[i], actual: actual1))
        }
        
        XCTAssertEqual(expected2.count, actual2.count)
        
        for i in 0..<expected2.count {
            XCTAssertTrue(matchExists(expected: expected2[i], actual: actual2))
        }
    }
    
    func test_combinations_3() {
        let things = [3, 4, 5]
        let expected3 = [
            [3, 4, 5]
        ]
        let expected2 = [
            [3, 4],
            [3, 5],
            [4, 5]
        ]
        let expected1 = [
            [3],
            [4],
            [5]
        ]
        let actual1 = Array(combinations(things, length: 1))
        let actual2 = Array(combinations(things, length: 2))
        let actual3 = Array(combinations(things, length: 3))
        XCTAssertEqual(expected1.count, actual1.count)
        
        for i in 0..<expected1.count {
            XCTAssertTrue(matchExists(expected: expected1[i], actual: actual1))
        }
        
        XCTAssertEqual(expected2.count, actual2.count)
        
        for i in 0..<expected2.count {
            XCTAssertTrue(matchExists(expected: expected2[i], actual: actual2))
        }
        
        XCTAssertEqual(expected3.count, actual3.count)
        
        for i in 0..<expected3.count {
            XCTAssertTrue(matchExists(expected: expected3[i], actual: actual3))
        }
    }
    
    func test_combinations_all() {
        let things = [3, 4, 5]
        let expected = [
            [3, 4, 5],
            [3, 4],
            [3, 5],
            [4, 5],
            [3],
            [4],
            [5]
        ]
        let actual = Array(combinations(things))
        XCTAssertEqual(expected.count, actual.count)
        
        for i in 0..<expected.count {
            XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
        }
    }
    
    
    func test_combinationsWithSum() {
        let things = [20, 15, 10, 5, 5]
        let expected = [
            [15, 10],
            [20, 5],
            [20, 5],
            [15, 5, 5]
        ]
        let actual = combinations(things, withSum: 25)
        XCTAssertEqual(expected.count, actual.count)
        
        for i in 0..<expected.count {
            XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
        }
    }
    
    func matchExists<T>(expected: [T], actual: [[T]]) -> Bool where T: Comparable {
        let expectedSorted = expected.sorted()
        
        for a in actual {
            if expectedSorted == a.sorted() {
                return true
            }
        }
        
        return false
    }
}
