import XCTest

func include(_ thing: Any) -> Bool {
    return true
}

class day17_1_tests: XCTestCase {
	func test_combinations_1() {
		let things = ["a"]
        let actual = combinations(things)
        XCTAssertEqual(1, actual.count)
        XCTAssertEqual(1, actual.first?.count)
        XCTAssertEqual("a", actual.first?.first)
	}
	
	func test_combinations_2() {
		let things = ["a", "b"]
		let expected = [
			["a", "b"],
			["a"],
			["b"]
		]
		let actual = combinations(things)
        XCTAssertEqual(expected.count, actual.count)
        
        for i in 0..<expected.count {
            XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
        }
	}
	
	func test_combinations_3() {
		let things = ["a", "b", "c"]
		let expected = [
			["a", "b", "c"],
			["a", "b"],
			["a", "c"],
			["a"],
			["b", "c"],
			["b"],
			["c"]
		]
		let actual = combinations(things)
        XCTAssertEqual(expected.count, actual.count)
        
        for i in 0..<expected.count {
            XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
        }
	}
	
	func test_combinations_dupes() {
		let things = ["a", "a"]
        let expected = [
            ["a"],
            ["a"],
            ["a", "a"]
        ]
        let actual = combinations(things)
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
    
    func test_combinationsWithSum_performance() {
        let things = [1, 2, 3, 4, 5, 1, 2, 3]
        measure {
            _ = combinations(things, withSum: 10)
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
