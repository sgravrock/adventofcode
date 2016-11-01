import XCTest

class day24_1_tests: XCTestCase {
    func test_configsWithEqualWeight() {
		let packages = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]
		// Not an exhaustive list
        let expected = [
            Config(passenger: [11, 9], left: [10, 8, 2], right: [7, 5, 4, 3, 1]),
            Config(passenger: [10, 9, 1], left: [11, 7, 2,], right: [8, 5, 4, 3]),
            Config(passenger: [10, 8, 2], left: [11, 9], right: [7, 5, 4, 3, 1]),
            Config(passenger: [10, 7, 3], left: [11, 9], right: [8, 5, 4, 2, 1]),
            Config(passenger: [10, 5, 4, 1], left: [11, 9], right: [8, 7, 3, 2]),
            Config(passenger: [10, 5, 3, 2], left: [11, 9], right: [8, 7, 4, 1]),
            Config(passenger: [10, 4, 3, 2, 1], left: [11, 9], right: [8, 7, 5]),
            Config(passenger: [9, 8, 3], left: [11, 7, 2], right: [10, 5, 4, 1]),
            Config(passenger: [9, 7, 4], left: [11, 8, 1], right: [10, 5, 3, 2]),
            Config(passenger: [9, 5, 4, 2], left: [11, 8, 1], right: [10, 7, 3]),
            Config(passenger: [8, 7, 5], left: [11, 9], right: [10, 4, 3, 2, 1]),
            Config(passenger: [8, 5, 4, 3], left: [11, 9], right: [10, 7, 2, 1]),
            Config(passenger: [7, 5, 4, 3, 1], left: [11, 9], right: [10, 8, 2])
        ]
		let actual = configsWithEqualWeight(packages: packages)
		
		for a in actual {
			let n = sum(a.passenger)
			XCTAssertEqual(n, sum(a.left))
			XCTAssertEqual(n, sum(a.right))
		}

		for e in expected {
			XCTAssertTrue(actual.contains(e))
		}
    }
	
	func test_configsWithMinPassenger() {
		let packages = [1, 2, 3, 4, 5]
		let expected = Set([
			Config(passenger: [5], left: [4, 1], right: [2, 3]),
			Config(passenger: [5], left: [2, 3], right: [4, 1]),
		])
		let actual = configsWithMinPassenger(packages: packages)
		XCTAssertEqual(expected, actual)
	}
	
	func test_idealConfiguration() {
		let packages = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]
		let actual = idealConfigurations(packages: packages)
		XCTAssertLessThan(0, actual.count)
		
		for config in actual {
			XCTAssertEqual(Set([9, 11]), config.passenger)
		}
	}
	
	func test_config_quantumEntanglement() {
		XCTAssertEqual(99, Config(passenger: [11, 9], left: [], right: []).quantumEntanglement)
		XCTAssertEqual(90, Config(passenger: [10, 9, 1], left: [], right: []).quantumEntanglement)
	}
    
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
