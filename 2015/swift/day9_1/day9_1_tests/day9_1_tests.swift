import XCTest
import Foundation

class day9_1_tests: XCTestCase {
	func test_permute() {
		let input = ["a", "b", "c"]
		// Swift doesn't provide for comparison, hashing, etc of arrays of arrays,
		// so we'll convert each permutation to a string so that we can non-painfully
		// check the results.
		let expected = Set<String>([
			"abc",
			"acb",
			"bac",
			"bca",
			"cab",
			"cba"
		])
		let actual = Set<String>(permute(input).map { $0.joined() })
		XCTAssertEqual(expected, actual)
	}

	func test_shortestPath() {
		let input = [
			"London to Dublin = 464",
			"London to Belfast = 518",
			"Dublin to Belfast = 141"
		]
		let result = shortestPath(input: input)
		XCTAssertEqual(605, result)
	}
}
