import XCTest

class findPathLengthTests: XCTestCase {
	
	func testExample() {
		let input =
"     |          \n" +
"     |  +--+    \n" +
"     A  |  C    \n" +
" F---|----E|--+ \n" +
"     |  |  |  D \n" +
"     +B-+  +--+ ";
		XCTAssertEqual(findPathLength(input: input), 38)
	}
}
