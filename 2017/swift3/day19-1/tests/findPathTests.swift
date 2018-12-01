import XCTest

class findPathTests: XCTestCase {
	
	func testExample() {
		let input =
"     |          \n" +
"     |  +--+    \n" +
"     A  |  C    \n" +
" F---|----E|--+ \n" +
"     |  |  |  D \n" +
"     +B-+  +--+ ";
		XCTAssertEqual(findPath(input: input), "ABCDEF")
	}
}
