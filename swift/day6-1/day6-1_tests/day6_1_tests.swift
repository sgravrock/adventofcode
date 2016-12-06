import XCTest

class day6_1_tests: XCTestCase {
	func test_find_message() {
		let input = [
			"eedadn",
			"drvtee",
			"eandsr",
			"raavrd",
			"atevrs",
			"tsrnev",
			"sdttsa",
			"rasrtv",
			"nssdts",
			"ntnada",
			"svetve",
			"tesnvt",
			"vntsnd",
			"vrdear",
			"dvrsen",
			"enarar",
		]
		XCTAssertEqual("easter", find_message(input))
	}
}
