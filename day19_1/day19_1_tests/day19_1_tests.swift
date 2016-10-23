import XCTest

class day19_1_tests: XCTestCase {
	func test_possibleMolecules() {
		let config = [
			Replacement(input: "H", output: "HO"),
			Replacement(input: "H", output: "OH"),
			Replacement(input: "O", output: "HH")
		]
		let expected = Set([
			"HOOH",
			"HOHO",
			"OHOH",
			"HOOH",
			"HHHH"
		])
		XCTAssertEqual(expected, try possibleMolecules(Problem(config: config, input: "HOH")))
	}
	
	func test_parseInput() {
		let input = [
			"H => HO",
			"H => OH",
			"O => HH",
			"",
			"HOH"
		]
		let expectedConfig = [
			Replacement(input: "H", output: "HO"),
			Replacement(input: "H", output: "OH"),
			Replacement(input: "O", output: "HH")
		]
		let actual = parseInput(input)
		XCTAssertEqual(expectedConfig, actual.config)
		XCTAssertEqual("HOH", actual.input)
	}
}
