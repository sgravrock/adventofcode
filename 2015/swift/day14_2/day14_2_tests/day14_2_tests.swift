import XCTest

class day14_2_tests: XCTestCase {
	func test_race() {
		let config = [
			"Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
			"Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
		]
		let r1s = race(reindeer: config, seconds: 1)
		XCTAssertEqual(r1s[0], Score(reindeer: "Dancer", score: 1))
		XCTAssertEqual(r1s[1], Score(reindeer: "Comet", score: 0))
		let r140s = race(reindeer: config, seconds: 140)
		XCTAssertEqual(r140s[0], Score(reindeer: "Dancer", score: 139))
		XCTAssertEqual(r140s[1], Score(reindeer: "Comet", score: 1))
		let r1000s = race(reindeer: config, seconds: 1000)
		XCTAssertEqual(r1000s[0], Score(reindeer: "Dancer", score: 689))
		XCTAssertEqual(r1000s[1], Score(reindeer: "Comet", score: 312))
	}
}
