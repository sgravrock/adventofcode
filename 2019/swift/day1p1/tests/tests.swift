import XCTest

class tests: XCTestCase {

    func testFuelRequirements() {
		let input =
"12\n" +
"14\n" +
"1969\n" +
"100756\n"
		XCTAssertEqual(34241, fuelRequirements(input))
    }

}
