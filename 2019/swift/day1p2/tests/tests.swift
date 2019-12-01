import XCTest

class tests: XCTestCase {

    func testFuelRequirements() {
		let input =
"14\n" +
"1969\n" +
"100756\n"
		XCTAssertEqual(51314, fuelRequirements(input))
    }
	
	func testFuelRequirement_14() {
		XCTAssertEqual(2, fuelRequirement(forMass: 14))
	}
	
	func testFuelRequirement_1969() {
		XCTAssertEqual(966, fuelRequirement(forMass: 1969))
	}
	
	func testFuelRequirement_100756() {
		XCTAssertEqual(50346, fuelRequirement(forMass: 100756))
	}
}
