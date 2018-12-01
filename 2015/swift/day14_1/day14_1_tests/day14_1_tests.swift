import XCTest

class day14_1_tests: XCTestCase {
    func test_bestDistanceTraveled() {
        let config = [
            "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
            "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
        ]
        let result = bestDistanceTraveled(reindeer: config, seconds: 1000)
        XCTAssertEqual(1120, result)
    }
    
    func test_parseLine() {
        let line = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
        let expected = Reindeer(name: "Comet", speed: 14, duration: 10, rest: 127)
        XCTAssertEqual(expected, parseLine(line))
    }
    
    func test_distanceTraveled() {
        let reindeer = Reindeer(name: "Comet", speed: 14, duration: 10, rest: 127)
        XCTAssertEqual(14, distanceTraveled(reindeer: reindeer, seconds: 1))
        XCTAssertEqual(140, distanceTraveled(reindeer: reindeer, seconds: 10))
        XCTAssertEqual(140, distanceTraveled(reindeer: reindeer, seconds: 11))
        XCTAssertEqual(140, distanceTraveled(reindeer: reindeer, seconds: 137))
        XCTAssertEqual(154, distanceTraveled(reindeer: reindeer, seconds: 138))
    }
}
