import XCTest

class day16_2_tests: XCTestCase {
    func test_parseLine() {
        let result = parseLine(line: "Sue 1: goldfish: 6, trees: 9, akitas: 0")
        let expected = SueSpec(number: 1, compounds: [
            "goldfish": 6,
            "trees": 9,
            "akitas": 0
            ])
        XCTAssertEqual(expected, result)
    }
    
    func test_whichSue_nil() {
        let sues = [
            SueSpec(number: 1, compounds: [
                "goldfish": 6,
                "trees": 9,
                "akitas": 0
                ]),
            SueSpec(number: 2, compounds: [
                "goldfish": 5,
                "akitas": 0,
                "cats": 7
                ])
        ]
        let sample = [
            "goldfish": 3,
            "akitas": 0
        ]
        XCTAssertNil(whichSue(sues: sues, sample: sample))
    }
    
    func test_whichSue_basic() {
        let sues = [
            SueSpec(number: 1, compounds: [
                "cars": 6,
                "samoyeds": 9,
                "akitas": 0
                ]),
            SueSpec(number: 2, compounds: [
                "cars": 5,
                "akitas": 0,
                "vizslas": 7
                ])
        ]
        let sample = [
            "akitas": 0,
            "vizslas": 7,
            "pomeranians": 5,
            "cars": 5
        ]
        XCTAssertEqual(2, whichSue(sues: sues, sample: sample))
    }
    
    func test_whichSue_catsAreGreater() {
        let sues = [
            SueSpec(number: 1, compounds: ["cats": 5]),
            SueSpec(number: 2, compounds: ["cats": 6])
        ]
        let sample = ["cats": 5]
        XCTAssertEqual(2, whichSue(sues: sues, sample: sample))
    }
    
    func test_whichSue_treesAreGreater() {
        let sues = [
            SueSpec(number: 1, compounds: ["trees": 5]),
            SueSpec(number: 2, compounds: ["trees": 6])
        ]
        let sample = ["trees": 5]
        XCTAssertEqual(2, whichSue(sues: sues, sample: sample))
    }
    
    func test_whichSue_pomeraniansAreLess() {
        let sues = [
            SueSpec(number: 1, compounds: ["pomeranians": 5]),
            SueSpec(number: 2, compounds: ["pomeranians": 6])
        ]
        let sample = ["pomeranians": 6]
        XCTAssertEqual(1, whichSue(sues: sues, sample: sample))
    }
    
    func test_whichSue_goldfishAreLess() {
        let sues = [
            SueSpec(number: 1, compounds: ["goldfish": 5]),
            SueSpec(number: 2, compounds: ["goldfish": 6])
        ]
        let sample = ["goldfish": 6]
        XCTAssertEqual(1, whichSue(sues: sues, sample: sample))
    }
}
