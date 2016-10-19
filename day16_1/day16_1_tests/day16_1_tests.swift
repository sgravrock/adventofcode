import XCTest

class day16_1_tests: XCTestCase {
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
    
    func test_whichSue_notNil() {
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
            "akitas": 0,
            "cats": 7,
            "pomeranians": 5,
            "goldfish": 5
        ]
        XCTAssertEqual(2, whichSue(sues: sues, sample: sample))
    }
}
