import XCTest

class day13_1_tests: XCTestCase {
    func test_totalHappiness() {
        let input = ["Alice would gain 54 happiness units by sitting next to Bob.",
            "Alice would lose 79 happiness units by sitting next to Carol.",
            "Alice would lose 2 happiness units by sitting next to David.",
            "Bob would gain 83 happiness units by sitting next to Alice.",
            "Bob would lose 7 happiness units by sitting next to Carol.",
            "Bob would lose 63 happiness units by sitting next to David.",
            "Carol would lose 62 happiness units by sitting next to Alice.",
            "Carol would gain 60 happiness units by sitting next to Bob.",
            "Carol would gain 55 happiness units by sitting next to David.",
            "David would gain 46 happiness units by sitting next to Alice.",
            "David would lose 7 happiness units by sitting next to Bob.",
            "David would gain 41 happiness units by sitting next to Carol."
        ]
        let arrangement = ["Alice", "Bob", "Carol", "David"]
        XCTAssertEqual(330, totalHappiness(input: input, arrangement: arrangement))
    }
    
    func test_bestHappiness() {
        let input = ["Alice would gain 54 happiness units by sitting next to Bob.",
                     "Alice would lose 79 happiness units by sitting next to Carol.",
                     "Alice would lose 2 happiness units by sitting next to David.",
                     "Bob would gain 83 happiness units by sitting next to Alice.",
                     "Bob would lose 7 happiness units by sitting next to Carol.",
                     "Bob would lose 63 happiness units by sitting next to David.",
                     "Carol would lose 62 happiness units by sitting next to Alice.",
                     "Carol would gain 60 happiness units by sitting next to Bob.",
                     "Carol would gain 55 happiness units by sitting next to David.",
                     "David would gain 46 happiness units by sitting next to Alice.",
                     "David would lose 7 happiness units by sitting next to Bob.",
                     "David would gain 41 happiness units by sitting next to Carol."
        ]
        XCTAssertEqual(330, bestHappiness(input: input))
    }
    
    func test_parseLine() {
        XCTAssertEqual(Preference(person: "Alice", nextTo: "Bob", delta: 54),
                       parseLine("Alice would gain 54 happiness units by sitting next to Bob."))
        XCTAssertEqual(Preference(person: "David", nextTo: "Bob", delta: -7),
                       parseLine("David would lose 7 happiness units by sitting next to Bob."))
    }
    
    func test_permute() {
        let input = ["a", "b", "c"]
        // Swift doesn't provide for comparison, hashing, etc of arrays of arrays,
        // so we'll convert each permutation to a string so that we can non-painfully
        // check the results.
        let expected = Set<String>([
            "abc",
            "acb",
            "bac",
            "bca",
            "cab",
            "cba"
            ])
        let actual = Set<String>(permute(input).map { $0.joined() })
        XCTAssertEqual(expected, actual)
    }
}
