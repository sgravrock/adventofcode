import XCTest

class day13_2_tests: XCTestCase {
    func test_addSelf() {
        let input = [
            "Alice would gain 54 happiness units by sitting next to Bob.",
            "Bob would gain 83 happiness units by sitting next to Alice."
        ]
        let expected = [
            Preference(person: "Alice", nextTo: "Bob", delta: 54),
            Preference(person: "Bob", nextTo: "Alice", delta: 83),
            Preference(person: "Self", nextTo: "Alice", delta: 0),
            Preference(person: "Alice", nextTo: "Self", delta: 0),
            Preference(person: "Self", nextTo: "Bob", delta: 0),
            Preference(person: "Bob", nextTo: "Self", delta: 0)
        ]
        let actual = addSelf(input: input)
        XCTAssertEqual(expected.count, actual.count)
        
        for pref in expected {
            XCTAssertTrue(actual.contains(pref))
        }
    }
}
