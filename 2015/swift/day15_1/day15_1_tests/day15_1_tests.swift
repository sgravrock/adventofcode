import XCTest

class day15_1_tests: XCTestCase {
	func test_bestCookieScore() {
		let input = ["Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
		             "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
		]
		XCTAssertEqual(62842880, bestCookieScore(input: input))
	}
    
    func test_score() {
        let ingredients = [
            parseInput("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"),
            parseInput("Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
        ]
        let recipe = Recipe(amounts: [
            "Butterscotch": 44,
            "Cinnamon": 56
        ])
        let result = score(recipe: recipe, ingredients: ingredients)
        XCTAssertEqual(62842880, result)
    }
	
    func test_scoreForAttribute() {
        let ingredients = [
            parseInput("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"),
            parseInput("Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
        ]
        let recipe = Recipe(amounts: [
            "Butterscotch": 44,
            "Cinnamon": 56
        ])
        XCTAssertEqual(68, score(attribute: "capacity", recipe: recipe, ingredients: ingredients))
        XCTAssertEqual(80, score(attribute: "durability", recipe: recipe, ingredients: ingredients))
        XCTAssertEqual(152, score(attribute: "flavor", recipe: recipe, ingredients: ingredients))
        XCTAssertEqual(76, score(attribute: "texture", recipe: recipe, ingredients: ingredients))
    }
    
    func test_scoreForAttribute_negative() {
        let ingredients = [
            parseInput("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"),
            parseInput("Cinnamon: capacity 1, durability 3, flavor -2, texture -1, calories 3")
        ]
        let recipe = Recipe(amounts: [
            "Butterscotch": 51,
            "Cinnamon": 49
            ])
        XCTAssertEqual(0, score(attribute: "capacity", recipe: recipe, ingredients: ingredients))
    }

    func test_scoreForAttributeForIngredient() {
        let ingredient = parseInput("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")
        let recipe = Recipe(amounts: ["Butterscotch": 44])
        let result = score(attribute: "capacity", ingredient: ingredient, recipe: recipe)
        XCTAssertEqual(-44, result)
    }
    
	func test_parseInput() {
		let result = parseInput("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")
		XCTAssertEqual("Butterscotch", result.name)
        XCTAssertEqual(-1, result.attributes["capacity"])
		XCTAssertEqual(-2, result.attributes["durability"])
		XCTAssertEqual(6, result.attributes["flavor"])
		XCTAssertEqual(3, result.attributes["texture"])
	}
	
	func test_permute() {
		let result = permute(names: ["A", "B"], amount: 3)
		let expected = Set([
            Recipe(amounts: ["A": 3, "B": 0]),
			Recipe(amounts: ["A": 2, "B": 1]),
			Recipe(amounts: ["A": 1, "B": 2]),
			Recipe(amounts: ["A": 0, "B": 3])
		])
		XCTAssertEqual(expected, result)
	}
    
    func test_permute_3() {
        let result = permute(names: ["A", "B", "C"], amount: 2)
        let expected = Set([
            Recipe(amounts: ["A": 2, "B": 0, "C": 0]),
            Recipe(amounts: ["A": 1, "B": 1, "C": 0]),
            Recipe(amounts: ["A": 1, "B": 0, "C": 1]),
            Recipe(amounts: ["A": 0, "B": 2, "C": 0]),
            Recipe(amounts: ["A": 0, "B": 1, "C": 1]),
            Recipe(amounts: ["A": 0, "B": 0, "C": 2]),
        ])
        XCTAssertEqual(expected, result)
    }
}
