func bestCookieScore(input: [String]) -> Int? {
    let ingredients = input.map(parseInput)
    let names = ingredients.map { $0.name }
    let recipes = permute(names: names, amount: 100)
    let valid = recipes.filter { isValid(recipe: $0, ingredients: ingredients) }
    let scores = valid.map { score(recipe: $0, ingredients: ingredients) }
    return scores.sorted().last
}

func isValid(recipe: Recipe, ingredients: [Ingredient]) -> Bool {
    let calories = ingredients.map { $0.attributes["calories"]! * recipe.amounts[$0.name]! }.reduce(0, +)
    return calories == 500
}

func score(recipe: Recipe, ingredients: [Ingredient]) -> Int {
    return ["capacity", "durability", "flavor", "texture"]
        .map({ score(attribute: $0, recipe: recipe, ingredients: ingredients) })
        .reduce(1, *)
}

func score(attribute: String, recipe: Recipe, ingredients: [Ingredient]) -> Int {
    let subtotal = ingredients.map { $0.attributes[attribute]! * recipe.amounts[$0.name]! }
        .reduce(0, +)
    return max(0, subtotal)
}

func score(attribute: String, ingredient: Ingredient, recipe: Recipe) -> Int {
    return ingredient.attributes[attribute]! * recipe.amounts[ingredient.name]!
}

struct Ingredient {
    let name: String
    let attributes: Dictionary<String, Int>
}

func parseInput(_ input: String) -> Ingredient {
    let tokens = input
        .replacingOccurrences(of: "[:,]", with: "", options: .regularExpression)
        .components(separatedBy: " ")
    
    return Ingredient(
        name: tokens[0],
        attributes: [
            "capacity": Int(tokens[2])!,
            "durability": Int(tokens[4])!,
            "flavor": Int(tokens[6])!,
            "texture": Int(tokens[8])!,
            "calories": Int(tokens[10])!
        ]
    )
}


struct Recipe : Equatable, Hashable {
    var amounts = Dictionary<String, Int>()
    
    var hashValue: Int {
        return hash(dict: amounts)
    }
    
    static func ==(lhs: Recipe, rhs: Recipe) -> Bool {
        return equals(dict1: lhs.amounts, dict2: rhs.amounts)
    }
}

func hash<TKey, TValue>(dict: Dictionary<TKey, TValue>) -> Int where TKey: Hashable, TValue: Hashable {
    // DJB hash algorighm
    var hash = 5381
    
    for kv in dict {
        hash = ((hash << 5) &+ hash) &+ kv.key.hashValue
        hash = ((hash << 5) &+ hash) &+ kv.value.hashValue
    }
    
    return hash
}

func equals<TKey, TValue>(dict1: Dictionary<TKey, TValue>, dict2: Dictionary<TKey, TValue>) -> Bool
    where TKey: Hashable, TValue: Hashable {
        
        if dict1.count != dict2.count {
            return false
        }
        
        for kv in dict1 {
            if dict2[kv.key] != kv.value {
                return false
            }
        }
        
        return true
}

func permute(names: [String], amount: Int) -> Set<Recipe> {
    return Set(permuteNonUnique(names: names, amount: amount))
}

func permuteNonUnique(names: [String], amount: Int) -> [Recipe] {
    if names.count == 0 {
        return []
    } else if names.count == 1 {
        return [Recipe(amounts: [names.first!: amount])]
    }
    
    return names.map { permute2(names: names, first: $0, amount: amount) }.reduce([], +)
    
}

func permute2(names: [String], first: String, amount: Int) -> [Recipe] {
    let rest = names.filter { $0 != first }
    var result: [Recipe] = []
    
    for firstAmt in 0...amount {
        let subresult = permuteNonUnique(names: rest, amount: amount - firstAmt)
        for var p in subresult {
            p.amounts[first] = firstAmt
            result.append(p)
        }
    }
    
    return result
}
