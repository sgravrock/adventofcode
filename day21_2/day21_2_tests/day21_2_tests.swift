import XCTest

class day21_3_tests: XCTestCase {
	func test_player_initialDamage() {
		let subject = Player(hitPoints: 1,inventory: Set())
		XCTAssertEqual(0, subject.damagePoints)
	}
	
	func test_player_damageIsSumOfItems() {
		let inventory = Set([
			Item(type: .Weapon, cost: 0, damage: 1, armor: 0),
			Item(type: .Ring, cost: 0, damage: 2, armor: 0),
			])
		let subject = Player(hitPoints: 1,inventory: inventory)
		XCTAssertEqual(3, subject.damagePoints)
	}
	
	func test_player_initialArmor() {
		let subject = Player(hitPoints: 1,inventory: [])
		XCTAssertEqual(0, subject.armorPoints)
	}
	
	func test_player_armorIsSumOfItems() {
		let inventory = Set([
			Item(type: .Weapon, cost: 0, damage: 0, armor: 1),
			Item(type: .Ring, cost: 0, damage: 0, armor: 2),
			])
		let subject = Player(hitPoints: 1,inventory: inventory)
		XCTAssertEqual(3, subject.armorPoints)
	}
	
	func test_turn() {
		var player = Player(hitPoints: 8,
		                    inventory: [Item(type: .Weapon, cost: 0, damage: 5, armor: 5)])
		var boss = Boss(damagePoints: 7, armorPoints: 2, hitPoints: 12)
		turns(player: &player, boss: &boss)
		XCTAssertEqual(boss.hitPoints, 9)
		XCTAssertEqual(player.hitPoints, 6)
		turns(player: &player, boss: &boss)
		XCTAssertEqual(boss.hitPoints, 6)
		XCTAssertEqual(player.hitPoints, 4)
		turns(player: &player, boss: &boss)
		XCTAssertEqual(boss.hitPoints, 3)
		XCTAssertEqual(player.hitPoints, 2)
		turns(player: &player, boss: &boss)
		XCTAssertEqual(boss.hitPoints, 0)
		XCTAssertEqual(player.hitPoints, 2)
	}
	
	func test_turn_damageDoneIsAtLeastOne() {
		var player = Player(hitPoints: 8,
		                    inventory: [Item(type: .Weapon, cost: 0, damage: 0, armor: 0)])
		var boss = Boss(damagePoints: 0, armorPoints: 0, hitPoints: 12)
		turns(player: &player, boss: &boss)
		XCTAssertEqual(7, player.hitPoints)
		XCTAssertEqual(11, boss.hitPoints)
		
	}
	
	func test_game_playerWins() {
		var player = Player(hitPoints: 8,
		                    inventory: [Item(type: .Weapon, cost: 0, damage: 5, armor: 5)])
		var boss = Boss(damagePoints: 7, armorPoints: 2, hitPoints: 12)
		XCTAssertTrue(player === playGame(player: &player, boss: &boss))
	}
	
	func test_game_bossWins() {
		var player = Player(hitPoints: 8,
		                    inventory: [Item(type: .Weapon, cost: 0, damage: 3, armor: 5)])
		var boss = Boss(damagePoints: 7, armorPoints: 2, hitPoints: 12)
		XCTAssertTrue(boss === playGame(player: &player, boss: &boss))
	}
	
	func test_selectItems_requiresExactlyOneWeapon() {
		let w1 = Item(type: .Weapon, cost: 0, damage: 1, armor: 0)
		let w2 = Item(type: .Weapon, cost: 0, damage: 2, armor: 0)
		let r = Item(type: .Ring, cost: 0, damage: 0, armor: 1)
		let items = [w1, w2, r]
		let expected = Set<Set<Item>>([
			Set<Item>([w1]),
			Set<Item>([w1, r]),
			Set<Item>([w2]),
			Set<Item>([w2, r])
			])
		XCTAssertEqual(expected, selectItems(items))
	}
	
	func test_selectItems_allowsUpToOneArmor() {
		let a1 = Item(type: .Armor, cost: 0, damage: 1, armor: 0)
		let a2 = Item(type: .Armor, cost: 0, damage: 2, armor: 0)
		let w = Item(type: .Weapon, cost: 0, damage: 0, armor: 1)
		let items = [a1, a2, w]
		let expected = Set<Set<Item>>([
			Set<Item>([w]),
			Set<Item>([w, a1]),
			Set<Item>([w, a2])
			])
		XCTAssertEqual(expected, selectItems(items))
	}
	
	func test_selectItems_allowsUpToTwoRings() {
		let r1 = Item(type: .Ring, cost: 0, damage: 1, armor: 0)
		let r2 = Item(type: .Ring, cost: 0, damage: 2, armor: 0)
		let r3 = Item(type: .Ring, cost: 0, damage: 3, armor: 0)
		let w = Item(type: .Weapon, cost: 0, damage: 0, armor: 1)
		let items = [r1, r2, r3, w]
		let expected = Set<Set<Item>>([
			Set<Item>([w]),
			Set<Item>([w, r1]),
			Set<Item>([w, r2]),
			Set<Item>([w, r3]),
			Set<Item>([w, r1, r2]),
			Set<Item>([w, r1, r3]),
			Set<Item>([w, r2, r3])
			])
		XCTAssertEqual(expected, selectItems(items))
	}
	
	func test_mostExpensiveLoss() {
		let w1 = Item(type: .Weapon, cost: 1, damage: 200, armor: 5)
		let w2 = Item(type: .Weapon, cost: 2, damage: 500, armor: 5)
		let w3 = Item(type: .Weapon, cost: 4, damage: 0, armor: 0)
		let expected = 4 // w3
		let actual = mostExpensiveLoss(items: [w1, w2, w3],
		                         playerHitPoints: 8,
		                         bossDamagePoints: 7,
		                         bossArmorPoints: 2,
		                         bossHitPoints: 12)
		XCTAssertEqual(expected, actual)
	}
	
	func test_combinations_1() {
		let things = ["a"]
		let actual = combinations(things)
		XCTAssertEqual(1, actual.count)
		XCTAssertEqual(1, actual.first?.count)
		XCTAssertEqual("a", actual.first?.first)
	}
	
	func test_combinations_2() {
		let things = ["a", "b"]
		let expected = [
			["a", "b"],
			["a"],
			["b"]
		]
		let actual = combinations(things)
		XCTAssertEqual(expected.count, actual.count)
		
		for i in 0..<expected.count {
			XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
		}
	}
	
	func test_combinations_3() {
		let things = ["a", "b", "c"]
		let expected = [
			["a", "b", "c"],
			["a", "b"],
			["a", "c"],
			["a"],
			["b", "c"],
			["b"],
			["c"]
		]
		let actual = combinations(things)
		XCTAssertEqual(expected.count, actual.count)
		
		for i in 0..<expected.count {
			XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
		}
	}
	
	func test_combinations_dupes() {
		let things = ["a", "a"]
		let expected = [
			["a"],
			["a"],
			["a", "a"]
		]
		let actual = combinations(things)
		XCTAssertEqual(expected.count, actual.count)
		
		for i in 0..<expected.count {
			XCTAssertTrue(matchExists(expected: expected[i], actual: actual))
		}
	}
	
	func matchExists<T>(expected: [T], actual: [[T]]) -> Bool where T: Comparable {
		let expectedSorted = expected.sorted()
		
		for a in actual {
			if expectedSorted == a.sorted() {
				return true
			}
		}
		
		return false
	}
}
