protocol Actor : class {
	var hitPoints: Int { get set }
	var damagePoints: Int { get }
	var armorPoints: Int { get }
}

class Player : Actor {
	let inventory: Set<Item>
	var hitPoints: Int
	
	init(hitPoints: Int, inventory: Set<Item>) {
		self.inventory = inventory
		self.hitPoints = hitPoints
	}
	
	var damagePoints: Int {
		return inventory.map { $0.damage }.reduce(0, +)
	}
	
	var armorPoints: Int {
		return inventory.map { $0.armor }.reduce(0, +)
	}
}

class Boss : Actor {
	let damagePoints: Int
	let armorPoints: Int
	var hitPoints: Int
	
	init(damagePoints: Int, armorPoints: Int, hitPoints: Int) {
		self.damagePoints = damagePoints
		self.armorPoints = armorPoints
		self.hitPoints = hitPoints
	}
}

enum ItemType {
	case Weapon
	case Ring
	case Armor
}

struct Item : Hashable, Equatable {
	let type: ItemType
	let cost: Int
	let damage: Int
	let armor: Int
	
	var hashValue: Int {
		// DJB hash algorighm
		var hash = 5381
		hash = ((hash << 5) &+ hash) &+ type.hashValue
		hash = ((hash << 5) &+ hash) &+ cost.hashValue
		hash = ((hash << 5) &+ hash) &+ damage.hashValue
		hash = ((hash << 5) &+ hash) &+ armor.hashValue
		return hash
	}
	
	static func ==(lhs: Item, rhs: Item) -> Bool {
		return lhs.type == rhs.type &&
			lhs.cost == rhs.cost &&
			lhs.damage == rhs.damage &&
			lhs.armor == rhs.armor
	}
}

func cheapestWin(items: [Item],
                 playerHitPoints: Int,
                 bossDamagePoints: Int,
                 bossArmorPoints: Int,
                 bossHitPoints: Int) -> Int? {
	
	let combos = selectItems(items)
	let winners = combos.filter { (combo: Set<Item>) -> Bool in
		var player = Player(hitPoints: playerHitPoints, inventory: combo)
		var boss = Boss(damagePoints: bossDamagePoints, armorPoints: bossArmorPoints, hitPoints: bossHitPoints)
		return playGame(player: &player, boss: &boss) === player
	}
	let costs = winners.map { (combo: Set<Item>) -> Int in
		return combo.map { $0.cost }.reduce(0, +)
	}
	
	return costs.min()
}

func selectItems(_ items: [Item]) -> Set<Set<Item>> {
	let combos = combinations(items)
	let valid = combos.filter(isValidCombo)
	return Set(valid.map { (combo: [Item]) -> Set<Item> in
		return Set(combo)
	})
}

func isValidCombo(_ combo: [Item]) -> Bool {
	var nByType = Dictionary<ItemType, Int>()
	nByType[.Weapon] = 0
	nByType[.Armor] = 0
	nByType[.Ring] = 0
	
	for item in combo {
		nByType[item.type]! += 1
	}
	
	return nByType[.Weapon]! == 1 && nByType[.Armor]! <= 1 && nByType[.Ring]! <= 2
}

func playGame(player: inout Player, boss: inout Boss) -> Actor {
	while player.hitPoints > 0 &&  boss.hitPoints > 0 {
		turns(player: &player, boss: &boss)
	}
	
	return player.hitPoints > 0 ? player : boss
}

func turns(player: inout Player, boss: inout Boss) {
	applyDamage(attacker: &player, defender: &boss)
	
	if boss.hitPoints > 0 {
		applyDamage(attacker: &boss, defender: &player)
	}
}

// Needs to be generic because upcasting an inout parameter isn't valid in Swift.
// (A consequence of Swift conflating reference mutability and value mutability.)
func applyDamage<Ta, Td>(attacker: inout Ta, defender: inout Td) where Ta: Actor, Td: Actor {
	let damageDone = max(1, attacker.damagePoints - defender.armorPoints)
	defender.hitPoints -= damageDone
}

func combinations<T>(_ things: [T]) -> [[T]] {
	var result: [[T]] = []
	
	for i in 1...things.count {
		let subresult = combinations(length: i, outOf: things.count, startingAt: 0)
		for sr in subresult {
			result.append(sr.map { things[$0] })
		}
	}
	
	return result
}

func combinations(length: Int, outOf: Int, startingAt: Int) -> [[Int]] {
	assert(length >= 1)
	assert(length <= outOf - startingAt)
	
	var wip: [Int] = []
	var result: [[Int]] = []
	
	func generateSub(length: Int, startingAt: Int) {
		if wip.count == length {
			result.append(wip)
			return
		}
		
		for i in startingAt..<outOf {
			wip.append(i)
			generateSub(length: length, startingAt: i + 1)
			wip.removeLast()
		}
	}
	
	generateSub(length: length, startingAt: startingAt)
	return result
}
