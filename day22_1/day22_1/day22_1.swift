func minManaToWin(player: Player, boss: Boss) -> Int? {
	let validSpells = spells.filter { player.canCast(spell: $0) }	
	let results = validSpells
		.map({ (s: Spell) -> Int? in
			return minManaToWin(player: player.clone(), boss: boss.clone(), nextSpell: s)
		})
		.filter { $0 != nil }
		.sorted(by: {$0! < $1! })
	
	if let best = results.first {
		return best
	}
	
	return nil
}

func minManaToWin(player: Player, boss: Boss, nextSpell: Spell) -> Int? {
	player.cast(spell: nextSpell, target: boss)
	playerTurn(player: player, boss: boss)
	bossTurn(player: player, boss: boss)
	
	switch currentGameState(player: player, boss: boss) {
	case .BossWon:
		return nil
	case .PlayerWon:
		return nextSpell.cost
	case .Running:
		if let subresult = minManaToWin(player: player.clone(), boss: boss.clone()) {
			return subresult + nextSpell.cost
		} else {
			return nil
		}
	}
}

struct Spell {
	let cost: Int
	let damage: Int
	let healing: Int
	let effect: Effect?
}

struct Effect : Equatable {
	let duration: Int
	let armor: Int
	let damage: Int
	let mana: Int
	
	static func ==(lhs: Effect, rhs: Effect) -> Bool {
		return lhs.duration == rhs.duration &&
			lhs.armor == rhs.armor &&
			lhs.damage == rhs.damage &&
			lhs.mana == rhs.mana
	}
}

let magicMissile = Spell(cost: 53, damage: 4, healing: 0, effect: nil)
let drain = Spell(cost: 73, damage: 2, healing: 2, effect: nil)
let shield = Spell(cost: 113, damage: 0, healing: 0, effect: Effect(duration: 6, armor: 7, damage: 0, mana: 0))
let poison = Spell(cost: 173, damage: 0, healing: 0, effect: Effect(duration: 6, armor: 0, damage: 3, mana: 0))
let recharge = Spell(cost: 229, damage: 0, healing: 0, effect: Effect(duration: 5, armor: 0, damage: 0, mana: 101))
let spells = [
	magicMissile,
	drain,
	shield,
	poison,
	recharge,
]


class Player {
	var activeEffects: [Effect]
	var pendingEffect: Effect?
	var hitPoints: Int
	var mana: Int
	var armorPoints: Int {
		return activeEffects.map { $0.armor }.reduce(0, +)
	}
	
	init(hitPoints: Int, mana: Int) {
		self.hitPoints = hitPoints
		self.mana = mana
		self.activeEffects = []
	}
	
	func clone() -> Player {
		let other = Player(hitPoints: hitPoints, mana: mana)
		other.activeEffects = activeEffects
		other.pendingEffect = pendingEffect
		return other
	}
	
	func canCast(spell: Spell) -> Bool {
		if spell.cost > mana {
			return false
		}
		if spell.effect == nil {
			return true
		}

		let match = activeEffects.first(where: { (e: Effect) -> Bool in
			return e.duration > 1 && effectsOverlap(a: e, b: spell.effect!)
		})
		return match == nil
	}
	
	func cast(spell: Spell, target: Boss) {
		assert(canCast(spell: spell))
		if let effect = spell.effect {
			assert(pendingEffect == nil)
			pendingEffect = effect
		}
		
		mana -= spell.cost
		hitPoints += spell.healing
		target.hitPoints -= spell.damage
	}
	
	func applyEffects(target: Boss) {
		for e in activeEffects {
			target.hitPoints -= e.damage
			mana += e.mana
		}
		ageEffects()
		
		if let e = pendingEffect {
			activeEffects.append(e)
			pendingEffect = nil
		}
	}
	
	func ageEffects() {
		activeEffects = activeEffects
			.filter { $0.duration > 1 }
			.map { Effect(duration: $0.duration - 1, armor: $0.armor, damage: $0.damage, mana: $0.mana) }
	}
}

func effectsOverlap(a: Effect, b: Effect) -> Bool {
	return (a.armor != 0 && b.armor != 0) ||
		(a.damage != 0 && b.damage != 0) ||
		(a.mana != 0 && b.mana != 0)
}

class Boss {
	let damagePoints: Int
	var hitPoints: Int
	
	init(hitPoints: Int, damagePoints: Int) {
		self.damagePoints = damagePoints
		self.hitPoints = hitPoints
	}
	
	func clone() -> Boss {
		return Boss(hitPoints: hitPoints, damagePoints: damagePoints)
	}
	
	func attack(player: Player) {
		let damageDone = max(1, damagePoints - player.armorPoints)
		player.hitPoints -= damageDone
	}
}

enum GameState {
	case Running
	case PlayerWon
	case BossWon
}

func currentGameState(player: Player, boss: Boss) -> GameState {
	if player.hitPoints <= 0 {
		return .BossWon
	} else if boss.hitPoints <= 0 {
		return .PlayerWon
	} else {
		return .Running
	}
}

func turns(player: Player, boss: Boss) {
	playerTurn(player: player, boss: boss)
	bossTurn(player: player, boss: boss)
}

func playerTurn(player: Player, boss: Boss) {
	player.applyEffects(target: boss)
}

func bossTurn(player: Player, boss: Boss) {
	player.applyEffects(target: boss)

	if boss.hitPoints > 0 {
		boss.attack(player: player)
	}
}
