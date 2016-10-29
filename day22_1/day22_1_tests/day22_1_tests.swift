import XCTest

class day22_1_tests: XCTestCase {
	func test_player_cast() {
		let subject = Player(hitPoints: 1, mana: 100)
		let boss = Boss(hitPoints: 4, damagePoints: 1)
		let spell = Spell(cost: 1, damage: 2, healing: 3,
		                  effect: Effect(duration: 1, armor: 2, damage: 3, mana: 4))
		subject.cast(spell: spell, target: boss)
		XCTAssertEqual(99, subject.mana)
		XCTAssertEqual(spell.effect, subject.pendingEffect)
		XCTAssertEqual(2, boss.hitPoints)
		XCTAssertEqual(4, subject.hitPoints)
	}
	
	func test_player_cast_noEffect() {
		let subject = Player(hitPoints: 1, mana: 100)
		let boss = Boss(hitPoints: 4, damagePoints: 1)
		let spell = Spell(cost: 1, damage: 2, healing: 3, effect: nil)
		subject.cast(spell: spell, target: boss)
		XCTAssertEqual(99, subject.mana)
		XCTAssertEqual(2, boss.hitPoints)
		XCTAssertEqual(4, subject.hitPoints)
	}

	func test_turns() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 13, damagePoints: 8)
		
		player.cast(spell: poison, target: boss)
		XCTAssertEqual(13, boss.hitPoints)		// no damage yet
		XCTAssertEqual(77, player.mana)
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(13, boss.hitPoints)		// poison has done no damage yet
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(10, boss.hitPoints)		// poison does 3 damage
		XCTAssertEqual(5, player.activeEffects[0].duration)
		XCTAssertEqual(2, player.hitPoints)		// boss does 8 damage

		player.cast(spell: magicMissile, target: boss)
		XCTAssertEqual(6, boss.hitPoints)		// Magic Missile does 4 instant damage
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(3, boss.hitPoints)		// Poison does 3 damage
		XCTAssertEqual(4, player.activeEffects[0].duration)
		
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(0, boss.hitPoints)		// Poison does 3 damage
		XCTAssertEqual(2, player.hitPoints)		// boss did 0 damage because dead
	}
	
	func test_turns_secondExample() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 14, damagePoints: 8)
		
		/*
		-- Player turn --
		- Player has 10 hit points, 0 armor, 250 mana
		- Boss has 14 hit points
		Player casts Recharge.
		*/
		player.cast(spell: recharge, target: boss)
		XCTAssertEqual(14, boss.hitPoints)
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(21, player.mana)

		/*
		-- Boss turn --
		- Player has 10 hit points, 0 armor, 21 mana
		- Boss has 14 hit points
		Recharge provides 101 mana; its timer is now 4.
		Boss attacks for 8 damage!
		*/
		XCTAssertEqual(10, player.hitPoints)
		XCTAssertEqual(14, boss.hitPoints)
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(1, player.activeEffects.count)
		XCTAssertEqual(4, player.activeEffects[0].duration)
		XCTAssertEqual(122, player.mana)

		/*
		-- Player turn --
		- Player has 2 hit points, 0 armor, 122 mana
		- Boss has 14 hit points
		Recharge provides 101 mana; its timer is now 3.
		Player casts Shield, increasing armor by 7.
		*/
		XCTAssertEqual(2, player.hitPoints)
		XCTAssertEqual(14, boss.hitPoints)
		player.cast(spell: shield, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(110, player.mana)	// 122 + 101 (recharge) - 113 (shield)
		XCTAssertEqual(2, player.activeEffects.count)
		XCTAssertEqual(3, player.activeEffects[0].duration)
		XCTAssertEqual(7, player.armorPoints)

		/*
		-- Boss turn --
		- Player has 2 hit points, 7 armor, 110 mana
		- Boss has 14 hit points
		Shield's timer is now 5.
		Recharge provides 101 mana; its timer is now 2.
		Boss attacks for 8 - 7 = 1 damage!
		*/
		XCTAssertEqual(2, player.hitPoints)
		XCTAssertEqual(14, boss.hitPoints)
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(2, player.activeEffects.count)
		XCTAssertEqual(2, player.activeEffects[0].duration)
		XCTAssertEqual(5, player.activeEffects[1].duration)
		XCTAssertEqual(211, player.mana)
		
		/*
		-- Player turn --
		- Player has 1 hit point, 7 armor, 211 mana
		- Boss has 14 hit points
		Shield's timer is now 4.
		Recharge provides 101 mana; its timer is now 1.
		Player casts Drain, dealing 2 damage, and healing 2 hit points.
		*/
		XCTAssertEqual(1, player.hitPoints)
		XCTAssertEqual(14, boss.hitPoints)
		player.cast(spell: drain, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(2, player.activeEffects.count)		// drain has no effects
		XCTAssertEqual(1, player.activeEffects[0].duration)	// recharge
		XCTAssertEqual(4, player.activeEffects[1].duration)	// shield

		
		/*
		-- Boss turn --
		- Player has 3 hit points, 7 armor, 239 mana
		- Boss has 12 hit points
		Shield's timer is now 3.
		Recharge provides 101 mana; its timer is now 0.
		Recharge wears off.
		Boss attacks for 8 - 7 = 1 damage!
		*/
		XCTAssertEqual(3, player.hitPoints)
		XCTAssertEqual(12, boss.hitPoints)
		XCTAssertEqual(239, player.mana)
		XCTAssertEqual(2, player.activeEffects.count)
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(1, player.activeEffects.count)
		XCTAssertEqual(3, player.activeEffects[0].duration)
		
		/*
		-- Player turn --
		- Player has 2 hit points, 7 armor, 340 mana
		- Boss has 12 hit points
		Shield's timer is now 2.
		Player casts Poison.
		*/
		
		XCTAssertEqual(2, player.hitPoints)
		XCTAssertEqual(7, player.armorPoints)
		XCTAssertEqual(340, player.mana)
		player.cast(spell: poison, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(2, player.activeEffects.count)
		XCTAssertEqual(2, player.activeEffects[0].duration)
		
		/*
		-- Boss turn --
		- Player has 2 hit points, 7 armor, 167 mana
		- Boss has 12 hit points
		Shield's timer is now 1.
		Poison deals 3 damage; its timer is now 5.
		Boss attacks for 8 - 7 = 1 damage!
		*/
		XCTAssertEqual(2, player.hitPoints)
		XCTAssertEqual(7, player.armorPoints)
		XCTAssertEqual(167, player.mana)
		XCTAssertEqual(12, boss.hitPoints)
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(2, player.activeEffects.count)
		XCTAssertEqual(1, player.activeEffects[0].duration)
		XCTAssertEqual(5, player.activeEffects[1].duration)
		
		/*
		-- Player turn --
		- Player has 1 hit point, 7 armor, 167 mana
		- Boss has 9 hit points
		Shield's timer is now 0.
		Shield wears off, decreasing armor by 7.
		Poison deals 3 damage; its timer is now 4.
		Player casts Magic Missile, dealing 4 damage.
		*/
		XCTAssertEqual(1, player.hitPoints)
		XCTAssertEqual(7, player.armorPoints)
		XCTAssertEqual(167, player.mana)
		XCTAssertEqual(9, boss.hitPoints)
		player.cast(spell: magicMissile, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertEqual(1, player.activeEffects.count)
		XCTAssertEqual(4, player.activeEffects[0].duration)

		/*
		-- Boss turn --
		- Player has 1 hit point, 0 armor, 114 mana
		- Boss has 2 hit points
		Poison deals 3 damage. This kills the boss, and the player wins.
		*/
		XCTAssertEqual(1, player.hitPoints)
		XCTAssertEqual(2, boss.hitPoints)
		bossTurn(player: player, boss: boss)
		XCTAssertEqual(-1, boss.hitPoints)
	}
	
	func test_spellArmorEffects() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 10)
		let spell = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 2, damage: 0, mana: 0))
		player.cast(spell: spell, target: boss)
		player.applyEffects(target: boss)
		boss.attack(player: player)
		XCTAssertEqual(2, player.hitPoints)
	}
	
	func test_boss_doesAtLeastOneDamage() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 2)
		let spell = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 2, damage: 0, mana: 0))
		player.cast(spell: spell, target: boss)
		player.applyEffects(target: boss)
		boss.attack(player: player)
		XCTAssertEqual(9, player.hitPoints)
	}
	
	func test_player_cantCastSpellWithSameEffectAsActive() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 2)
		let spell1 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 2, armor: 2, damage: 0, mana: 0))
		let spell2 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 1, damage: 0, mana: 0))
		player.cast(spell: spell1, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertFalse(player.canCast(spell: spell2))
	}
	
	func test_player_canCastSpellWithDifferentEffectAsActive() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 2)
		let spell1 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 2, armor: 2, damage: 0, mana: 0))
		let spell2 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 0, damage: 1, mana: 0))
		player.cast(spell: spell1, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertTrue(player.canCast(spell: spell2))
	}
	
	func test_player_canCastSpellWitNoEffect() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 2)
		let spell1 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 2, armor: 2, damage: 0, mana: 0))
		let spell2 = Spell(cost: 0, damage: 0, healing: 0, effect: nil)
		player.cast(spell: spell1, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertTrue(player.canCast(spell: spell2))
	}

	func test_player_cantCastSpellWithSameEffectAsSpellEndingThisTurn() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 2)
		let spell1 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 2, damage: 0, mana: 0))
		let spell2 = Spell(cost: 0, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 1, damage: 0, mana: 0))
		player.cast(spell: spell1, target: boss)
		playerTurn(player: player, boss: boss)
		XCTAssertTrue(player.canCast(spell: spell2))
	}
	
	func test_player_needsEnoughManaToCast() {
		let player = Player(hitPoints: 10, mana: 10)
		let spell = Spell(cost: 11, damage: 0, healing: 0, effect: Effect(duration: 1, armor: 1, damage: 0, mana: 0))
		XCTAssertFalse(player.canCast(spell: spell))
	}
	
	func test_currentGameState() {
		let player = Player(hitPoints: 10, mana: 250)
		let boss = Boss(hitPoints: 5, damagePoints: 2)
		XCTAssertEqual(.Running, currentGameState(player: player, boss: boss))
		
		player.hitPoints = 0
		XCTAssertEqual(.BossWon, currentGameState(player: player, boss: boss))
		
		player.hitPoints = 1
		boss.hitPoints = 0
		XCTAssertEqual(.PlayerWon, currentGameState(player: player, boss: boss))
	}
}
