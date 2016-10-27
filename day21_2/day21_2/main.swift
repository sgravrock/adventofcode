import Foundation

let items = [
	Item(type: .Weapon, cost: 8, damage: 4, armor: 0),
	Item(type: .Weapon, cost: 10, damage: 5, armor: 0),
	Item(type: .Weapon, cost: 25, damage: 6, armor: 0),
	Item(type: .Weapon, cost: 40, damage: 7, armor: 0),
	Item(type: .Weapon, cost: 74, damage: 8, armor: 0),
	Item(type: .Armor, cost: 13, damage: 0, armor: 1),
	Item(type: .Armor, cost: 31, damage: 0, armor: 2),
	Item(type: .Armor, cost: 53, damage: 0, armor: 3),
	Item(type: .Armor, cost: 75, damage: 0, armor: 4),
	Item(type: .Armor, cost: 102, damage: 0, armor: 5),
	Item(type: .Ring, cost: 25, damage: 1, armor: 0),
	Item(type: .Ring, cost: 50, damage: 2, armor: 0),
	Item(type: .Ring, cost: 100, damage: 3, armor: 0),
	Item(type: .Ring, cost: 20, damage: 0, armor: 1),
	Item(type: .Ring, cost: 40, damage: 0, armor: 2),
	Item(type: .Ring, cost: 80, damage: 0, armor: 3)
]
print(mostExpensiveLoss(items: items, playerHitPoints: 100, bossDamagePoints: 8, bossArmorPoints: 2, bossHitPoints: 109))
