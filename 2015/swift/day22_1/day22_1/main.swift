import Foundation

let player = Player(hitPoints: 50, mana: 500)
let boss = Boss(hitPoints: 55, damagePoints: 8)
print("Start: \(NSDate())")
print(minManaToWin(player: player, boss: boss, spells: allSpells))
print("End: \(NSDate())")
