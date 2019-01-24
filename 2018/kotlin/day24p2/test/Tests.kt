import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun findMinBoostForImmuneWin_example() {
        val groups = parseArmies(
                """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        assertEquals(
                BoostedResult(boost = 1570, unitsLeft = 51),
                findMinBoostForImmuneWin(groups)
        )
    }

    @Test
    fun fightUntilDone_puzzleInput() {
        val classLoader = UnitGroup::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        val armies = parseArmies(input)
        assertEquals(FightResult.Win("Infection", 21070), fightUntilDone(armies))
    }

    @Test
    fun fightUntilDone_example() {
        val groups = parseArmies(
            """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        val numLeft = fightUntilDone(groups)
        assertEquals(0, groups[0].numUnits)
        assertEquals(0, groups[1].numUnits)
        assertEquals(782, groups[2].numUnits)
        assertEquals(4434, groups[3].numUnits)
        assertEquals(FightResult.Win("Infection", 782 + 4434), numLeft)
    }

    @Test
    fun fight() {
        val armies = parseArmies(
            """
            Immune System:
            905 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            797 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4434 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        fight(armies)
        assertEquals(761, armies[0].numUnits)
        assertEquals(793, armies[1].numUnits)
        assertEquals(4434, armies[2].numUnits)
    }

    @Test
    fun selectTargets() {
        val groups = parseArmies(
            """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        val expected = mapOf(
            groups[1] to groups[2],
            groups[0] to groups[3],
            groups[3] to groups[1],
            groups[2] to groups[0]
        )
        assertEquals(expected, selectTargets(groups))
    }

    @Test
    fun selectTargets_skipsOddGroup() {
        val groups = listOf(
            arbitraryGroup.copy(id = 3, numUnits = 1, army = "a"),
            arbitraryGroup.copy(id = 1, numUnits = 2, attack = 1, army = "b"),
            arbitraryGroup.copy(id = 2, numUnits = 1, attack = 1, army = "b")
        )
        assertEquals(
            mapOf(groups[1] to groups[0], groups[0] to groups[1]),
            selectTargets(groups)
        )
    }

    @Test
    fun selectTargets_skipsDeadGroups() {
        val groups = listOf(
            arbitraryGroup.copy(id = 1, army = "a"),
            arbitraryGroup.copy(id = 2, army = "a"),
            arbitraryGroup.copy(id = 3, numUnits = 0, army = "b"),
            arbitraryGroup.copy(id = 4, numUnits = 1, army = "b")
        )
        assertEquals(
            mapOf(groups[0] to groups[3], groups[3] to groups[0]),
            selectTargets(groups)
        )
    }

    @Test
    fun selectTarget_doesNotSelectIfDamageIs0() {
        val groups = listOf(
            arbitraryGroup.copy(army = "a", attackType = "Slashing"),
            arbitraryGroup.copy(army = "b", immunities = listOf("Slashing"))
        )
        assertEquals(mapOf(groups[1] to groups[0]), selectTargets(groups))
    }

    @Test
    fun targetSelectionOrder_favorsHigherEffectivePower() {
        val groups = listOf(
            arbitraryGroup.copy(numUnits = 2, attack = 2),
            arbitraryGroup.copy(numUnits = 3, attack = 2)
        )
        assertEquals(listOf(groups[1], groups[0]), targetSelectionOrder(groups))
    }

    @Test
    fun targetSelectionOrder_breaksTiesWithInitiative() {
        val groups = listOf(
            arbitraryGroup.copy(numUnits = 2, attack = 3, initiative = 2),
            arbitraryGroup.copy(numUnits = 3, attack = 2, initiative = 1)
        )
        assertEquals(listOf(groups[0], groups[1]), targetSelectionOrder(groups))
    }

    @Test
    fun targetPreferenceOrder_favorsMostDamageDealt() {
        val attacker = arbitraryGroup.copy(
            army = "a",
            numUnits = 1, attack = 5, attackType = "Cold"
        )
        val defenders = listOf(
            arbitraryGroup.copy(army = "b"),
            arbitraryGroup.copy(army = "b", weaknesses = listOf("Cold"))
        )
        assertEquals(
            listOf(defenders[1], defenders[0]),
            targetPreferenceOrder(defenders, attacker, emptyMap())
        )
    }

    @Test
    fun targetPreferenceOrder_breaksDamageTiesWithEffectivePower() {
        val attacker = arbitraryGroup.copy(
            army = "a",
            numUnits = 1, attack = 5, attackType = "Cold"
        )
        val defenders = listOf(
            arbitraryGroup.copy(army = "b", numUnits = 1, attack = 2),
            arbitraryGroup.copy(army = "b", numUnits = 1, attack = 1)
        )
        assertEquals(
            listOf(defenders[0], defenders[1]),
            targetPreferenceOrder(defenders, attacker, emptyMap())
        )
    }

    @Test
    fun targetPreferenceOrder_breaksEffectivePowerTiesWithInitiative() {
        val attacker = arbitraryGroup.copy(
            army = "b",
            numUnits = 1, attack = 5, attackType = "Cold")
        val defenders = listOf(
            arbitraryGroup.copy(army = "a", initiative = 1),
            arbitraryGroup.copy(army = "a", initiative = 2)
        )
        assertEquals(
            listOf(defenders[1], defenders[0]),
            targetPreferenceOrder(defenders, attacker, emptyMap())
        )
    }

    @Test
    fun doAttacks_example() {
        val groups = parseArmies(
            """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        val targets = mapOf(
            groups[1] to groups[2],
            groups[0] to groups[3],
            groups[3] to groups[1],
            groups[2] to groups[0]
        )
        doAttacks(groups, targets)
        assertEquals(17 - 17, groups[0].numUnits)
        assertEquals(989 - 84, groups[1].numUnits)
        assertEquals(801 - 4, groups[2].numUnits)
        assertEquals(4485 - 51, groups[3].numUnits)
    }

    @Test
    fun damageDealtTo_weakness() {
        val attacker = arbitraryGroup.copy(
            army = "b",
            numUnits = 4485,
            attack = 12,
            attackType = "Slashing"
        )
        val defender = arbitraryGroup.copy(
            army = "a",
            weaknesses = listOf("Slashing")
        )
        assertEquals(107640, attacker.damageDealtTo(defender))
    }

    @Test
    fun receiveDamage_greaterThanZeroLeft() {
        val defender = arbitraryGroup.copy(numUnits = 989, hitPoints = 1274)
        defender.receiveDamage(107640)
        assertEquals(989 - 84, defender.numUnits)
    }

    @Test
    fun receiveDamage_lessThanZeroLeft() {
        val defender = arbitraryGroup.copy(numUnits = 83, hitPoints = 1274)
        defender.receiveDamage(107640)
        assertEquals(0, defender.numUnits)
    }

    @Test
    fun testParseArmies() {
        val actual = parseArmies(
            """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (weak to fire, cold; immune to radiation) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        val expected = listOf(
            UnitGroup(
                id = 1,
                army = "Immune System",
                numUnits = 17,
                hitPoints = 5390,
                attack = 4507,
                attackType = "fire",
                weaknesses = listOf("radiation", "bludgeoning"),
                immunities = emptyList(),
                initiative = 2
            ),
            UnitGroup(
                id = 2,
                army = "Immune System",
                numUnits = 989,
                hitPoints = 1274,
                attack = 25,
                attackType = "slashing",
                weaknesses = listOf("bludgeoning", "slashing"),
                immunities = listOf("fire"),
                initiative = 3
            ),
            UnitGroup(
                id = 1,
                army = "Infection",
                numUnits = 801,
                hitPoints = 4706,
                attack = 116,
                attackType = "bludgeoning",
                weaknesses = listOf("radiation"),
                immunities = emptyList(),
                initiative = 1
            ),
            UnitGroup(
                id = 2,
                army = "Infection",
                numUnits = 4485,
                hitPoints = 2961,
                attack = 12,
                attackType = "slashing",
                weaknesses = listOf("fire", "cold"),
                immunities = listOf("radiation"),
                initiative = 4
            )
        )

        assertEquals(expected, actual)
    }
}

val arbitraryGroup = UnitGroup(
    id = -1,
    army = "b",
    numUnits = Int.MAX_VALUE,
    hitPoints = Int.MAX_VALUE,
    attackType = "",
    attack = Int.MAX_VALUE,
    weaknesses = emptyList(),
    immunities = emptyList(),
    initiative = Int.MAX_VALUE
)