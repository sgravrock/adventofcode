import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun fightUntilDone_example() {
        val armies = Combat.parse(
            """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        val numLeft = fightUntilDone(armies, true)
        assertEquals(0, armies.immuneSystem.groups[0].numUnits)
        assertEquals(0, armies.immuneSystem.groups[1].numUnits)
        assertEquals(782, armies.infection.groups[0].numUnits)
        assertEquals(4434, armies.infection.groups[1].numUnits)
        assertEquals(782 + 4434, numLeft)
    }

    @Test
    fun fight() {
        val armies = Combat.parse(
            """
            Immune System:
            905 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            797 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4434 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        fight(armies.allGroups(), true)
        assertEquals(761, armies.immuneSystem.groups[0].numUnits)
        assertEquals(793, armies.infection.groups[0].numUnits)
        assertEquals(4434, armies.infection.groups[1].numUnits)
    }

    @Test
    fun selectTargets() {
        val armies = Combat.parse(
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
            3 to 1, // Infection group 2 attacks defending group 2
            1 to 2, // Immune System group 2 attacks defending group 1
            0 to 3, // Immune System group 2 attacks defending group 1
            2 to 0  // Infection group 1 attacks defending group 1
        )
        assertEquals(expected, selectTargets(armies.allGroups()))
    }

    @Test
    fun selectTargets_skipsOddGroup() {
        val armies = Combat(
            Army(
                type = ArmyType.Infection,
                groups = listOf(
                    arbitraryGroup.copy(id = 1, numUnits = 2, attack = 1, army = ArmyType.Infection),
                    arbitraryGroup.copy(id = 2, numUnits = 1, attack = 1, army = ArmyType.Infection)
                )
            ),
            Army(
                type = ArmyType.ImmmuneSystem,
                groups = listOf(
                    arbitraryGroup.copy(id = 3, numUnits = 1, army = ArmyType.ImmmuneSystem)
                )
            )
        )
        val expected = mapOf(
            0 to 2,
            2 to 0
        )
        assertEquals(expected, selectTargets(armies.allGroups()))
    }


    @Test
    fun selectTargets_skipsDeadGroups() {
        val armies = Combat(
            Army(
                type = ArmyType.ImmmuneSystem,
                groups = listOf(
                    arbitraryGroup.copy(id = 1, army = ArmyType.ImmmuneSystem)
                )
            ),
            Army(
                type = ArmyType.Infection,
                groups = listOf(
                    arbitraryGroup.copy(id = 2, numUnits = 0, army = ArmyType.Infection),
                    arbitraryGroup.copy(id = 3, numUnits = 1, army = ArmyType.Infection)
                )
            )
        )
        val expected = mapOf(
            0 to 2,
            2 to 0
        )
        assertEquals(expected, selectTargets(armies.allGroups()))
    }

    @Test
    fun targetSelectionOrder_favorsHigherEffectivePower() {
        val groups = listOf(
            arbitraryGroup.copy(numUnits = 2, attack = 2),
            arbitraryGroup.copy(numUnits = 3, attack = 2)
        )
        assertEquals(listOf(1, 0), targetSelectionOrder(groups))
    }

    @Test
    fun targetSelectionOrder_breaksTiesWithInitiative() {
        val groups = listOf(
            arbitraryGroup.copy(numUnits = 2, attack = 3, initiative = 2),
            arbitraryGroup.copy(numUnits = 3, attack = 2, initiative = 1)
        )
        assertEquals(listOf(0, 1), targetSelectionOrder(groups))
    }

    @Test
    fun targetPreferenceOrder_favorsMostDamageDealt() {
        val groups = listOf(
            arbitraryGroup.copy(
                army = ArmyType.ImmmuneSystem,
                numUnits = 1, attack = 5, attackType = AttackType.Cold
            ),
            arbitraryGroup.copy(army = ArmyType.Infection),
            arbitraryGroup.copy(
                army = ArmyType.Infection,
                weaknesses = listOf(AttackType.Cold)
            )
        )
        assertEquals(listOf(2, 1), targetPreferenceOrder(groups, groups[0], emptyMap()))
    }

    @Test
    fun targetPreferenceOrder_breaksDamageTiesWithEffectivePower() {
        val groups = listOf(
            arbitraryGroup.copy(
                army = ArmyType.Infection,
                numUnits = 1, attack = 5, attackType = AttackType.Cold
            ),
            arbitraryGroup.copy(
                army = ArmyType.ImmmuneSystem, numUnits = 1, attack = 2
            ),
            arbitraryGroup.copy(
                army = ArmyType.ImmmuneSystem, numUnits = 1, attack = 1
            )
        )
        assertEquals(listOf(1, 2), targetPreferenceOrder(groups, groups[0], emptyMap()))
    }

    @Test
    fun targetPreferenceOrder_breaksEffectivePowerTiesWithInitiative() {
        val groups = listOf(
            arbitraryGroup.copy(army = ArmyType.ImmmuneSystem, numUnits = 1, attack = 5, attackType = AttackType.Cold),
            arbitraryGroup.copy(army = ArmyType.Infection, initiative = 1),
            arbitraryGroup.copy(army = ArmyType.Infection, initiative = 2)
        )
        assertEquals(listOf(2, 1), targetPreferenceOrder(groups, groups[0], emptyMap()))
    }

    @Test
    fun doAttacks_example() {
        val armies = Combat.parse(
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
            3 to 1,
            1 to 2,
            0 to 3,
            2 to 0
        )
        doAttacks(armies.allGroups(), targets)
        assertEquals(17 - 17, armies.immuneSystem.groups[0].numUnits)
        assertEquals(989 - 84, armies.immuneSystem.groups[1].numUnits)
        assertEquals(801 - 4, armies.infection.groups[0].numUnits)
        assertEquals(4485 - 51, armies.infection.groups[1].numUnits)
    }

    @Test
    fun damageDealtTo_weakness() {
        val attacker = arbitraryGroup.copy(
            numUnits = 4485,
            attack = 12,
            attackType = AttackType.Slashing
        )
        val defender = arbitraryGroup.copy(weaknesses = listOf(AttackType.Slashing))
        assertEquals(107640, attacker.damageDealtTo(defender))
    }

    @Test
    fun receiveDamage_greaterThanZeroLeft() {
        val defender = arbitraryGroup.copy(numUnits=989, hitPoints=1274)
        defender.receiveDamage(107640)
        assertEquals(989 - 84, defender.numUnits)
    }

    @Test
    fun receiveDamage_lessThanZeroLeft() {
        val defender = arbitraryGroup.copy(numUnits=83, hitPoints=1274)
        defender.receiveDamage(107640)
        assertEquals(0, defender.numUnits)
    }

    @Test
    fun testCombatParse() {
        val actual = Combat.parse(
            """
            Immune System:
            17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
            989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

            Infection:
            801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
            4485 units each with 2961 hit points (weak to fire, cold; immune to radiation) with an attack that does 12 slashing damage at initiative 4
        """.trimIndent()
        )
        val expected = Combat(
            Army(
                type = ArmyType.ImmmuneSystem,
                groups = listOf(
                    UnitGroup(
                        id = 1,
                        army = ArmyType.ImmmuneSystem,
                        numUnits = 17,
                        hitPoints = 5390,
                        attack = 4507,
                        attackType = AttackType.Fire,
                        weaknesses = listOf(AttackType.Radiation, AttackType.Bludgeoning),
                        immunities = emptyList(),
                        initiative = 2
                    ),
                    UnitGroup(
                        id = 2,
                        army = ArmyType.ImmmuneSystem,
                        numUnits = 989,
                        hitPoints = 1274,
                        attack = 25,
                        attackType = AttackType.Slashing,
                        weaknesses = listOf(AttackType.Bludgeoning, AttackType.Slashing),
                        immunities = listOf(AttackType.Fire),
                        initiative = 3
                    )
                )
            ),
            Army(
                type = ArmyType.Infection,
                groups = listOf(
                    UnitGroup(
                        id = 1,
                        army = ArmyType.Infection,
                        numUnits = 801,
                        hitPoints = 4706,
                        attack = 116,
                        attackType = AttackType.Bludgeoning,
                        weaknesses = listOf(AttackType.Radiation),
                        immunities = emptyList(),
                        initiative = 1
                    ),
                    UnitGroup(
                        id = 2,
                        army = ArmyType.Infection,
                        numUnits = 4485,
                        hitPoints = 2961,
                        attack = 12,
                        attackType = AttackType.Slashing,
                        weaknesses = listOf(AttackType.Fire, AttackType.Cold),
                        immunities = listOf(AttackType.Radiation),
                        initiative = 4
                    )
                )
            )
        )
        assertEquals(expected, actual)
    }
}

val arbitraryGroup = UnitGroup(
    id = -1,
    army = ArmyType.Infection,
    numUnits = Int.MAX_VALUE,
    hitPoints = Int.MAX_VALUE,
    attackType = AttackType.Cold,
    attack = Int.MAX_VALUE,
    weaknesses = emptyList(),
    immunities = emptyList(),
    initiative = Int.MAX_VALUE
)