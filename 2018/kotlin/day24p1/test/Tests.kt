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
        fight(armies, true)
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
        val immuneExpected = mapOf(
            armies.immuneSystem.groups[1] to armies.infection.groups[0],
            armies.immuneSystem.groups[0] to armies.infection.groups[1]
        )
        val infectionExpected = mapOf(
            armies.infection.groups[1] to armies.immuneSystem.groups[1],
            armies.infection.groups[0] to armies.immuneSystem.groups[0]
        )
        assertEquals(
            immuneExpected,
            selectTargets(armies.immuneSystem.groups, armies.infection.groups)
        )
        assertEquals(
            infectionExpected,
            selectTargets(armies.infection.groups, armies.immuneSystem.groups)
        )
    }

    @Test
    fun selectTargets_skipsOddGroup() {
        val armies = Combat(
            Army(
                type = ArmyType.ImmmuneSystem,
                groups = listOf(
                    arbitraryGroup.copy(id = 3, numUnits = 1, army = ArmyType.ImmmuneSystem)
                )
            ),
            Army(
                type = ArmyType.Infection,
                groups = listOf(
                    arbitraryGroup.copy(id = 1, numUnits = 2, attack = 1, army = ArmyType.Infection),
                    arbitraryGroup.copy(id = 2, numUnits = 1, attack = 1, army = ArmyType.Infection)
                )
            )
        )
        assertEquals(
            mapOf(armies.infection.groups[0] to armies.immuneSystem.groups[0]),
            selectTargets(armies.infection.groups, armies.immuneSystem.groups)
        )
        assertEquals(
            mapOf(armies.immuneSystem.groups[0] to armies.infection.groups[0]),
            selectTargets(armies.immuneSystem.groups, armies.infection.groups)
        )
    }

    @Test
    fun selectTargets_skipsDeadGroups() {
        val armies = Combat(
            Army(
                type = ArmyType.ImmmuneSystem,
                groups = listOf(
                    arbitraryGroup.copy(id = 1, army = ArmyType.ImmmuneSystem),
                    arbitraryGroup.copy(id = 2, army = ArmyType.ImmmuneSystem)
                )
            ),
            Army(
                type = ArmyType.Infection,
                groups = listOf(
                    arbitraryGroup.copy(id = 3, numUnits = 0, army = ArmyType.Infection),
                    arbitraryGroup.copy(id = 4, numUnits = 1, army = ArmyType.Infection)
                )
            )
        )
        assertEquals(
            mapOf(armies.immuneSystem.groups[0] to armies.infection.groups[1]),
            selectTargets(armies.immuneSystem.groups, armies.infection.groups)
        )
        assertEquals(
            mapOf(armies.infection.groups[1] to armies.immuneSystem.groups[0]),
            selectTargets(armies.infection.groups, armies.immuneSystem.groups)
        )
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
            numUnits = 1, attack = 5, attackType = AttackType.Cold
        )
        val defenders = listOf(
            arbitraryGroup,
            arbitraryGroup.copy(weaknesses = listOf(AttackType.Cold))
        )
        assertEquals(
            listOf(defenders[1], defenders[0]),
            targetPreferenceOrder(defenders, attacker, emptyMap())
        )
    }

    @Test
    fun targetPreferenceOrder_breaksDamageTiesWithEffectivePower() {
        val attacker = arbitraryGroup.copy(
            numUnits = 1, attack = 5, attackType = AttackType.Cold
        )
        val defenders = listOf(
            arbitraryGroup.copy(numUnits = 1, attack = 2),
            arbitraryGroup.copy(numUnits = 1, attack = 1)
        )
        assertEquals(
            listOf(defenders[0], defenders[1]),
            targetPreferenceOrder(defenders, attacker, emptyMap())
        )
    }

    @Test
    fun targetPreferenceOrder_breaksEffectivePowerTiesWithInitiative() {
        val attacker = arbitraryGroup.copy(numUnits = 1, attack = 5, attackType = AttackType.Cold)
        val defenders = listOf(
            arbitraryGroup.copy(initiative = 1),
            arbitraryGroup.copy(initiative = 2)
        )
        assertEquals(
            listOf(defenders[1], defenders[0]),
            targetPreferenceOrder(defenders, attacker, emptyMap())
        )
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
        val immuneTargets = mapOf(
            armies.immuneSystem.groups[1] to armies.infection.groups[0],
            armies.immuneSystem.groups[0] to armies.infection.groups[1]
        )
        val infectionTargets = mapOf(
            armies.infection.groups[1] to armies.immuneSystem.groups[1],
            armies.infection.groups[0] to armies.immuneSystem.groups[0]
        )
        doAttacks(armies, immuneTargets, infectionTargets)
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