import kotlin.math.max
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val ms = measureTimeMillis {
        val classLoader = Army::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        val armies = Combat.parse(input)
        println(fightUntilDone(armies))
    }
    println("in ${ms}ms")
}

enum class AttackType {
    Fire,
    Radiation,
    Bludgeoning,
    Slashing,
    Cold
}

data class UnitGroup(
    val id: Int,
    val army: ArmyType,
    var numUnits: Int,
    val hitPoints: Int,
    val attack: Int,
    val attackType: AttackType,
    val weaknesses: List<AttackType>,
    val immunities: List<AttackType>,
    val initiative: Int
) {
    fun effectivePower(): Int {
        return numUnits * attack
    }

    fun damageDealtTo(other: UnitGroup): Int {
        return if (attackType in other.immunities) {
            0
        } else if (attackType in other.weaknesses) {
            effectivePower() * 2
        } else {
            effectivePower()
        }
    }

    fun receiveDamage(damage: Int) {
        val unitsKilled = damage / hitPoints
        numUnits = max(0, numUnits - unitsKilled)
    }
    
    companion object {
        fun parse(input: String, id: Int, army: ArmyType): UnitGroup {
            val m = re.matchEntire(input)
                ?: throw Exception("Parse error: $input")
            val (weaknesses, immunities) = parseCapabilities(
                m.groups["capabilities"]?.value
            )
            return UnitGroup(
                id = id,
                army = army,
                numUnits = m.groups["numUnits"]!!.value.toInt(),
                hitPoints = m.groups["hitPoints"]!!.value.toInt(),
                attack = m.groups["damage"]!!.value.toInt(),
                attackType = attackTypeFromString(m.groups["attackType"]!!.value),
                weaknesses = weaknesses,
                immunities = immunities,
                initiative = m.groups["initiative"]!!.value.toInt()
            )
        }

        private fun parseCapabilities(
            input: String?
        ): Pair<List<AttackType>, List<AttackType>> {
            if (input == null) {
                return Pair(emptyList(), emptyList())
            }

            var weaknesses = emptyList<AttackType>()
            var immunities = emptyList<AttackType>()

            for (chunk in input.split("; ")) {
                val (name, values) = chunk.split(" to ")
                val caps = values.split(", ").map { attackTypeFromString(it) }

                when (name) {
                    "weak" -> weaknesses = caps
                    "immune" -> immunities = caps
                    else -> throw Error("Expected weak or immune but got $name")
                }
            }

            return Pair(weaknesses, immunities)
        }

        private fun attackTypeFromString(s: String): AttackType {
            return when (s) {
                "fire" -> AttackType.Fire
                "radiation" -> AttackType.Radiation
                "bludgeoning" -> AttackType.Bludgeoning
                "slashing" -> AttackType.Slashing
                "cold" -> AttackType.Cold
                else -> throw Exception("Unexpected attack type: ${s}")
            }
        }

        private val re = Regex(
            "^(?<numUnits>[0-9]+) units each with (?<hitPoints>[0-9]+) hit points " +
                    "(\\((?<capabilities>[^)]+)\\) )?with an attack that does " +
                    "(?<damage>[0-9]+) (?<attackType>[^ ]+) damage at initiative " +
                    "(?<initiative>[0-9]+)\$"
        )
    }
}

enum class ArmyType {
    ImmmuneSystem,
    Infection;

    companion object {
        fun fromString(s: String): ArmyType {
            return when (s) {
                "Immune System" -> ArmyType.ImmmuneSystem
                "Infection" -> ArmyType.Infection
                else -> throw Error("Unrecognized army type: $s")
            }
        }
    }
}

data class Army(val type: ArmyType, var groups: List<UnitGroup>) {
    fun unitsLeft(): Int = groups.sumBy { it.numUnits }
    
    companion object {
        fun parse(input: String): Army {
            val lines = input.lines()
            val type = ArmyType.fromString(lines[0].replace(":", ""))
            return Army(
                type = type,
                groups = lines.drop(1).mapIndexed { i, s ->
                    UnitGroup.parse(s, i + 1, type)
                }
            )
        }
    }
}

data class Combat(val immuneSystem: Army, val infection: Army) {
    fun allGroups(): List<UnitGroup> {
        return listOf(immuneSystem, infection).flatMap { it.groups }
    }

    companion object {
        fun parse(input: String): Combat {
            val chunks = input.split("\n\n")
            val army0 = Army.parse(chunks[0])
            val army1 = Army.parse(chunks[1])
            return when (army0.type) {
                ArmyType.ImmmuneSystem -> Combat(army0, army1)
                ArmyType.Infection -> Combat(army1, army0)
            }
        }
    }
}

fun fightUntilDone(armies: Combat): Int {
    while (armies.immuneSystem.unitsLeft() != 0 && armies.infection.unitsLeft() != 0) {
        fight(armies)
    }

    return armies.allGroups().sumBy { it.numUnits }
}

fun fight(armies: Combat) {
    val infectionTargets = selectTargets(
        armies.infection.groups, armies.immuneSystem.groups
    )
    val immuneTargets = selectTargets(
        armies.immuneSystem.groups, armies.infection.groups
    )
    doAttacks(armies, immuneTargets, infectionTargets)
}

fun selectTargets(
    attackers: List<UnitGroup>,
    defenders: List<UnitGroup>
): Map<UnitGroup, UnitGroup> {

    assert(attackers.map { it.army }.distinct().size == 1)
    assert(defenders.map { it.army }.distinct().size == 1)
    val result = mutableMapOf<UnitGroup, UnitGroup>()
    val attackers = attackers.filter { it.numUnits > 0 }
    val defenders = defenders.filter { it.numUnits > 0 }
    targetSelectionOrder(attackers).forEach { attacker ->
        val target = targetPreferenceOrder(defenders, attacker, result)
            .firstOrNull()

        if (target != null && attacker.damageDealtTo(target) > 0) {
            result[attacker] = target
        }
    }

    return result
}

fun targetSelectionOrder(groups: List<UnitGroup>): List<UnitGroup> {
    return groups.sortedWith(DescendingCascadingComparator(
        listOf(
            { g: UnitGroup -> g.effectivePower() },
            { g: UnitGroup -> g.initiative }
        )
    ))
}

fun targetPreferenceOrder(
    groups: List<UnitGroup>,
    attacker: UnitGroup,
    choicesSoFar: Map<UnitGroup, UnitGroup>
): List<UnitGroup> {
    return groups
        .filter {
            !choicesSoFar.containsValue(it)
        }
        .sortedWith(DescendingCascadingComparator(
            listOf(
                { g: UnitGroup -> attacker.damageDealtTo(g) },
                { g: UnitGroup -> g.effectivePower() },
                { g: UnitGroup -> g.initiative }

            )
        ))
}

class DescendingCascadingComparator(
    val selectors: List<(UnitGroup) -> Int>
) : Comparator<UnitGroup> {
    override fun compare(a: UnitGroup, b: UnitGroup): Int {
        for (sel in selectors) {
            val c = sel(b).compareTo(sel(a))

            if (c != 0) {
                return c
            }
        }

        return 0
    }
}

fun doAttacks(
    armies: Combat,
    immuneTargets: Map<UnitGroup, UnitGroup>,
    infectionTargets: Map<UnitGroup, UnitGroup>
) {
    fun targetFor(attacker: UnitGroup): UnitGroup? {
        val targetMap = when (attacker.army) {
            ArmyType.ImmmuneSystem -> immuneTargets
            ArmyType.Infection -> infectionTargets
        }
        return targetMap[attacker]
    }

    armies.allGroups()
        .map { attacker -> Pair(attacker, targetFor(attacker)) }
        .filter { (_, defender) -> defender != null }
        .sortedByDescending { (attacker, _) -> attacker.initiative }
        .forEach { (attacker, defender) ->
            defender!!.receiveDamage(attacker.damageDealtTo(defender))
        }
}