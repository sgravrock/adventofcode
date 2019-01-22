import kotlin.math.max
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val ms = measureTimeMillis {
        val classLoader = Army::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        val armies = parseArmies(input)
        println(fightUntilDone(armies))
    }
    println("in ${ms}ms")
    // 20996  is too low
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
    val army: String,
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

    fun identify(): String {
        return "$army group $id"
    }

    companion object {
        fun parse(input: String, id: Int, army: String): UnitGroup {
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

data class Army(val name: String, val groups: List<UnitGroup>) {
    fun unitsLeft(): Int = groups.sumBy { it.numUnits }

    fun toDebugString(): String {
        val groupsLeft = groups.filter { it.numUnits > 0 }
        val groupStr = if (groupsLeft.isEmpty()) {
            "No groups remain."
        } else {
            groupsLeft
                .map { "Group ${it.id} contains ${it.numUnits} units"}
                .joinToString("\n")
        }
        return "$name\n$groupStr"
    }

    companion object {
        fun parse(input: String): Army {
            val lines = input.lines()
            val name = lines[0].replace(":", "")
            return Army(
                name = name,
                groups = lines.drop(1).mapIndexed { i, s ->
                    UnitGroup.parse(s, i + 1, name)
                }
            )
        }
    }
}

fun parseArmies(input: String): Pair<Army, Army> {
    val chunks = input.split("\n\n")
    val army0 = Army.parse(chunks[0])
    val army1 = Army.parse(chunks[1])
    return Pair(army0, army1)
}

fun fightUntilDone(armies: Pair<Army, Army>, debug: Boolean = false): Int {
    val groups = listOf(armies.first, armies.second).flatMap { it.groups }

    while (armies.first.unitsLeft() != 0 && armies.second.unitsLeft() != 0) {
        if (debug) {
            println(armies.first.toDebugString())
            println(armies.second.toDebugString())
            println()
        }
        fight(groups, debug)
    }

    return groups.sumBy { it.numUnits }
}

fun fight(groups: List<UnitGroup>, debug: Boolean = false) {
    doAttacks(groups, selectTargets(groups), debug)
}

fun selectTargets(groups: List<UnitGroup>): Map<Int, Int> {
    val result = mutableMapOf<Int, Int>()
    targetSelectionOrder(groups).forEach { i ->
        val target = targetPreferenceOrder(groups, groups[i], result).firstOrNull()

        if (target != null) {
            result[i] = target
        }
    }

    return result
}

fun targetSelectionOrder(groups: List<UnitGroup>): List<Int> {
    return groups.indices.sortedWith(DescendingCascadingComparator(
        groups,
        listOf(
            { g: UnitGroup -> g.effectivePower() },
            { g: UnitGroup -> g.initiative }
        )
    ))
}

fun targetPreferenceOrder(
    groups: List<UnitGroup>,
    attacker: UnitGroup,
    choicesSoFar: Map<Int, Int>
): List<Int> {
    return groups.indices
        .filter {
            !choicesSoFar.containsValue(it) && groups[it].army != attacker.army
        }
        .sortedWith(DescendingCascadingComparator(
            groups,
            listOf(
                { g: UnitGroup -> attacker.damageDealtTo(g) },
                { g: UnitGroup -> g.effectivePower() },
                { g: UnitGroup -> g.initiative }

            )
        ))
}

class DescendingCascadingComparator(
    val groups: List<UnitGroup>,
    val selectors: List<(UnitGroup) -> Int>
): Comparator<Int> {
    override fun compare(a: Int?, b: Int?): Int {
        val ga = groups[a!!]
        val gb = groups[b!!]

        for (sel in selectors) {
            val c = sel(gb).compareTo(sel(ga))

            if (c != 0) {
                return c
            }
        }

        return 0
    }
}

fun doAttacks(groups: List<UnitGroup>, targets: Map<Int, Int>, debug: Boolean = false) {
    groups.indices
        .filter { targets[it] != null }
        .sortedByDescending { groups[it].initiative }
        .forEach { ai ->
            val attacker = groups[ai]
            val defender = groups[targets[ai]!!]
            val before = defender.numUnits
            defender.receiveDamage(attacker.damageDealtTo(defender))

            if (debug) {
                println("${attacker.identify()} attacks defending group ${defender.id}, " +
                        "killing ${before - defender.numUnits} units")
            }
        }
}