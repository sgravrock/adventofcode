import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val ms = measureTimeMillis {
        val classLoader = Army::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
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

    fun identify(): String {
        return "UnitGroup(id=$id army=$army...)"
    }

    companion object {
        fun parse(input: String, id: Int, army: String): UnitGroup {
            val m = re.matchEntire(input)
                ?: throw Exception("Parse error: $input")
            return UnitGroup(
                // TODO: are id and army useful?
                id = id,
                army = army,
                numUnits = m.groupValues[1].toInt(),
                hitPoints = m.groupValues[2].toInt(),
                attack = m.groupValues[8].toInt(),
                attackType = when (m.groupValues[9]) {
                    "fire" -> AttackType.Fire
                    "radiation" -> AttackType.Radiation
                    "bludgeoning" -> AttackType.Bludgeoning
                    "slashing" -> AttackType.Slashing
                    "cold" -> AttackType.Cold
                    else -> throw Exception("Unexpected attack type: ${m.groupValues[9]}")
                },
                weaknesses = parseAttackTypeList(m.groupValues[7]),
                immunities = parseAttackTypeList(m.groupValues[4]),
                initiative = m.groupValues[10].toInt()
            )
        }

        private fun parseAttackTypeList(s: String): List<AttackType> {
            return if (s == "") {
                emptyList()
            } else {
                s.split(", ").map { attackTypeFromString(it) }
            }
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
            "^([0-9]+) units each with ([0-9]+) hit points " +
                    "\\((immune to ([^;)]+))?(; )?(weak to ([^)]+))?\\) " +
                    "with an attack that does ([0-9]+) ([^ ]+) damage " +
                    "at initiative ([0-9]+)\$"
        )
    }
}

data class Army(val name: String, val groups: List<UnitGroup>) {
    fun unitsLeft(): Int = groups.sumBy { it.numUnits }

    companion object {
        fun parse(input: String, firstId: Int): Army {
            val lines = input.lines()
            val name = lines[0].replace(":", "")
            return Army(
                name = name,
                groups = lines.drop(1).mapIndexed { i, s ->
                    UnitGroup.parse(s, firstId + i, name)
                }
            )
        }
    }
}

fun parseArmies(input: String): Pair<Army, Army> {
    val chunks = input.split("\n\n")
    val army0 = Army.parse(chunks[0], 0)
    val army1 = Army.parse(chunks[1], army0.groups.last().id + 1)
    return Pair(army0, army1)
}

fun fightUntilDone(armies: Pair<Army, Army>) {
    while (armies.first.unitsLeft() != 0 && armies.second.unitsLeft() != 0) {
        fight(armies)
    }
}

fun fight(armies: Pair<Army, Army>) {
    TODO()
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