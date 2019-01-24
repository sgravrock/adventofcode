import kotlin.math.max
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    val ms = measureTimeMillis {
        val classLoader = UnitGroup::class.java.classLoader
        val input = classLoader.getResource("input.txt").readText()
        val armies = parseArmies(input)
        println(findMinBoostForImmuneWin(armies))
    }
    println("in ${ms}ms")
}

data class BoostedResult(val boost: Int, val unitsLeft: Int)
sealed class FightResult {
    object Stalemate : FightResult()
    data class Win(val winner: String, val numUnits: Int) : FightResult()
}

data class UnitGroup(
        val id: Int,
        val army: String,
        var numUnits: Int,
        val hitPoints: Int,
        val attack: Int,
        val attackType: String,
        val weaknesses: List<String>,
        val immunities: List<String>,
        val initiative: Int
) {
    fun effectivePower(): Int {
        return numUnits * attack
    }

    fun damageDealtTo(other: UnitGroup): Int {
        assert(other.army != army)
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
}

fun findMinBoostForImmuneWin(groups: List<UnitGroup>): BoostedResult {
    assert(groups.any { it.army == "Immune System" })

    for (boost in 0..Int.MAX_VALUE) {
        val boostedGroups = groups.map {
            if (it.army == "Immune System") {
                it.copy(attack = it.attack + boost)
            } else {
                it.copy()
            }
        }
        val result = fightUntilDone(boostedGroups)

        when (result) {
            FightResult.Stalemate -> {
            }
            is FightResult.Win -> {
                if (result.winner == "Immune System") {
                    return BoostedResult(boost, result.numUnits)
                }

            }
        }
    }

    throw Error("Immmune system did not win with any boost")
}

fun fightUntilDone(groups: List<UnitGroup>): FightResult {
    fun armies(): List<String> =
            groups.filter { it.numUnits > 0 }.map { it.army }.distinct()

    while (armies().size > 1) {
        if (!fight(groups)) {
            return FightResult.Stalemate
        }
    }

    return FightResult.Win(
            armies().first(),
            groups.sumBy { it.numUnits }
    )
}

fun fight(groups: List<UnitGroup>): Boolean {
    val unitsBefore = groups.sumBy { it.numUnits }
    doAttacks(groups, selectTargets(groups))
    val unitsAfter = groups.sumBy { it.numUnits }
    return unitsBefore != unitsAfter

}

fun selectTargets(groups: List<UnitGroup>): Map<UnitGroup, UnitGroup> {
    val result = mutableMapOf<UnitGroup, UnitGroup>()
    targetSelectionOrder(groups).forEach { attacker ->
        val target = targetPreferenceOrder(groups, attacker, result)
                .firstOrNull()

        if (target != null && attacker.damageDealtTo(target) > 0) {
            result[attacker] = target
        }
    }

    return result
}

fun doAttacks(groups: List<UnitGroup>, targets: Map<UnitGroup, UnitGroup>) {
    groups
            .map { attacker -> Pair(attacker, targets[attacker]) }
            .filter { (_, defender) -> defender != null }
            .sortedByDescending { (attacker, _) -> attacker.initiative }
            .forEach { (attacker, defender) ->
                defender!!.receiveDamage(attacker.damageDealtTo(defender))
            }
}

fun targetSelectionOrder(groups: List<UnitGroup>): List<UnitGroup> {
    return groups
            .filter { it.numUnits > 0 }
            .sortedWith(DescendingCascadingComparator(
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
                it.army != attacker.army
                        && it.numUnits > 0
                        && !choicesSoFar.containsValue(it)
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

fun parseArmies(input: String): List<UnitGroup> {
    val chunks = input.split("\n\n")
    return listOf(
            parseArmy(chunks[0]),
            parseArmy(chunks[1])
    ).flatten()
}

fun parseArmy(input: String): List<UnitGroup> {
    val lines = input.lines()
    val type = lines[0].replace(":", "")
    return lines.drop(1).mapIndexed { i, s ->
        parseUnitGroup(s, i + 1, type)
    }
}

fun parseUnitGroup(input: String, id: Int, army: String): UnitGroup {
    val m = unitGroupRegex.matchEntire(input)
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
            attackType = m.groups["String"]!!.value,
            weaknesses = weaknesses,
            immunities = immunities,
            initiative = m.groups["initiative"]!!.value.toInt()
    )
}

private fun parseCapabilities(
        input: String?
): Pair<List<String>, List<String>> {
    if (input == null) {
        return Pair(emptyList(), emptyList())
    }

    var weaknesses = emptyList<String>()
    var immunities = emptyList<String>()

    for (chunk in input.split("; ")) {
        val (name, values) = chunk.split(" to ")
        val caps = values.split(", ")

        when (name) {
            "weak" -> weaknesses = caps
            "immune" -> immunities = caps
            else -> throw Error("Expected weak or immune but got $name")
        }
    }

    return Pair(weaknesses, immunities)
}

private val unitGroupRegex = Regex(
        "^(?<numUnits>[0-9]+) units each with (?<hitPoints>[0-9]+) hit points " +
                "(\\((?<capabilities>[^)]+)\\) )?with an attack that does " +
                "(?<damage>[0-9]+) (?<String>[^ ]+) damage at initiative " +
                "(?<initiative>[0-9]+)\$"
)
