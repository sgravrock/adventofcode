import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun riskLevel() {
        assertEquals(114, riskLevel(510, 10, 10))
    }

    @Test
    fun riskLevelForPuzzleInput() {
        assertEquals(5786, riskLevel(9171, 7, 721))
    }
}