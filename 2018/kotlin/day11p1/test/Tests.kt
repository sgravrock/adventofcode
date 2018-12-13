import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testMostPowerful3x3() {
        assertEquals(Coord(x = 33, y = 45), mostPowerful3x3(18))
        assertEquals(Coord(x = 21, y = 61), mostPowerful3x3(42))
    }

    @Test
    fun testPowerOf3x3() {
        assertEquals(29, powerOf3x3(18, Coord(x = 33, y = 45)))
        assertEquals(30, powerOf3x3(42, Coord(x = 21, y = 61)))
    }

    @Test
    fun hundredsDigit() {
        assertEquals(3, hundredsDigit(12345))
    }
}