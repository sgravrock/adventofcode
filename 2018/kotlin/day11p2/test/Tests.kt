import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testMostPowerful() {
        assertEquals(Grid(x = 90, y = 269, size = 16), mostPowerfulGrid(18))
    }

    @Test
    fun testPowerOfGrid() {
        assertEquals(113, powerOfGrid(18, Grid(x = 90, y = 269, size = 16)))
        assertEquals(119, powerOfGrid(42, Grid(x = 232, y = 251, size = 12)))
    }

    @Test
    fun testHundredsDigit() {
        assertEquals(3, hundredsDigit(12345))
    }
}