import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testFirstIndexOf() {
        assertEquals(9, firstIndexOf(listOf(5, 1, 5, 8, 9)))
        assertEquals(5, firstIndexOf(listOf(0, 1, 2, 4, 5)))
        assertEquals(18, firstIndexOf(listOf(9, 2, 5, 1, 0)))
        assertEquals(2018, firstIndexOf(listOf(5, 9, 4, 1, 4)))
    }

    @Test
    fun testDigits() {
        assertEquals(listOf(1, 2, 3), digits(123))
        assertEquals(listOf(1, 2, 0), digits(120))
        assertEquals(listOf(0), digits(0))
    }
}