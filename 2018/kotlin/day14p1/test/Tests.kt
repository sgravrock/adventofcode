import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testNext10ScoresAfter() {
        assertEquals("5158916779", next10ScoresAfter(9))
        assertEquals("0124515891", next10ScoresAfter(5))
        assertEquals("9251071085", next10ScoresAfter(18))
        assertEquals("5941429882", next10ScoresAfter(2018))
    }

    @Test
    fun testDigits() {
        assertEquals(listOf(1, 2, 3), digits(123))
        assertEquals(listOf(1, 2, 0), digits(120))
        assertEquals(listOf(0), digits(0))
    }
}