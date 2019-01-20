import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun fewestMinutes() {
        assertEquals(45, Cave(510, 10, 10).fewestMinutesToTarget())
    }

    @Test
    fun fewestMinutes_tinyExample() {
        val subject = Cave(3, 1, 1)
        subject.printToWidth(5)
        assertEquals(2, subject.fewestMinutesToTarget())
    }
}
/*
  012
0 M=.
1 .T.
2 ..=

00T -> 01T(1) -> 11T(2)
00T -> 01T(1) -> 11C
00T -> 01T(1) -> 02
00T -> 10N(8) -> 20C(16) PRUNE
00T -> 10N(8) -> 20T(16) PRUNE
00T -> 10N(8) -> 11C(16) PRUNE
00T -> 10N(8) -> 11T(16)
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 11C(11) +7=18
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 11T(18)
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 22C(11)
00T -> 10C(8) -> 20C(9) -> 21C(10) -> 22N(18) PRUNE
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 11C(25) PRUNE
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 11T(18)
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 22C(25) PRUNE
00T -> 10C(8) -> 20C(9) -> 21T(17) -> 22N(18) PRUNE
00T -> 10C(8) -> 20T(16) -> 21C(24) PRUNE
00T -> 10C(8) -> 20T(16) -> 21T(17) -> 11C(25) PRUNE
00T -> 10C(8) -> 20T(16) -> 21T(17) -> 11T(18)
00T -> 10C(8) -> 20T(16) -> 21T(17) -> 22*(>=18) PRUNE
00T -> 10C(8) -> 11C(9) + 7 = 16
00T -> 10C(8) -> 20C(9) -> 21 PRUNE
00T -> 10C(8) -> 20T(15) -> 21C(22) PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 22N(23) PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 22C(23) PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 11C PRUNE
00T -> 10C(8) -> 20T(15) -> 21T(16) -> 11T(17)
00T -> 10C(8) -> 11C(9) + 7 = 16
00T -> 10C(8) -> 11T(16)
00T -> 01C(8)


best:
    11* -> 16
 */