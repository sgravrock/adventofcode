import java.lang.Exception
import java.util.*
import kotlin.math.max
import kotlin.math.min

fun main(args: Array<String>) {
    val start = Date()
    val input = "position=< 54347, -32361> velocity=<-5,  3>\n" +
            "position=<-21463, -32354> velocity=< 2,  3>\n" +
            "position=< 54347,  32630> velocity=<-5, -3>\n" +
            "position=< 21839,  32628> velocity=<-2, -3>\n" +
            "position=<-21490, -21528> velocity=< 2,  2>\n" +
            "position=< 54359,  43457> velocity=<-5, -4>\n" +
            "position=<-21470, -10698> velocity=< 2,  1>\n" +
            "position=<-43100, -21528> velocity=< 4,  2>\n" +
            "position=< 21842, -21524> velocity=<-2,  2>\n" +
            "position=< 11011, -32358> velocity=<-1,  3>\n" +
            "position=<-10651, -54021> velocity=< 1,  5>\n" +
            "position=<-10651, -32355> velocity=< 1,  3>\n" +
            "position=< 43533, -21523> velocity=<-4,  2>\n" +
            "position=< 32700, -43185> velocity=<-3,  4>\n" +
            "position=< 54367,  43459> velocity=<-5, -4>\n" +
            "position=< 32697,  43463> velocity=<-3, -4>\n" +
            "position=< 11043,  43463> velocity=<-1, -4>\n" +
            "position=< 11051, -21531> velocity=<-1,  2>\n" +
            "position=<-21445,  21794> velocity=< 2, -2>\n" +
            "position=< 43544,  54288> velocity=<-4, -5>\n" +
            "position=< 43528, -21531> velocity=<-4,  2>\n" +
            "position=<-10651,  43463> velocity=< 1, -4>\n" +
            "position=<-53930,  54294> velocity=< 5, -5>\n" +
            "position=< 21882, -43187> velocity=<-2,  4>\n" +
            "position=< 32665,  43460> velocity=<-3, -4>\n" +
            "position=<-10646, -43191> velocity=< 1,  4>\n" +
            "position=< 32709, -21528> velocity=<-3,  2>\n" +
            "position=< 11046,  21792> velocity=<-1, -2>\n" +
            "position=<-53991,  43457> velocity=< 5, -4>\n" +
            "position=<-53940, -10697> velocity=< 5,  1>\n" +
            "position=< 43545,  21801> velocity=<-4, -2>\n" +
            "position=< 11043, -32361> velocity=<-1,  3>\n" +
            "position=< 43513,  43463> velocity=<-4, -4>\n" +
            "position=<-10627,  21794> velocity=< 1, -2>\n" +
            "position=< 32706,  10965> velocity=<-3, -1>\n" +
            "position=<-21497,  21792> velocity=< 2, -2>\n" +
            "position=< 54329,  54289> velocity=<-5, -5>\n" +
            "position=< 32693,  10961> velocity=<-3, -1>\n" +
            "position=< 43501, -54025> velocity=<-4,  5>\n" +
            "position=< 54372,  21795> velocity=<-5, -2>\n" +
            "position=< 11037,  43463> velocity=<-1, -4>\n" +
            "position=< 43496, -32357> velocity=<-4,  3>\n" +
            "position=<-43147, -21529> velocity=< 4,  2>\n" +
            "position=<-21442, -21531> velocity=< 2,  2>\n" +
            "position=< 11056,  10961> velocity=<-1, -1>\n" +
            "position=< 21826,  43455> velocity=<-2, -4>\n" +
            "position=< 43539, -32359> velocity=<-4,  3>\n" +
            "position=<-32313, -54025> velocity=< 3,  5>\n" +
            "position=< 43490,  54294> velocity=<-4, -5>\n" +
            "position=< 21839,  10970> velocity=<-2, -1>\n" +
            "position=< 21853,  54294> velocity=<-2, -5>\n" +
            "position=< 10996,  10961> velocity=<-1, -1>\n" +
            "position=<-43125, -54025> velocity=< 4,  5>\n" +
            "position=<-21477, -21529> velocity=< 2,  2>\n" +
            "position=<-10667,  43463> velocity=< 1, -4>\n" +
            "position=<-53951,  54289> velocity=< 5, -5>\n" +
            "position=<-32273, -43187> velocity=< 3,  4>\n" +
            "position=<-21462, -32355> velocity=< 2,  3>\n" +
            "position=<-53954,  54286> velocity=< 5, -5>\n" +
            "position=< 43496,  10970> velocity=<-4, -1>\n" +
            "position=<-32289,  54291> velocity=< 3, -5>\n" +
            "position=< 21834,  54285> velocity=<-2, -5>\n" +
            "position=<-43120, -54017> velocity=< 4,  5>\n" +
            "position=< 21834, -21527> velocity=<-2,  2>\n" +
            "position=<-43099, -43185> velocity=< 4,  4>\n" +
            "position=< 11032, -32355> velocity=<-1,  3>\n" +
            "position=< 54377, -10697> velocity=<-5,  1>\n" +
            "position=< 54351, -10696> velocity=<-5,  1>\n" +
            "position=< 43544, -43193> velocity=<-4,  4>\n" +
            "position=<-53987,  54294> velocity=< 5, -5>\n" +
            "position=<-43140, -10697> velocity=< 4,  1>\n" +
            "position=< 32659,  21801> velocity=<-3, -2>\n" +
            "position=<-53935,  21792> velocity=< 5, -2>\n" +
            "position=<-21493,  54288> velocity=< 2, -5>\n" +
            "position=< 43504,  32628> velocity=<-4, -3>\n" +
            "position=< 21854, -21524> velocity=<-2,  2>\n" +
            "position=< 21839, -43193> velocity=<-2,  4>\n" +
            "position=<-32271, -10692> velocity=< 3,  1>\n" +
            "position=<-32313, -10696> velocity=< 3,  1>\n" +
            "position=<-32288, -54016> velocity=< 3,  5>\n" +
            "position=< 21826, -10699> velocity=<-2,  1>\n" +
            "position=< 21842, -54025> velocity=<-2,  5>\n" +
            "position=<-32316, -43188> velocity=< 3,  4>\n" +
            "position=< 32689, -43191> velocity=<-3,  4>\n" +
            "position=<-43160, -54025> velocity=< 4,  5>\n" +
            "position=<-43139,  43455> velocity=< 4, -4>\n" +
            "position=< 32714,  43458> velocity=<-3, -4>\n" +
            "position=< 43501,  54293> velocity=<-4, -5>\n" +
            "position=<-21446,  10965> velocity=< 2, -1>\n" +
            "position=< 43522, -21532> velocity=<-4,  2>\n" +
            "position=< 32677, -43190> velocity=<-3,  4>\n" +
            "position=< 11051,  43462> velocity=<-1, -4>\n" +
            "position=<-32321,  32628> velocity=< 3, -3>\n" +
            "position=<-53932, -54025> velocity=< 5,  5>\n" +
            "position=<-10611,  21798> velocity=< 1, -2>\n" +
            "position=<-32276,  54287> velocity=< 3, -5>\n" +
            "position=< 21839, -54019> velocity=<-2,  5>\n" +
            "position=<-53975, -54018> velocity=< 5,  5>\n" +
            "position=<-53978, -21532> velocity=< 5,  2>\n" +
            "position=<-53959, -43192> velocity=< 5,  4>\n" +
            "position=<-21458, -10699> velocity=< 2,  1>\n" +
            "position=<-43158,  21792> velocity=< 4, -2>\n" +
            "position=<-53983, -32361> velocity=< 5,  3>\n" +
            "position=<-43120,  10961> velocity=< 4, -1>\n" +
            "position=< 54367,  10961> velocity=<-5, -1>\n" +
            "position=<-10615, -43194> velocity=< 1,  4>\n" +
            "position=< 43496, -10698> velocity=<-4,  1>\n" +
            "position=<-10665, -32354> velocity=< 1,  3>\n" +
            "position=<-21470,  54288> velocity=< 2, -5>\n" +
            "position=< 11043, -54025> velocity=<-1,  5>\n" +
            "position=< 43504,  43458> velocity=<-4, -4>\n" +
            "position=<-53931,  32623> velocity=< 5, -3>\n" +
            "position=<-32271,  32627> velocity=< 3, -3>\n" +
            "position=< 11035,  43462> velocity=<-1, -4>\n" +
            "position=< 10999, -54025> velocity=<-1,  5>\n" +
            "position=< 32662,  10963> velocity=<-3, -1>\n" +
            "position=<-32272,  54289> velocity=< 3, -5>\n" +
            "position=<-32289,  54286> velocity=< 3, -5>\n" +
            "position=< 11043, -54023> velocity=<-1,  5>\n" +
            "position=< 21831,  32630> velocity=<-2, -3>\n" +
            "position=<-21498,  43454> velocity=< 2, -4>\n" +
            "position=<-21449,  43454> velocity=< 2, -4>\n" +
            "position=<-10631, -43189> velocity=< 1,  4>\n" +
            "position=< 11051,  54289> velocity=<-1, -5>\n" +
            "position=< 11011, -32360> velocity=<-1,  3>\n" +
            "position=<-21482,  32626> velocity=< 2, -3>\n" +
            "position=<-32289,  21796> velocity=< 3, -2>\n" +
            "position=< 10995,  43457> velocity=<-1, -4>\n" +
            "position=<-53982, -21528> velocity=< 5,  2>\n" +
            "position=<-10662, -21525> velocity=< 1,  2>\n" +
            "position=<-53954, -43188> velocity=< 5,  4>\n" +
            "position=< 21834,  21801> velocity=<-2, -2>\n" +
            "position=< 11027,  54292> velocity=<-1, -5>\n" +
            "position=< 21874,  43455> velocity=<-2, -4>\n" +
            "position=<-21488, -43190> velocity=< 2,  4>\n" +
            "position=<-10647,  54285> velocity=< 1, -5>\n" +
            "position=<-53955, -54025> velocity=< 5,  5>\n" +
            "position=<-43123,  43459> velocity=< 4, -4>\n" +
            "position=< 10995,  32628> velocity=<-1, -3>\n" +
            "position=<-21485,  43457> velocity=< 2, -4>\n" +
            "position=<-32313, -32354> velocity=< 3,  3>\n" +
            "position=< 32657, -43190> velocity=<-3,  4>\n" +
            "position=< 10995, -10697> velocity=<-1,  1>\n" +
            "position=< 11044,  54289> velocity=<-1, -5>\n" +
            "position=<-53941, -10701> velocity=< 5,  1>\n" +
            "position=<-53935,  21792> velocity=< 5, -2>\n" +
            "position=< 43508, -54021> velocity=<-4,  5>\n" +
            "position=<-43157, -10701> velocity=< 4,  1>\n" +
            "position=<-21458,  32631> velocity=< 2, -3>\n" +
            "position=< 21834,  43462> velocity=<-2, -4>\n" +
            "position=<-10611, -21527> velocity=< 1,  2>\n" +
            "position=< 54347, -43193> velocity=<-5,  4>\n" +
            "position=<-32293, -21532> velocity=< 3,  2>\n" +
            "position=<-10666, -32359> velocity=< 1,  3>\n" +
            "position=< 43520,  54286> velocity=<-4, -5>\n" +
            "position=< 54319, -32361> velocity=<-5,  3>\n" +
            "position=<-32326,  10965> velocity=< 3, -1>\n" +
            "position=<-53955, -21527> velocity=< 5,  2>\n" +
            "position=<-53939,  54289> velocity=< 5, -5>\n" +
            "position=<-10640, -32363> velocity=< 1,  3>\n" +
            "position=<-53987,  43463> velocity=< 5, -4>\n" +
            "position=<-53978,  54289> velocity=< 5, -5>\n" +
            "position=< 32708, -54025> velocity=<-3,  5>\n" +
            "position=< 11056,  32623> velocity=<-1, -3>\n" +
            "position=<-43147,  43459> velocity=< 4, -4>\n" +
            "position=<-32308, -32362> velocity=< 3,  3>\n" +
            "position=<-53935, -43194> velocity=< 5,  4>\n" +
            "position=< 11012, -10697> velocity=<-1,  1>\n" +
            "position=<-53930, -21532> velocity=< 5,  2>\n" +
            "position=< 10995, -54018> velocity=<-1,  5>\n" +
            "position=<-43157,  10970> velocity=< 4, -1>\n" +
            "position=<-21470, -10698> velocity=< 2,  1>\n" +
            "position=< 21862, -10693> velocity=<-2,  1>\n" +
            "position=<-32316, -10699> velocity=< 3,  1>\n" +
            "position=< 32707, -10697> velocity=<-3,  1>\n" +
            "position=<-43107, -21529> velocity=< 4,  2>\n" +
            "position=<-32324, -10700> velocity=< 3,  1>\n" +
            "position=<-10639,  54290> velocity=< 1, -5>\n" +
            "position=<-32289, -21528> velocity=< 3,  2>\n" +
            "position=<-21449, -32363> velocity=< 2,  3>\n" +
            "position=<-32321, -54016> velocity=< 3,  5>\n" +
            "position=< 54340,  32626> velocity=<-5, -3>\n" +
            "position=<-21466,  43458> velocity=< 2, -4>\n" +
            "position=<-10648,  54285> velocity=< 1, -5>\n" +
            "position=< 54327, -43190> velocity=<-5,  4>\n" +
            "position=<-53958, -43185> velocity=< 5,  4>\n" +
            "position=< 32678,  43456> velocity=<-3, -4>\n" +
            "position=< 11007,  10965> velocity=<-1, -1>\n" +
            "position=<-32300,  10961> velocity=< 3, -1>\n" +
            "position=<-43125,  43459> velocity=< 4, -4>\n" +
            "position=<-53975,  32630> velocity=< 5, -3>\n" +
            "position=< 32716, -43185> velocity=<-3,  4>\n" +
            "position=< 32662, -10693> velocity=<-3,  1>\n" +
            "position=<-10667,  54285> velocity=< 1, -5>\n" +
            "position=< 54347, -32362> velocity=<-5,  3>\n" +
            "position=< 32676,  54289> velocity=<-3, -5>\n" +
            "position=< 11023,  10967> velocity=<-1, -1>\n" +
            "position=<-21482,  32631> velocity=< 2, -3>\n" +
            "position=< 21876, -10701> velocity=<-2,  1>\n" +
            "position=< 32683, -21523> velocity=<-3,  2>\n" +
            "position=< 32681, -21525> velocity=<-3,  2>\n" +
            "position=< 11011, -32359> velocity=<-1,  3>\n" +
            "position=<-32294,  10961> velocity=< 3, -1>\n" +
            "position=< 32716, -10697> velocity=<-3,  1>\n" +
            "position=< 21866, -10694> velocity=<-2,  1>\n" +
            "position=<-32312, -43194> velocity=< 3,  4>\n" +
            "position=< 21871, -32354> velocity=<-2,  3>\n" +
            "position=< 21876,  21796> velocity=<-2, -2>\n" +
            "position=<-43141, -21532> velocity=< 4,  2>\n" +
            "position=<-43158, -10697> velocity=< 4,  1>\n" +
            "position=< 43496, -21525> velocity=<-4,  2>\n" +
            "position=<-10651,  43455> velocity=< 1, -4>\n" +
            "position=<-21466,  21799> velocity=< 2, -2>\n" +
            "position=< 43488, -10697> velocity=<-4,  1>\n" +
            "position=<-53973,  54289> velocity=< 5, -5>\n" +
            "position=<-53986, -32358> velocity=< 5,  3>\n" +
            "position=<-21494,  43454> velocity=< 2, -4>\n" +
            "position=<-21442,  21796> velocity=< 2, -2>\n" +
            "position=< 21830,  54289> velocity=<-2, -5>\n" +
            "position=<-43112,  32631> velocity=< 4, -3>\n" +
            "position=< 43545, -43194> velocity=<-4,  4>\n" +
            "position=< 32682,  21801> velocity=<-3, -2>\n" +
            "position=< 21875,  32627> velocity=<-2, -3>\n" +
            "position=< 43501,  32629> velocity=<-4, -3>\n" +
            "position=<-10639,  21798> velocity=< 1, -2>\n" +
            "position=< 21860,  10970> velocity=<-2, -1>\n" +
            "position=<-43152, -32358> velocity=< 4,  3>\n" +
            "position=< 11027, -43190> velocity=<-1,  4>\n" +
            "position=< 11011,  10962> velocity=<-1, -1>\n" +
            "position=< 32715, -21532> velocity=<-3,  2>\n" +
            "position=<-53975,  43456> velocity=< 5, -4>\n" +
            "position=<-10639,  10965> velocity=< 1, -1>\n" +
            "position=<-21466,  54289> velocity=< 2, -5>\n" +
            "position=<-21470, -43188> velocity=< 2,  4>\n" +
            "position=<-32304, -32354> velocity=< 3,  3>\n" +
            "position=<-10634, -43194> velocity=< 1,  4>\n" +
            "position=< 11043, -32355> velocity=<-1,  3>\n" +
            "position=<-53951, -43192> velocity=< 5,  4>\n" +
            "position=< 32657, -21526> velocity=<-3,  2>\n" +
            "position=< 32713, -43185> velocity=<-3,  4>\n" +
            "position=< 11035, -21523> velocity=<-1,  2>\n" +
            "position=< 21854,  54293> velocity=<-2, -5>\n" +
            "position=<-53957,  43454> velocity=< 5, -4>\n" +
            "position=< 32713,  54292> velocity=<-3, -5>\n" +
            "position=< 32694, -32358> velocity=<-3,  3>\n" +
            "position=< 11043,  21796> velocity=<-1, -2>\n" +
            "position=< 11000, -32357> velocity=<-1,  3>\n" +
            "position=< 43520, -21525> velocity=<-4,  2>\n" +
            "position=<-43160, -43188> velocity=< 4,  4>\n" +
            "position=< 11043,  10967> velocity=<-1, -1>\n" +
            "position=<-43117,  32632> velocity=< 4, -3>\n" +
            "position=< 11051,  43460> velocity=<-1, -4>\n" +
            "position=<-53963,  43461> velocity=< 5, -4>\n" +
            "position=<-53988,  10965> velocity=< 5, -1>\n" +
            "position=< 43546,  43454> velocity=<-4, -4>\n" +
            "position=< 32697,  10961> velocity=<-3, -1>\n" +
            "position=< 54338,  54285> velocity=<-5, -5>\n" +
            "position=< 43525,  43461> velocity=<-4, -4>\n" +
            "position=< 43536,  10961> velocity=<-4, -1>\n" +
            "position=<-10639,  43455> velocity=< 1, -4>\n" +
            "position=<-21458, -10698> velocity=< 2,  1>\n" +
            "position=< 32714,  54285> velocity=<-3, -5>\n" +
            "position=<-32325, -32359> velocity=< 3,  3>\n" +
            "position=<-43155,  54290> velocity=< 4, -5>\n" +
            "position=< 21842,  10967> velocity=<-2, -1>\n" +
            "position=<-21493, -32360> velocity=< 2,  3>\n" +
            "position=<-43149, -21528> velocity=< 4,  2>\n" +
            "position=< 10995,  32629> velocity=<-1, -3>\n" +
            "position=<-43158, -21532> velocity=< 4,  2>\n" +
            "position=<-21493,  32624> velocity=< 2, -3>\n" +
            "position=<-21441, -21532> velocity=< 2,  2>\n" +
            "position=< 54351,  21800> velocity=<-5, -2>\n" +
            "position=<-10639, -10694> velocity=< 1,  1>\n" +
            "position=<-21485,  43458> velocity=< 2, -4>\n" +
            "position=<-10609,  32623> velocity=< 1, -3>\n" +
            "position=< 32689, -43192> velocity=<-3,  4>\n" +
            "position=< 11055, -32354> velocity=<-1,  3>\n" +
            "position=<-10651,  43455> velocity=< 1, -4>\n" +
            "position=< 21874,  32630> velocity=<-2, -3>\n" +
            "position=<-21450, -10698> velocity=< 2,  1>\n" +
            "position=< 43536, -21529> velocity=<-4,  2>\n" +
            "position=<-43147, -21527> velocity=< 4,  2>\n" +
            "position=<-43120,  32628> velocity=< 4, -3>\n" +
            "position=< 32665, -54017> velocity=<-3,  5>\n" +
            "position=<-53946, -21523> velocity=< 5,  2>\n" +
            "position=< 54363,  10970> velocity=<-5, -1>\n" +
            "position=<-43110, -32363> velocity=< 4,  3>\n" +
            "position=< 32689, -21526> velocity=<-3,  2>\n" +
            "position=<-53938, -54024> velocity=< 5,  5>\n" +
            "position=<-53943, -54017> velocity=< 5,  5>\n" +
            "position=<-10656,  43458> velocity=< 1, -4>\n" +
            "position=< 54343,  43462> velocity=<-5, -4>\n" +
            "position=<-10627, -32354> velocity=< 1,  3>\n" +
            "position=< 54369, -10697> velocity=<-5,  1>\n" +
            "position=< 11011,  54287> velocity=<-1, -5>\n" +
            "position=<-10659, -43192> velocity=< 1,  4>\n" +
            "position=< 11039, -21523> velocity=<-1,  2>\n" +
            "position=< 21826,  43457> velocity=<-2, -4>\n" +
            "position=<-32308,  32624> velocity=< 3, -3>\n" +
            "position=<-53935, -32361> velocity=< 5,  3>\n" +
            "position=< 11011,  32630> velocity=<-1, -3>\n" +
            "position=<-43134,  21801> velocity=< 4, -2>\n" +
            "position=<-43131, -21532> velocity=< 4,  2>\n" +
            "position=<-32289, -54019> velocity=< 3,  5>\n" +
            "position=< 21883, -54021> velocity=<-2,  5>\n" +
            "position=<-43148, -21528> velocity=< 4,  2>\n" +
            "position=<-32289, -32362> velocity=< 3,  3>\n" +
            "position=<-43108,  32623> velocity=< 4, -3>\n" +
            "position=< 21868,  32632> velocity=<-2, -3>\n" +
            "position=<-10619,  54287> velocity=< 1, -5>\n" +
            "position=<-21493,  54288> velocity=< 2, -5>\n" +
            "position=< 54319,  32631> velocity=<-5, -3>\n" +
            "position=< 54346,  54294> velocity=<-5, -5>\n" +
            "position=< 11032,  54292> velocity=<-1, -5>\n" +
            "position=<-21458,  21798> velocity=< 2, -2>\n" +
            "position=< 32657,  43463> velocity=<-3, -4>\n" +
            "position=< 11051, -43186> velocity=<-1,  4>\n" +
            "position=<-32281, -43188> velocity=< 3,  4>\n" +
            "position=<-53943,  21793> velocity=< 5, -2>\n" +
            "position=<-10611,  43456> velocity=< 1, -4>\n" +
            "position=< 32676, -43190> velocity=<-3,  4>\n" +
            "position=<-43136,  10969> velocity=< 4, -1>\n" +
            "position=< 11008,  32630> velocity=<-1, -3>\n" +
            "position=< 54332, -21525> velocity=<-5,  2>\n" +
            "position=<-21494, -32363> velocity=< 2,  3>\n" +
            "position=<-10639, -21524> velocity=< 1,  2>\n" +
            "position=< 43499, -10697> velocity=<-4,  1>\n" +
            "position=<-43128, -32360> velocity=< 4,  3>\n" +
            "position=< 43525, -10692> velocity=<-4,  1>\n" +
            "position=< 11032,  54286> velocity=<-1, -5>\n" +
            "position=< 21834, -54024> velocity=<-2,  5>\n" +
            "position=<-53951,  10964> velocity=< 5, -1>\n" +
            "position=<-21497, -10692> velocity=< 2,  1>\n" +
            "position=<-32273, -10695> velocity=< 3,  1>\n" +
            "position=<-10627,  43454> velocity=< 1, -4>\n" +
            "position=< 54378,  10970> velocity=<-5, -1>\n" +
            "position=<-10633,  54294> velocity=< 1, -5>\n" +
            "position=<-10639, -32363> velocity=< 1,  3>\n" +
            "position=<-53959,  54287> velocity=< 5, -5>\n" +
            "position=< 11048,  10964> velocity=<-1, -1>\n" +
            "position=<-10649,  10961> velocity=< 1, -1>\n" +
            "position=< 43524,  43462> velocity=<-4, -4>\n" +
            "position=< 10995,  21801> velocity=<-1, -2>\n" +
            "position=< 43523,  32623> velocity=<-4, -3>\n" +
            "position=<-53932, -32354> velocity=< 5,  3>\n" +
            "position=< 11008,  54285> velocity=<-1, -5>\n" +
            "position=<-32301,  32627> velocity=< 3, -3>\n" +
            "position=< 11003, -54021> velocity=<-1,  5>\n" +
            "position=< 21858, -32362> velocity=<-2,  3>\n" +
            "position=<-43128, -32357> velocity=< 4,  3>\n" +
            "position=< 43504, -21532> velocity=<-4,  2>\n" +
            "position=<-53972,  54289> velocity=< 5, -5>\n" +
            "position=<-43104,  32627> velocity=< 4, -3>\n" +
            "position=< 54324,  10966> velocity=<-5, -1>\n" +
            "position=<-21442,  21794> velocity=< 2, -2>"
    var grid = Grid.parse(input)
    var secs = 0

    while (!grid.smallEnough()) {
        grid = grid.advance()
        secs++
    }

    println(secs)
    println("in ${Date().time - start.time}ms")
}

data class XY(val x: Int, val y: Int) {
    fun add(other: XY) = XY(x + other.x, y + other.y)
}

data class Range2D(val min: XY, val max: XY)

data class Particle(val position: XY, val velocity: XY) {
    fun advance(): Particle {
        return Particle(position.add(velocity), velocity)
    }
}

data class Grid(val particles: List<Particle>) {
    fun advance(): Grid {
        return Grid(particles.map { it.advance() })
    }

    fun smallEnough(): Boolean {
        val rng = range()
        return rng.max.y - rng.min.y <= 10
    }

    fun range(): Range2D {
        var minX = Int.MAX_VALUE
        var maxX = Int.MIN_VALUE
        var minY = Int.MAX_VALUE
        var maxY = Int.MIN_VALUE

        for (p in particles) {
            minX = min(minX, p.position.x)
            maxX = max(maxX, p.position.x)
            minY = min(minY, p.position.y)
            maxY = max(maxY, p.position.y)
        }

        return Range2D(XY(minX, minY), XY(maxX, maxY))
    }

    override fun toString(): String {
        println("start toString")
        val rng = range()

        val lines = (rng.min.y..rng.max.y).map { y ->
            (rng.min.x..rng.max.x).map { x ->
                if (particleAt(XY(x, y))) "#" else "."
            }.joinToString("")
        }
        return "\n" + lines.joinToString("\n") + "\n"
    }

    private fun particleAt(c: XY): Boolean {
        return particles.any { p -> p.position == c }
    }


    companion object {
        fun parse(input: String): Grid {
            val num = "((?:-| )?[0-9]+)"
            val re = Regex("position=<${num}, ${num}> velocity=<${num}, ${num}>")
            val particles = input
                    .split('\n')
                    .map {line ->
                        val m = re.find(line) ?: throw Error("Parse error at ${line}")
                        Particle(
                                position = XY(
                                        m.groupValues[1].trimStart().toInt(),
                                        m.groupValues[2].trimStart().toInt()
                                ),
                                velocity = XY(
                                        m.groupValues[3].trimStart().toInt(),
                                        m.groupValues[4].trimStart().toInt()
                                )
                        )
                    }
            return Grid(particles)
        }
    }
}