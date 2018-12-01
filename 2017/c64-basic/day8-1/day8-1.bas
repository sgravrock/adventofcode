10 REM allocate and initialize globals
15 REM RAM is abundant nowadays but BASIC only lets us use a little
20 DIM PROGRAM$(1000)
30 DIM RN$(30)
40 DIM RV(30)
60 DIM OUT$(1000)
70 REM run some self-tests first
75 GOSUB 10000
80 REM read the program
90 GOSUB 500
100 REM initialize registers
110 GOSUB 1900
120 REM execute the program
130 FOR PC = 1 TO PROGRAMLN
140 LINE$ = PROGRAM$(PC)
150 GOSUB 1100
160 NEXT PC
170 REM find the largest register value
180 GOSUB 1800
185 GOSUB 2000
190 PRINT "Largest register value is "; OUT
200 END


500 REM Read program
510 PRINT "ENTER ONE INSTRUCTION PER LINE"
520 PRINT "FINISH BY ENTERING THE WORD END ON ITS OWN LINE"
530 PROGRAMLN = 0
540 INPUT "> "; LINE$
550 IF LINE$ = "END" THEN RETURN
560 PROGRAMLN = PROGRAMLN + 1
570 PROGRAM$(PROGRAMLN) = LINE$
580 GOTO 540

700 REM Split a line into string-delimited words
710 REM args: LINE$
715 REM return: fields in WS$(), count in N
720 N = 0: S = 1
730 FOR I = 1 TO LEN(LINE$)
740 C$ = MID$(LINE$, I, 1)
750 IF C$ <> " " THEN GOTO 790
760 N = N + 1
770 WS$(N) = MID$(LINE$, S, I - S)
780 S = I + 1
790 NEXT I
800 REM copy last token
810 N = N + 1
820 WS$(N) = MID$(LINE$, S, LEN(LINE$) - S + 1)
830 RETURN

1000 REM find the index of a named register
1010 REM args: RN$, the register name
1020 FOR I = 1 TO 30
1030 IF RN$(I) <> RN$ THEN GOTO 1060
1040 OUT = I
1050 RETURN
1060 NEXT I
1070 REM Not found. Allocate.
1080 GOSUB 2100
1090 RETURN

1100 REM execute an instruction
1110 REM args: LINE$, the instruction
1115 REM split the line
1120 GOSUB 700
1130 REM should we execute?
1140 GOSUB 1300
1150 IF EX$ <> "y" THEN RETURN
1160 REM find delta
1170 LINE$ = WS$(3)
1180 GOSUB 2300
1190 D = OUT
1200 IF WS$(2) = "dec" THEN D = D * -1
1210 RN$ = WS$(1)
1230 GOSUB 1000
1240 RV(OUT) = RV(OUT) + D
1270 RETURN

1300 REM Determine if an instruction should be executed
1310 REM args: fields in WS$()
1320 REM return: EX$ = "y" or "n"
1330 REM find the register to check
1340 RN$ = WS$(5)
1350 GOSUB 1000
1360 RN = OUT
1370 REM convert the right operand
1380 LINE$ = WS$(7)
1390 GOSUB 2300
1400 ROP = OUT
1410 EX$ = "n"
1420 REM check the condition
1430 IF WS$(6) = "!=" THEN GOTO 1500
1440 IF WS$(6) = "<" THEN GOTO 1520
1450 IF WS$(6) = "<=" THEN GOTO 1540
1460 IF WS$(6) = "==" THEN GOTO 1560
1470 IF WS$(6) = ">" THEN GOTO 1580
1480 IF WS$(6) = ">=" THEN GOTO 1600
1490 PRINT "Fatal: Unrecognized operator '"; WS$(6); "'"
1495 END
1500 IF RV(RN) <> ROP THEN EX$ = "y"
1510 RETURN
1520 IF RV(RN) < ROP THEN EX$ = "y"
1530 RETURN
1540 IF RV(RN) <= ROP THEN EX$ = "y"
1550 RETURN
1560 IF RV(RN) = ROP THEN EX$ = "y"
1570 RETURN
1580 IF RV(RN) > ROP THEN EX$ = "y"
1590 RETURN
1600 IF RV(RN) >= ROP THEN EX$ = "y"
1610 RETURN

1800 REM find the largest register value
1810 REM output: OUT
1820 OUT = 0
1830 FOR I = 1 TO 30
1840 IF RV(I) > OUT THEN OUT = RV(I)
1850 NEXT I
1860 RETURN


1900 REM initialize registers
1910 FOR I = 1 TO 30
1920 RV(I) = 0
1930 NEXT I
1940 RETURN

2000 REM list registers, for debugging purposes
2010 FOR I = 1 TO 30
2030 NEXT I
2040 RETURN

2100 REM allocate a register
2110 REM args: RN$, the register name
2120 FOR I = 1 TO 30
2130 IF RN$(I) <> "" THEN GOTO 2160
2140 RN$(I) = RN$
2145 OUT = I
2150 RETURN
2160 NEXT I
2170 PRINT "Fatal: Could not allocate a register"
2180 GOSUB 2000
2190 END

2300 REM convert a string of digits to a number
2310 REM input: LINE$, output: OUT
2320 OUT = 0
2330 M = 1
2340 FOR I = 1 TO LEN(LINE$)
2350 C$ = MID$(LINE$, I, 1)
2360 IF I = 1 AND C$ = "-" THEN GOTO 2390
2370 OUT = OUT * 10 + ASC(MID$(LINE$, I, 1)) - ASC("0")
2380 GOTO 2400
2390 M = -1
2400 NEXT I
2410 OUT = OUT * M
2420 RETURN

10000 REM self-tests
10010 GOSUB 10400
10020 REM initialize registers
10030 GOSUB 1900
10040 LINE$ = "z dec 231 if bfx > -10"
10050 EX = -231
10060 GOSUB 10200
10070 GOSUB 1900
10080 LINE$ = "z inc 1 if b < 5"
10090 EX = 1
10100 GOSUB 10200
10195 RETURN

10200 REM test an instruction
10210 REM input: LINE$, the instruction; EX: the expected value of z
10220 REM execute the instruction
10225 INS$ = LINE$
10230 Z = 0
10235 GOSUB 1100
10240 REM get the index of the destination register
10250 RN$ = "z"
10260 GOSUB 1000
10270 AC = RV(OUT)
10280 IF AC = EX THEN RETURN
10290 PRINT "Fatal: expected z to be "; STR$(EX); " but it was "; STR$(AC)
10300 PRINT "Fatal: after executing "; INS$
10310 END

10400 REM test integer parsing
10410 LINE$ = "12345"
10420 GOSUB 2300
10430 IF OUT = 12345 THEN GOTO 10460
10440 PRINT "Fatal: 12345 parsed to "; STR$(OUT)
10450 EXIT
10460 LINE$ = "-10"
10470 GOSUB 2300
10480 IF OUT = -10 THEN GOTO 10510
10490 PRINT "Fatal: -10 parsed to "; STR$(OUT)
10500 EXIT
10510 RETURN
