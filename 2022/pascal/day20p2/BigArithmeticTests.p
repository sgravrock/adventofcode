unit BigArithmeticTests;

interface

	uses
		BigArithmetic;

	procedure RunBigArithmeticTests (var outerNTests: integer; var outerNFailed: integer);

implementation

	var
		nTests, nFailed: integer;

	function BigEqual (a, b: BigInt): boolean;
		var
			i: integer;
	begin
		for i := 1 to bigIntNumBytes do
			if a[i] <> b[i] then
				begin
					BigEqual := false;
					exit(BigEqual);
				end;

		BigEqual := true;
	end;

	procedure BigDump (bi: BigInt);
		var
			i: integer;
	begin
		for i := 1 to bigIntNumBytes do
			write(bi[i] : 1, ', ');
	end;

	procedure TestBigmul1;
		var
			a, b, expected, actual: BigInt;
	begin
{ 2509 * 230 = 57500 }
		BigZero(a);
		BigZero(b);
		BigZero(expected);
		a[1] := 250;
		b[1] := 230;
		expected[1] := 156;
		expected[2] := 224;
		BigMul(a, b, actual);

		if BigEqual(expected, actual) then
			writeln('OK: Bigmul1')
		else
			begin
				writeln('FAIL: Bigmul1');
				write('expected: ');
				BigDump(expected);
				writeln;
				write('actual: ');
				BigDump(actual);
				writeln;
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;

	procedure TestBigmul2;
		var
			a, b, expected, actual: BigInt;
	begin
{ 2669856333 * 8 = 21358850664 }
		BigZero(a);
		BigZero(b);
		BigZero(expected);
		a[1] := 77;
		a[2] := 198;
		a[3] := 34;
		a[4] := 159;
		b[1] := 8;
		expected[1] := 104;
		expected[2] := 50;
		expected[3] := 22;
		expected[4] := 249;
		expected[5] := 4;
		BigMul(a, b, actual);

		if BigEqual(expected, actual) then
			writeln('OK: Bigmul2')
		else
			begin
				writeln('FAIL: Bigmul2');
				write('expected: ');
				BigDump(expected);
				writeln;
				write('actual: ');
				BigDump(actual);
				writeln;
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;


	procedure TestBigmul3;
		var
			a, b, product: BigInt;
			expected, actual: string;
	begin
		BigFromLong(1124, a);
		BigFromLong(811589153, b);
		expected := '912226207972';
		BigMul(a, b, product);
		actual := BigToString(product);

		if actual = expected then
			writeln('OK: Bigmul3')
		else
			begin
				writeln('FAIL: Bigmul3');
				writeln('Expected ', expected, ' but got ', actual);
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;


	procedure TestBigadd;
		var
			a, b, expected, actual: BigInt;
	begin
		BigZero(a);
		BigZero(b);
		BigZero(expected);
		a[1] := 179;
		a[2] := 196;
		a[3] := 214;
		a[4] := 196;
		a[5] := 98;
		a[6] := 74;
		b[1] := 16;
		b[2] := 48;
		b[3] := 9;
		b[4] := 246;
		b[5] := 166;
		b[6] := 125;
		expected[1] := 195;
		expected[2] := 244;
		expected[3] := 223;
		expected[4] := 186;
		expected[5] := 9;
		expected[6] := 200;

		Bigadd(a, b, actual);

		if BigEqual(expected, actual) then
			writeln('OK: Bigadd')
		else
			begin
				writeln('FAIL: Bigadd');
				write('expected: ');
				BigDump(expected);
				writeln;
				write('actual: ');
				BigDump(actual);
				writeln;
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;

	procedure TestBigdiv;
		var
			a, expectedQ: BigInt;
			actual: BigDivResult;
	begin
		a[1] := 16;
		a[2] := 48;
		a[3] := 9;
		a[4] := 246;
		a[5] := 166;
		a[6] := 125;
		expectedQ[1] := 5;
		expectedQ[2] := 16;
		expectedQ[3] := 3;
		expectedQ[4] := 82;
		expectedQ[5] := 226;
		expectedQ[6] := 41;
		Bigdiv(a, 3, actual);

		if BigEqual(expectedQ, actual.quotient) and (actual.remainder = 1) then
			writeln('OK: Bigdiv')
		else
			begin
				writeln('FAIL: Bigdiv');
				write('expected quotient: ');
				BigDump(expectedQ);
				writeln;
				write('actual quotient: ');
				BigDump(actual.quotient);
				writeln;
				writeln('expected remainder: 1, actual: ', actual.remainder : 1);
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;

	procedure RunBigArithmeticTests (var outerNTests: integer; var outerNFailed: integer);
	begin
		nTests := 0;
		nFailed := 0;

		TestBigmul1;
		TestBigmul2;
		TestBigmul3;
		TestBigadd;
		TestBigdiv;

		outerNTests := outerNTests + nTests;
		outerNFailed := outerNFailed + nFailed;
	end;
end.