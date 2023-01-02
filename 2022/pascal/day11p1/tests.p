unit Tests;

interface

	uses
		BigArithmetic;

	procedure RunTests;

implementation

	var
		nTests, nFailed: integer;

	function BigEqual (a, b: BigInt): boolean;
		var
			i: integer;
	begin
		for i := 1 to 8 do
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
		for i := 1 to 8 do
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

	procedure TestBigadd;
		var
			a, b, expected, actual: BigInt;
	begin
{ 1223372036854170803 + 138156045840400 = 1223510192900011203 }
		BigZero(a);
		BigZero(b);
		BigZero(expected);
		a[1] := 179;
		a[2] := 196;
		a[3] := 214;
		a[4] := 196;
		a[5] := 98;
		a[6] := 74;
		a[7] := 250;
		a[8] := 16;
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
		expected[7] := 250;
		expected[8] := 16;

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
		a[7] := 0;
		a[8] := 0;
		expectedQ[1] := 5;
		expectedQ[2] := 16;
		expectedQ[3] := 3;
		expectedQ[4] := 82;
		expectedQ[5] := 226;
		expectedQ[6] := 41;
		expectedQ[7] := 0;
		expectedQ[8] := 0;
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

	procedure RunTests;
	begin
		writeln('Running self-tests');
		nTests := 0;
		nFailed := 0;

		TestBigmul1;
		TestBigmul2;
		TestBigadd;
		TestBigdiv;

		if nFailed = 0 then
			writeln('All ', nTests : 1, ' tests passed')
		else
			begin
				writeln(nFailed : 1, ' of ', nTests : 1, ' tests failed');
				halt;
			end;
	end;
end.