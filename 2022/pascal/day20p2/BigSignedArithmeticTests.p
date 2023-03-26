unit BigSignedArithmeticTests;

interface

	uses
		BigArithmetic, BigSignedArithmetic;

	procedure RunBigSignedArithmeticTests (var outerNTests: integer; var outerNFailed: integer);

implementation

	var
		nTests, nFailed: integer;

	procedure TestBigSignedIntToStringZero;
		var
			n: BigSignedInt;
			actual: string;
	begin
		n.isNegative := false;
		BigFromByte(0, n.magnitude);
		actual := BigSignedIntToString(n);

		if actual = '0' then
			writeln('OK: TestBigSignedIntToStringZero')
		else
			begin
				writeln('FAIL: TestBigSignedIntToStringZero');
				writeln('Expected "0" but got "', actual, '"');
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;


	procedure TestBigSignedIntToStringPositive;
		var
			n: BigSignedInt;
			actual: string;
	begin
		n.isNegative := false;
		BigFromLong(1623178306, n.magnitude);
		actual := BigSignedIntToString(n);

		if actual = '1623178306' then
			writeln('OK: TestBigSignedIntToStringPositive')
		else
			begin
				writeln('FAIL: TestBigSignedIntToStringPositive');
				writeln('Expected "1623178306" but got "', actual, '"');
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;


	procedure TestBigSignedIntToStringNegative;
		var
			n: BigSignedInt;
			actual: string;
	begin
		n.isNegative := true;
		BigFromLong(1623178306, n.magnitude);
		actual := BigSignedIntToString(n);

		if actual = '-1623178306' then
			writeln('OK: TestBigSignedIntToStringNegative')
		else
			begin
				writeln('FAIL: TestBigSignedIntToStringNegative');
				writeln('Expected "-1623178306" but got "', actual, '"');
				nFailed := nFailed + 1;
			end;

		nTests := nTests + 1;
	end;


	procedure RunBigSignedArithmeticTests (var outerNTests: integer; var outerNFailed: integer);
	begin
		nTests := 0;
		nFailed := 0;

		TestBigSignedIntToStringZero;
		TestBigSignedIntToStringPositive;
		TestBigSignedIntToStringNegative;

		outerNTests := outerNTests + nTests;
		outerNFailed := outerNFailed + nFailed;
	end;
end.