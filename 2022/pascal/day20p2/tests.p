unit Tests;

interface

	uses
		Lists, BigArithmetic, BigSignedArithmetic, GPS, BigArithmeticTests, BigSignedArithmeticTests;

	procedure RunTests;

implementation

	procedure RunTests;
		var
			numTests, numFailures: integer;


		procedure TestListConstruction;
			var
				list: CircularListPtr;

			procedure Fail (msg: string);
			begin
				writeln('FAIL: list construction');
				writeln(msg);
				numFailures := numFailures + 1;
				DisposeList(list);
				exit(TestListConstruction);
			end;

		begin
			numTests := numTests + 1;
			list := NewList;

			if list^.head <> nil then
				Fail('Expected list head to be initially nil');
			if list^.sz <> 0 then
				Fail('Expected list size to be initially 0');
			if list^.bufp = nil then
				Fail('Expected list to initially have a buffer');

			DisposeList(list);
			writeln('OK: list construction');
		end;


		procedure TestListAppend;
			var
				list: CircularListPtr;
				contents: array[1..4] of integer;

			procedure Fail (msg: string);
			begin
				writeln('FAIL: list append');
				writeln(msg);
				numFailures := numFailures + 1;
				DisposeList(list);
				exit(TestListAppend);
			end;

			procedure EnumerateForward (numExpected: integer);
				var
					i: integer;
					nodep: ListNodePtr;
			begin
				nodep := list^.head;
				for i := 1 to numExpected do
					begin
						contents[i] := nodep^.val;
						nodep := nodep^.next;
					end;
				if nodep <> list^.head then
					Fail('Expected to be back at the head after enumerating forward');
			end;

			procedure EnumerateBackward (numExpected: integer);
				var
					i: integer;
					nodep: ListNodePtr;
			begin
				nodep := list^.head^.prev;
				for i := 1 to numExpected do
					begin
						contents[i] := nodep^.val;
						nodep := nodep^.prev;
					end;
				if nodep^.next <> list^.head then
					Fail('Expected to be back at the head after enumerating backward');
			end;

		begin
			numTests := numTests + 1;
			list := NewList;
			ListAppend(list, 10);
			EnumerateForward(1);
			if contents[1] <> 10 then
				Fail('Wrong forward contents after first append');
			EnumerateBackward(1);
			if contents[1] <> 10 then
				Fail('Wrong backward contents after first append');

			ListAppend(list, 11);
			ListAppend(list, 12);
			ListAppend(list, 13);
			EnumerateForward(4);
			if not ((contents[1] = 10) and (contents[2] = 11) and (contents[3] = 12) and (contents[4] = 13)) then
				Fail('Wrong forward contents after last append');
			EnumerateBackward(4);
			if not ((contents[1] = 13) and (contents[2] = 12) and (contents[3] = 11) and (contents[4] = 10)) then
				Fail('Wrong backward contents after last append');

			writeln('OK: list append');
			DisposeList(list);
		end;


		procedure TestMix;
			var
				list: CircularListPtr;
				contents: array[1..7] of integer;

			procedure Fail (msg: string);
			begin
				writeln('FAIL: mix');
				writeln(msg);
				numFailures := numFailures + 1;
				DisposeList(list);
				exit(TestMix);
			end;

			procedure Enumerate (numExpected: integer);
				var
					i: integer;
					nodep: ListNodePtr;
			begin
				nodep := list^.head;
				for i := 1 to numExpected do
					begin
						contents[i] := nodep^.val;
						nodep := nodep^.next;
					end;
				if nodep <> list^.head then
					Fail('Expected to be back at the head after enumerating');
			end;

		begin
			numTests := numTests + 1;
			list := NewList;
			ListAppend(list, 1);
			ListAppend(list, 2);
			ListAppend(list, -3);
			ListAppend(list, 3);
			ListAppend(list, -2);
			ListAppend(list, 0);
			ListAppend(list, 4);

			Mix(list);

			Enumerate(7);
			if contents[1] <> 0 then
				Fail('Mismatch at [1]');
			if contents[2] <> -3 then
				Fail('Mismatch at [2]');
			if contents[3] <> 2 then
				Fail('Mismatch at [3]');
			if contents[4] <> 4 then
				Fail('Mismatch at [4]');
			if contents[5] <> -2 then
				Fail('Mismatch at [5]');
			if contents[6] <> 3 then
				Fail('Mismatch at [6]');
			if contents[7] <> 1 then
				Fail('Mismatch at [7]');

			DisposeList(list);
			writeln('OK: mix');
		end;


		procedure TestDecrypt;
			var
				list: CircularListPtr;
				result: BigSignedInt;
				asString: string;

			procedure Fail (msg: string);
			begin
				writeln('FAIL: decrypt');
				writeln(msg);
				numFailures := numFailures + 1;
				DisposeList(list);
				exit(TestDecrypt);
			end;

		begin
			numTests := numTests + 1;
			list := NewList;
			ListAppend(list, 1);
			ListAppend(list, 2);
			ListAppend(list, -3);
			ListAppend(list, 3);
			ListAppend(list, -2);
			ListAppend(list, 0);
			ListAppend(list, 4);

			result := Decrypt(list);
			asString := BigSignedIntToString(result);
			if asString <> '1623178306' then
				Fail(StringOf('Expected sum to be 1623178306 but it was ', asString));

			DisposeList(list);
			writeln('OK: decrypt');
		end;


	begin
		numTests := 0;
		numFailures := 0;

		RunBigArithmeticTests(numTests, numFailures);
		RunBigSignedArithmeticTests(numTests, numFailures);
		TestListConstruction;
		TestListAppend;
		TestMix;
		TestDecrypt;

		if numFailures = 0 then
			writeln('All ', numTests : 1, ' tests passed.')
		else
			begin
				writeln(numFailures : 1, ' of ', numTests : 1, ' tests failed');
				halt;
			end;
	end;

end.