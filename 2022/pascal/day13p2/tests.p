unit Tests;

interface

	uses
		Config, Packets, Sorting;

	procedure RunTests;

implementation

	procedure RunTests;
		var
			numTests, numFailed: integer;


		procedure RunPacketTests;
			var
				buf: ^InputBuffer;

			procedure CopyToBuf (s: string);
				var
					i: integer;
			begin
				for i := 1 to length(s) do
					buf^.data[i + buf^.sz] := s[i];

				buf^.sz := buf^.sz + length(s) + 1;
				buf^.data[buf^.sz] := chr($d);
			end;

			procedure Test (left, right, desc: string; expected: OrderType);
				var
					leftRange, rightRange: range;
					actual: OrderType;
			begin
				buf^.sz := 0;
				CopyToBuf(left);
				CopyToBuf(right);
				leftRange.firstIx := 1;
				leftRange.lastIx := length(left);
				rightRange.firstIx := length(left) + 2;
				rightRange.lastIx := buf^.sz - 1;
				actual := Order(buf^, leftRange, rightRange);

				if actual = expected then
					writeln('OK: ', desc)
				else
					begin
						writeln('FAIL: ', desc, ': expected ', expected, ' but got ', actual);
						numFailed := numFailed + 1;
					end;

				numTests := numTests + 1;
			end;

		begin
			new(buf);

			Test('[5]', '[6]', 'Greater number on right', inOrder);
			Test('[6]', '[5]', 'Greater number on left', outOfOrder);
			Test('[1,2]', '[1,2,4] ', 'Shorter list on left ', inOrder);
			Test('[1,2,4]', '[1,2]', 'Shorter list on right', outOfOrder);
			Test('[[1],5]', '[[1,2],4] ', 'Shorter nested list on left ', inOrder);
			Test('[[1,2],4]', '[[1],5]', 'Shorter nested list on right', outOfOrder);
			Test('[1,2]', '[[2],1]', 'Mismatch in promoted list on left', inOrder);
			Test('[[1],2]', '[2,1]', 'Mismatch in promoted list on right', inOrder);
			Test('[1,2]', '[[1],1]', 'Match in promoted list on left', outOfOrder);
			Test('[[1],1]', '[1,2]', 'Match in promoted list on right', inOrder);
			Test('[[[2]]]', '[[2]]', 'Deeper nesting on left', TBD);
			Test('[[2]]', '[[[2]]]', 'Deeper nesting on right', TBD);
			Test('[[],5]', '[[[]],4]', 'Empty lists one deeper on right ', inOrder);
			Test('[[[]],4]', '[[],5]', 'Empty lists one deeper on left', outOfOrder);
			Test('[[2,[7,4]]]', '[2,[9]]', 'Pair 137 reduction', outOfOrder);
			Test('[[[6,10],[4,3,[4]]]]', '[[6,[[3,10],[],[],2,10],[[6,8,4,2]]],[]]', 'Part 2 failure case', outOfOrder);

			dispose(buf);
		end;


		procedure RunSortingTest;
			var
				arr: Sortable;
				i: integer;

			function cmp (a, b: integer): integer;
			begin
				if a < b then
					cmp := 1
				else if a = b then
					cmp := 0
				else
					cmp := -1;
			end;

		begin
			numTests := numTests + 1;
			arr[1] := 5;
			arr[2] := 10;
			arr[3] := 4;
			arr[4] := 6;
			arr[5] := 7;
			arr[6] := 2;
			arr[7] := 3;
			arr[8] := 1;
			arr[9] := 9;
			arr[10] := 8;

			Sort(arr, 10, cmp);

			for i := 1 to 10 do
				if arr[i] <> 10 - i + 1 then
					begin
						writeln('FAIL: sorting');
						numFailed := numFailed + 1;
						exit(RunSortingTest);
					end;

			writeln('OK: sorting');
		end;

	begin
		writeln('Running self-tests');
		numTests := 0;
		numFailed := 0;
		RunPacketTests;
		RunSortingTest;

		if numFailed = 0 then
			writeln('All ', numTests : 1, ' tests passed')
		else
			begin
				writeln(numFailed : 1, ' of ', numTests : 1, ' tests failed');
				halt;
			end;

	end;
end.