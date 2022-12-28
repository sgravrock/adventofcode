unit Tests;

interface

	uses
		Packets;

	procedure RunTests;

implementation

	procedure RunTests;
		var
			buf: ^InputBuffer;
			numTests, numFailed: integer;

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
				actual: OrderType;
		begin
			buf^.sz := 0;
			CopyToBuf(left);
			CopyToBuf(right);
			actual := Order(buf^, 1, length(left), length(left) + 2, buf^.sz - 1);

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
		writeln('Running self-tests');
		new(buf);
		numTests := 0;
		numFailed := 0;

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

		dispose(buf);
		if numFailed = 0 then
			writeln('All ', numTests : 1, ' tests passed')
		else
			begin
				writeln(numFailed : 1, ' of ', numTests : 1, ' tests failed');
				halt;
			end;
	end;

end.