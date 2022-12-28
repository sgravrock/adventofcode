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

		procedure TestOrder (left, right, desc: string; expected: OrderType);
			var
				actual: OrderType;
				leftList, rightList: ListPtr;
				ignored: integer;
		begin
			InitListPool;
			buf^.sz := 0;
			CopyToBuf(left);
			CopyToBuf(right);
			leftList := ParseList(buf^, 1, ignored);
			rightList := ParseList(buf^, length(left) + 2, ignored);
			actual := Order(leftList, rightList);
			DisposeListPool;

			if actual = expected then
				writeln('OK: ', desc)
			else
				begin
					writeln('FAIL: ', desc, ': expected ', expected, ' but got ', actual);
					numFailed := numFailed + 1;
				end;

			numTests := numTests + 1;
		end;

		procedure TestParseList;
			var
				root: ListPtr;
				lastI: integer;

			procedure Fail;
			begin
				numFailed := numFailed + 1;
				writeln('FAIL: parse list');
				exit(TestParseList);
			end;

			procedure ExpectNumber (desc: string; actual: ListElement; expected: integer);
			begin
				if actual.t <> listElNum then
					begin
						writeln('Expected ', desc, ' to be ', expected : 1, ' but it was a list');
						Fail;
					end;

				if actual.n <> expected then
					begin
						writeln('Expected ', desc, ' to be ', expected : 1, ' but it was ', actual.n : 1);
						Fail;
					end;
			end;

		begin
			numTests := numTests + 1;
			CopyToBuf('[1,[2,3,[4]],56]');
			lastI := buf^.sz - 1;
			InitListPool;
			root := ParseList(buf^, 1, lastI);

			if root^.sz <> 3 then
				begin
					writeln('Expected root to have sz 3 but it had ', root^.sz);
				end;

			ExpectNumber('[1]', root^.els[1], 1);
			ExpectNumber('[3]', root^.els[3], 56);
			DisposeListPool;
			writeln('OK: parse list');
		end;

	begin
		writeln('Running self-tests');
		new(buf);
		numTests := 0;
		numFailed := 0;

		TestParseList;

		TestOrder('[5]', '[6]', 'Greater number on right', inOrder);
		TestOrder('[6]', '[5]', 'Greater number on left', outOfOrder);
		TestOrder('[1,2]', '[1,2,4]', 'Shorter list on left ', inOrder);
		TestOrder('[1,2,4]', '[1,2]', 'Shorter list on right', outOfOrder);
		TestOrder('[[1],5]', '[[1,2],4]', 'Shorter nested list on left ', inOrder);
		TestOrder('[[1,2],4]', '[[1],5]', 'Shorter nested list on right', outOfOrder);
		TestOrder('[1,2]', '[[2],1]', 'Mismatch in promoted list on left', inOrder);
		TestOrder('[[1],2]', '[2,1]', 'Mismatch in promoted list on right', inOrder);
		TestOrder('[1,2]', '[[1],1]', 'Match in promoted list on left', outOfOrder);
		TestOrder('[[1],1]', '[1,2]', 'Match in promoted list on right', inOrder);
		TestOrder('[[[2]]]', '[[2]]', 'Deeper nesting on left', TBD);
		TestOrder('[[2]]', '[[[2]]]', 'Deeper nesting on right', TBD);
		TestOrder('[[],5]', '[[[]],4]', 'Empty lists one deeper on right ', inOrder);
		TestOrder('[[[]],4]', '[[],5]', 'Empty lists one deeper on left', outOfOrder);
		TestOrder('[[2,[7,4]]]', '[2,[9]]', 'Pair 137 reduction', outOfOrder);

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