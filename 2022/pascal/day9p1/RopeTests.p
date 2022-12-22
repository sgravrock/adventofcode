unit RopeTests;

interface
	uses
		Rope;

	procedure RunRopeTests;

implementation

	procedure WriteRope (var r: Rope);
	begin
		writeln('h(x=', r.h.x : 1, ' y=', r.h.y : 1, ') t(x=', r.t.x : 1, ' y=', r.t.y : 1, ')');
	end;

	function RopesEqual (var a, b: Rope): boolean;
	begin
		RopesEqual := (a.h.x = b.h.x) and (a.h.y = b.h.y) and (a.t.x = b.t.x) and (a.t.y = b.t.y);
	end;

	procedure Mk (var r: Rope; hx, hy, tx, ty: integer);
	begin
		r.h.x := hx;
		r.h.y := hy;
		r.t.x := tx;
		r.t.y := ty;
	end;

	procedure RunRopeTests;
		var
			src, expectedDest, actualDest: Rope;
			numTests, numFailed: integer;

		procedure CopyAndMove (dir: char; dist: integer);
			procedure Visit;
			begin
			end;
		begin
			actualDest.h.x := src.h.x;
			actualDest.h.y := src.h.y;
			actualDest.t.x := src.t.x;
			actualDest.t.y := src.t.y;
			MoveRope(actualDest, dir, dist, Visit);
		end;

		procedure AssertMove (desc: string);
		begin
			numTests := numTests + 1;
			if not RopesEqual(actualDest, expectedDest) then
				begin
					writeln(desc, ':');
					write('   expected move from ');
					WriteRope(src);
					write('   to                 ');
					WriteRope(expectedDest);
					write('   but ended up at    ');
					WriteRope(actualDest);
					writeln;
					numFailed := numFailed + 1;
				end;
		end;

	begin
		writeln('Running rope tests');
		numTests := 0;
		numFailed := 0;

		Mk(src, 0, 0, 0, 0);
		Mk(expectedDest, 0, -1, 0, 0);
		CopyAndMove('U', 1);
		AssertMove('up 1 from h on t');

		Mk(expectedDest, 3, 0, 2, 0);
		CopyAndMove('R', 3);
		AssertMove('right 3 from h on t');

		Mk(src, 0, 0, 1, 1);
		Mk(expectedDest, -1, 0, 0, 0);
		CopyAndMove('L', 1);
		AssertMove('Left 1 away from diagonal');

		Mk(src, 0, 0, 1, 1);
		Mk(expectedDest, 0, 1, 1, 1);
		CopyAndMove('D', 1);
		AssertMove('Down 1 toward from diagonal');

		Mk(src, 0, 0, 1, 0);
		Mk(expectedDest, 1, 0, 1, 0);
		CopyAndMove('R', 1);
		AssertMove('Back to h on t');

		Mk(src, 1, -4, 2, -4);
		Mk(expectedDest, 1, -3, 2, -4);
		CopyAndMove('D', 1);
		AssertMove('Into diagonal');

		if numFailed = 0 then
			writeln('Rope tests ok')
		else
			begin
				writeln(numFailed : 1, ' of ', numTests : 1, ' rope tests failed');
				halt;
			end;
	end;

end.