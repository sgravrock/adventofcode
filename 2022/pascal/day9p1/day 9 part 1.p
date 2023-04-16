program day9p1;
	uses
		FileUtils, Rope, RopeTests, BigBitSet;

	const
		enableDebugging = false;
		findingRanges = false;

	function Travel (var f: text): integer;
		var
			dir, space: char;
			dist: integer;
			theRope: Rope;
			visited: BigBitSet;
			minSeenX, minSeenY, maxSeenX, maxSeenY: integer;
			numVisited: LongInt;

		procedure Visit;
		begin
			if false then
{if (theRope.t.x < minX) or (theRope.t.x > maxX) or (theRope.t.y < minY) or (theRope.t.y > maxY) then}
				begin
					writeln('out of range: x=', theRope.t.x : 1, ' y=', theRope.t.y : 1);
					halt;
				end;

			BBSInsert(visited, theRope.t.x, theRope.t.y);
		end;

		procedure VisitForRangeFinding;
		begin
			if theRope.t.x < minSeenX then
				minSeenX := theRope.t.x;
			if theRope.t.x > maxSeenX then
				maxSeenX := theRope.t.x;
			if theRope.t.y < minSeenY then
				minSeenY := theRope.t.y;
			if theRope.t.y > maxSeenY then
				maxSeenY := theRope.t.y;
		end;

	begin
		theRope.h.x := 0;
		theRope.h.y := 0;
		theRope.t := theRope.h;
		minSeenX := 0;
		minSeenY := 0;
		maxSeenX := 0;
		maxSeenY := 0;
		BBSInit(visited);

		while not eof(f) do
			begin
				read(f, dir);
				read(f, space);
				read(f, dist);
				readln(f);

				if findingRanges then
					MoveRope(theRope, dir, dist, VisitForRangeFinding)
				else
					MoveRope(theRope, dir, dist, Visit);

				if enableDebugging then
					begin
						writeln('moving dir=', dir, ' dist=', dist : 1);
						writeln('h=', theRope.h.x : 1, ',', theRope.h.y : 1, ' t=', theRope.t.x : 1, ',', theRope.t.y : 1);
						writeln('press enter');
						readln;
					end;
			end;

		if findingRanges then
			begin
				writeln('x=', minSeenX : 1, '..', maxSeenX : 1);
				writeln('y=', minSeenY : 1, '..', maxSeenY : 1);
			end;

		writeln('The tail visited ', visited.size : 1, ' spaces');
		writeln('Used ', BBSPageCount(visited), ' pages');
		BBSFree(visited);
	end;

	var
		inputFile: Text;
		result: LongInt;

begin
	ShowText;
	RunRopeTests;

	if OpenInputFile(inputFile) then
		begin
			result := Travel(inputFile);
			writeln('Press enter');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');

end.