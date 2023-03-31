program day14p2;

{ x: 326..675, y: 0..175 }

	uses
		MicroSysFileUtils, CaveInterface, UI;

	const
		bufsize = 11264;

	type
		EntireFile = packed array[1..bufsize] of char;

	function ReadInt (var buf: EntireFile; var i: LongInt): integer;
		var
			result, c: integer;
	begin
		result := 0;
		c := ord(buf[i]);

		while (c >= 48) and (c <= 57) do
			begin
				result := result * 10 + c - 48;
				i := i + 1;
				c := ord(buf[i]);
			end;

		ReadInt := result;
	end;

	procedure ReadCave (var cave: CaveType; path: integer);
		var
			x, y, x1, y1, x2, y2, n: integer;
			hasPrevPoint: boolean;
			c: char;
			bufp: ^EntireFile;
			fileSz, i: LongInt;
	begin
		new(bufp);
		fileSz := bufsize;
		if ReadEntireFile(path, Ptr(bufp), fileSz) <> FileErrorOk then
			begin
				ShowError('read failed');
				halt;
			end;

		cave.floorY := 0;
		i := 1;

		while i <= fileSz do
			begin
				hasPrevPoint := false;

				while (i <= fileSz) and (bufp^[i] <> chr($d)) do
					begin
						x2 := ReadInt(bufp^, i);
						i := i + 1; { consume ',' }
						y2 := ReadInt(bufp^, i);

						if y2 > cave.floorY then
							cave.floorY := y2;

						if hasPrevPoint then
							if x1 = x2 then
								begin
									if y2 < y1 then
										for y := y1 downto y2 do
											cave.cells[y][x1] := true
									else
										for y := y1 to y2 do
											cave.cells[y][x1] := true;
									DrawWall(x1, y1, y2);
								end
							else if y1 = y2 then
								begin
									if x2 < x1 then
										for x := x1 downto x2 do
											cave.cells[y1][x] := true
									else
										for x := x1 to x2 do
											cave.cells[y1][x] := true;
									DrawLedge(x1, x2, y1);
								end
							else
								begin
									ShowError('Can''t have diagonal rocks');
									halt;
								end;

						x1 := x2;
						y1 := y2;
						hasPrevPoint := true;

						if bufp^[i] <> chr($d) then
							i := i + 4; { Consume ' -> ' }
					end;

				i := i + 1; { Consume newline }
			end;

		cave.floorY := cave.floorY + 2;
		dispose(bufp);
	end;


	function Solve (var cave: CaveType): integer;
		const
{ Inside Macintosh says we should handle events (or otherwise call SystemTask) }
{ at least once per tick (a tick is 1/60 seconds). But doing that more than doubles }
{ our run time. Every 12 ticks strikes a reasonable balance between speed and UI }
{ responsiveness, although it's still pretty hostile to any background processes. }
{ If we were trying to handle more complex user interactions than just quitting, }
{ 12 ticks might be too much. }
			ticksBetweenEventHandling = 12;
{ Allow *some* background processing without sacrificing too much speed }
			maxSleepTicks = 1;
		var
			numAtRest, sandX, sandY: integer;
			atRest: boolean;
			i, itersBetwenEventHandling: integer;
			startTicks, ticks: LongInt;
	begin
		numAtRest := 0;
		itersBetwenEventHandling := -1;
		i := maxint;
		startTicks := TickCount;
		while not cave.cells[0][500] do
			begin
				sandX := 500;
				sandY := caveMinY;
				atRest := false;

				while not atRest do
					begin
						i := i - 1;

{ Give the user a chance to quit. }
{ We can't afford to call TickCount every time through the loop }
{ (doing so adds 3 minutes run time, or about 67%), so figure out }
{ how many iterations correspond to the desired number of ticks }
{ and then use a counter instead. "Micro-optimizations" to the }
{ counter itself also matter a great deal: decrementing and comparing }
{ to 0 is appreciably faster than incrementing and comparing to a }
{ limit. Doing modulus instead of an equality comparison more than }
{ doubles the total run time of the program. }
{ The result of all this is that UI event handling carries about a 9% }
{ overhead, vs more than 100% with a "by the book" approach. }

						if itersBetwenEventHandling = -1 then
							begin { Still determining timing }
								ticks := TickCount;
								if ticks >= startTicks + ticksBetweenEventHandling then
									begin
										itersBetwenEventHandling := maxint - i;
										i := itersBetwenEventHandling;
										HandleOneEvent(maxSleepTicks);
									end;
							end
						else if i = 0 then
							begin
								i := itersBetwenEventHandling;
								HandleOneEvent(maxSleepTicks);
							end;

						if gShouldQuit then
							exit(Solve);

						if sandY + 1 = cave.floorY then
							atRest := true
						else if not cave.cells[sandY + 1][sandX] then
							sandY := sandY + 1
						else if not cave.cells[sandY + 1][sandX - 1] then
							begin
								sandX := sandX - 1;
								sandY := sandY + 1;
							end
						else if not cave.cells[sandY + 1][sandX + 1] then
							begin
								sandX := sandX + 1;
								sandY := sandY + 1;
							end
						else
							atRest := true;

						if atRest then
							begin
								cave.cells[sandY][sandX] := true;
								atRest := true;
								numAtRest := numAtRest + 1;
								DrawSand(sandX, sandY);

								if numAtRest mod 50 = 0 then
									ShowSandCount(numAtRest);
							end;
					end;
			end;

		Solve := numAtRest;
	end;


	var
		cave: ^CaveType;
		inputFile: Text;
		result: integer;
		path: integer;
		openResult: FileError;

begin
	gShouldQuit := false;
	openResult := OpenInputFile(path);

	if openResult = FileErrorOk then
		begin
			ShowWaitCursor;
			new(cave);
			InitUI;
			ReadCave(cave^, path);
			DrawCaveCeiling;
			DrawCaveFloor(cave^.floorY);
			InitCursor;

			result := Solve(cave^);

			if not gShouldQuit then
				begin
					ShowStatus(StringOf('Result: ', result : 1, ' grains of sand at rest.'));
					SysBeep(10);
					PostSimulationEventLoop;
				end;
		end
	else if openResult <> FileErrorCanceled then
		ShowError('Error opening input file');
end.