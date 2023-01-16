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
		ShowDrawing; { since we draw walls and ledges as we read }

		new(bufp);
		fileSz := bufsize;
		if ReadEntireFile(path, Ptr(bufp), fileSz) <> FileErrorOk then
			begin
				ShowError('read failed');
				halt;
			end;

		i := 1;

		while i <= fileSz do
			begin
				hasPrevPoint := false;

				while bufp^[i] <> chr($d) do
					begin
						x2 := ReadInt(bufp^, i);
						i := i + 1; { consume ',' }
						y2 := ReadInt(bufp^, i);

						if hasPrevPoint then
							if x1 = x2 then
								begin
									if y2 < y1 then
										for y := y1 downto y2 do
											cave[y][x1] := true
									else
										for y := y1 to y2 do
											cave[y][x1] := true;
									DrawWall(x1, y1, y2);
								end
							else if y1 = y2 then
								begin
									if x2 < x1 then
										for x := x1 downto x2 do
											cave[y1][x] := true
									else
										for x := x1 to x2 do
											cave[y1][x] := true;
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

		dispose(bufp);
	end;


	function Solve (var cave: CaveType): integer;
		var
			numAtRest, sandX, sandY: integer;
			atRest: boolean;
	begin
		numAtRest := 0;
		while not cave[0][500] do
			begin
				sandX := 500;
				sandY := caveMinY;
				atRest := false;

				while not atRest do
					begin
						if sandY + 1 = caveMaxY then
							atRest := true
						else if not cave[sandY + 1][sandX] then
							sandY := sandY + 1
						else if not cave[sandY + 1][sandX - 1] then
							begin
								sandX := sandX - 1;
								sandY := sandY + 1;
							end
						else if not cave[sandY + 1][sandX + 1] then
							begin
								sandX := sandX + 1;
								sandY := sandY + 1;
							end
						else
							atRest := true;

						if atRest then
							begin
								cave[sandY][sandX] := true;
								atRest := true;
								numAtRest := numAtRest + 1;
								DrawSand(sandX, sandY);

								if ((numAtRest <= 500) and (numAtRest mod 50 = 0)) or (numAtRest mod 1000 = 0) then
									ShowStatus(StringOf(numAtRest : 1, ' grains of sand at rest so far'));
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
	SetUpDrawingWindow;
	openResult := OpenInputFile(path);

	if openResult = FileErrorOk then
		begin
			DrawCaveFloor;
			new(cave);

			ShowStatus('Reading input file...');
			ReadCave(cave^, path);

			ShowStatus('solving');
			result := Solve(cave^);
			ShowStatus(StringOf('Result: ', result : 1, ' grains of sand at rest. Cmd-q to exit.'));
			SysBeep(10);
			PostSimulationEventLoop;
		end
	else if openResult <> FileErrorCanceled then
		ShowError('Error opening input file');
end.