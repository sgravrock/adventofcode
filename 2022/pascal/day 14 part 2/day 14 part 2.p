program day14p2;

{ x: 326..675, y: 0..175 }

	uses
		FileUtils, CaveInterface, Graphics;

	procedure ReadCave (var cave: CaveType; var f: text);
		var
			x, y, x1, y1, x2, y2, n: integer;
			hasPrevPoint: boolean;
			c: char;
	begin
		ShowDrawing; { since we draw walls and ledges as we read }

		while not eof(f) do
			begin
				hasPrevPoint := false;

				while not eoln(f) do
					begin
						read(f, x2);
						read(f, c); { consume ', ' }
						read(f, y2);

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
									writeln('Can''t have diagonal rocks');
									halt;
								end;

						x1 := x2;
						y1 := y2;
						hasPrevPoint := true;

						if not eoln(f) then
							begin
				{ Consume ' -> ' }
								read(f, c);
								read(f, c);
								read(f, c);
								read(f, c);
							end;
					end;

				readln(f);
			end;
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
									writeln(numAtRest : 1, ' so far');
							end;
					end;
			end;

		Solve := numAtRest;
	end;


	var
		cave: ^CaveType;
		inputFile: Text;
		result: integer;

begin
	SetUpDrawingWindow;

	if OpenInputFile(inputFile) then
		begin
			DrawCaveFloor;
			new(cave);
			writeln('reading input');
			ReadCave(cave^, inputFile);
			writeln('solving');
			result := Solve(cave^);
			writeln(result);
			SysBeep(10);
			writeln('Press return to exit');
			readln;
		end
	else
		writeln('Did not open input file');
end.