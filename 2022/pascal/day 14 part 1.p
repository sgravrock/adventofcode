program day14p1;

{ x: 449..505, y: 13..173 }

	uses
		FileUtils;

	const
		MIN_X = 448;
		MAX_X = 505;
		MIN_Y = 0;
		MAX_Y = 173; { 173 for puzzle input, 9 for sample }
 { TODO don't hardcode this }

	type
		Cell = (air, rock, sand);
		CaveType = array[MIN_Y..MAX_Y, MIN_X..MAX_X] of Cell;

	procedure ReadCave (var cave: CaveType; var f: text);
		var
			x, y, x1, y1, x2, y2, n: integer;
			hasPrevPoint: boolean;
			c: char;
	begin
		for x := MIN_X to MAX_X do
			for y := MIN_Y to MAX_Y do
				cave[y][x] := air;

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
											cave[y][x1] := rock
									else
										for y := y1 to y2 do
											cave[y][x1] := rock;
								end
							else if y1 = y2 then
								begin
									if x2 < x1 then
										for x := x1 downto x2 do
											cave[y1][x] := rock
									else
										for x := x1 to x2 do
											cave[y1][x] := rock;
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
		while true do
			begin
				sandX := 500;
				sandY := MIN_Y;
				atRest := false;

				while not atRest do
					begin
						if sandY >= MAX_Y then
							begin
								Solve := numAtRest;
								exit(Solve);
							end;

						if cave[sandY + 1][sandX] = air then
							sandY := sandY + 1
						else if cave[sandY + 1][sandX - 1] = air then
							begin
								sandX := sandX - 1;
								sandY := sandY + 1;
							end
						else if cave[sandY + 1][sandX + 1] = air then
							begin
								sandX := sandX + 1;
								sandY := sandY + 1;
							end
						else
							begin
								cave[sandY][sandX] := sand;
								atRest := true;
								numAtRest := numAtRest + 1;

								if (numAtRest mod 50) = 0 then
									writeln(numAtRest : 1, ' so far');
							end;
					end;
			end;
	end;


	var
		cave: ^CaveType;
		inputFile: Text;
		result: integer;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			new(cave);
			writeln('reading input');
			ReadCave(cave^, inputFile);
			writeln('solving');
			result := Solve(cave^);
			writeln(result);
{PrintCave(cave^);}
		end
	else
		writeln('Did not open input file');
end.