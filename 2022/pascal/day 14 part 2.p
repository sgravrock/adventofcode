program day14p2;

{ Completes in about 1 hour and 9 minutes. }
{ x: 326..675, y: 0..175 }

	uses
		FileUtils;

	const
		MIN_X = 326;
		MAX_X = 674;
		MIN_Y = 0;
		MAX_Y = 175; { 175 for puzzle input, 11 for sample }
 { TODO don't hardcode this }

	type
		CaveType = packed array[MIN_Y..MAX_Y, MIN_X..MAX_X] of boolean;

	procedure ReadCave (var cave: CaveType; var f: text);
		var
			x, y, x1, y1, x2, y2, n: integer;
			hasPrevPoint: boolean;
			c: char;
	begin
		for x := MIN_X to MAX_X do
			for y := MIN_Y to MAX_Y do
				cave[y][x] := false;

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
								end
							else if y1 = y2 then
								begin
									if x2 < x1 then
										for x := x1 downto x2 do
											cave[y1][x] := true
									else
										for x := x1 to x2 do
											cave[y1][x] := true;
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


	function GetCellType (var cave: CaveType; x, y: integer): boolean;
	begin
		if y = MAX_Y then
			GetCellType := true
		else
			GetCellType := cave[y][x];
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
				sandY := MIN_Y;
				atRest := false;

				while not atRest do
					begin
						if not GetCellType(cave, sandX, sandY + 1) then
							sandY := sandY + 1
						else if not GetCellType(cave, sandX - 1, sandY + 1) then
							begin
								sandX := sandX - 1;
								sandY := sandY + 1;
							end
						else if not GetCellType(cave, sandX + 1, sandY + 1) then
							begin
								sandX := sandX + 1;
								sandY := sandY + 1;
							end
						else
							begin
								cave[sandY][sandX] := true;
								atRest := true;
								numAtRest := numAtRest + 1;

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