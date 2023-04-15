program day8p2;
	uses
		FileUtils;

	const
		maxGridSz = 100;

	type
		HeightGrid = record
				h: array[1..maxGridSz, 1..maxGridSz] of byte;
				sz: integer;
			end;


	procedure ReadGrid (var f: Text; var grid: HeightGrid);
		var
			j, i: integer;
			line: string;
	begin
		j := 0;
		while not eof(f) do
			begin
				readln(f, line);
				if line <> '' then
					begin
						j := j + 1;
						for i := 1 to length(line) do
							grid.h[j][i] := ord(line[i]) - 48;
					end;
			end;
		grid.sz := j;
	end;


	function ScenicScoreUp (var grid: HeightGrid; houseI, houseJ: integer): LongInt;
		var
			score: LongInt;
			i: integer;
			blocked: boolean;
	begin
		score := 0;
		blocked := false;
		i := houseI - 1;

		while (i >= 1) and not blocked do
			begin
				score := score + 1;
				if grid.h[i][houseJ] >= grid.h[houseI][houseJ] then
					blocked := true;
				i := i - 1;
			end;

		ScenicScoreUp := score;
	end;


	function ScenicScoreDown (var grid: HeightGrid; houseI, houseJ: integer): LongInt;
		var
			score: LongInt;
			i: integer;
			blocked: boolean;
	begin
		score := 0;
		blocked := false;
		i := houseI + 1;

		while (i <= grid.sz) and not blocked do
			begin
				score := score + 1;
				if grid.h[i][houseJ] >= grid.h[houseI][houseJ] then
					blocked := true;
				i := i + 1;
			end;

		ScenicScoreDown := score;
	end;


	function ScenicScoreLeft (var grid: HeightGrid; houseI, houseJ: integer): LongInt;
		var
			score: LongInt;
			j: integer;
			blocked: boolean;
	begin
		score := 0;
		blocked := false;
		j := houseJ - 1;

		while (j >= 1) and not blocked do
			begin
				score := score + 1;
				if grid.h[houseI][j] >= grid.h[houseI][houseJ] then
					blocked := true;
				j := j - 1;
			end;

		ScenicScoreLeft := score;
	end;


	function ScenicScoreRight (var grid: HeightGrid; houseI, houseJ: integer): LongInt;
		var
			score: LongInt;
			j: integer;
			blocked: boolean;
	begin
		score := 0;
		blocked := false;
		j := houseJ + 1;

		while (j <= grid.sz) and not blocked do
			begin
				score := score + 1;
				if grid.h[houseI][j] >= grid.h[houseI][houseJ] then
					blocked := true;
				j := j + 1;
			end;

		ScenicScoreRight := score;
	end;


	function ScenicScore (var grid: HeightGrid; i, j: integer): LongInt;
		var
			score: LongInt;
	begin
{ This whole thing could be just one expression, }
{ but THINK Pascal won't let us have line breaks within statements }
		score := 1;
		score := score * ScenicScoreUp(grid, i, j);
		score := score * ScenicScoreDown(grid, i, j);
		score := score * ScenicScoreLeft(grid, i, j);
		score := score * ScenicScoreRight(grid, i, j);
		ScenicScore := score;
	end;

	function HighestScenicScore (var grid: HeightGrid): LongInt;
		var
			i, j: integer;
			max, cur: LongInt;
	begin
		max := -1;
		for i := 1 to grid.sz do
			for j := 1 to grid.sz do
				begin
					cur := ScenicScore(grid, i, j);
					if cur > max then
						max := cur;
				end;

		HighestScenicScore := max;
	end;


	var
		inputFile: Text;
		grid: HeightGrid;
		result: LongInt;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			ReadGrid(inputFile, grid);
			writeln('grid size: ', grid.sz);
			result := HighestScenicScore(grid);
{ 31754 is too low }
			writeln('Highest score: ', result);
		end
	else
		writeln('Did not open input file');

end.