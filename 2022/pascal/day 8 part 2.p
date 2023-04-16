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
		FlexibleRange = record
				first: integer;
				last: integer;
				step: integer;
			end;


	procedure ReadGrid (var f: Text;
									var grid: HeightGrid);
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


	procedure ForEachInRange (var range: FlexibleRange;
									function callback (i: integer): boolean);
		var
			i: integer;
	begin
		i := range.first;

		while ((range.step < 0) and (i >= range.last)) or ((range.step > 0) and (i <= range.last)) do
			begin
				if not callback(i) then
					exit(ForEachInRange);
				i := i + range.step;
			end;
	end;

	function NumUntilMismatchInclusive (var range: FlexibleRange;
									function predicate (i: integer): boolean): integer;
		var
			n: integer;

		function CountingPredicate (i: integer): boolean;
		begin
			n := n + 1;
			CountingPredicate := predicate(i);
		end;

	begin
		n := 0;
		ForEachInRange(range, CountingPredicate);
		NumUntilMismatchInclusive := n;
	end;


	function ScenicScoreUp (var grid: HeightGrid;
									houseI, houseJ: integer): LongInt;
		var
			range: FlexibleRange;

		function IsNotBlocking (i: integer): boolean;
		begin
			IsNotBlocking := grid.h[i][houseJ] < grid.h[houseI][houseJ];
		end;

	begin
		range.first := houseI - 1;
		range.last := 1;
		range.step := -1;
		ScenicScoreUp := NumUntilMismatchInclusive(range, IsNotBlocking);
	end;


	function ScenicScoreDown (var grid: HeightGrid;
									houseI, houseJ: integer): LongInt;
		var
			range: FlexibleRange;

		function IsNotBlocking (i: integer): boolean;
		begin
			IsNotBlocking := grid.h[i][houseJ] < grid.h[houseI][houseJ];
		end;

	begin
		range.first := houseI + 1;
		range.last := grid.sz;
		range.step := 1;
		ScenicScoreDown := NumUntilMismatchInclusive(range, IsNotBlocking);
	end;


	function ScenicScoreLeft (var grid: HeightGrid;
									houseI, houseJ: integer): LongInt;
		var
			range: FlexibleRange;

		function IsNotBlocking (j: integer): boolean;
		begin
			IsNotBlocking := grid.h[houseI][j] < grid.h[houseI][houseJ];
		end;

	begin
		range.first := houseJ - 1;
		range.last := 1;
		range.step := -1;
		ScenicScoreLeft := NumUntilMismatchInclusive(range, IsNotBlocking);
	end;


	function ScenicScoreRight (var grid: HeightGrid;
									houseI, houseJ: integer): LongInt;
		var
			range: FlexibleRange;

		function IsNotBlocking (j: integer): boolean;
		begin
			IsNotBlocking := grid.h[houseI][j] < grid.h[houseI][houseJ];
		end;

	begin
		range.first := houseJ + 1;
		range.last := grid.sz;
		range.step := 1;
		ScenicScoreRight := NumUntilMismatchInclusive(range, IsNotBlocking);
	end;


	function ScenicScore (var grid: HeightGrid;
									i, j: integer): LongInt;
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
			writeln('Highest score: ', result);
			writeln('Press enter');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');

end.