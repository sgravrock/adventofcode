program day8p1;
	uses
		FileUtils;

	const
		maxGridSz = 100;

	type
		HeightGrid = record
				h: array[1..maxGridSz, 1..maxGridSz] of byte;
				sz: integer;
			end;
		VisibilityGrid = array[1..maxGridSz, 1..maxGridSz] of boolean;

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

	procedure PopulateVisibility (var heights: HeightGrid; var visibility: VisibilityGrid);
		var
			i, j: integer;
			max: byte;
	begin
		for i := 1 to heights.sz do
			for j := 1 to heights.sz do
				visibility[i][j] := false;

		for i := 1 to heights.sz do
			begin
				max := -1;
				for j := 1 to heights.sz do
					if max < heights.h[i][j] then
						begin
							visibility[i][j] := true;
							max := heights.h[i][j];
						end;

				max := -1;
				for j := heights.sz downto 1 do
					if max < heights.h[i][j] then
						begin
							visibility[i][j] := true;
							max := heights.h[i][j];
						end;
			end;

		for j := 1 to heights.sz do
			begin
				max := -1;
				for i := 1 to heights.sz do
					if max < heights.h[i][j] then
						begin
							visibility[i][j] := true;
							max := heights.h[i][j];
						end;

				max := -1;
				for i := heights.sz downto 1 do
					if max < heights.h[i][j] then
						begin
							visibility[i][j] := true;
							max := heights.h[i][j];
						end;
			end;
	end;

	procedure PrintVisibility (var visibility: VisibilityGrid; sz: integer);
		var
			i, j: integer;
	begin
		for j := 1 to sz do
			begin
				for i := 1 to sz do
					if visibility[i][j] then
						write('V')
					else
						write('X');
				writeln;
			end;
	end;

	function NumVisible (var visibility: VisibilityGrid; sz: integer): integer;
		var
			i, j, result: integer;
	begin
		result := 0;
		for i := 1 to sz do
			for j := 1 to sz do
				if visibility[i][j] then
					result := result + 1;
		NumVisible := result;
	end;

	var
		inputFile: Text;
		grid: HeightGrid;
		visibility: VisibilityGrid;
		result: integer;

begin

	ShowText;

	if OpenInputFile(inputFile) then
		begin
			ReadGrid(inputFile, grid);
			PopulateVisibility(grid, visibility);
{PrintVisibility(visibility, grid.sz);}
			result := NumVisible(visibility, grid.sz);
			writeln(result : 1, ' visible trees');
		end
	else
		writeln('Did not open input file');

end.