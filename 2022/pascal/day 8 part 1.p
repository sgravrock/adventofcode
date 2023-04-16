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
		BoolPtr = ^boolean;
		IterationDirection = (ascending, descending);

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


	procedure PopulateVisibility (var heights: HeightGrid;
									var visibility: VisibilityGrid);

		procedure PopulateVisibilityAlongLine (direction: iterationDirection;
										otherAxisIx: integer;
										function IndexToHeight (i, otherAxisIx: integer): byte;
										function IndexToVisPtr (i, otherAxisIx: integer): BoolPtr);
			var
				i, endIx, step: integer;
				h, max: byte;
				p: BoolPtr;
		begin
			max := -1;
			if direction = ascending then
				begin
					i := 1;
					endIx := heights.sz;
					step := 1;
				end
			else
				begin
					i := heights.sz;
					endIx := 1;
					step := -1;
				end;

			while i <> endIx do
				begin
					h := IndexToHeight(i, otherAxisIx);
					if max < h then
						begin
							p := IndexToVisPtr(i, otherAxisIx);
							p^ := true;
							max := h;
						end;
					i := i + step;
				end;
		end;

		function IndexToHeightH (i, j: integer): byte;
		begin
			IndexToHeightH := heights.h[i][j];
		end;
		function IndexToHeightV (i, j: integer): byte;
		begin
			IndexToHeightV := heights.h[j][i];
		end;
		function IndexToVisPtrH (i, j: integer): BoolPtr;
		begin
			IndexToVisPtrH := @visibility[i][j];
		end;
		function IndexToVisPtrV (i, j: integer): BoolPtr;
		begin
			IndexToVisPtrV := @visibility[j][i];
		end;

		var
			i: integer;

	begin
		for i := 1 to heights.sz do
			begin
				PopulateVisibilityAlongLine(ascending, i, IndexToHeightH, IndexToVisPtrV);
				PopulateVisibilityAlongLine(ascending, i, IndexToHeightV, IndexToVisPtrH);
				PopulateVisibilityAlongLine(descending, i, IndexToHeightH, IndexToVisPtrV);
				PopulateVisibilityAlongLine(descending, i, IndexToHeightV, IndexToVisPtrH);
			end;
	end;

	procedure PrintVisibility (var visibility: VisibilityGrid;
									sz: integer);
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

	function NumVisible (var visibility: VisibilityGrid;
									sz: integer): integer;
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
			result := NumVisible(visibility, grid.sz);
			writeln(result : 1, ' visible trees');
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');

end.