program day4p2;

	type
		Range = record
				first: integer;
				last: integer;
			end;

{ Prompts for a file and opens it using Pascal I/O, which is significantly more }
{ convenient for line- or char-at-a-time reading than Mac Toolbox I/O }
	function OpenInputFile (var f: Text): boolean;
		var
			path: string[255];
	begin
		path := OldFileName;

		if path = '' then
			OpenInputFile := false
		else
			begin
				reset(f, path);
			end;
	end;

	procedure ReadRange (var f: Text; var result: Range);
		var
			dash: char;
	begin
		read(f, result.first);
		read(f, dash);
		read(f, result.last);
	end;

	function Overlaps (a, b: Range): boolean;
	begin
{ There's the right way to do operator precedence in a language, and then there's this. }
		Overlaps := not ((a.last < b.first) or (a.first > b.last));
	end;

	function NextLineOverlaps (var f: Text): boolean;
		var
			a, b: Range;
			comma: char;
	begin
		ReadRange(f, a);
		read(f, comma);
		ReadRange(f, b);
		readln(f);
		NextLineOverlaps := Overlaps(a, b);
	end;

	function NumOverlaps (var f: Text): integer;
		var
			total: integer;
	begin
		total := 0;
		while not eof(f) do
			if NextLineOverlaps(f) then
				total := total + 1;
		NumOverlaps := total;
	end;

	var
		inputFile: Text;
		result: integer;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			result := NumOverlaps(inputFile);
			writeln('Total priorities: ', result);
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.