program day3p1;

	uses
		FileUtils;

{ Run time on Mac Classic, run standalone, w/full puzzle input: ~32 seconds }

{ Reading the entire file in is extravagantly wasteful vs using a constant-space }
{ approach. But this program was writtin on & for a machine with 4 megabytes }
{ of RAM. Why buy luxuries if you aren't going to let yourself benefit from them? }
	const
		MaxNumLines = 140; { Adjust this if your puzzle input is > 140 lines long }
		MaxLineLength = 140;
		EnableDebugging = false; { True for debug info, at great cost in speed }

	type
{ Using a packed array with a shorter length instead of the built in string type }
{ keeps us under the 32kb object size  limit. }
		SingleLine = record
				len: integer;
				chars: packed array[1..MaxLineLength] of char;
			end;
		LinesArr = record
				len: integer;
				lines: array[1..MaxNumLines] of SingleLine;
			end;


{ Call InitCursor to undo this }
	procedure ShowWaitCursor;
		var
			theCursor: CursHandle;
	begin
		theCursor := GetCursor(4);
		SetCursor(theCursor^^);
	end;


	procedure ReadFile (var inputFile: text; var dest: LinesArr);
		var
			s: string;
			i: integer;
	begin
		dest.len := 0;
		while not eof(inputFile) do
			begin
				readln(inputFile, s);
				dest.len := dest.len + 1;
				dest.lines[dest.len].len := length(s);
				dest.lines[dest.len].chars := s;
			end;
	end;

{ TODO: consider inlining this }
	function IsDigit (c: char): boolean;
		var
			n: integer;
	begin
		n := ord(c);
		IsDigit := (n >= 48) and (n <= 57)
	end;

{ TODO: consider inlining }
	function IsSymbol (c: char): boolean;
	begin
		IsSymbol := not ((c = '.') or IsDigit(c));
	end;


	function HasSymbol (var lines: LinesArr; i, sj, ej: integer): boolean;
		var
			j: integer;
	begin
		if sj < 1 then
			sj := 1;
		if ej > lines.lines[i].len then
			ej := lines.lines[i].len;

		for j := sj to ej do
			begin
				if IsSymbol(lines.lines[i].chars[j]) then
					begin
						HasSymbol := true;
						exit(HasSymbol);
					end;
			end;

		HasSymbol := false;
	end;

	procedure DbgAnnounceNumber (var lines: LinesArr; i, sj, ej: integer);
		var
			j: integer;
	begin
		write('Found part number at i=', i : 1, ' j=', sj : 1, '..', ej : 1, ': ');
		for j := sj to ej do
			write(lines.lines[i].chars[j]);

		writeln;
	end;


	function HasSymbolNeighbor (var lines: LinesArr; i, sj, ej: integer): boolean;
{ TODO: inline this after debugging }
		procedure FoundSymbol;
		begin
			HasSymbolNeighbor := true;
			if EnableDebugging then
				DbgAnnounceNumber(lines, i, sj, ej);
			exit(HasSymbolNeighbor);

		end;
	begin
		if sj > 1 then
			if IsSymbol(lines.lines[i].chars[sj - 1]) then
				FoundSymbol;

		if ej < lines.lines[i].len then
			if IsSymbol(lines.lines[i].chars[ej + 1]) then
				FoundSymbol;

		if i > 1 then
			if HasSymbol(lines, i - 1, sj - 1, ej + 1) then
				FoundSymbol;

		if i < lines.lines[i].len then
			if HasSymbol(lines, i + 1, sj - 1, ej + 1) then
				FoundSymbol;

		HasSymbolNeighbor := false;
	end;


{ Returns the part starting at [i][j], or 0 if not a part number. }
{ Advances j just past the last character consumed. }
	function PartNumOrZero (var lines: LinesArr; i: integer; var j: integer): longint;
		var
			n: longint;
			sj, ej: integer;
			foundNonDigit: boolean;
	begin
		n := 0;
		sj := -1;
		foundNonDigit := false;

		while (j <= lines.lines[i].len) and (not foundNonDigit) do
			if not IsDigit(lines.lines[i].chars[j]) then
				begin
					foundNonDigit := true;
					j := j + 1;
				end
			else
				begin
					n := n * 10 + (ord(lines.lines[i].chars[j]) - 48);

					if sj = -1 then
						sj := j;

					ej := j;
					j := j + 1;
				end;

		if sj = -1 then
			PartNumOrZero := 0
		else if HasSymbolNeighbor(lines, i, sj, ej) then
			PartNumOrZero := n
		else
			PartNumOrZero := 0;
	end;


{ longint is necessary to avoid overflow }
	function Solve (var lines: LinesArr): longint;
		var
			result: longint;
			i, j: integer;
	begin
		result := 0;
		for i := 1 to lines.len do
			begin
				begin
					j := 1;
					while j <= lines.lines[i].len do
						result := result + PartNumOrZero(lines, i, j); { mutates J }
				end;

				if EnableDebugging then
					writeln('after line ', i : 1, ' total is ', result : 1);
			end;

		Solve := result;
	end;

	var
		inputFile: text;
		lines: LinesArr;
		result: longint;

begin

	if OpenInputFile(inputFile) then
		begin
			ShowText;
			ShowWaitCursor;
			ReadFile(inputFile, lines);
			writeln('Done reading. Starting to solve.');
			result := Solve(lines);
			writeln(result);
			writeln('Press return to exit');
			SysBeep(10);
			InitCursor;
			readln;
		end;

end.