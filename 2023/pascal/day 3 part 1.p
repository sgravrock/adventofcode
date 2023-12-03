program day3p1;

	uses
		MicroSysFileUtils;

{ Run time on Mac Classic, run standalone, w/full puzzle input: ~7 seconds }

{ Reading the entire file in is extravagantly wasteful vs using a constant-space }
{ approach. But this program was writtin on & for a machine with 4 megabytes }
{ of RAM. Why buy luxuries if you aren't going to let yourself benefit from them? }
	const
{ Adjust BufSize for your puzzle input. Must be >= file size in bytes and <= 32k }
		BufSize = 19740;
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
{ TODO: do we actually need to unpack into LinesArr,}
{ or could this be used directly? }
		EntireFile = packed array[1..bufsize] of char;


{ Call InitCursor to undo this }
	procedure ShowWaitCursor;
		var
			theCursor: CursHandle;
	begin
		theCursor := GetCursor(4);
		SetCursor(theCursor^^);
	end;


	procedure ReadFile (filePath: integer; var dest: LinesArr);
		var
			readResult: FileError;
			bufp: ^EntireFile;
			fileSz: longint;
			k: integer;
			linep: ^SingleLine;
	begin
		new(bufp);
		fileSz := BufSize;
		readResult := ReadEntireFile(filePath, Ptr(bufp), fileSz);
		if readResult <> FileErrorOk then
			begin
				writeln('Error reading file');
				writeln('Press return to exit');
				readln;
				halt;
			end;

		dest.len := 1;
		linep := @dest.lines[1];
		linep^.len := 0;

		for k := 1 to fileSz do
			begin
				if ord(bufp^[k]) = 13 then { carriage return }
					begin
						dest.len := dest.len + 1;
						linep := @dest.lines[dest.len];
						linep^.len := 0;
					end
				else
					begin
						linep^.len := linep^.len + 1;
						linep^.chars[linep^.len] := bufp^[k];
					end;
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
		openResult: FileError;
		filePath: integer; { welcome to the Mac operating system }
		lines: LinesArr;
		answer: longint;

begin
	openResult := OpenInputFile(filePath);

	if openResult <> FileErrorOk then
		begin
			writeln('Error opening input file');
			writeln('Press return to exit');
			readln;
			halt;
		end;

	ShowText;
	ShowWaitCursor;
	ReadFile(filePath, lines);
	writeln('Done reading. Starting to solve.');
	answer := Solve(lines);
	writeln(answer);
	writeln('Press return to exit');
	SysBeep(10);
	InitCursor;
	readln;
end.