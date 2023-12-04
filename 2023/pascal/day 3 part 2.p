program day3p2;

	uses
		MicroSysFileUtils;

{ Run time on Mac Classic, run standalone, w/full puzzle input: 3.7 seconds }

{ Reading the entire file in is extravagantly wasteful vs using a constant-space }
{ approach. But this program was writtin on & for a machine with 4 megabytes }
{ of RAM. Why buy luxuries if you aren't going to let yourself benefit from them? }
	const
{ Adjust BufSize for your puzzle input. Must be >= file size in bytes and <= 32k }
		BufSize = 19740;
		MaxNumLines = 140; { Adjust this if your puzzle input is > 140 lines long }
		MaxLineLength = 140;
		MaxNumNumbers = 16071; { Set to >= the  number of numbers in the file }
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
		EntireFile = packed array[1..bufsize] of char;
		Coord = record
				i, j: integer;
			end;


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

	function IsDigit (c: char): boolean;
		var
			n: integer;
	begin
		n := ord(c);
		IsDigit := (n >= 48) and (n <= 57)
	end;

{ Returns the number that includes c }
	function NumberAt (var lines: LinesArr; c: Coord): longint;
		var
			j: integer;
			startFound: boolean;
			n: longint;
	begin
		n := 0;
		j := c.j;
		startFound := false;

		while not startFound do
			begin
				if j = 1 then
					startFound := true
				else if not IsDigit(lines.lines[c.i].chars[j - 1]) then
					startFound := true
				else
					j := j - 1;
			end;

		while (j <= lines.lines[c.i].len) and IsDigit(lines.lines[c.i].chars[j]) do
			begin
				n := n * 10 + ord(lines.lines[c.i].chars[j]) - ord('0');
				j := j + 1;
			end;

		if EnableDebugging then
			writeln('Number at i=', c.i : 1, ' j=', c.j : 1, ' is ', n : 1);
		NumberAt := n;
	end;


{ Returns the gear ratio of the gear at [i][j], }
{ or 0 if it is not adjacent to exactly two numbers. }
	function MaybeGearRatio (var lines: LinesArr; i, j: integer): longint;
		var
			numNums: integer;
			coords: array[1..2] of Coord;
			left, right, top, bottom, ignored: boolean;
			ratio: longint;

		function Check (ni, nj: integer): boolean;
			var
				result: boolean;
		begin
			if (ni < 1) or (ni > lines.len) or (nj < 1) or (nj > lines.lines[i].len) then
				result := false
			else
				result := IsDigit(lines.lines[ni].chars[nj]);

			if result then
				begin
					numNums := numNums + 1;
					if numNums <= 2 then
						begin
							coords[numNums].i := ni;
							coords[numNums].j := nj;
						end;
				end;

			Check := result;
		end;

	begin
		numNums := 0;
		top := Check(i - 1, j);
		bottom := Check(i + 1, j);
		left := Check(i, j - 1);
		right := Check(i, j + 1);

		if not (top or left) then
			ignored := Check(i - 1, j - 1);
		if not (top or right) then
			ignored := Check(i - 1, j + 1);
		if not (bottom or left) then
			ignored := Check(i + 1, j - 1);
		if not (bottom or right) then
			ignored := Check(i + 1, j + 1);

		if numNums <> 2 then
			begin
				if EnableDebugging then
					writeln(i : 1, ', ', j : 1, ' is not a gear because it is adjacent to ', numNums : 1, ' numbers');
				MaybeGearRatio := 0;
			end
		else
			begin
				ratio := NumberAt(lines, coords[1]) * NumberAt(lines, coords[2]);
				MaybeGearRatio := ratio;

				if EnableDebugging then
					writeln(i : 1, ', ', j : 1, ' is a gear with ratio ', ratio : 1);
			end;
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
				for j := 1 to lines.lines[i].len do
					if lines.lines[i].chars[j] = '*' then
						result := result + MaybeGearRatio(lines, i, j);

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