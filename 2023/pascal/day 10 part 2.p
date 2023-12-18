program day10p1;

{ Run time: ~3s on Mac Classic with full puzzle input }

	uses
		MicroSysFileUtils;

	const
{ MaxBufSize must be at least the size of the biggezt input file, }
{ and not so big that Grid exceeds the 32kb object size limit }
		MaxBufSize = 28672;
		MaxBufIx = MaxBufSize - 1;

	type
		Direction = (north, south, east, west);
{ Buffer and grid X and Y are all 0-based. }
{ This is not idiomatic Pascal but makes things considerably easier. }
		Buffer = packed array[0..MaxBufIx] of char;
		Grid = record
				bytes: Buffer;
{ lineLength is width + 1 (to account for the carriage return) }
				width, height, lineLength: integer;
			end;


{ Assumes that the buffer contains at least one carriage return }
{ and is made up of equal length lines. }
	function LineLength (var buf: Buffer): integer;
		var
			i: integer;
			cr: char;
	begin
		i := 0;
		cr := chr(13);

		while buf[i] <> cr do
			i := i + 1;

		LineLength := i + 1;
	end;

{ Call InitCursor to undo this }
	procedure ShowWaitCursor;
		var
			theCursor: CursHandle;
	begin
		theCursor := GetCursor(4);
		SetCursor(theCursor^^);
	end;


	procedure Die (errmsg: string);
	begin
		writeln(errmsg);
		writeln('Press return to exit');
		InitCursor;
		readln;
		halt;
	end;


	procedure ReadGrid (filePath: integer; var g: grid);
		var
			fileSize: LongInt;
			err: FileError;
	begin
		fileSize := MaxBufSize;
		err := ReadEntireFile(filePath, Ptr(@g.bytes), fileSize);

		if err <> FileErrorOk then
			die('Error reading input file');

		writeln('read ', fileSize : 1, ' bytes');
		g.lineLength := LineLength(g.bytes);
		g.width := g.lineLength - 1;
		g.height := integer(fileSize) div (g.width + 1);
	end;


	function FindStart (var g: Grid): integer;
		var
			i, maxI: integer;
	begin
		maxI := g.lineLength * g.height - 1;

		for i := 0 to maxI do
			if g.bytes[i] = 'S' then
				begin
					FindStart := i;
					exit(FindStart);
				end;

		die('Could not find start');
	end;


	procedure ReplaceStart (var g: Grid; start: integer);
		var
			n, s, e, w, c: char;
			pipeN, pipeS, pipeE, pipeW: boolean;
	begin
{ Assumption: start is not along the edge }
		n := g.bytes[start - g.lineLength];
		s := g.bytes[start + g.lineLength];
		e := g.bytes[start + 1];
		w := g.bytes[start - 1];
		pipeN := (n = 'F') or (n = '7') or (n = '|');
		pipeS := (s = 'L') or (s = 'J') or (s = '|');
		pipeE := (e = '7') or (e = 'J') or (e = '-');
		pipeW := (w = 'F') or (w = 'L') or (w = '-');

		if pipeS and pipeE then
			c := 'F'
		else if pipeS and pipeW then
			c := 'J'
		else if pipeN and pipeE then
			c := 'L'
		else if pipeN and pipeW then
			c := '7'
		else if pipeN and pipeS then
			c := '|'
		else if pipeE and pipeW then
			c := '-'
		else
			die('Could not replace start');

		g.bytes[start] := c;
	end;


{ Finds cells that are part of the cycle at start and replaces north-facing pipes with ! }
{ and all others with * }
	procedure MarkCycle (var g: grid; start: integer);
		var
			pos, prev, tmp: integer;
			any: boolean;
			cameFrom: Direction;
			c: char;
	begin
		any := false;
		pos := start;

		while (not any) or (pos <> start) do
			begin
				c := g.bytes[pos];

				if any then
					begin
						if prev = pos - 1 then
							cameFrom := west
						else if prev = pos + 1 then
							cameFrom := east
						else if prev < pos then
							cameFrom := north
						else
							cameFrom := south;
					end
				else
					begin
{ Arbitrarily choose which way we "came from" }
						if (c = 'F') or (c = '7') or (c = '|') then
							cameFrom := south
						else if (c = 'J') or (c = 'L') then
							cameFrom := north
						else if (c = '-') then
							cameFrom := west
						else
							die('Unexpected start character');
					end;

				any := true;
				tmp := pos;

				if (c = 'J') or (c = 'L') or (c = '|') then
					g.bytes[pos] := '!'
				else
					g.bytes[pos] := '*';

				if ((c = 'J') and (cameFrom = west)) or ((c = 'L') and (cameFrom = east)) or ((c = '|') and (cameFrom = south)) then
					pos := pos - g.lineLength
				else if ((c = '7') and (cameFrom = west)) or ((c = 'F') and (cameFrom = east)) or ((c = '|') and (cameFrom = north)) then
					pos := pos + g.lineLength
				else if ((c = 'J') and (cameFrom = north)) or ((c = '7') and (cameFrom = south)) or ((c = '-') and (cameFrom = east)) then
					pos := pos - 1
				else if ((c = 'L') and (cameFrom = north)) or ((c = 'F') and (cameFrom = south)) or ((c = '-') and (cameFrom = west)) then
					pos := pos + 1
				else
					die('Cannot move');

				prev := tmp;
			end;
	end;


	function Solve (var g: Grid): integer;
		var
			start, i, maxI, numInside: integer;
			isInside: boolean;
			c: char;
	begin
		start := FindStart(g);
		writeln('start is at x=', (start mod g.lineLength) : 1, ' y=', (start div g.lineLength) : 1);
		ReplaceStart(g, start);
		writeln('replaced start with ', g.bytes[start]);
		MarkCycle(g, start);
		writeln('Done marking cycle. Counting inside spaces.');

		maxI := g.lineLength * g.height - 1;
		isInside := false;

		for i := 0 to maxI do
			begin
				c := g.bytes[i];

				if c = chr(13) then
					isInside := false
				else if c = '!' then
					isInside := not isInside
				else if isInside and (c <> '*') then
					numInside := numInside + 1;
			end;

		Solve := numInside;
	end;


	var
		openResult: FileError;
		filePath: integer;
		g: Grid;
		result: integer;

begin
	openResult := OpenInputFile(filePath);

	if openResult <> FileErrorOk then
		die('Error opening input file');

	ShowText;
	ShowWaitCursor;
	ReadGrid(filePath, g);

	if FSClose(filePath) <> noErr then
		writeln('Warning: error closing input file');

	writeln('width: ', g.width : 1, ' height: ', g.height : 1);
	result := Solve(g);
	writeln('Result: ', result : 1);
	writeln('Press return to exit');
	SysBeep(10);
	readln;
end.
