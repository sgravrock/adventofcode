program day10p1;

{ Run time: ~4s on Mac Classic with full puzzle input }

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
		Coord = record
				x, y: integer;
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


	function FindStart (var g: Grid): Coord;
		var
			x, y: integer;
			c: Coord;
	begin
		for y := 0 to g.height do
			for x := 0 to g.width do
				if g.bytes[y * g.lineLength + x] = 'S' then
					begin
						c.x := x;
						c.y := y;
						FindStart := c;
						exit(FindStart);
					end;

		die('Could not find start');
	end;


	procedure ReplaceStart (var g: Grid; start: Coord);
		var
			n, s, e, w, c: char;
			pipeN, pipeS, pipeE, pipeW: boolean;
	begin
{ Assumption: start is not along the edge }
		n := g.bytes[start.y - 1 * g.lineLength + start.x];
		s := g.bytes[start.y + 1 * g.lineLength + start.x];
		e := g.bytes[start.y * g.lineLength + start.x + 1];
		w := g.bytes[start.y * g.lineLength + start.x - 1];
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

		g.bytes[start.y * g.lineLength + start.x] := c;
	end;


{ Finds cells that are part of the cycle at start and replaces north-facing pipes with ! }
{ and all others with * }
	procedure MarkCycle (var g: grid; start: Coord);
		var
			pos, prev, tmp: Coord;
			any: boolean;
			cameFrom: Direction;
			c: char;
	begin
		any := false;
		pos := start;

		while not (any and (pos.x = start.x) and (pos.y = start.y)) do
			begin
				c := g.bytes[pos.y * g.lineLength + pos.x];

				if any then
					begin
						if pos.y = prev.y - 1 then
							cameFrom := south
						else if pos.y = prev.y + 1 then
							cameFrom := north
						else if pos.x = prev.x - 1 then
							cameFrom := east
						else
							cameFrom := west;
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
					g.bytes[pos.y * g.lineLength + pos.x] := '!'
				else
					g.bytes[pos.y * g.lineLength + pos.x] := '*';

				if ((c = 'J') and (cameFrom = west)) or ((c = 'L') and (cameFrom = east)) or ((c = '|') and (cameFrom = south)) then
					pos.y := pos.y - 1
				else if ((c = '7') and (cameFrom = west)) or ((c = 'F') and (cameFrom = east)) or ((c = '|') and (cameFrom = north)) then
					pos.y := pos.y + 1
				else if ((c = 'J') and (cameFrom = north)) or ((c = '7') and (cameFrom = south)) or ((c = '-') and (cameFrom = east)) then
					pos.x := pos.x - 1
				else if ((c = 'L') and (cameFrom = north)) or ((c = 'F') and (cameFrom = south)) or ((c = '-') and (cameFrom = west)) then
					pos.x := pos.x + 1
				else
					die('Cannot move');

				prev := tmp;
			end;
	end;


	function Solve (var g: Grid): integer;
		var
			start: Coord;
			x, y, numInside: integer;
			isInside: boolean;
			c: char;
	begin
		start := FindStart(g);
		writeln('start is at x=', start.x : 1, ' y=', start.y : 1);
		ReplaceStart(g, start);
		writeln('replaced start with ', g.bytes[start.y * g.lineLength + start.x]);
		MarkCycle(g, start);
		writeln('Done marking cycle. Counting inside spaces.');

		for y := 0 to g.height - 1 do
			begin
				isInside := false;
				for x := 0 to g.width - 1 do
					begin
						c := g.bytes[y * g.lineLength + x];
						if c = '!' then
							isInside := not isInside
						else if isInside and (c <> '*') then
							numInside := numInside + 1;
					end;
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