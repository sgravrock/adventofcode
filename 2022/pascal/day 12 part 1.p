program day;

	uses
		FileUtils;

	const
		MAX_X = 68;
		MAX_Y = 42;

	type
		DebugLogLevelType = (debugLogNone, debugLogSome, debugLogPainfullyVerbose);
		Grid = record
				cells: array[1..MAX_Y, 1..MAX_X] of char;
				width, height: integer;
			end;
		Coordinate = record
				x, y: integer;
			end;
		DirectionsArr = array[1..4] of Coordinate;
		DistanceArr = array[1..MAX_Y, 1..MAX_X] of integer;
		PrevArr = array[1..MAX_Y, 1..MAX_X] of Coordinate;
		FrontierArr = array[1..MAX_X] of Coordinate;
		VisitedArr = array[1..MAX_y, 1..MAX_X] of boolean;
		DistanceArrPtr = ^DistanceArr;
		PrevArrPtr = ^PrevArr;
		FrontierArrPtr = ^FrontierArr;
		VisitedArrPtr = ^VisitedArr;

	const
		LOG_LEVEL = debugLogNone;

	procedure ReadGrid (var f: Text; var g: Grid);
		var
			x: integer;
	begin
		g.width := 0;
		g.height := 0;
		while not eof(f) do
			begin
				g.height := g.height + 1;
				x := 0;

				while not eoln(f) do
					begin
						x := x + 1;
						read(f, g.cells[g.height][x]);
					end;

				readln(f);
				g.width := x;
			end;
	end;


	function Find (var haystack: Grid; needle: char): Coordinate;
		var
			x, y: integer;
			result: Coordinate;
	begin
		for y := 1 to haystack.height do
			for x := 1 to haystack.width do
				if haystack.cells[y][x] = needle then
					begin
						result.x := x;
						result.y := y;
						Find := result;
						exit(Find);
					end;

		writeln('could not find "', needle, '"');
		halt;
	end;


	procedure InitDirections (var dirs: DirectionsArr);
	begin
		dirs[1].x := -1;
		dirs[1].y := 0;
		dirs[2].x := 1;
		dirs[2].y := 0;
		dirs[3].x := 0;
		dirs[3].y := -1;
		dirs[4].x := 0;
		dirs[4].y := 1;
	end;


	function InFrontier (var frontier: FrontierArr; frontierSz, x, y: integer): boolean;
		var
			i, m: integer;
	begin
		m := frontierSz;
		if m > MAX_X then
			m := MAX_X;

		for i := 1 to m do
			if (frontier[i].x = x) and (frontier[i].y = y) then
				begin
					InFrontier := true;
					exit(InFrontier);
				end;
		InFrontier := false;
	end;


	procedure DumpFrontier (var frontier: FrontierArr; frontierSz: integer);
		var
			i, m: integer;
	begin
		writeln('Frontier:');
		m := frontierSz;
		if m > MAX_X then
			m := MAX_X;

		for i := 1 to m do
			begin
				write('x=', frontier[i].x : 1, ' y=', frontier[i].y : 1, ', ');
				if i mod 8 = 0 then
					writeln;
			end;
		writeln;
	end;


	procedure Dump (var g: Grid; var dist: DistanceArr; var frontier: FrontierArr; frontierSz: integer);
		var
			x, y: integer;
	begin
		for y := 1 to g.height do
			begin
				for x := 1 to g.width do
					begin
						if g.cells[y][x] = 'S' then
							write('S')
						else if g.cells[y][x] = 'E' then
							write('E')
						else if dist[y][x] = maxint then
							begin
								if InFrontier(frontier, frontierSz, x, y) then
									write('F')
								else
									write('.');
							end
						else
							begin
								if InFrontier(frontier, frontierSz, x, y) then
									write('!') { should not happen }
								else
									write('v');
							end;
					end;
				writeln
			end;

		DumpFrontier(frontier, frontierSz);
	end;


	function Dijkstra (var g: Grid; var origin, dest: Coordinate): integer;
		var
			dist: DistanceArrPtr;
			prev: PrevArrPtr;
			frontier: FrontierArrPtr; { TODO priority queue? }
			visited: VisitedArrPtr;
			directions: DirectionsArr;
			frontierSz: integer;
			u, v: Coordinate;
			x, y, i, alt: integer;
			debug: integer;

		function FindNextNode: Coordinate;
			var
				i, j: integer;
				c: integer;
		begin
			c := 1;
			for i := 2 to frontierSz do
				if dist^[frontier^[i].y][frontier^[i].x] < dist^[frontier^[c].y][frontier^[c].x] then
					c := i;
			FindNextNode := frontier^[c];

			frontierSz := frontierSz - 1;
			for j := c to frontierSz do
				frontier^[j] := frontier^[j + 1];
		end;

		function Height (coord: Coordinate): integer;
			var
				c: char;
		begin
			c := g.cells[coord.y][coord.x];

			if c = 'S' then
				c := 'a'
			else if c = 'E' then
				c := 'z';

			Height := ord(c);
		end;

		function IsValidEdge (f, t: Coordinate): boolean;
		begin
			if (t.x < 1) or (t.x > g.width) or (t.y < 1) or (t.y > g.height) then
				IsValidEdge := false
			else
				IsValidEdge := Height(t) <= (1 + Height(f));
		end;

		function CountSteps: integer;
			var
				p: Coordinate;
				n: integer;
		begin
			p := dest;
			n := 0;

			while (p.x <> origin.x) or (p.y <> origin.y) do
				begin
					if LOG_LEVEL <> debugLogNone then
						write('x=', p.x : 1, ' y=', p.y : 1, ' <- ');
					p := prev^[p.y][p.x];
					n := n + 1;
				end;

			if LOG_LEVEL <> debugLogNone then
				writeln;

			CountSteps := n;
		end;

		procedure Cleanup;
		begin
			dispose(dist);
			dispose(prev);
			dispose(visited);
			dispose(frontier);
		end;

	begin
		InitDirections(directions);
		new(dist);
		new(prev);
		new(visited);
		new(frontier);
		for y := 1 to g.height do
			for x := 1 to g.width do
				begin
					dist^[y][x] := maxint;
					prev^[y][x].x := -1;
					visited^[y][x] := false;
				end;
		frontierSz := 1;
		frontier^[1] := origin;
		dist^[origin.y][origin.x] := 0;

		while frontierSz > 0 do
			begin
				debug := debug + 1;
				u := FindNextNode;
				if LOG_LEVEL <> debugLogNone then
					writeln('Visiting x=', u.x : 1, ' y=', u.y : 1);
				visited^[u.y][u.x] := true;

				if (u.x = dest.x) and (u.y = dest.y) then
					begin
						Dijkstra := CountSteps;
						Cleanup;
						exit(Dijkstra);
					end;

				for i := 1 to 4 do
					begin
						v.x := u.x + directions[i].x;
						v.y := u.y + directions[i].y;

						if not IsValidEdge(u, v) then
							begin
								if LOG_LEVEL = debugLogPainfullyVerbose then
									begin
										write('Edge to x=', v.x : 1, ' y=', v.y : 1, ' is not valid ');
										if (v.x < 1) or (v.x > g.width) or (v.y < 1) or (v.y > g.height) then
											writeln('(out of range)')
										else
											writeln('(from ', g.cells[u.y][u.x], ' to ', g.cells[v.y][v.x], ' ) ');
									end;
							end
						else if not visited^[v.y][v.x] then
							begin
								if LOG_LEVEL = debugLogPainfullyVerbose then
									writeln('x=', v.x : 1, ' y=', v.y : 1, ' is a next neighbor of x=', u.x : 1, ' y=', u.y : 1);
								if not InFrontier(frontier^, frontierSz, v.x, v.y) then
									begin
										frontierSz := frontierSz + 1;
										if frontierSz > MAX_X then
											Dump(g, dist^, frontier^, frontierSz);
										frontier^[frontierSz] := v;
									end;
								alt := dist^[u.y][u.x] + 1; { All edges have the same cost }
								if alt < dist^[v.y][v.x] then
									begin
										dist^[v.y][v.x] := alt;
										prev^[v.y][v.x] := u;
									end;
							end
						else if LOG_LEVEL = debugLogPainfullyVerbose then
							writeln('x=', v.x : 1, ' y=', v.y : 1, ' is a visited neighbor of x=', u.x : 1, ' y=', u.y : 1)
						else if (v.x > 0) and (v.y > 0) and (LOG_LEVEL = debugLogPainfullyVerbose) then
							writeln('x=', v.x : 1, ' y=', v.y : 1, ' is not a valid neighbor of x=', u.x : 1, ' y=', u.y : 1);
					end;
			end;

		writeln('Did not find destination!');
		if LOG_LEVEL <> debugLogNone then
			Dump(g, dist^, frontier^, frontierSz);
		writeln('visited ', debug : 1, ' nodes');
		halt;
	end;


	function Solve (var g: grid): integer;
		var
			origin, dest: Coordinate;
	begin
		origin := Find(g, 'S');
		dest := Find(g, 'E');
		Solve := Dijkstra(g, origin, dest);
	end;

	var
		inputFile: Text;
		g: Grid;
		result: integer;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			ReadGrid(inputFile, g);
			result := Solve(g);
			writeln(result);
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');

end.