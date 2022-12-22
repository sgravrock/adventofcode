program day6p2;

	type
		Buffer = record
				chars: array[1..14] of char;
				n: integer;
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

	procedure Append (var buf: Buffer; c: char);
		var
			i: integer;
	begin
		if buf.n = 14 then
			for i := 2 to 14 do
				buf.chars[i - 1] := buf.chars[i]
		else
			buf.n := buf.n + 1;

		buf.chars[buf.n] := c;
	end;

	function IsMarker (var buf: Buffer): boolean;
		var
			seen: array[1..26] of boolean;
			i, n: integer;
	begin
		for i := 1 to 26 do
			seen[i] := false;

		for i := 1 to 14 do
			begin
				n := ord(buf.chars[i]) - 96;
				if seen[n] then
					begin
						IsMarker := false;
						exit(IsMarker);
					end;
				seen[n] := true;
			end;

		IsMarker := true;
	end;

	procedure Solve (var f: Text);
		var
			c: char;
			buf: Buffer;
			i: integer;
	begin
		buf.n := 0;
		i := 0;

		while not eoln(f) do
			begin
				read(f, c);
				Append(buf, c);
				i := i + 1;
				if i >= 4 then
					begin
						if IsMarker(buf) then
							begin
								writeln(i);
								exit(solve);
							end;
					end;
			end;

		writeln('not found');
	end;

	var
		inputFile: Text;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			Solve(inputFile);
		end
	else
		writeln('Did not open input file');
end.