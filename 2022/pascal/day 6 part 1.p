program day5p2;

	uses
		FileUtils;

	type
		Buffer = record
				chars: array[1..4] of char;
				n: integer;
			end;

	procedure Append (var buf: Buffer; c: char);
		var
			i: integer;
	begin
		if buf.n = 4 then
			for i := 2 to 4 do
				buf.chars[i - 1] := buf.chars[i]
		else
			buf.n := buf.n + 1;

		buf.chars[buf.n] := c;
	end;

	function IsMarker (var buf: Buffer): boolean;
		var
			a, b, c, d: char;
	begin
		a := buf.chars[1];
		b := buf.chars[2];
		c := buf.chars[3];
		d := buf.chars[4];
		IsMarker := (a <> b) and (a <> c) and (a <> d) and (b <> c) and (b <> d) and (c <> d);
	end;

	procedure Solve (var f: Text);
		var
			c: char;
			buf: Buffer;
			i: integer;
	begin
		buf.n := 0;
		buf.chars[1] := '-';
		buf.chars[2] := '-';
		buf.chars[3] := '-';
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
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.