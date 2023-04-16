program day6p2;

	uses
		FileUtils;

	type
		Buffer = record
				chars: array[1..14] of char;
				n: integer;
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
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.