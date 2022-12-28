program day13p1;

{ Note: Set enableDebugOutput in config.p to true for debugging, }
{ or false to make this run "fast". }

	uses
		Config, IO, Packets, Tests;

	function FindNext (var haystack: InputBuffer; needle: char; startIx, endIx: integer): integer;
		var
			i: integer;
	begin
		if endIx = -1 then
			endIx := haystack.sz;

		for i := startIx to endIx do
			if haystack.data[i] = needle then
				begin
					FindNext := i;
					exit(FindNext);
				end;

		writeln('Could not find next ', needle);
		halt;
	end;


	procedure Solve (var inputBuf: InputBuffer);
		var
			pos, i, result, e: integer;
			left, right: ListPtr;
	begin
		pos := 1;
		i := 0;
		result := 0;
		InitListPool;

		while (pos < inputBuf.sz) and (pos > 0) do
			begin
				i := i + 1;
				left := ParseList(inputBuf, pos, e);
				right := ParseList(inputBuf, e + 1, e);
				pos := e + 2; { consume the line separating each pair }

				case Order(left, right) of
					inOrder: 
						begin
							if enableDebugOutput then
								writeln(i : 1, ' is in order');
							result := result + i;
						end;
					outOfOrder: 
						if enableDebugOutput then
							writeln(i : 1, ' is out of order');
					TBD: 
						writeln('Could not determine order for', i : 1);
				end;

				DisposeListPool;
			end;

		writeln('Result: ', result); { 5630 is too high, 4163 is too low }
	end;

	var
		inputPath: integer;
		bufp: ^InputBuffer;
		err: OSErr;

begin
	ShowText;
	RunTests;

	if OpenInputFile(inputPath) then
		begin
			new(bufp);
			bufp^.sz := 22000;
			writeln('about to read file');

			if ReadEntireFile(inputPath, @bufp^.data[1], bufp^.sz) then
				begin
					writeln('about to find pairs');
					Solve(bufp^);
					writeln('done');
				end
			else
				writeln('read failed');

			err := FSClose(inputPath);
			if err <> noErr then
				writeln('Error closing input file: ', err);
		end
	else
		writeln('Did not open input file');
end.