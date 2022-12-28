program day13p2;

{ Note: Set enableDebugOutput in config.p to true for debugging, }
{ or false to make this run "fast". }

	uses
		Config, IO, Packets, Tests;

	type
		RangeArr = array[1..maxNumPackets] of Range;

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


	procedure FindRanges (var inputBuf: InputBuffer; var ranges: RangeArr; var nRanges: integer);
		var
			pos: integer;
			leftRange, rightRange: Range;
	begin
		pos := 1;
		nRanges := 0;

		while (pos < inputBuf.sz) and (pos > 0) do
			begin
		{ Read and consume a pair of packets }
				leftRange.firstIx := pos;
				leftRange.lastIx := FindNext(inputBuf, chr($d), leftRange.firstIx, -1) - 1;
				rightRange.firstIx := leftRange.lastIx + 2;
				rightRange.lastIx := FindNext(inputBuf, chr($d), rightRange.firstIx, -1) - 1;
				pos := rightRange.lastIx + 3; { consume the line separating each pair }

				ranges[nRanges + 1] := leftRange;
				ranges[nRanges + 2] := rightRange;
				nRanges := nRanges + 2;
			end;
	end;

	procedure Solve (var inputBuf: InputBuffer);
		var
			i, result, nRanges: integer;
			leftRange, rightRange: Range;
			ranges: RangeArr;
	begin
		FindRanges(inputBuf, ranges, nRanges);
		result := 0;

		for i := 1 to nRanges div 2 do
			begin
				leftRange := ranges[i * 2 - 1];
				rightRange := ranges[i * 2];
				case Order(inputBuf, leftRange, rightRange) of
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
			end;

		writeln('Result: ', result);
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