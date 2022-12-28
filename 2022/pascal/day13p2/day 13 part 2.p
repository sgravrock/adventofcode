program day13p2;

{ Note: Set enableDebugOutput in config.p to true for debugging, }
{ or false to make this run "fast". }

	uses
		Config, IO, Packets, Sorting, Tests;

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


	procedure AppendLine (var inputBuf: InputBuffer; toAppend: string);
		var
			i, len: integer;
	begin
		len := length(toAppend);

		for i := 1 to len do
			inputBuf.data[inputBuf.sz + i] := toAppend[i];

		inputBuf.data[inputBuf.sz + len + 1] := chr($d);
		inputBuf.sz := inputBuf.sz + len + 1;
	end;


	function IndexOf (var haystack: Sortable; len, needle: integer): integer;
		var
			i: integer;
	begin
		for i := 1 to len do
			if haystack[i] = needle then
				begin
					IndexOf := i;
					exit(IndexOf);
				end;

		writeln('could not find ', needle);
		halt;
	end;


	procedure DumpSortedPackets (var inputBuf: InputBuffer; var ranges: RangeArr; nRanges: integer; indexOrder: Sortable);
		var
			i, j: integer;
			r: Range;
	begin
		for i := 1 to nRanges do
			begin
				if debugLogLevel = debugPainfullyVerbose then
					begin
						r := ranges[indexOrder[i]];
						write(i : 3, '(', r.firstIx : 1, '-', r.lastIx : 1, '): ');
						for j := r.firstIx to r.lastIx do
							write(inputBuf.data[j]);
						writeln;
					end
				else
					writeln(i : 3, ': ', indexOrder[i] : 3);
			end;
	end;


	procedure Solve (var inputBuf: InputBuffer; var ranges: RangeArr; nRanges: integer);
		var
			i, div1, div2: integer;
			sorted: Sortable;

		function Cmp (a, b: integer): integer;
		begin
			case Order(inputBuf, ranges[a], ranges[b]) of
				inOrder: 
					Cmp := -1;
				TBD: 
					Cmp := 0;
				outOfOrder: 
					Cmp := 1;
			end;
		end;

	begin
		for i := 1 to nRanges do
			sorted[i] := i;

		Sort(sorted, nRanges, Cmp);
		div1 := IndexOf(sorted, nRanges, nRanges - 1);
		div2 := IndexOf(sorted, nRanges, nRanges);

		if debugLogLevel <> debugNone then
			begin
				DumpSortedPackets(inputBuf, ranges, nRanges, sorted);
				writeln('Dividers are at ', div1 : 1, ' and ', div2 : 1);
			end;

		writeln('Result: ', div1 * div2);
	end;


	var
		inputPath: integer;
		bufp: ^InputBuffer;
		err: OSErr;
		ranges: RangeArr;
		nRanges: integer;

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
					AppendLine(bufp^, '[[2]]');
					AppendLine(bufp^, '[[6]]');
					AppendLine(bufp^, '');
					writeln('about to find pairs');
					FindRanges(bufp^, ranges, nRanges);
					writeln('about to solve');
					Solve(bufp^, ranges, nRanges);
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