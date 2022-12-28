program day13p2;

{ Note: This needs 256KB zone (heap) size. (Run -> Run Options in THINK Pascal.}

{ Note: Set enableDebugOutput in config.p to true for debugging, }
{ or false to make this run "fast". }

	uses
		Config, IO, Packets, Sorting, Tests;

	type
		PacketArr = array[1..maxNumPackets] of ListPtr;

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


	procedure ParsePackets (var inputBuf: InputBuffer; var packets: PacketArr; var nPackets: integer);
		var
			i, e: integer;
	begin
		i := 1;
		nPackets := 0;

		while i < inputBuf.sz do
			begin
				nPackets := nPackets + 1;
				packets[nPackets] := ParseList(inputBuf, i, e);
				i := e + 1;

				while inputBuf.data[i] = chr($d) do
					i := i + 1;
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

	procedure Solve (var packets: PacketArr; nPackets: integer);
		var
			i, div1, div2: integer;
			sorted: Sortable;

		function Cmp (a, b: integer): integer;
		begin
			case Order(packets[a], packets[b]) of
				inOrder: 
					Cmp := -1;
				TBD: 
					Cmp := 0;
				outOfOrder: 
					Cmp := 1;
			end;
		end;

	begin
		for i := 1 to nPackets do
			sorted[i] := i;

		Sort(sorted, nPackets, Cmp);
		div1 := IndexOf(sorted, nPackets, nPackets - 1);
		div2 := IndexOf(sorted, nPackets, nPackets);

		if debugLogLevel <> debugNone then
			begin
				writeln('Dividers are at ', div1 : 1, ' and ', div2 : 1);
			end;

		writeln('Result: ', div1 * div2);
	end;


	var
		inputPath: integer;
		bufp: ^InputBuffer;
		err: OSErr;
		packets: PacketArr;
		nPackets: integer;

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
					writeln('about to parse');
					InitListPool;
					ParsePackets(bufp^, packets, nPackets);
					writeln('about to solve');
					Solve(packets, nPackets);
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