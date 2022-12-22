program day13p1;

	uses
		IO;

	const
		enableDebugOutput = true;

	type
		InputBuffer = record
				data: packed array[1..22000] of char;
				sz: LongInt;
			end;
		OrderType = (inOrder, outOfOrder, TBD);

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


	procedure DumpAround (var inputBuf: InputBuffer; i: integer);
		var
			j, s, e: integer;
	begin
		s := i - 10;
		if s < 1 then
			s := 1;
		e := i + 10;
		if e > inputBuf.sz then
			e := inputBuf.sz;

		for j := s to e do
			if inputBuf.data[j] = chr($d) then
				write('_')
			else
				write(inputBuf.data[j]);
		writeln;

		for j := s to e do
			if j = i then
				write('^')
			else
				write(' ');
		writeln;
	end;

	procedure Require (var inputBuf: InputBuffer; i: integer; expected: char);
	begin
		if inputBuf.data[i] <> expected then
			begin
				writeln('expected ', expected, ' at position ', i : 1, ' but found ', inputBuf.data[i]);
				DumpAround(inputBuf, i);
				halt;
			end;
	end;


	function ConsumeNumber (var buf: inputBuffer; var i: integer): integer;
		var
			n: integer;
	begin
		n := 0;
		while (buf.data[i] <> ',') and (buf.data[i] <> ']') do
			begin
				n := n * 10 + ord(buf.data[i]) - ord('0');
				i := i + 1;
			end;

		if buf.data[i] = ',' then
			i := i + 1;

		ConsumeNumber := n;
	end;


	function Order (var inputBuf: InputBuffer; leftStart, leftEnd, rightStart, rightEnd: integer): OrderType;
		var
			depth, leftI, rightI, leftN, rightN: integer;
			leftIsPromotedList, rightIsPromotedList: boolean;
	begin
		require(inputBuf, leftStart, '[');
		require(inputBuf, rightStart, '[');
		depth := 1;
		leftI := leftStart + 1;
		rightI := rightStart + 1;
		leftIsPromotedList := false;
		rightIsPromotedList := false;

		while (leftI <= leftEnd) and (rightI <= rightEnd) do
			if (inputBuf.data[leftI] = '[') and (inputBuf.data[rightI] = '[') then
				begin
					depth := depth + 1;
					leftI := leftI + 1;
					rightI := rightI + 1
				end
			else if inputBuf.data[leftI] = '[' then
				if inputBuf.data[rightI] = ']' then
					begin
						Order := outOfOrder;
						exit(Order);
					end
				else
					begin
						depth := depth + 1;
						leftI := leftI + 1;
						rightIsPromotedList := true;
					end
			else if inputBuf.data[rightI] = '[' then
				if inputBuf.data[leftI] = ']' then
					begin
						Order := outOfOrder;
						exit(Order);
					end
				else
					begin
						depth := depth + 1;
						rightI := rightI + 1;
						leftIsPromotedList := true;
					end
			else if inputBuf.data[leftI] = ']' then
				begin
					depth := depth - 1;
					leftI := leftI + 1;

					if inputBuf.data[rightI] = ']' then
						rightI := rightI + 1
					else if rightIsPromotedList then
						begin
							if enableDebugOutput then
								writeln('popping promoted list on right');
							while (inputBuf.data[rightI] <> ',') and (inputBuf.data[rightI] <> ']') do
								rightI := rightI + 1;
							if inputBuf.data[rightI] = ',' then
								rightI := rightI + 1;

{    Require(inputBuf, rightI, ',');}
						end
					else
						begin
							Order := inOrder;
							exit(Order);
						end;

					rightIsPromotedList := false;
				end
			else if (inputBuf.data[rightI] = ']') then
				begin
					depth := depth - 1;
					rightI := rightI + 1;
					if leftIsPromotedList then
						begin
							if enableDebugOutput then
								writeln('popping promoted list on left');
							while (inputBuf.data[leftI] <> ',') and (inputBuf.data[leftI] <> ']') do
								leftI := leftI + 1;
{    Require(inputBuf, leftI, ',');}
							if inputBuf.data[leftI] = ',' then
								leftI := leftI + 1;
						end
					else
						begin
							Order := outOfOrder;
							exit(Order);
						end;
					leftIsPromotedList := false;
				end
			else { numbers on both sides }
				begin
					leftN := ConsumeNumber(inputBuf, leftI);
					rightN := ConsumeNumber(inputBuf, rightI);
					if leftN < rightN then
						begin
							Order := inOrder;
							exit(Order);
						end
					else if leftN > rightN then
						begin
							Order := outOfoRder;
							exit(Order);
						end;
				end;

		Order := TBD;
	end;

	procedure Solve (var inputBuf: InputBuffer);
		var
			pos, i, result, aStart, aEnd, bEnd: integer;
	begin
		pos := 1;
		i := 0;
		result := 0;

		while (pos < inputBuf.sz) and (pos > 0) do
			begin
				i := i + 1;
		{ Read and consume a pair of packets }
				aStart := pos;
				aEnd := FindNext(inputBuf, chr($d), aStart, -1);
				bEnd := FindNext(inputBuf, chr($d), aEnd + 1, -1);
				pos := bEnd + 2; { consume the line separating each pair }

				case Order(inputBuf, aStart, aEnd - 1, aEnd + 1, bEnd - 1) of
					inOrder: 
						begin
							if enableDebugOutput then
								writeln(i : 1, ' is in order');
							result := result + 1;
						end;
					outOfOrder: 
						if enableDebugOutput then
							writeln(i : 1, ' is out of order');
					TBD: 
						writeln('Could not determine order for', i : 1);
				end;
			end;

		writeln('Result: ', result); { 5630 is too high, 4163 is too low }
	end;

	var
		inputPath: integer;
		bufp: ^InputBuffer;
		err: OSErr;

begin
	ShowText;

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