unit Packets;

interface

	uses
		Config;

	type
		InputBuffer = record
				data: packed array[1..maxBufSize] of char;
				sz: LongInt;
			end;
		Range = record
				firstIx, lastIx: integer;
			end;
		OrderType = (inOrder, outOfOrder, TBD);

	function Order (var inputBuf: InputBuffer; leftRange, rightRange: Range): OrderType;

implementation

	type
		PacketParsingState = record
				last, i: integer;
				promotedListDepth: integer;
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


	procedure InitParsingState (var pps: PacketParsingState; first, last: integer);
	begin
		pps.last := last;
		pps.i := first;
		pps.promotedListDepth := 0;
	end;


	procedure EnterPromotedList (var sideToPromote, otherSide: PacketParsingState);
	begin
{ Sanity check }
		if otherSide.promotedListDepth > 0 then
			begin
				writeln('Can''t enter a promoted list when the other side is in one');
				halt;
			end;

		sideToPromote.promotedListDepth := sideToPromote.promotedListDepth + 1;
		otherSide.i := otherSide.i + 1; { consume [ }
	end;


	function Order (var inputBuf: InputBuffer; leftRange, rightRange: Range): OrderType;
		var
			leftN, rightN: integer;
			leftC, rightC: char;
			leftState, rightState: PacketParsingState;
			result: OrderType;
	begin
		Require(inputBuf, leftRange.firstIx, '[');
		Require(inputBuf, rightRange.firstIx, '[');
		InitParsingState(leftState, leftRange.firstIx + 1, leftRange.lastIx);
		InitParsingState(rightState, rightRange.firstIx + 1, rightRange.lastIx);
		result := TBD;

{ Invariants, for both leftI and rightI: }
{ 1. Index is >= start and <= end. }
{ 2. The character at [index] has not been processed yet. }
{ 3. The character at [index] is not a comma. }
{ It follows that the character at [index] is one of the following: }
{ * The first digit of the next number in the current list }
{ * The ] at the end of the current list }
{ * The [ at the start of a list }
{ If in a number promoted to a list, all of the above belong to the parent list. }
		while (result = TBD) and (leftState.i <= leftState.last) and (rightState.i <= rightState.last) do
			begin
				leftC := inputBuf.data[leftState.i];
				rightC := inputBuf.data[rightState.i];

				if (leftC = ']') and (rightC = ']') then
					begin
						leftState.i := leftState.i + 1;
						rightState.i := rightState.i + 1;
					end
				else if (leftC = '[') and (rightC = '[') then
					begin
						if leftState.promotedListDepth > 0 then
							result := inOrder
						else if rightState.promotedListDepth > 0 then
							result := outOfOrder
						else
							begin
								leftState.i := leftState.i + 1;
								rightState.i := rightState.i + 1;
							end;
					end
				else if leftC = ']' then
					if rightState.promotedListDepth > 0 then
						rightState.promotedListDepth := rightState.promotedListDepth - 1
					else
						result := inOrder
				else if rightC = ']' then
					if leftState.promotedListDepth > 0 then
						leftState.promotedListDepth := leftState.promotedListDepth - 1
					else
						result := outOfOrder
				else if leftC = '[' then
					begin
						if leftState.promotedListDepth > 0 then
							result := inOrder
						else
							EnterPromotedList(rightState, leftState)
					end
				else if rightC = '[' then
					begin
						if rightState.promotedListDepth > 0 then
							result := outOfOrder
						else
							EnterPromotedList(leftState, rightState)
					end
				else if (inputBuf.data[leftState.i] = ']') or (inputBuf.data[rightState.i] = ']') then
					begin
						writeln('don''t know how to exit lists yet');
						halt;
					end
				else { both are numbers }
					begin
						leftN := ConsumeNumber(inputBuf, leftState.i);
						rightN := ConsumeNumber(inputBuf, rightState.i);

						if leftN < rightN then
							result := inOrder
						else if leftN > rightN then
							result := outOfOrder;
	       { else keep going }
					end;
			end;

		Order := result;
	end;

end.