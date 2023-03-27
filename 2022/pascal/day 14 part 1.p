program day14p1;

{ x: 449..505, y: 13..173 }

	uses
		SysFileUtils;

	const
		bufsize = 11264; { round  number big enough for puzzle input }

	type
		InputBuffer = record
				data: packed array[1..bufsize] of char;
				pos, sz: LongInt;
			end;
		InputBufferPtr = ^InputBuffer;

	function BEof (var buf: InputBuffer): boolean;
	begin
		BEof := buf.pos > buf.sz;
	end;

	function BEoln (var buf: InputBuffer): boolean;
	begin
		BEoln := (buf.pos > buf.sz) or (ord(buf.data[buf.pos]) = $d);
	end;

	function Peekc (var buf: InputBuffer): char;
	begin
		if buf.pos > buf.sz then
			begin
				writeln('Attempted to peek past end of buffer');
				halt;
			end;

		Peekc := buf.data[buf.pos];
	end;

	function Getc (var buf: InputBuffer): char;
	begin
		if buf.pos > buf.sz then
			begin
				writeln('Attempted to read past end of buffer');
				halt;
			end;

		Getc := buf.data[buf.pos];
		buf.pos := buf.pos + 1;
	end;

	procedure Consume (var buf: InputBuffer; expected: string);
		var
			i: integer;
			c: char;
	begin
		for i := 1 to length(expected) do
			begin
				c := Getc(buf);
				if c <> expected[i] then
					begin
						writeln('Expected ', expected[i], ' but got ', c, ' at position ', buf.pos - 1);
						halt;
					end;
			end;
	end;

	function ReadInput (var buf: InputBuffer): boolean;
		var
			path: integer;
	begin
		if OpenInputFile(path) then
			begin
				buf.sz := bufsize;
				buf.pos := 1;
				ReadInput := ReadEntireFile(path, @buf.data[1], buf.sz);
			end
		else
			ReadInput := false;
	end;


	function ReadInt (var buf: InputBuffer): integer;
		var
			n: integer;
			c: char;
			done: boolean;
	begin
		n := 0;
		done := false;

		while not done do
			begin
				if buf.pos > buf.sz then
					done := true
				else
					begin
						c := Peekc(buf);
						if (c >= '0') and (c <= '9') then
							n := n * 10 + ord(Getc(buf)) - ord('0')
						else if n > 0 then
							done := true
						else
							begin
								writeln('Tried to read a number but got ', c, ' (at offset ', buf.pos : 1, ')');
								halt;
							end;
					end;
			end;

		ReadInt := n;
	end;

	var
		bufp: InputBufferPtr;
		minX, minY, maxX, maxY: integer;
		n: integer;
		c: char;

begin
	ShowText;
	new(bufp);

	if ReadInput(bufp^) then
		begin
			minX := maxint;
			minY := maxint;
			maxX := -maxint - 1;
			maxY := -maxint - 1;

			while not BEof(bufp^) do
				begin
					while not BEoln(bufp^) do
						begin
							n := ReadInt(bufp^);
							if n < minX then
								minX := n;
							if n > maxX then
								maxX := n;
							Consume(bufp^, ',');
							n := ReadInt(bufp^);
							if n < minY then
								minY := n;
							if n > maxY then
								maxY := n;

							if not BEoln(bufp^) then
								Consume(bufp^, ' -> ');
						end;

					if not BEof(bufp^) then
						Consume(bufp^, chr($d));
				end;

			writeln('x: ', minX : 1, '..', maxX : 1, ' y: ', minY : 1, '..', maxY : 1);
		end
	else
		writeln('Did not open input file');
end.