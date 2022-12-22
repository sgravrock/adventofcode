program day13p1;

	uses
		FileUtils;

{ [[1], [2, 3, 4]] }

	type
		PacketDatumType = (list, atom);
		PacketDatumListPtr = ^PacketDatumList;
		PacketDatum = record
				case datumType : PacketDatumType of
					atom: (
							val: integer
					);
					list: (
							head: PacketDatumListPtr
					)
			end;
		PacketDatumList = record
				d: PacketDatum;
				nextp: PacketDatumListPtr;
			end;
		LineBuffer = record
				line: string;
				i: integer;
			end;

	function Getc (var buf: LineBuffer): char;
	begin
		buf.i := buf.i + 1;
		Getc := buf.line[buf.i - 1];
	end;

	procedure Ungetc (var buf: LineBuffer);
	begin
		buf.i := buf.i - 1;
	end;

	function IsDigit (c: char): boolean;
	begin
		IsDigit := (c >= '0') and (c <= '9');
	end;

	procedure Free (lp: PacketDatumListPtr);
	begin
		if lp <> nil then
			begin
				if lp^.d.datumType = list then
					free(lp^.d.head);
				dispose(lp);
			end;
	end;

	procedure ReadAtom (var buf: LineBuffer; var dest: PacketDatum);
		var
			c: char;
	begin
		dest.datumType := atom;
		dest.val := 0;
		c := Getc(buf);

		while IsDigit(c) do
			begin
				dest.val := dest.val * 10 + ord(c) - ord('0');
				c := Getc(buf);
			end;

		Ungetc(buf);
	end;


{ Precondition: leading [ has been consumed }
	function ReadList (var buf: LineBuffer): PacketDatumListPtr;
		var
			result: PacketDatumListPtr;
			c: char;
			removeMe: PacketDatum;
	begin
		c := Getc(buf);

		if c = ']' then { special case: empty list }
			begin
				ReadList := nil;
				exit(ReadList);
			end;

{new(result);}

		if c = '[' then
			begin
{result^.d.datumType := list;}
{result^.d.head}
				result := ReadList(buf);
				result := nil;
			end
		else if IsDigit(c) then
			begin
				Ungetc(buf);
				ReadAtom(buf, removeMe); {result^.d }
			end
		else
			begin
				writeln('Unexpecxted "', c);
				readln;
				halt;
			end;

		c := Getc(buf);

		case c of
			']':
{result^.nextp := nil;}
				begin
				end;
			',': 
				begin
{result^.nextp}
					result := ReadList(buf);
					result := nil;
				end;
			otherwise
				begin
					writeln('Unexpected at end of list: ', c);
					readln;
					halt;
				end;
		end;

{    ReadList := result;}
	end;


{ A packet is just a datum that we know is a list. }
	function ReadPacket (var f: text): PacketDatumListPtr;
		var
			buf: LineBuffer;
	begin
		readln(f, buf.line);
		buf.i := 2; { skip leading '[' }
		ReadPacket := ReadList(buf);
		if buf.i <= length(buf.line) then
			begin
				writeln('Found garbage after a packet');
				readln;
				halt;
			end;
	end;


	procedure DumpList (list: PacketDatumListPtr; indent: string);
	forward;

	procedure DumpDatum (var d: PacketDatum; indent: string);
	begin
		case d.datumType of
			atom: 
				writeln(indent, d.val);
			list: 
				DumpList(d.head, indent);
		end;
	end;

	procedure DumpList (list: PacketDatumListPtr; indent: string);
		var
			p: PacketDatumListPtr;
	begin
		writeln(indent, '[');
		p := list;
		while p <> nil do
			begin
				DumpDatum(p^.d, Concat(indent, '   '));
				p := p^.nextp;
			end;
		writeln(indent, ']');
	end;

	procedure Foo (var f: Text);
		var
			packet: PacketDatumListPtr;
			forDebugging: string;
	begin
		while not eof(f) do
			begin
				packet := ReadPacket(f);
{Free(packet);}
				packet := ReadPacket(f);
{    Free(packet);}
				readln(f, forDebugging);
			end;
	end;

	var
		inputFile: Text;
		removeMe: string;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			while not eof(inputFile) do
				readln(inputFile, removeMe);
			Foo(inputFile);
		end
	else
		writeln('Did not open input file');


end.