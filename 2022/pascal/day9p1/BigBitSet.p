unit BigBitSet;

interface

	const
		BBSPageSizeLongs = 248; { 120 gives ~2 mins }
		BBSPageMaxIndex = BBSPageSizeLongs - 1;

	type
		BBSBuf = array[0..BBSPageMaxIndex] of LongInt;
		BBSPagePtr = ^BBSPage;
		BigBitSet = record
{    recent: BBSPagePtr; }
				pxpy, pxny, nxpy, nxny: BBSPagePtr; { quadrants }
				size: LongInt;
			end;
		BBSPage = record
				buf: BBSBuf;
				base: LongInt;
{prev, }
				next: BBSPagePtr;
			end;

	procedure BBSInit (var s: BigBitSet);
	procedure BBSFree (var s: BigBitSet);
	procedure BBSInsert (var s: BigBitSet; x, y: integer);
	function BBSPageCount (var s: BigBitSet): integer;

implementation

	const
		BitsPerPage = BBSPageSizeLongs * 4 * 8;

	function MakePage (base: LongInt): BBSPagePtr;
		var
			page: BBSPagePtr;
			i: integer;
	begin
		new(page);
		page^.base := base;
{page^.prev := nil;}
		page^.next := nil;
		for i := 0 to BBSPageMaxIndex do
			page^.buf[i] := 0;
		MakePage := page;
	end;

{ TODO probably inline this }
	function IndexFromCoords (x, y: integer): LongInt;
		var
			msw, lsw: integer;
	begin
		x := abs(x);
		y := abs(y);
{ Interleave the two coordinates like so: xMSB yMSB xLSB yLSB }
{ This ensures that adjacent coordinate pairs go on the same or adjacent pages,}
{ as long as they're all non-negative. }
		lsw := BOR(BSL(BAND(x, $FF), 8), BAND(y, $FF));
		msw := BOR(BAND(x, $FF00), BSR(y, 8));
		IndexFromCoords := BOR(BSL(msw, 16), lsw);
	end;

	procedure BBSInit (var s: BigBitSet);
	begin
		s.size := 0;
		s.pxpy := MakePage(0);
		s.pxny := MakePage(IndexFromCoords(0, 1));
		s.nxpy := MakePage(IndexFromCoords(1, 0));
		s.nxny := MakePage(IndexFromCoords(1, 1));
	end;

	procedure FreePages (p: BBSPagePtr);
		var
			f: BBSPagePtr;
	begin
		while p <> nil do
			begin
				f := p;
				p := p^.next;
				dispose(f);
			end;
	end;

	procedure BBSFree (var s: BigBitSet);
	begin
		FreePages(s.pxpy);
		FreePages(s.pxny);
		FreePages(s.nxpy);
		FreePages(s.nxny);
	end;

	procedure InsertIndex (var s: BigBitSet; page: BBSPagePtr; bitIndex: LongInt);
		var
			prevVal: LongInt;
	begin
		if bitIndex >= page^.base + BitsPerPage then
			begin
				if page^.next = nil then
					page^.next := MakePage(page^.base + BitsPerPage);
				InsertIndex(s, page^.next, bitIndex);
			end
		else
			begin
				bitIndex := bitIndex - page^.base;
				prevVal := page^.buf[bitIndex div 32];
				BSET(page^.buf[bitIndex div 32], bitIndex mod 32);
				if page^.buf[bitIndex div 32] <> prevVal then
					s.size := s.size + 1;
			end;
	end;


	procedure BBSInsert (var s: BigBitSet; x, y: integer);
		var
			bitIndex: LongInt;
	begin
		bitIndex := IndexFromCoords(x, y);
		if x < 0 then
			if y < 0 then
				InsertIndex(s, s.nxny, bitIndex)
			else
				InsertIndex(s, s.nxpy, bitIndex)
		else if y < 0 then
			InsertIndex(s, s.pxny, bitIndex)
		else
			InsertIndex(s, s.pxpy, bitIndex);
	end;

	function BBSPageCount (var s: BigBitSet): integer;
		var
			n: integer;
			p: BBSPagePtr;
	begin
		n := 0;
		p := s.pxpy;
		while p <> nil do
			begin
				n := n + 1;
				p := p^.next;
			end;
		p := s.pxny;
		while p <> nil do
			begin
				n := n + 1;
				p := p^.next;
			end;
		p := s.nxpy;
		while p <> nil do
			begin
				n := n + 1;
				p := p^.next;
			end;
		p := s.nxny;
		while p <> nil do
			begin
				n := n + 1;
				p := p^.next;
			end;
		BBSPageCount := n;
	end;

end.