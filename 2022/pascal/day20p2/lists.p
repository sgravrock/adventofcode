unit Lists;

interface

	const
		MaxListSize = 5000;

	type
		ListNodePtr = ^ListNode;
		ListNode = record
				val: integer;
				mixOffset: integer;
				prev: ListNodePtr;
				next: ListNodePtr;
			end;
		ListBuf = array[1..MaxListSize] of ListNode;
		ListBufPtr = ^ListBuf;
		CircularList = record
				bufp: ListBufPtr;
				sz: integer;
				head: ListNodePtr;
			end;
		CircularListPtr = ^CircularList;

	function NewList: CircularListPtr;
	procedure DisposeList (listp: CircularListPtr);
	procedure ListAppend (listp: CircularListPtr; val: integer);
	procedure MoveListNode (listp: CircularListPtr; toMove: ListNodePtr; deltaPos: integer);
{ PrintList is useful for debugging e.g. from the Instant window }
	procedure PrintList (listp: CircularListPtr);

implementation

	function NewList: CircularListPtr;
		var
			result: CircularListPtr;
	begin
		new(result);
		if result <> nil then
			begin
				new(result^.bufp);
				if result^.bufp = nil then
					begin
						dispose(result);
						result := nil;
					end;
			end;
		NewList := result;
	end;

	procedure DisposeList (listp: CircularListPtr);
	begin
		dispose(listp^.bufp);
		dispose(listp);
	end;

	function AllocateNode (listp: CircularListPtr): ListNodePtr;
	begin
	{ Let Pascal's bounds checking crash us if OOB }
		listp^.sz := listp^.sz + 1;
		AllocateNode := @listp^.bufp^[listp^.sz];
	end;

	procedure ListAppend (listp: CircularListPtr; val: integer);
		var
			newNodep: ListNodePtr;
	begin
		newNodep := AllocateNode(listp);
		newNodep^.val := val;

		if listp^.head = nil then
			begin
				newNodep^.next := newNodep;
				newNodep^.prev := newNodep;
				listp^.head := newNodep;
			end
		else
			begin
				newNodep^.prev := listp^.head^.prev;
				newNodep^.next := listp^.head;
				newNodep^.prev^.next := newNodep;
				listp^.head^.prev := newNodep;
			end;
	end;

	procedure MoveListNode (listp: CircularListPtr; toMove: ListNodePtr; deltaPos: integer);
		var
			i: integer;
			newPrev, newNext: ListNodePtr;
	begin
		if deltaPos = 0 then
			exit(MoveListNode);

		newPrev := toMove^.prev;
{ Unlink toMove }
		toMove^.next^.prev := newPrev;
		newPrev^.next := toMove^.next;
		if listp^.head = toMove then
			listp^.head := toMove^.next;

		if deltaPos < 0 then
			for i := deltaPos to -1 do
				newPrev := newPrev^.prev
		else
			for i := 1 to deltaPos do
				newPrev := newPrev^.next;

		newNext := newPrev^.next;
		newPrev^.next := toMove;
		newNext^.prev := toMove;
		toMove^.prev := newPrev;
		toMove^.next := newNext;
	end;

	procedure PrintList (listp: CircularListPtr);
		var
			p: ListNodePtr;
	begin
		p := listp^.head;

		while true do
			begin
				write(p^.val : 1);
				p := p^.next;

				if p = listp^.head then
					begin
						writeln;
						leave;
					end
				else
					write(', ');
			end;
	end;
end.