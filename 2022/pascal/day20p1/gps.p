unit GPS;

interface

	uses
		Lists;

	type
		GroveCoordinates = array[1..3] of integer;

	procedure Mix (listp: CircularListPtr);
	function Decrypt (listp: CircularListPtr): GroveCoordinates;

implementation

	type
		NodeOrder = array[1..MaxListSize] of ListNodePtr;
		NodeOrderPtr = ^NodeOrder;

	function CopyListBuf (src: CircularListPtr): NodeOrderPtr;
		var
			result: NodeOrderPtr;
			i: integer;
	begin
		new(result);
		for i := 1 to src^.sz do
			result^[i] := @src^.bufp^[i];
		CopyListBuf := result;
	end;


	procedure Mix (listp: CircularListPtr);
		var
			originalOrder: NodeOrderPtr;
			i: integer;
	begin
		originalOrder := CopyListBuf(listp);

		for i := 1 to listp^.sz do
			begin
				MoveListNode(listp, originalOrder^[i], originalOrder^[i]^.val);
			end;

		dispose(originalOrder);
	end;

	function Decrypt (listp: CircularListPtr): GroveCoordinates;
		var
			result: GroveCoordinates;
			nodep: ListNodePtr;
			i, j: integer;
	begin
		Mix(listp);
		nodep := listp^.head;
		while nodep^.val <> 0 do
			nodep := nodep^.next;

		for i := 1 to 3 do
			begin
				for j := 1 to 1000 do
					nodep := nodep^.next;
				result[i] := nodep^.val;
			end;

		Decrypt := result;
	end;

end.
