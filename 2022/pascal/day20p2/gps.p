unit GPS;

interface

	uses
		Lists, BigArithmetic, BigSignedArithmetic;

	const
		DecryptionKey = 811589153;

	procedure Mix (listp: CircularListPtr);
	function Decrypt (listp: CircularListPtr): BigSignedInt;

implementation

	type
		MixBufEntry = record
				nodep: ListNodePtr;
				mixBy: LongInt;
			end;
		MixBuf = array[1..MaxListSize] of MixBufEntry;
		MixBufPtr = ^MixBuf;

	function CopyListBuf (src: CircularListPtr): MixBufPtr;
		var
			result: MixBufPtr;
			i: integer;
	begin
		new(result);
		for i := 1 to src^.sz do
			result^[i].nodep := @src^.bufp^[i];
		CopyListBuf := result;
	end;


	procedure Mix (listp: CircularListPtr);
		var
			originalOrder: MixBufPtr;
			j, i, sz: integer;
			x, keyMultiplier: LongInt;
	begin
		originalOrder := CopyListBuf(listp);

{ Applying modulo list size to the operands lets us stick with native integers, }
{ and doing it to the result vastly cuts down on unnecessary mixing later on. }
		sz := listp^.sz - 1;
		keyMultiplier := DecryptionKey mod sz;
		for i := 1 to listp^.sz do
			begin
				x := originalOrder^[i].nodep^.val mod sz;
				originalOrder^[i].mixBy := (x * keyMultiplier) mod sz;
			end;

		for j := 1 to 10 do
			begin
				writeln('Beginning mix iteration', j);
				for i := 1 to listp^.sz do
					begin
						MoveListNode(listp, originalOrder^[i].nodep, originalOrder^[i].mixBy);
					end;
			end;

		dispose(originalOrder);
	end;


	function Decrypt (listp: CircularListPtr): BigSignedInt;
		var
			result: BigSignedInt;
			nodep: ListNodePtr;
			i, j, sumOfEncryptedCoords: integer;
			bigDecryptionKey, tmp: BigInt;
	begin
		Mix(listp);
		nodep := listp^.head;
		while nodep^.val <> 0 do
			nodep := nodep^.next;

		BigFromLong(DecryptionKey, bigDecryptionKey);
		sumOfEncryptedCoords := 0;

		for i := 1 to 3 do
			begin
				for j := 1 to 1000 do
					nodep := nodep^.next;

				sumOfEncryptedCoords := sumOfEncryptedCoords + nodep^.val;
			end;

		BigFromLong(abs(sumOfEncryptedCoords), tmp);
		BigMul(tmp, bigDecryptionKey, result.magnitude);
		result.isNegative := sumOfEncryptedCoords < 0;

		Decrypt := result;
	end;

end.