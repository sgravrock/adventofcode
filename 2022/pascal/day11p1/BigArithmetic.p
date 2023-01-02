unit BigArithmetic;

interface

	type
{ Represents a 64 bit unsigned number. }
{ Although the numbers we're working with are all non-negative, }
{ Pascal doesn't have any unsigned types. So rather than reimplement }
{ the entirety of arithmetic from scratch, we use larger types }
{ (in this case, 16 bit intes to store conceptually unsigned 8 bit numbers) }
{ Using LongInt and working in 16 bit chunks would work and might }
{ or might not be more efficient -- it's hard to know without timing it. }
{ There would be half as many operations, but they'd be 32 bit  operations }
{which are slower on the 68000. }
		BigInt = array[1..8] of integer;

		BigDivResult = record
				quotient: BigInt;
				remainder: byte;
			end;

	procedure BigZero (var bi: BigInt);
	procedure BigFromByte (b: byte; var bi: BigInt);
	procedure BigAdd (var a: BigInt; b: BigInt; var result: BigInt);
	procedure BigMul (var a: BigInt; b: BigInt; var result: BigInt);
	procedure BigDiv (var a: BigInt; b: byte; var result: BigDivResult);
{ Convenience wrapper around BigDiv that discards the quotient }
	function BigMod (var a: BigInt; b: byte): byte;

implementation

	procedure BigZero (var bi: BigInt);
		var
			i: integer;
	begin
		for i := 1 to 8 do
			bi[i] := 0;
	end;

	procedure BigFromByte (b: byte; var bi: bigint);
	begin
		BigZero(bi);
		bi[1] := b;
	end;

	procedure BigAdd (var a: BigInt; b: BigInt; var result: BigInt);
		var
			i, tmp, carry: integer;
	begin
		carry := 0;

		for i := 1 to 8 do
			begin
				tmp := a[i] + b[i] + carry;
				result[i] := BAnd(tmp, $ff);
				carry := BSR(tmp, 8);
			end;
	end;

	procedure BigMul (var a: BigInt; b: BigInt; var result: BigInt);
		var
			i, j, k: integer;
			tmp: LongInt;
			sp: BigInt;
	begin
		BigZero(result);

		for i := 1 to 8 do
			for j := 1 to 8 - i + 1 do
				begin
					tmp := LongInt(a[i]) * LongInt(b[j]) + LongInt(result[i + j - 1]);
					result[i + j - 1] := BAnd(tmp, $ff);
					tmp := BSR(tmp, 8);

		{ Carry }
					for k := i + j to 8 do
						if tmp <> 0 then
							begin
								result[k] := BAnd(tmp, $ff);
								tmp := BSR(tmp, 8);
							end;
				end;
	end;

	procedure BigDiv (var a: BigInt; b: byte; var result: BigDivResult);
		var
			i, tmp: integer;
	begin
		BigZero(result.quotient);
		tmp := 0;

		for i := 8 downto 1 do
			begin
				tmp := BOR(BSL(tmp, 8), a[i]);
				result.quotient[i] := tmp div b;
				tmp := tmp mod b;
			end;

		result.remainder := tmp;
	end;

	function BigMod (var a: BigInt; b: byte): byte;
		var
			r: BigDivResult;
	begin
		BigDiv(a, b, r);
		BigMod := r.remainder;
	end;
end.