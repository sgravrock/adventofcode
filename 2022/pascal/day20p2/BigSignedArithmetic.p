unit BigSignedArithmetic;

interface

	uses
		BigArithmetic;

	type
		BigSignedInt = record
				magnitude: BigInt;
				isNegative: boolean;
			end;

	function BigSignedIntToString (n: BigSignedInt): string;

implementation

	function BigSignedIntToString (n: BigSignedInt): string;
		var
			s: string;
	begin
		s := BigToString(n.magnitude);

		if n.isNegative then
			s := Concat('-', s);

		BigSignedIntToString := s;
	end;

end.