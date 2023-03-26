program day20p2;

	uses
		FileUtils, BigArithmetic, BigSignedArithmetic, Lists, GPS, Tests;

	var
		inputFile: Text;
		listp: CircularListPtr;
		result: BigSignedInt;
		n: LongInt;

begin
	ShowText;
	RunTests;

	if OpenInputFile(inputFile) then
		begin
			listp := NewList;

			while not eof(inputFile) do
				begin
					readln(inputFile, n);
					ListAppend(listp, n);
				end;

			result := Decrypt(listp);
			DisposeList(listp);
			listp := nil;
			writeln(BigSignedIntToString(result));
			SysBeep(10);
			writeln('Press return');
			readln;
		end;
end.