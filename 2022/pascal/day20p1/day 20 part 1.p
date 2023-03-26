program day20p1;

	uses
		FileUtils, Lists, GPS, Tests;

	var
		inputFile: Text;
		listp: CircularListPtr;
		coords: GroveCoordinates;
		n, result: integer;

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

			coords := Decrypt(listp);
			DisposeList(listp);
			listp := nil;
			result := coords[1] + coords[2] + coords[3];
			writeln(result);
			SysBeep(10);
			writeln('Press return');
			readln;
		end;
end.