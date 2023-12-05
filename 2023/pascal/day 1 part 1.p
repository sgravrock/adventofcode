program day1p1;

{ Run time on Mac Classic, run standalone, w/full puzzle input: ~2.5 seconds }

	uses
		MicroSysFileUtils;

	const
		MaxObjectSize = 32767;

	type
{ Extravagantly wasteful. But why spend the extra dollars for 4MB RAM and not use it? }
		EntireFile = packed array[1..MaxObjectSize] of char;
		EntireFilePtr = ^EntireFile;


	procedure OpenAndReadFile (inputp: EntireFilePtr; var inputLen: LongInt);
		var
			openResult: FileError;
			filePath: integer; { welcome to the Mac operating system }
	begin
		openResult := OpenInputFile(filePath);

		if openResult <> FileErrorOk then
			begin
				writeln('Error opening input file');
				writeln('Press return to exit');
				readln;
				halt;
			end;

		inputLen := MaxObjectSize;

		if ReadEntireFile(filePath, Ptr(inputp), inputLen) <> FileErrorOk then
			begin
				writeln('Error reading input');
				writeln('Press return to exit');
				readln;
				halt;
			end;
	end;


	function Solve (var input: EntireFile; inputLen: LongInt): LongInt;
		var
			i, endln, c, result: LongInt;
	begin
		result := 0;
		i := 1;

		while i < inputLen do
			begin
{ Assume each line contains at least one digit }
{ Find first digit in line }
				while (input[i] < '0') or (input[i] > '9') do
					i := i + 1;
				c := ord(input[i]) - ord('0');

{ Find end of line }
				while input[i] <> chr(13) do
					i := i + 1;
				endln := i;

{ Find last digit in line }
				while (input[i] < '0') or (input[i] > '9') do
					i := i - 1;
				c := c * 10 + ord(input[i]) - ord('0');

				result := result + c;
				i := endln + 1;
			end;

		Solve := result;
	end;


	var
		inputp: EntireFilePtr;
		inputLen, result: LongInt;

begin
	ShowText;
	new(inputp);
	OpenAndReadFile(inputp, inputLen);
	result := Solve(inputp^, inputLen);
	writeln(result);
	writeln('Press return to exit');
	SysBeep(10);
	readln;
end.