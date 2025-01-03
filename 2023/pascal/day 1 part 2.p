program day1p2;

{ Run time on Mac Classic, run standalone, w/full puzzle input: ~6 seconds }

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


	function NumberStartingAt (var input: EntireFile; inputLen, i: LongInt): LongInt;
		function IsStartOf (target: string): boolean;
			var
				j: LongInt;
		begin
			if i + length(target) - 1 > inputLen then
				IsStartOf := false
			else
				begin
					for j := 1 to length(target) do
						if input[i + j - 1] <> target[j] then
							begin
								IsStartOf := false;
								exit(IsStartOf);
							end;
					IsStartOf := true;
				end;
		end;
	begin
		NumberStartingAt := -1;

		if (input[i] >= '0') and (input[i] <= '9') then
			NumberStartingAt := ord(input[i]) - ord('0')
		else if input[i] = 'o' then
			begin
				if IsStartOf('one') then
					NumberStartingAt := 1;
			end
		else if input[i] = 't' then
			begin
				if IsStartOf('two') then
					NumberStartingAt := 2
				else if IsStartOf('three') then
					NumberStartingAt := 3;
			end
		else if input[i] = 'f' then
			begin
				if IsStartOf('four') then
					NumberStartingAt := 4
				else if IsStartOf('five') then
					NumberStartingAt := 5;
			end
		else if input[i] = 's' then
			begin
				if IsStartOf('six') then
					NumberStartingAt := 6
				else if IsStartOf('seven') then
					NumberStartingAt := 7;
			end
		else if input[i] = 'e' then
			begin
				if IsStartOf('eight') then
					NumberStartingAt := 8;
			end
		else if input[i] = 'n' then
			begin
				if IsStartOf('nine') then
					NumberStartingAt := 9;
			end
	end;


	function Solve (var input: EntireFile; inputLen: LongInt): LongInt;
		var
			i, endln, n, c, result: LongInt;
	begin
		result := 0;
		i := 1;

		while i < inputLen do
			begin
{ Assume each line contains at least one digit }
{ Find first digit in line }
				c := -1;
				while c = -1 do
					begin
						c := NumberStartingAt(input, inputLen, i);
						i := i + 1;
					end;

{ Find end of line }
				while input[i] <> chr(13) do
					i := i + 1;
				endln := i;

{ Find last digit in line }
				n := -1;
				while n = -1 do
					begin
						n := NumberStartingAt(input, inputLen, i);
						i := i - 1;
					end;
				c := c * 10 + n;

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