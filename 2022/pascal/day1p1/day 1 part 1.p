program day1p1;

{ Prompts for a file and opens it using Pascal I/O, which is significantly more }
{ convenient for line- or number-at-a-time reading than Mac Toolbox I/O }
	function OpenInputFile (var f: Text): boolean;
		var
			path: string[255];
	begin
		path := OldFileName;

		if path = '' then
			OpenInputFile := false
		else
			begin
				reset(f, path);
			end;
	end;

{ Reads the next line. If it represents an integer, stores the }
{ integer in result and returns true. Returns false if a blank }
{ line is encountered. }
	function ReadIntLine (var f: Text; var result: LongInt): boolean;
		var
			line: string[10];
	begin
		readln(f, line); { trap if input line is too long }
		if line = '' then
			readIntLine := false
		else
			begin
				readString(line, result); { trap if not parseable }
				readIntLine := true;
			end;
	end;

	function ReadElf (var f: Text): LongInt;
		var
			totalCalories: LongInt;
			latestCalories: LongInt;
			done: boolean;
	begin
		totalCalories := 0;
		done := false;
		while not eof(f) and not done do
			begin
				if ReadIntLine(f, latestCalories) then
					totalCalories := totalCalories + latestCalories
				else
					done := true;
			end;
		ReadElf := totalCalories;
	end;

	function FindElfWithMostCalories (var f: Text): LongInt;
		var
			latest: LongInt;
			most: LongInt;
	begin
		most := 0;
		while not eof(f) do
			begin
				latest := ReadElf(f);
				if latest > most then
					most := latest;
				writeln('latest: ', latest, '     running max: ', most);
			end;
		FindElfWithMostCalories := most;
	end;

	var
		inputFile: Text;
		result: LongInt;

begin
	if OpenInputFile(inputFile) then
		begin
			result := FindElfWithMostCalories(inputFile);
			writeln('Result: ', result);
		end
	else
		writeln('Did not open input file');
end.