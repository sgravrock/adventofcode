program day1p2;

	type
		ThreeLongs = array[1..3] of LongInt;

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

{ Replaces the smallest value in arr with newVal, if newVal is larger. }
{ Pre- and post-condition: arr is sorted in descending order. }
	procedure MaybeReplaceSmallest (var arr: ThreeLongs; newVal: LongInt);
		var
			i, j: Integer;
	begin
		for i := 1 to 3 do
			if newVal > arr[i] then
				begin
					for j := 2 downto i do
						arr[j + 1] := arr[j];
					arr[i] := newVal;
					leave;
				end;
	end;

	function FindElvesWithMostCalories (var f: Text): LongInt;
		var
			latest: LongInt;
			acc: ThreeLongs;
	begin
		acc[1] := 0;
		acc[2] := 0;
		acc[3] := 0;
		while not eof(f) do
			begin
				latest := ReadElf(f);
				MaybeReplaceSmallest(acc, latest);
			end;
		writeln('Top 3:', acc[1], ' ', acc[2], ' ', acc[3]);
		FindElvesWithMostCalories := acc[1] + acc[2] + acc[3];
	end;

	var
		inputFile: Text;
		result: LongInt;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			result := FindElvesWithMostCalories(inputFile);
			writeln('Result: ', result);
			SysBeep(10);
			writeln('Press return');
			readln;
		end
	else
		writeln('Did not open input file');
end.