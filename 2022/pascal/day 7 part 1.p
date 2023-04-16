program day7p1;

	uses
		FileUtils;

	type
		DirStack = record
				dirs: array[1..10] of LongInt;
				n: integer;
			end;

	const
		maxSmallDirSize = 100000;


	procedure Pushd (var stack: DirStack);
	begin
		stack.n := stack.n + 1;
		stack.dirs[stack.n] := 0;
	end;

	procedure Popd (var stack: DirStack);
	begin
		stack.n := stack.n - 1;
	end;

	function CountSmallDirs (var f: Text): LongInt;
		var
			stack: DirStack;
			line: string;
			nSmall: integer;
			fileSz, result: LongInt;

		procedure PopAndCheck;
			var
				curSz: LongInt;
		begin
			curSz := stack.dirs[stack.n];
			if curSz <= maxSmallDirSize then
				begin
					nSmall := nSmall + 1;
					result := result + curSz;
				end;
			stack.dirs[stack.n - 1] := stack.dirs[stack.n - 1] + curSz;
			Popd(stack);
		end;

	begin
		stack.n := 0;
		nSmall := 0;
		result := 0;
		Pushd(stack);
		readln(f, line); { discard leading '$ cd /' }
		while not eof(f) do
			begin
				readln(f, line);
				if line = '$ cd ..' then
					PopAndCheck
				else if Pos('$ cd ', line) = 1 then
					begin
						Pushd(stack);
					end
				else if (Pos('dir ', line) <> 1) and (line <> '$ ls') then
					begin
						ReadString(line, fileSz);
						stack.dirs[stack.n] := stack.dirs[stack.n] + fileSz;
					end;
			end;

		while stack.n > 1 do
			PopAndCheck;

		CountSmallDirs := result;
	end;

	var
		inputFile: Text;
		result: LongInt;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			result := CountSmallDirs(inputFile);
			writeln('Result: ', result : 1);
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.