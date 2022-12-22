program day7p1;

	type
		Dir = record
{ name: string; TODO either use or remove }
				sz: LongInt;
			end;
		DirStack = record
				dirs: array[1..10] of Dir;
				n: integer;
			end;

	const
		maxSmallDirSize = 100000;
		enableDebugging = true; { enables interactive visual debugging }

{ Prompts for a file and opens it using Pascal I/O, which is significantly more }
{ convenient for line- or char-at-a-time reading than Mac Toolbox I/O }
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

	procedure Pushd (var stack: DirStack);
	begin
		stack.n := stack.n + 1;
		if enableDebugging then
			writeln('pushing to ', stack.n);
{    stack.dirs[stack.n].name := name;}
		stack.dirs[stack.n].sz := 0;
	end;

	procedure Popd (var stack: DirStack);
	begin
		if enableDebugging then
			writeln('popping from ', stack.n);
		stack.n := stack.n - 1;
	end;

{ TODO fix }
	procedure ReportSmall (var stack: DirStack);
		var
			i: integer;
	begin
		writeln('Found small dir: ', stack.dirs[stack.n].sz);
		for i := 1 to stack.n do
			begin
{write(stack.dirs[i].name);}
{write('/');}
			end;
{writeln;}
	end;

	function CountSmallDirs (var f: Text): LongInt;
		var
			stack: DirStack;
			line, arg: string;
			nSmall: integer;
			fileSz, result: LongInt;

		procedure PopAndCheck;
			var
				curSz: LongInt;
		begin
			curSz := stack.dirs[stack.n].sz;
			writeln('Directory at depth ', stack.n, ' has total size ', curSz : 1);
			if curSz <= maxSmallDirSize then
				begin
					nSmall := nSmall + 1;
					result := result + curSz;
					writeln('Found small dir:  ', stack.n : 1, ' of size ', curSz : 1, '. Total now ', result : 1);
				end;
			stack.dirs[stack.n - 1].sz := stack.dirs[stack.n - 1].sz + curSz;
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
{ TODO why doesn't this work? }
{arg := copy(line, 5, length(line) - 5);}
						writeln('pushing due to ', line);
{writeln('push to ', line, ' from ', line);}
						Pushd(stack);
					end
				else if (Pos('dir ', line) <> 1) and (line <> '$ ls') then
					begin
						ReadString(line, fileSz);
						writeln('Adding ', fileSz : 1, ' to dir ');
						stack.dirs[stack.n].sz := stack.dirs[stack.n].sz + fileSz;
					end;
			end;

		while stack.n > 1 do
			PopAndCheck;

		writeln('about to return ', result : 1);
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
			writeln('Found ', result : 1, ' small dirs');
		end
	else
		writeln('Did not open input file');
end.