program day7p2;

	type
		DirStack = record
				dirs: array[1..10] of LongInt;
				n: integer;
			end;
		DirSizes = record
				sizes: array[1..255] of Longint;
				n: integer;
			end;

	const
		capacity = 70000000;
		spaceNeeded = 30000000;

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
		stack.dirs[stack.n] := 0;
	end;

	procedure Popd (var stack: DirStack);
	begin
		stack.n := stack.n - 1;
	end;

	procedure PushSz (var sizes: DirSizes; newVal: LongInt);
	begin
		sizes.n := sizes.n + 1;
		sizes.sizes[sizes.n] := newVal;
	end;

	function SmallestAtLeast (var sizes: DirSizes; min: LongInt): LongInt;
		var
			i: integer;
			candidate: LongInt;
	begin
		candidate := maxLongInt;
		for i := 1 to sizes.n do
			if (sizes.sizes[i] < candidate) and (sizes.sizes[i] >= min) then
				candidate := sizes.sizes[i];
		SmallestAtLeast := candidate;
	end;

	function Sum (var sizes: DirSizes): LongInt;
		var
			i: integer;
			result: LongInt;
	begin
		result := 0;
		for i := 1 to sizes.n do
			result := sizes.sizes[i];
		Sum := result;
	end;

	procedure FindDirSizes (var f: Text; var sizes: DirSizes);
		var
{ TODO maybe a pointer to the top dir in the stack would help? }
			stack: DirStack;
			line: string;
			fileSz, curDirSz: LongInt;

		procedure RecordAndPop;
		begin
			PushSz(sizes, stack.dirs[stack.n]);
			stack.dirs[stack.n - 1] := stack.dirs[stack.n - 1] + stack.dirs[stack.n];
			Popd(stack);
		end;

	begin
		stack.n := 0;
		sizes.n := 0;
		Pushd(stack);
		readln(f, line); { discard leading '$ cd /' }
		while not eof(f) do
			begin
				readln(f, line);
				if line = '$ cd ..' then
					RecordAndPop
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

		while stack.n > 0 do
			RecordAndPop;
	end;

	function SizeOfDirToRemove (var f: Text): LongInt;
		var
			sizes: DirSizes;
			totalUsed, needed: LongInt;
	begin
		FindDirSizes(f, sizes);
		totalUsed := Sum(sizes);
		writeln('total used: ', totalUsed);
		needed := spaceNeeded - (capacity - totalUsed);
		writeln('needed: ', needed);
		SizeOfDirToRemove := SmallestAtLeast(sizes, needed);
	end;

	var
		inputFile: Text;
		result: LongInt;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			result := SizeOfDirToRemove(inputFile);
			writeln('Result: ', result : 1);
		end
	else
		writeln('Did not open input file');
end.