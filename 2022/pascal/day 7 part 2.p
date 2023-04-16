program day7p2;

	uses
		FileUtils;

	type
		LongPtr = ^LongInt;
		DirStack = object
				vals: array[1..10] of LongInt;
				n: LongInt;

				procedure Init;
				procedure Push;
				procedure Pop;
				function Top: LongPtr;
				function Parent: LongPtr;
			end;

		DirSizes = record
				sizes: array[1..255] of LongInt;
				n: integer;
			end;

	const
		capacity = 70000000;
		spaceNeeded = 30000000;


	procedure DirStack.Init;
	begin
		self.n := 0;
	end;

	procedure DirStack.Push;
	begin
		self.n := self.n + 1;
		self.vals[self.n] := 0;
	end;

	procedure DirStack.Pop;
	begin
		self.n := self.n - 1;
	end;

	function DirStack.Top: LongPtr;
	begin
		Top := @self.vals[self.n];
	end;

	function DirStack.Parent: LongPtr;
	begin
		if self.n > 1 then
			Parent := @self.vals[self.n - 1]
		else
			Parent := nil;
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
			stack: DirStack;
			line: string;
			fileSz, curDirSz: LongInt;

		procedure Pushd;
		begin
			stack.Push;
		end;

		procedure Popd;
		begin
			PushSz(sizes, stack.Top^);
			stack.Parent^ := stack.Parent^ + stack.Top^;
			stack.Pop;
		end;

	begin
		new(DirStack(stack));
		sizes.n := 0;
		Pushd;
		readln(f, line); { discard leading '$ cd /' }
		while not eof(f) do
			begin
				readln(f, line);
				if line = '$ cd ..' then
					Popd
				else if Pos('$ cd ', line) = 1 then
					begin
						Pushd;
					end
				else if (Pos('dir ', line) <> 1) and (line <> '$ ls') then
					begin
						ReadString(line, fileSz);
						stack.Top^ := stack.Top^ + fileSz;
					end;
			end;

		while stack.n > 0 do
			Popd;
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
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.