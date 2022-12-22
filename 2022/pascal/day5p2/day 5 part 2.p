program day4p2;

	type
		CrateStack = record
				crates: array[1..255] of char;
				sz: integer;
			end;
		StackList = record
				stacks: array[1..9] of CrateStack;
				sz: integer;
			end;

	const
		enableDebugging = false; { enables interactive visual debugging }

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

	function IsDigit (c: char): boolean;
		var
			n: integer;
	begin
		n := ord(c);
		IsDigit := (n >= 48) and (n <= 57);
	end;

	procedure Push (var stack: CrateStack; crate: char);
	begin
		stack.sz := stack.sz + 1;
		stack.crates[stack.sz] := crate;
	end;

	function Pop (var stack: CrateStack): char;
	begin
		Pop := stack.crates[stack.sz];
		stack.sz := stack.sz - 1;
	end;

	procedure Reverse (var stack: CrateStack);
		var
			i: integer;
			tmp: char;
	begin
		for i := 1 to stack.sz div 2 do
			begin
				tmp := stack.crates[i];
				stack.crates[i] := stack.crates[stack.sz - i + 1];
				stack.crates[stack.sz - i + 1] := tmp;
			end;
	end;

	function Top (var stack: CrateStack): char;
	begin
		Top := stack.crates[stack.sz];
	end;

	procedure PrintStacks (var stacks: StackList);
		var
			i, j, maxHeight: integer;
	begin
		maxHeight := 0;
		for i := 1 to stacks.sz do
			if stacks.stacks[i].sz > maxHeight then
				maxHeight := stacks.stacks[i].sz;

{ Formatting assumes <= 9 stacks }
		for j := maxHeight downto 1 do
			begin
				for i := 1 to stacks.sz do
					if j > stacks.stacks[i].sz then
						write('    ')
					else
						begin
							write('[');
							write(stacks.stacks[i].crates[j]);
							write('] ');
						end;
				writeln;
			end;

		for i := 1 to stacks.sz do
			begin
				write(' ');
				write(i : 1);
				write('  ');
			end;
		writeln;
	end;

	procedure ReadStacks (var f: text; var stacks: StackList);
		var
			line: string;
			done: boolean;
			i: integer;
			c: char;
	begin
		stacks.sz := 0;
		while not done do
			begin
				readln(f, line);
				if IsDigit(line[2]) then { e.g. ' 1 2 3 4 ' }
					done := true
				else
					begin
{ Input is e.g. '[X] [Y] [Z]', but crates may be missing e.g. '[X]     [Z]' }
						if stacks.sz = 0 then
							begin
								stacks.sz := (length(line) + 1) div 4;
								for i := 1 to stacks.sz do
									stacks.stacks[i].sz := 0;
							end;

						for i := 1 to stacks.sz do
							begin
								c := line[(i - 1) * 4 + 2];
								if c <> ' ' then
									Push(stacks.stacks[i], c);
							end;
					end;
			end;

		for i := 1 to stacks.sz do
			Reverse(stacks.stacks[i]);
	end;

	procedure ConsumeChars (var f: Text; n: integer);
		var
			i: integer;
			c: char;
	begin
		for i := 1 to n do
			read(f, c);
	end;

	procedure NextMove (var f: Text; var stacks: StackList);
		var
			numCrates, srcIx, destIx, i: integer;
	begin
		consumeChars(f, length('move '));
		read(f, numCrates);
		consumeChars(f, length(' from '));
		read(f, srcIx);
		consumeChars(f, length(' to '));
		read(f, destIx);
		readln(f);

		if enableDebugging then
			writeln('Moving ', numCrates, ' crates from stack ', srcIx, ' to ', destIx);

		for i := 1 to numCrates do
			Push(stacks.stacks[destIx], Pop(stacks.stacks[srcIx]));

		if enableDebugging then
			begin
				PrintStacks(stacks);
				writeln('Press enter to continue');
				readln;
			end;
	end;

	procedure PrintTopCrates (var stacks: StackList);
		var
			i: integer;
	begin
		for i := 1 to stacks.sz do
			write(Top(stacks.stacks[i]));
		writeln;
	end;

	procedure FindTopCrates (var f: Text);
		var
			stacks: StackList;
	begin
		ReadStacks(f, stacks);
		readln(f); { consume blank separator line }

		if enableDebugging then
			begin
				writeln('Initial state:');
				PrintStacks(stacks);
			end;

		while not eof(f) do
			NextMove(f, stacks);

		PrintTopCrates(stacks);
	end;


	var
		inputFile: Text;
		result: string;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			FindTopCrates(inputFile);
		end
	else
		writeln('Did not open input file');
end.