program day3p1;

	type
		WholeLine = string[50];
		HalfLine = string[25];
		Rucksack = record
				binA: HalfLine;
				binB: HalfLine;
			end;

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

	procedure ReadRucksack (var f: Text; var result: Rucksack);
		var
			line: WholeLine;
			i, n, half: integer;
	begin
		readln(f, line);
		n := length(line);
		half := n div 2;
		result.binA := copy(line, 1, half);
		result.binB := copy(line, half + 1, half);
	end;

	function FindDuplicate (a, b: HalfLine): char;
		var
			i, j, lenA, lenB: integer;
	begin
		lenA := length(a);
		lenB := length(b);
		for i := 1 to lenA do
			for j := 1 to lenB do
				if a[i] = b[j] then
					begin
						FindDuplicate := a[i];
						Exit(FindDuplicate);
					end;
		writeln('Fatal: did not find dupe between ', a, ' and ', b);
		Halt;
	end;

	function PriorityForItem (item: char): integer;
		var
			ascii: integer;
	begin
		ascii := ord(item);
		if ascii < 97 then { uppercase}
			PriorityForItem := ascii - 38 { 27 to 52 }
		else
			PriorityForItem := ascii - 96; { 1 to 26 }
	end;

	function TotalPriorities (var f: Text): integer;
		var
			r: Rucksack;
			dupe: char;
			total: integer;
			tmp: integer;
	begin
		total := 0;
		while not eof(f) do
			begin
				ReadRucksack(f, r);
				total := total + PriorityForItem(FindDuplicate(r.binA, r.binB));
			end;
		TotalPriorities := total;
	end;

	var
		inputFile: Text;
		result: integer;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			result := TotalPriorities(inputFile);
			writeln('Total priorities: ', result);
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.