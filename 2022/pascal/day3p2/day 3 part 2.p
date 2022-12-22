program day3p2;

	type
		Rucksack = string[50];
		HalfLine = string[25];
		RucksackGroup = array[1..3] of Rucksack;

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

	procedure ReadRucksackGroup (var f: Text; var result: RucksackGroup);
		var
			i: integer;
	begin
		for i := 1 to 3 do
			readln(f, result[i]);
	end;

{ Kernighan said I can't write a function that operates on strings of arbitrary length, }
{ but it's 1991 now so I can. }
	function Contains (haystack: string; needle: char): boolean;
		var
			i: integer;
	begin
		Contains := false;
		for i := 1 to length(haystack) do
			if haystack[i] = needle then
				begin
					Contains := true;
					leave;  { and Wirth said I couldn't do this }
				end;
	end;

	function FindBadge (var group: RucksackGroup): char;
		var
			i: integer;
			c: char;
	begin
		for i := 1 to length(group[1]) do
			begin
				c := group[1][i];
				if Contains(group[2], c) and Contains(group[3], c) then
					begin
						FindBadge := c;
						exit(FindBadge);
					end;
			end;
		writeln('Fatal: Could not find badge');
		halt;
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
			g: RucksackGroup;
			total: integer;
	begin
		total := 0;
		while not eof(f) do
			begin
				ReadRucksackGroup(f, g);
				total := total + PriorityForItem(FindBadge(g));
			end;
		TotalPriorities := total;
	end;

	var
		inputFile: Text;
		result: integer;

begin
	if OpenInputFile(inputFile) then
		begin
			result := TotalPriorities(inputFile);
			writeln('Total priorities: ', result);
		end
	else
		writeln('Did not open input file');
end.