program day7p1;

	const
		maxSmallDirSize = 100000;

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

	procedure FindSmallDirs (var f: Text);
		var
			line: string;
			nSmall: integer;
			totalSmallSizes, ignored: LongInt;

		function Descend: LongInt;
			var
				line: string;
				fileSz, dirSz: LongInt;
				done: boolean;

			procedure CheckSize;
			begin
				write('found dir with size ', dirSz : 1);
				if dirSz > maxSmallDirSize then
					writeln(' (not small)')
				else
					begin
						writeln(' (small)');
						nSmall := nSmall + 1;
						writeln('total was ', totalSmallSizes : 1, ', adding ', dirSz);
						totalSmallSizes := totalSmallSizes + dirSz;
						writeln('total now ', totalSmallSizes : 1);
						done := true;
					end;
			end;

		begin
			dirSz := 0;
			done := false;
			while (not eof(f)) and (not done) do
				begin
					readln(f, line);
{writeln('line: ', line);}
					if line = '$ cd ..' then
						CheckSize
					else if Pos('$ cd ', line) = 1 then
						begin
							writeln('pushing: ', line);
							writeln('size before recursion: ', dirSz);
							dirSz := dirSz + Descend;
							writeln('size after recursion: ', dirSz);
						end
					else if (Pos('dir ', line) <> 1) and (line <> '$ ls') then
						begin
							ReadString(line, fileSz);
{writeln('adding ', fileSz : 1, ' to ', dirSz : 1);}
							dirSz := dirSz + fileSz;
{writeln('=> ', dirSz : 1);}
						end;
				end;

			if not done then { hit EOF rather than '$ cd ..' }
				CheckSize;
			writeln('popping');
			Descend := dirSz;
		end;

	begin
		nSmall := 0;
		totalSmallSizes := 0;
		readln(f, line); { discard leading '$ cd /' }
		while not eof(f) do
			ignored := Descend;
		writeln('Found ', nSmall : 1, ' small dirs with total size ', totalSmallSizes : 1);
	end;


	var
		inputFile: Text;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			FindSmallDirs(inputFile);
		end
	else
		writeln('Did not open input file');
end.