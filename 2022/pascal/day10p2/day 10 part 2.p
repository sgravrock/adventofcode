program day10p2;

	uses
		FileUtils;

	type
		CPU = record
				clock: integer;
				x: integer;
			end;

	procedure Tick (var machine: CPU);
		var
			pos: integer;
	begin
		pos := (machine.clock - 1) mod 40;
		machine.clock := machine.clock + 1;

		if pos = 0 then
			writeln;
		if (pos <= machine.x + 1) and (pos >= machine.x - 1) then
			write('#')
		else
			write('.');
	end;

	procedure noop (var machine: CPU);
	begin
		Tick(machine);
	end;

	procedure addx (var machine: CPU; operand: integer);
	begin
		Tick(machine);
		Tick(machine);
		machine.x := machine.x + operand;
	end;

	procedure RunProgram (var f: Text);
		var
			line, chunk: string;
			operand: integer;
			machine: CPU;
	begin
		machine.clock := 1;
		machine.x := 1;

		while not eof(f) do
			begin
				readln(f, line);
				if line = 'noop' then
					noop(machine)
				else
					begin
						ReadString(copy(line, 5, length(line) - 5 + 1), operand);
						addx(machine, operand);
					end;
			end;
	end;

	var
		inputFile: Text;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			RunProgram(inputFile);
		end
	else
		writeln('Did not open input file');

end.