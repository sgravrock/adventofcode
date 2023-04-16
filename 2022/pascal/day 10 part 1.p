program day10p1;

	uses
		FileUtils;

	const
		enableDebugLogging = false;

	type
		CPU = record
				clock: integer;
				x: integer;
				tss: LongInt;
			end;

	procedure Tick (var machine: CPU);
		var
			ss: LongInt;
	begin
		machine.clock := machine.clock + 1;
		if (machine.clock <> 0) and ((machine.clock - 20) mod 40 = 0) then
			begin
				ss := machine.x * machine.clock;
				if enableDebugLogging then
					write('at cycle ', machine.clock : 1, ': x=', machine.x : 1, ' ss= ', ss : 1, ', tss ', machine.tss : 1, ' -> ');
				machine.tss := machine.tss + machine.x * machine.clock;
				if enableDebugLogging then
					writeln(machine.tss);
			end;
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
		machine.clock := 0;
		machine.x := 1;
		machine.tss := 0;

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

		writeln(machine.tss);
	end;

	var
		inputFile: Text;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			RunProgram(inputFile);
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');

end.