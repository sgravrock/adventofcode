program day11p1;

	uses
		FileUtils, BigArithmetic, Tests;

	const
		enableDebugLogging = false;

	type
		Operation = (mul, add);
		Monkey = record
				items: array[1..50] of BigInt;
				numItems: integer;
				op: Operation;
				operand: byte;
				operandIsOldValue: boolean;
				test: LongInt;
				ifFalse: LongInt;
				ifTrue: LongInt;
				activity: LongInt;
			end;
		MonkeyArr = record
				monkeys: array[0..10] of Monkey;
				n: integer;
			end;

	procedure Skip (var f: Text; toSkip: string);
		var
			i, len: integer;
			c: char;
	begin
		len := length(toSkip);
		for i := 1 to len do
			read(f, c);
	end;

	procedure ReadMonkey (var f: Text; var m: Monkey);
		var
			n: integer;
			c: char;
			s: string;
	begin
		readln(f); { discard 'Monkey n:' }
		Skip(f, '  Starting items: ');
		m.numItems := 0;
		m.activity := 0;
		while not eoln(f) do
			begin
				m.numItems := m.numItems + 1;
				read(f, n);
				BigFromByte(n, m.items[m.numItems]);
				if not eoln(f) then
					Skip(f, ', ');
			end;
		readln(f);

		Skip(f, '  Operation: new = old ');
		read(f, c);
		case c of
			'*': 
				m.op := mul;
			'+': 
				m.op := add;
		end;
		Skip(f, ' ');
		readln(f, s);
		m.operandIsOldValue := s = 'old';
		if not m.operandIsOldValue then
			ReadString(s, m.operand);

		Skip(f, '  Test: divisible by ');
		readln(f, m.test);

		Skip(f, '    If true: throw to monkey ');
		readln(f, m.ifTrue);

		Skip(f, '    If false: throw to monkey ');
		readln(f, m.ifFalse);
	end;

	procedure ReadMonkeys (var f: Text; var monkeys: MonkeyArr);
	begin
		monkeys.n := 0;
		while not eof(f) do
			begin
				ReadMonkey(f, monkeys.monkeys[monkeys.n]);
				monkeys.n := monkeys.n + 1;

				if not eof(f) then
					readln(f);
			end;

		if enableDebugLogging then
			writeln('Read', monkeys.n, ' monkeys');
	end;


	procedure Inspect (var m: Monkey; itemIx: integer);
		var
			operand: BigInt;
			r: BigInt;
	begin
		if m.operandIsOldValue then
			operand := m.items[itemIx]
		else
			begin
				BigZero(operand);
				operand[1] := m.operand;
			end;

		case m.op of
			mul: 
				BigMul(m.items[itemIx], operand, r);
			add: 
				BigAdd(m.items[itemIx], operand, r);
		end;

		m.items[itemIx] := r;
		m.activity := m.activity + 1;
	end;

	procedure Throw (var monkeys: MonkeyArr; monkeyIx, itemIx: integer);
		var
			src, dest: ^Monkey;
	begin
		src := @monkeys.monkeys[monkeyIx];

		if BigMod(src^.items[itemIx], src^.test) = 0 then
			dest := @monkeys.monkeys[src^.ifTrue]
		else
			dest := @monkeys.monkeys[src^.ifFalse];

		dest^.numItems := dest^.numItems + 1;
		dest^.items[dest^.numItems] := src^.items[itemIx];
	end;


{    procedure DumpRound (round: integer; var monkeys: MonkeyArr); }
{    var}
{    i, j: integer; }
{    begin}
{    writeln('After round ', round : 1, ', the monkeys are holding items with these worry levels:');}
{    for i := 0 to monkeys.n - 1 do }
{    begin }
{    write('Monkey ', i : 1, ' : ');  }
{    for j := 1 to monkeys.monkeys[i].numItems do }
{    write(monkeys.monkeys[i].items[j] : 1, ', '); }
{    writeln; }
{    end; }
{    end; }


	function MonkeyBusiness (var monkeys: MonkeyArr): LongInt;
		var
			round, i, j: integer;
			mostActive, nextMostActive, maxWorry: LongInt;
			divResult: BigDivResult;
	begin
		maxWorry := 1;
		for i := 0 to monkeys.n - 1 do
			if maxWorry mod monkeys.monkeys[i].test <> 0 then
				maxWorry := maxWorry * monkeys.monkeys[i].test;

		for round := 1 to 20 do
			begin
				for i := 0 to monkeys.n - 1 do
					begin
						for j := 1 to monkeys.monkeys[i].numItems do
							begin
								Inspect(monkeys.monkeys[i], j);
								BigDiv(monkeys.monkeys[i].items[j], 3, divResult);
								monkeys.monkeys[i].items[j] := divResult.quotient;
								Throw(monkeys, i, j);
							end;
						monkeys.monkeys[i].numItems := 0;
					end;

{    if enableDebugLogging then}
{    DumpRound(round, monkeys);}
			end;

		mostActive := 0;
		nextMostActive := 0;

		for i := 0 to monkeys.n - 1 do
			begin
				if enableDebugLogging then
					writeln('Monkey ', i : 1, ' inspected items ', monkeys.monkeys[i].activity : 1, ' times');
				if monkeys.monkeys[i].activity > mostActive then
					begin
						nextMostActive := mostActive;
						mostActive := monkeys.monkeys[i].activity;
					end
				else if monkeys.monkeys[i].activity > nextMostActive then
					nextMostActive := monkeys.monkeys[i].activity;
			end;

		MonkeyBusiness := mostActive * nextMostActive;
	end;

	var
		inputFile: Text;
		monkeys: MonkeyArr;
		result: LongInt;

begin
	ShowText;
	RunTests;

	if OpenInputFile(inputFile) then
		begin
			ReadMonkeys(inputFile, monkeys);
			result := MonkeyBusiness(monkeys);
			writeln('Monkey business: ', result);
			writeln('Press Return to exit');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.