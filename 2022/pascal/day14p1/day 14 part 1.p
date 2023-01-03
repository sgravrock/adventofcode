program day14p1;

{ x: 449..505, y: 13..173 }

	uses
		FileUtils;

	var
		inputFile: Text;
		minX, minY, maxX, maxY: integer;
		n: integer;
		c: char;

begin
	ShowText;

	if OpenInputFile(inputFile) then
		begin
			minX := maxint;
			minY := maxint;
			maxX := -maxint - 1;
			maxY := -maxint - 1;

			while not eof(inputFile) do
				begin
					while not eoln(inputFile) do
						begin
							read(inputFile, n);
							if n < minX then
								minX := n;
							if n > maxX then
								maxX := n;
							read(inputFile, c); { consume ',' }
							read(inputFile, n);
							if n < minY then
								minY := n;
							if n > maxY then
								maxY := n;

							if not eoln(inputFile) then
								begin
				{ Consume ' -> ' }
									read(inputFile, c);
									read(inputFile, c);
									read(inputFile, c);
									read(inputFile, c);
								end;
						end;

					readln(inputFile);
				end;

			writeln('x: ', minX : 1, '..', maxX : 1, ' y: ', minY : 1, '..', maxY : 1);
		end
	else
		writeln('Did not open input file');
end.