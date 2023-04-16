program day2p1;


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

	function ScoreForMove (opponentMove, ourMove: char): integer;
		var
			outcomeScore: integer;
			shapeScore: integer;
	begin
		case (ourMove) of
			'X': { rock }
				begin
					shapeScore := 1;
					case opponentMove of
						'A': { rock }
							outcomeScore := 3;
						'B': { paper }
							outcomeScore := 0;
						'C': { scissors }
							outcomeScore := 6;
					end;
				end;
			'Y': { paper }
				begin
					shapeScore := 2;
					case opponentMove of
						'A': { rock }
							outcomeScore := 6;
						'B': { paper }
							outcomeScore := 3;
						'C': { scissors }
							outcomeScore := 0;
					end;
				end;
			'Z': { scissors }
				begin
					shapeScore := 3;
					case opponentMove of
						'A': { rock }
							outcomeScore := 0;
						'B': { paper }
							outcomeScore := 6;
						'C': { scissors }
							outcomeScore := 3;
					end;
				end;
		end;
{writeln(opponentMove, ', ', ourMove, ' -> ', shapeScore + outcomeScore);}
		ScoreForMove := shapeScore + outcomeScore;
	end;

	function TotalScore (var f: Text): integer;
		var
			opponentMove, ourMove, space: char;
			score: integer;
	begin
		score := 0;
		while not eof(f) do
			begin
				read(f, opponentMove);
				read(f, space);
				read(f, ourMove);
				readln(f);
				score := score + ScoreForMove(opponentMove, ourMove);
			end;
		TotalScore := score;
	end;


	var
		inputFile: Text;
		result: integer;

begin
	ShowText;
	if OpenInputFile(inputFile) then
		begin
			result := TotalScore(inputFile);
			writeln('Result: ', result);
			writeln('Press return');
			SysBeep(10);
			readln;
		end
	else
		writeln('Did not open input file');
end.