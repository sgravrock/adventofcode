program day2p2;

	const
		rockScore = 1;
		paperScore = 2;
		scissorsScore = 3;
		rock = 'A';
		paper = 'B';
		scissors = 'C';
		lose = 'X';
		draw = 'Y';
		win = 'Z';


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

	function ScoreForMove (opponentMove, desiredOutcome: char): integer;
		var
			outcomeScore: integer;
			shapeScore: integer;
	begin
		case (desiredOutcome) of
			lose: 
				begin
					outcomeScore := 0;
					case opponentMove of
						rock: 
							shapeScore := scissorsScore;
						paper: 
							shapeScore := rockScore;
						scissors: 
							shapeScore := paperScore;
					end;
				end;
			draw: 
				begin
					outcomeScore := 3;
					case opponentMove of
						rock: 
							shapeScore := rockScore;
						paper: 
							shapeScore := paperScore;
						scissors: 
							shapeScore := scissorsScore;
					end;
				end;
			win: 
				begin
					outcomeScore := 6;
					case opponentMove of
						rock: 
							shapeScore := paperScore;
						paper: 
							shapeScore := scissorsScore;
						scissors: 
							shapeScore := rockScore;
					end;
				end;
		end;
		ScoreForMove := shapeScore + outcomeScore;
	end;

	function TotalScore (var f: Text): integer;
		var
			opponentMove, desiredOutcome, space: char;
			score: integer;
	begin
		score := 0;
		while not eof(f) do
			begin
				read(f, opponentMove);
				read(f, space);
				read(f, desiredOutcome);
				readln(f);
				score := score + ScoreForMove(opponentMove, desiredOutcome);
			end;
		TotalScore := score;
	end;


	var
		inputFile: Text;
		result: integer;

begin
	if OpenInputFile(inputFile) then
		begin
			result := TotalScore(inputFile);
			writeln('Result: ', result);
		end
	else
		writeln('Did not open input file');
end.