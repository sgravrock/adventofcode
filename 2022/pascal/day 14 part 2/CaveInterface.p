unit CaveInterface;

interface

	const
{ Min and max values based on puzzle input }
		caveMinX = 326;
		caveMaxX = 674;
		caveMinY = 0;
		caveMaxY = 175; { 175 for puzzle input, 11 for sample }
 { TODO don't hardcode this }

	type
		CaveType = packed array[caveMinY..caveMaxY, caveMinX..caveMaxX] of boolean;

	function IsCellOccupied (var cave: CaveType; x, y: integer): boolean;

implementation

	function IsCellOccupied (var cave: CaveType; x, y: integer): boolean;
	begin
		if y = caveMaxY then
			IsCellOccupied := true
		else
			IsCellOccupied := cave[y][x];
	end;

end.