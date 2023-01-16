unit CaveInterface;

interface

	const
{ Min and max values based on puzzle input }
		caveMinX = 326;
		caveMaxX = 674;
		caveMinY = 0;
		caveMaxY = 175;

	type
		CaveType = record
				cells: packed array[caveMinY..caveMaxY, caveMinX..caveMaxX] of boolean;
				floorY: integer;
			end;

implementation

end.