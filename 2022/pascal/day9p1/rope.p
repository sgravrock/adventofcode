unit rope;

interface
	type
		Coordinate = record
				x, y: integer;
			end;
		Rope = record
				h, t: Coordinate;
			end;

	procedure MoveRope (var theRope: Rope; dir: char; dist: integer; procedure cb);

implementation

	procedure MoveHead (var theRope: Rope; dir: char);
	begin
		case dir of
			'U': 
				theRope.h.y := theRope.h.y - 1;
			'D': 
				theRope.h.y := theRope.h.y + 1;
			'L': 
				theRope.h.x := theRope.h.x - 1;
			'R': 
				theRope.h.x := theRope.h.x + 1;
		end;
	end;


	procedure MoveTail (var theRope: Rope);
		var
			dx, dy: integer;
			absDx, absDy: integer;
			needsX, needsY: boolean;
	begin
		dx := theRope.h.x - theRope.t.x;
		dy := theRope.h.y - theRope.t.y;
		absDx := abs(dx);
		absDy := abs(dy);

		if (absDx <= 1) and (absDy <= 1) then
			exit(MoveTail); { In contact horizontally, vertically, or diagonally }

		needsX := (absDx > 1) or ((absDx > 0) and (absDx + absDy > 1));
		needsY := (absDy > 1) or ((absDy > 0) and (absDx + absDy > 1));

		if needsX then
			if dx > 0 then
				theRope.t.x := theRope.t.x + 1
			else
				theRope.t.x := theRope.t.x - 1;

		if needsY then
			if dy > 0 then
				theRope.t.y := theRope.t.y + 1
			else
				theRope.t.y := theRope.t.y - 1;
	end;


	procedure MoveRope (var theRope: Rope; dir: char; dist: integer; procedure cb);
		var
			i: integer;
	begin
		for i := 1 to dist do
			begin
				MoveHead(theRope, dir);
				MoveTail(theRope);
				cb;
			end;
	end;

end.