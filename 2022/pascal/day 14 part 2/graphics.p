unit Graphics;

interface

	uses
		CaveInterface;

	procedure SetUpDrawingWindow;
	procedure DrawCave (cave: CaveType);

implementation

	const
{ Just fits on a compact Mac screen, will work on any machine }
		winWidth = 506;
		drawingWinHeight = 208;
		textWinHeight = 67;
		winLeft = 3;
		winRight = 509;
		drawingWinTop = 41;
		textWinTop = 272;

	procedure SetRectToDrawingRect (var dest: Rect);
	begin
		SetRect(dest, winLeft, drawingWinTop, winWidth + winLeft, drawingWinHeight + drawingWinTop);
	end;

	procedure SetUpDrawingWindow;
		var
			r: Rect;
	begin
		SetRect(r, winLeft, textWinTop, winRight, textWinHeight + textWinTop);
		SetTextRect(r);
		ShowText;
		SetRectToDrawingRect(r);
		SetDrawingRect(r);
		ShowDrawing;
	end;


	procedure EraseEverything;
		var
			r: Rect;
	begin
		SetRectToDrawingRect(r);
		EraseRect(r);
	end;

	procedure DrawCave (cave: CaveType);
		var
			x0, y0, cx, cy: integer;
			pxRect: Rect;
	begin
		x0 := winWidth div 2; { Center horizontally }
		y0 := 12; { Some padding }

		writeln('starting to draw');
		ShowDrawing;
		EraseEverything;

		for cx := caveMinX to caveMaxX do
			for cy := caveMinY to caveMaxY - 1 do
				begin
					if cave[cy][cx] then
						begin
							SetRect(pxRect, cx - x0, cy, cx - x0 + 1, cy + 1);
							PaintRect(pxRect);
						end;
				end;

		SetRect(pxRect, 0, caveMaxY, winRight - winLeft, drawingWinHeight);
		PaintRect(pxRect);
		writeln('done drawing'); { 12s }
	end;

end.