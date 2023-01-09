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
			x0, y0, minX, maxX, cx, cy: integer;
			pxRect: Rect;
	begin
		x0 := winWidth div 2;
		y0 := 20; { TODO }

{ Cave may be wider than the window. Ignore everything out of bounds. }
{ (QuickDraw would clip the image for us, but we can save time by not }
{ drawing out of bounds in the first place.) }
{ TODO: draw the whole cave and support scrolling }
		minX := 0 - x0;
		if minX < caveMinX then
			minX := caveMinX;
		maxX := minX + winWidth;
		if maxX > caveMaxX then
			maxX := caveMaxX;

		ShowDrawing;
		EraseEverything;

		for cx := minX to maxX do
			for cy := caveMinY to caveMaxY do
				begin
{ TODO: if we don't draw sand until it comes to rest, we won't have to clear cells }
{ TODO: Draw to an off screen buffer, and manipulate it directly rather than via FrameRect }
					if IsCellOccupied(cave, cx, cy) then
						begin
							SetRect(pxRect, cx - x0, cy, cx - x0 + 1, cy + 1);
							FrameRect(pxRect);
						end;
				end;
	end;

end.