unit Graphics;

interface

	uses
		CaveInterface;

	procedure SetUpDrawingWindow;
	procedure DrawCaveFloor;
	procedure DrawLedge (startX, endX, y: integer);
	procedure DrawWall (x, startY, endY: integer);
	procedure DrawSand (cx, cy: integer);

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

	procedure DrawCaveFloor;
		var
			r: Rect;
	begin
		SetRect(r, 0, caveMaxY, winRight - winLeft, drawingWinHeight);
		PaintRect(r);
	end;

	procedure DrawLedge (startX, endX, y: integer);
		var
			x0: integer;
	begin
		x0 := winWidth div 2; { Center horizontally }
		MoveTo(startX - x0, y);
		LineTo(endX - x0, y);
	end;

	procedure DrawWall (x, startY, endY: integer);
	begin
		x := x - winWidth div 2; { Center horizontally }
		MoveTo(x, startY);
		LineTo(x, endY);
	end;


	procedure DrawSand (cx, cy: integer);
		var
			x0: integer;
			pxRect: Rect;
	begin
		ShowDrawing;
		x0 := winWidth div 2; { Center horizontally }
		SetRect(pxRect, cx - x0, cy, cx - x0 + 1, cy + 1);
		PaintRect(pxRect);
	end;

end.