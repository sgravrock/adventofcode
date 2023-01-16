unit UI;

interface

	uses
		CaveInterface;

	procedure SetUpDrawingWindow;
	procedure DrawCaveFloor (caveFloorY: integer);
	procedure DrawLedge (startX, endX, y: integer);
	procedure DrawWall (x, startY, endY: integer);
	procedure DrawSand (cx, cy: integer);
	procedure ShowStatus (status: string);
	procedure ShowError (msg: string);
	procedure PostSimulationEventLoop;

implementation

	const
{ Just fits on a compact Mac screen, will work on any machine }
		winWidth = 506;
		winHeight = 298;
		winLeft = 3;
		winRight = 509;
		winTop = 41;
		topPadding = 12;
		statusLineLeftPadding = 12;
		statusLineHeight = 20;
		caveTopOffset = 32; { topPadding + statusLineHeight }
		alertId = 128;

	procedure SetRectToDrawingRect (var dest: Rect);
	begin
		SetRect(dest, winLeft, winTop, winWidth + winLeft, winHeight + winTop);
	end;

	procedure SetUpDrawingWindow;
		var
			r: Rect;
	begin
		SetRectToDrawingRect(r);
		SetDrawingRect(r);
		ShowDrawing;
	end;

	procedure DrawCaveFloor (caveFloorY: integer);
		var
			r: Rect;
	begin
		SetRect(r, 0, caveFloorY + caveTopOffset, winRight - winLeft, winHeight);
		PaintRect(r);
	end;

	procedure DrawLedge (startX, endX, y: integer);
		var
			x0: integer;
	begin
		x0 := winWidth div 2; { Center horizontally }
		MoveTo(startX - x0, y + caveTopOffset);
		LineTo(endX - x0, y + caveTopOffset);
	end;

	procedure DrawWall (x, startY, endY: integer);
	begin
		x := x - winWidth div 2; { Center horizontally }
		MoveTo(x, startY + caveTopOffset);
		LineTo(x, endY + caveTopOffset);
	end;


	procedure DrawSand (cx, cy: integer);
		var
			x0: integer;
			pxRect: Rect;
	begin
		x0 := winWidth div 2; { Center horizontally }
		SetRect(pxRect, cx - x0, cy + caveTopOffset, cx - x0 + 1, cy + 1 + caveTopOffset);
		PaintRect(pxRect);
	end;

	procedure ShowStatus (status: string);
		var
			r: Rect;
	begin
		SetRect(r, 0, 0, winWidth, topPadding + statusLineHeight);
		EraseRect(r);
		MoveTo(statusLineLeftPadding, topPadding);
		DrawString(status);
	end;

	procedure ShowError (msg: string);
		var
			ignored: integer;
	begin
		ParamText(msg, '', '', '');
		ignored := StopAlert(alertId, nil);
	end;


	procedure PostSimulationEventLoop;
		var
			done: boolean;
			myEvent: EventRecord;
			keyChar: char;
	begin
		done := false;

		while not done do
			begin
				SystemTask;

				if GetNextEvent(everyEvent, myEvent) then
					case myEvent.what of
						keyDown: 
							begin
								keyChar := CHR(BitAnd(myEvent.message, charCodeMask));
								if (keyChar = 'q') and (BitAnd(myEvent.modifiers, cmdKey) <> 0) then
									done := true;
							end;
						otherwise
							begin
{ TODO: window drag, window resize, window close box click, update, maybe more }
							end;
					end;
			end;
	end;

end.