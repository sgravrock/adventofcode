unit UI;

interface

	uses
		CaveInterface;

	var
		gShouldQuit: boolean;

	procedure SetUpDrawingWindow;
	procedure DrawCaveFloor (caveFloorY: integer);
	procedure DrawLedge (startX, endX, y: integer);
	procedure DrawWall (x, startY, endY: integer);
	procedure DrawSand (cx, cy: integer);
	procedure ShowStatus (status: string);
	procedure ShowError (msg: string);
	procedure HandleOneEvent (maxSleepTicks: integer);
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

	procedure HandleOneEvent (maxSleepTicks: integer);
		var
			myEvent: EventRecord;
			keyChar: char;
	begin
		SystemTask;

{ WaitNextEvent is documented in Macintosh Toolbox Essentials (Apple). }
{ It does not seem to be mentioned in any volume of the original Inside }
{ Macintosh series. It appears to have been introduced with MultiFinder, }
{ so using it probably means this won't run on anything before System }
{ Software 5 (System 4.2,  Finder 4.2, MultiFinder 1.0). But that's just }
{ an educated guess. Macintosh Toolbox Essentials says that to run in }
{ System 6 with MultiFinder disabled we need to check whether }
{ WaitNextEvent is available and fall back to SystemTask+EventAvail+ }
{ GetNextEvent, but WaitNextEvent actually works even when MultiFinder }
{ is disabled. }
{ Code to detect WaitNextEvent and fall back to older APIs can be found }
{ in both Macintosh Toolbox Essentials and Macintosh Pascal Programming }
{ Primer Vol. 1, with the latter taking a simpler approach. }
		if WaitNextEvent(everyEvent, myEvent, maxSleepTicks, nil) then
			case myEvent.what of
				keyDown: 
					begin
						keyChar := CHR(BitAnd(myEvent.message, charCodeMask));
						if (keyChar = 'q') and (BitAnd(myEvent.modifiers, cmdKey) <> 0) then
							gShouldQuit := true;
					end;
				otherwise
					begin
{ TODO: window drag, window resize, window close box click, update, maybe more }
					end;
			end;
	end;

	procedure PostSimulationEventLoop;
		var
			myEvent: EventRecord;
			keyChar: char;
	begin
		while not gShouldQuit do
{ We've nothing else to do so be maximally nice to background processes }
			HandleOneEvent(maxInt);
	end;

end.