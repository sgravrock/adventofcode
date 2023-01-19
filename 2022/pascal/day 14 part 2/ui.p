unit UI;

interface

	uses
		CaveInterface;

	var
		gShouldQuit: boolean;

	procedure CreateWindow;
	procedure DrawCaveFloor (caveFloorY: integer);
	procedure DrawLedge (startX, endX, y: integer);
	procedure DrawWall (x, startY, endY: integer);
	procedure DrawSand (cx, cy: integer);
	procedure ShowStatus (status: string);
	procedure ShowError (msg: string);
	procedure HandleOneEvent (maxSleepTicks: integer);
	procedure PostSimulationEventLoop;

implementation

	var
		winWidth, winHeight: integer;

	const
		topPadding = 12;
		statusLineLeftPadding = 12;
		statusLineHeight = 20;
		caveTopOffset = 32; { topPadding + statusLineHeight }
		alertResId = 128;
		windowResId = 400;

	var
		window: WindowPtr;

	procedure CreateWindow;
	begin
		window := GetNewWindow(windowResId, nil, WindowPtr(-1));
		ShowWindow(window);
		SetPort(window);
		winWidth := window^.portRect.right - window^.portRect.left;
		winHeight := window^.portRect.bottom - window^.portRect.top;
	end;

	procedure DrawCaveFloor (caveFloorY: integer);
		var
			r: Rect;
	begin
		SetRect(r, 0, caveFloorY + caveTopOffset, winWidth, winHeight);
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
		ignored := StopAlert(alertResId, nil);
	end;


	procedure DoMouseDown (event: EventRecord);
		var
			part: integer;
			thisWindow: WindowPtr;
	begin
		part := FindWindow(event.where, thisWindow);
		case part of
			inGoAway: { handle mouse down in close box }
				if TrackGoAway(thisWindow, event.where) then
					gShouldQuit := true;
			otherwise
				begin
{ do nothing }
				end;
		end;
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
				mouseDown: 
					DoMouseDown(myEvent);
				otherwise
					begin
{ TODO: window drag, window resize, update, maybe more }
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
