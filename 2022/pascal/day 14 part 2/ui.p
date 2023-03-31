unit UI;

interface

	uses
		CaveInterface;

	var
		gShouldQuit: boolean;

	procedure InitUI;
	procedure ShowWaitCursor; { Call InitCursor to undo this }
	procedure DrawCaveCeiling;
	procedure DrawCaveFloor (caveFloorY: integer);
	procedure DrawLedge (startX, endX, y: integer);
	procedure DrawWall (x, startY, endY: integer);
	procedure DrawSand (cx, cy: integer);
	procedure ShowSandCount (n: integer);
	procedure ShowStatus (status: string);
	procedure ShowError (msg: string);
	procedure HandleOneEvent (maxSleepTicks: integer);
	procedure PostSimulationEventLoop;


implementation

	const
		topPadding = 18;
		statusLineLeftPadding = 12;
		statusLineHeight = 14;
		ceilingHeight = 20;
		caveTopOffset = 52; { topPadding + statusLineHeight + ceilingHeight }
		alertResId = 128;
		windowResId = 400;

	var
		winWidth, winHeight, hCenter: integer;
		window: WindowPtr;
		sandCountStart: Point;
		showingSandCount: boolean;
		offscreenPort: GrafPtr; { Used to quickly redraw the window }

	function CreateOffscreenGrafPort: GrafPtr;
		var
			savedPort, newPort: GrafPtr;
	begin
{ Save the current GrafPort so we can restore it before returning }
		GetPort(savedPort);

{ Offscreen GrafPorts are not well documented (Apple expected you to }
{ get them from windows) but this appears to work. See Inside Macintosh }
{ Vol 1 p.163-164 for the initial state of the port. }
		newPort := GrafPtr(NewPtr(sizeof(GrafPort)));
		OpenPort(newPort);

{ OpenPort initializes portBits to an exact copy of screenBits, meaning }
{ that portBits.baseAddr is the address of the actual bit image for the }
{ screen. We need to change that before drawing. }
		with newPort^.portBits do
			baseAddr := NewPtr(rowBytes * (bounds.bottom - bounds.top));

{ It's not obvious how to correctly change the size or origin of an offscreen }
{ GrafPort, so we won't. The created port is a bit bigger than our window }
{ so we can act as if the have the same coordinate system and everything }
{ will work out ok. }

{ Zero the bitmap so we don't draw random junk onto the screen. }
{ EraseRect is *vastly* faster than a Pascal for loop. }
		EraseRect(newPort^.portRect);

{ Copy font settings from the window }
		TextFont(savedPort^.txFont);
		TextFace(savedPort^.txFace);
		TextMode(savedPort^.txMode);
		TextSize(savedPort^.txSize);

{ Restore the original port and return the new one. }
		SetPort(savedPort);
		CreateOffscreenGrafPort := newPort;
	end;


	procedure InitUI;
	begin
		window := GetNewWindow(windowResId, nil, WindowPtr(-1));
		if window = nil then
			halt;
		ShowWindow(window);
		SetPort(window);
		winWidth := window^.portRect.right - window^.portRect.left;
		winHeight := window^.portRect.bottom - window^.portRect.top;
		hCenter := winWidth div 2;
		showingSandCount := false;
		offscreenPort := CreateOffscreenGrafPort;
		if offscreenPort = nil then
			halt;
	end;


	procedure ShowWaitCursor;
		var
			theCursor: CursHandle;
	begin
		theCursor := GetCursor(4);
		SetCursor(theCursor^^);
	end;


	procedure DrawCaveCeiling;
		var
			r: Rect;
			origin, ceilTop, ceilBottom: integer;
	begin
		origin := 500 - hCenter;
		ceilTop := caveTopOffset - ceilingHeight;
		ceilBottom := caveTopOffset;

		SetPort(offscreenPort);
		SetRect(r, 0, ceilTop, origin, ceilBottom);
		PaintRect(r);
		SetRect(r, origin + 1, ceilTop, winWidth, ceilBottom);
		PaintRect(r);
		SetPort(window);

		SetRect(r, 0, ceilTop, winWidth, ceilBottom);
		CopyBits(offscreenPort^.portBits, window^.portBits, r, r, srcCopy, nil);
	end;


	procedure DrawCaveFloor (caveFloorY: integer);
		var
			r: Rect;
	begin
		SetRect(r, 0, caveFloorY + caveTopOffset, winWidth, winHeight);
		SetPort(offscreenPort);
		PaintRect(r);
		SetPort(window);
		PaintRect(r);
	end;


	procedure DrawLedge (startX, endX, y: integer);
	begin
		SetPort(offscreenPort);
		MoveTo(startX - hCenter, y + caveTopOffset);
		LineTo(endX - hCenter, y + caveTopOffset);
		SetPort(window);
		MoveTo(startX - hCenter, y + caveTopOffset);
		LineTo(endX - hCenter, y + caveTopOffset);
	end;


	procedure DrawWall (x, startY, endY: integer);
	begin
		SetPort(offscreenPort);
		MoveTo(x - hCenter, startY + caveTopOffset);
		LineTo(x - hCenter, endY + caveTopOffset);
		SetPort(window);
		MoveTo(x - hCenter, startY + caveTopOffset);
		LineTo(x - hCenter, endY + caveTopOffset);
	end;


	procedure DrawSand (cx, cy: integer);
		var
			pxRect: Rect;
	begin
		SetRect(pxRect, cx - hCenter, cy + caveTopOffset, cx - hCenter + 1, cy + 1 + caveTopOffset);
		SetPort(offscreenPort);
		PaintRect(pxRect);
		SetPort(window)
		PaintRect(pxRect);
	end;


	procedure EraseStatusLine;
		var
			r: Rect;
	begin
		SetRect(r, 0, 0, winWidth, topPadding + statusLineHeight);
		SetPort(offscreenPort);
		EraseRect(r);
		SetPort(window);
		EraseRect(r);
	end;


	procedure CopyStatusLine;
		var
			r: Rect;
	begin
		SetRect(r, 0, 0, winWidth, caveTopOffset);
		CopyBits(offscreenPort^.portBits, window^.portBits, r, r, srcCopy, nil);
	end;

{ Update the status line without changing ports }
	procedure ShowStatusInternal (status: string);
	begin
		EraseStatusLine;
		MoveTo(statusLineLeftPadding, topPadding);
		DrawString(status);
		showingSandCount := false;
	end;

	procedure ShowSandCount (n: integer);
		var
			r: Rect;
	begin
		SetPort(offscreenPort);

		if showingSandCount then
			begin
{ erase previous number, but leave the rest to minize fickering }
				SetRect(r, sandCountStart.h, 0, winWidth, topPadding + statusLineHeight);
				EraseRect(r);
				MoveTo(sandCountStart.h, sandCountStart.v);
			end
		else
			begin
				ShowStatusInternal('Grains of sand at rest so far: ');
				GetPen(sandCountStart);
				showingSandCount := true;
			end;

		DrawString(StringOf(n : 1));
		SetPort(window);

		CopyStatusLine;
	end;

	procedure ShowStatus (status: string);
	begin
		SetPort(offscreenPort);
		ShowStatusInternal(status);
		SetPort(window);
		CopyStatusLine;
	end;

	procedure ShowError (msg: string);
		var
			ignored: integer;
	begin
		ParamText(msg, '', '', '');
		ignored := StopAlert(alertResId, nil);
	end;


	procedure DoUpdate;
		var
			srcRect: Rect;
	begin
		with window^.portRect do
			SetRect(srcRect, 0, 0, right - left, bottom - top);

		BeginUpdate(window);
		CopyBits(offscreenPort^.portBits, window^.portBits, srcRect, window^.portRect, srcCopy, nil);
		EndUpdate(window);
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
				updateEvt: 
					DoUpdate;
				otherwise
					begin
{ TODO: window drag, window resize, maybe more }
					end;
			end;
	end;

	procedure PostSimulationEventLoop;
	begin
		while not gShouldQuit do
{ We've nothing else to do so be maximally nice to background processes }
			HandleOneEvent(maxInt);
	end;

end.