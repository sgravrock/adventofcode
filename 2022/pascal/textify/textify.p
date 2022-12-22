program textify;

	const
		bufsize = 512;
		alertId = 128;

	type
		Buffer = packed array[1..bufsize] of byte;

	procedure InitToolbox;
	begin
		InitGraf(@thePort);
		InitFonts;
		InitWindows;
		InitMenus;
		TEInit;
		InitDialogs(nil);
		InitCursor;
	end;

{ Note: List of error codes is in Inside Macintosh Volume III, appendix A. }
{ Comon ones: -39 eofErr, -43 fnfErr (file not found), 0 noErr }
	procedure ShowError (msg: string; code: OSErr);
		var
			ignored: integer;
			codeStr: string;
	begin
		codeStr := StringOf(code : 1);
		ParamText(msg, ': ', codeStr, '');
		ignored := StopAlert(alertId, nil);
	end;

	procedure ShowNote (msg: string);
		var
			ignored: integer;
	begin
		ParamText(msg, '', '', '');
		ignored := NoteAlert(alertId, nil);
	end;

{ Shows the standard file get dialog. }
{ After calling, check reply.good to see if a file was selected. }
	procedure ShowOpenDialog (var reply: SFReply);
		var
			types: SFTypeList;
			where: Point;
	begin
{ TODO: center the dialog }
		where.v := 40;
		where.h := 20;
		SFGetFile(where, '', nil, -1, types, nil, reply);
	end;

{ Shows the standard file put dialog. }
{ After calling, check reply.good to see if a file was selected. }
	procedure ShowCreateDialog (var reply: SFReply);
		var
			types: SFTypeList;
			where: Point;
	begin
{ TODO: center the dialog }
		where.v := 40;
		where.h := 20;
		SFPutFile(where, 'Select output file', '', nil, reply);
	end;

	function OpenInputFile (var path: Integer): Boolean;
		var
			fileInfo: SFReply;
			err: OSErr;

	begin
		ShowOpenDialog(fileInfo);
		OpenInputFile := fileInfo.good;
		if fileInfo.good then
			begin
				err := FSOpen(fileInfo.fName, fileInfo.vRefNum, path);
				if err <> noErr then
					begin
						ShowError('Error opening input file', err);
						halt;
					end
			end;
	end;

	function OpenOutputFile (var path: Integer): boolean;
		var
			fileInfo: SFReply;
			err: OSErr;

	begin
		ShowCreateDialog(fileInfo);
		OpenOutputFile := fileInfo.good;
		if (fileInfo.good) then
			begin
{ TEXT is the standard text file type and ttxt is the creaor code for TeachText, }
{ likely the only plain text editor on hand. }
{ TODO: Add an option to set the appropriate type and creator for Pascal source files }
				err := Create(fileInfo.fName, fileInfo.vRefNum, 'ttxt', 'TEXT');
				if err <> noErr then
					begin
						ShowError('Error creating output file', err);
						halt;
					end;

				err := FSOpen(fileInfo.fName, fileInfo.vRefNum, path);
				if err <> noErr then
					begin
						ShowError('Error opening output file', err);
						halt;
					end;
			end;
	end;

	procedure UnixToMacBlock (var block: Buffer);
		const
			cr = 13;
			lf = 10;
		var
			i: integer;
	begin
		for i := 1 to bufsize do
			if block[i] = lf then
				block[i] := cr;
	end;

	procedure UnixToMacFile (outputPath, inputPath: Integer);
		var
			buf: Buffer;
			count: LongInt;
			err: OSErr;
			done: Boolean;
	begin
		done := false;
		while not done do
			begin
				count := 512;
				err := FSRead(inputPath, count, @buf);
				if err = eofErr then
					done := true
				else if err <> noErr then
					begin
						ShowError('Error reading input file', err);
						halt;
					end;

				UnixToMacBlock(buf);

				err := FSWrite(outputPath, count, @buf);
				if err <> noErr then
					begin
						ShowError('Error writing output file', err);
						halt;
					end;
			end;
	end;

	var
		inputPath: Integer;
		outputPath: Integer;
		err: OSErr;
		cursor: CursHandle;

begin
	InitToolbox;
	if OpenInputFile(inputPath) then
		if OpenOutputFile(outputPath) then
			begin
				cursor := GetCursor(watchCursor);
				SetCursor(cursor^^);
				UnixToMacFile(outputPath, inputPath);
				err := FSClose(outputPath);
				if err <> noErr then
					begin
						ShowError('Error closing output file', err);
						halt;
					end;
				err := FSClose(inputPath);
				if err <> noErr then
					begin
						ShowError('Error closing input file', err);
						halt;
					end;

				ShowNote('Conversion succeded.');
			end;
end.