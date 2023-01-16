unit MicroSysFileUtils;

{ Like SysFileUtils, but adapted for use with µRuntime }

interface

	type
		FileError = (FileErrorOk, FileErrorCanceled, FileErrorFileTooBig, FileErrorOther);

	function OpenInputFile (var path: Integer): FileError;
	function ReadEntireFile (path: integer; buf: Ptr; var bufsz: LongInt): FileError;

implementation


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


	function OpenInputFile (var path: Integer): FileError;
		var
			pb: ParamBlockRec;
			fileInfo: SFReply;
			err: OSErr;

	begin
		ShowOpenDialog(fileInfo);
		if not fileInfo.good then
			OpenInputFile := FileErrorCanceled
		else
			begin
 		{ Open the file using the low-level PBOpen function. }
		{ This is more involved than FSOpen, but it lets us open read-only }
		{ which avoids leaking a file lock if the program doesn't run to completion. }
				pb.ioNamePtr := @fileInfo.fName;
				pb.ioVRefNum := fileInfo.vRefNum;
				pb.ioVersNum := fileInfo.version;
				pb.ioMisc := nil;
				pb.ioPermssn := fsRdPerm;

				err := PBOpen(@pb, false);

				if err = noErr then
					begin
						path := pb.ioRefNum;
						OpenInputFile := FileErrorOk;
					end
				else
					OpenInputFile := FileErrorOther;
			end;
	end;

	function ReadEntireFile (path: integer; buf: Ptr; var bufsz: LongInt): FileError;
		var
			offset: LongInt;
			blksz: LongInt;
			err: OSErr;
			blockp: Ptr;
	begin
		err := noErr;
		offset := 0;

		while (err = noErr) and (offset < bufsz) do
			begin
				blockp := pointer(ord(buf) + offset);
				blksz := 512;

				if blksz > (bufsz - offset) then
					blksz := bufsz - offset;

				err := FSRead(path, blksz, blockp);

				if (err = noErr) or (err = eofErr) then
					offset := offset + blksz;
			end;

		if err <> eofErr then
			writeln('Buffer too small to read file');

		bufsz := offset;

		if err = eofErr then
			ReadEntireFile := FileErrorOk
		else if err = noErr then
			ReadEntireFile := FileErrorFileTooBig
		else
			ReadEntireFile := FileErrorOther;
	end;

end.