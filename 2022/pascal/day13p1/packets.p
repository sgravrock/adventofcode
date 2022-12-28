unit Packets;

interface

	uses
		Config;

	const
		maxListLen = 5;

	type
		InputBuffer = record
				data: packed array[1..22000] of char;
				sz: LongInt;
			end;
		OrderType = (inOrder, outOfOrder, TBD);
		ListElType = (listElList, listElNum);
		ListPtr = ^ListType;
		ListElement = record
				case t : ListElType of
					listElList: (
							list: ListPtr
					);
					listElNum: (
							n: integer
					);
			end;
		ListType = record
				els: array[1..maxListLen] of ListElement;
				sz: integer;
			end;

  { Simple pool allocator saves ~15s (~50% run time) vs new()ing each list }
	procedure InitListPool;
	procedure DisposeListPool;
	function ParseList (var inputBuf: InputBuffer; firstI: integer; var lastI: integer): ListPtr;
	function Order (left, right: ListPtr): OrderType;

implementation

	const
		listPoolSize = 100;

	type
		ListPool = record
				lists: array[1..listPoolSize] of ListType;
				sz: integer;
			end;

	var
		pool: ListPool;

	procedure InitListPool;
	begin
		pool.sz := 0;
	end;


	procedure DisposeListPool;
	begin
		pool.sz := 0;
	end;


	function ParseNumber (var buf: InputBuffer; var i: integer): ListElement;
		var
			result: ListElement;
	begin
		result.t := listElNum;
		result.n := 0;

		while (ord(buf.data[i]) >= ord('0')) and (ord(buf.data[i]) <= ord('9')) do
			begin
				result.n := result.n * 10 + ord(buf.data[i]) - ord('0');
				i := i + 1;
			end;

		ParseNumber := result;
	end;


	function ParseList (var inputBuf: InputBuffer; firstI: integer; var lastI: integer): ListPtr;
		var
			result: ListPtr;
			i, s: integer;
	begin
		pool.sz := pool.sz + 1;
		result := @pool.lists[pool.sz];
		result^.sz := 0;

		if inputBuf.data[firstI] <> '[' then
			begin
				writeln('List did not start with [');
				halt;
			end;

		i := firstI + 1;

		while (i <= inputBuf.sz) and (inputBuf.data[i] <> ']') do
			begin
				if inputBuf.data[i] = chr($d) then
					begin
						writeln('Found carraige return in list');
						halt;
					end;

				if inputBuf.data[i] = ',' then
					i := i + 1;

				if (ord(inputBuf.data[i]) >= ord('0')) and (ord(inputBuf.data[i]) <= ord('9')) then
					begin
			{ Advances i }
						result^.els[result^.sz + 1] := ParseNumber(inputBuf, i);
						result^.sz := result^.sz + 1;
					end
				else if inputBuf.data[i] = '[' then
					begin
						s := i;
						result^.sz := result^.sz + 1;
						result^.els[result^.sz].t := listElList;
			{ Advances i }
						result^.els[result^.sz].list := ParseList(inputBuf, s, i);
					end
				else
					begin
						writeln('Unexpected character: ', inputBuf.data[i]);
						halt;
					end;
			end;

		if inputBuf.data[i] = ']' then
			i := i + 1
		else
			begin
				writeln('Missing trailing ]');
				halt;
			end;

		lastI := i;
		ParseList := result;
	end;


	function Order2 (left, right: ListElement): OrderType;
		var
			promoted: ListType;
	begin
		if (left.t = listElList) and (right.t = listElList) then
			Order2 := Order(left.list, right.list)
		else if (left.t = listElNum) and (right.t = listElNum) then
			if left.n < right.n then
				Order2 := inOrder
			else if left.n = right.n then
				Order2 := TBD
			else
				Order2 := outOfOrder
		else if left.t = listElList then
			begin
				promoted.sz := 1;
				promoted.els[1].t := listElNum;
				promoted.els[1].n := right.n;
				Order2 := Order(left.list, @promoted);
			end
		else
			begin
				promoted.sz := 1;
				promoted.els[1].t := listElNum;
				promoted.els[1].n := left.n;
				Order2 := Order(@promoted, right.list);
			end;
	end;


	function Order (left, right: ListPtr): OrderType;
		var
			i: integer;
			result, subResult: OrderType;
	begin
		result := TBD;
		i := 1;

		while (result = TBD) and (i <= left^.sz) and (i <= right^.sz) do
			begin
				result := Order2(left^.els[i], right^.els[i]);
				i := i + 1;
			end;

		if result = TBD then
			if left^.sz < right^.sz then
				result := inOrder
			else if left^.sz > right^.sz then
				result := outOfOrder;

		Order := result;
	end;

end.