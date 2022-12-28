unit Sorting;

interface

	uses
		Config;

	type
		Sortable = array[1..maxNumPackets] of integer;

{ Unnecessarily generic, but see the old joke re: `DestroyBaghadad` vs `DestroyCity(baghdad)` }
	procedure Sort (var arr: Sortable; len: integer; function cmp (a, b: integer): integer);

implementation

{ Insertion sort strikes a good balance between speed and ease of implementation }
	procedure Sort (var arr: Sortable; len: integer; function cmp (a, b: integer): integer);
		var
			i, j, tmp: integer;
	begin
		for i := 2 to len do
			begin
				for j := i downto 2 do
					if cmp(arr[j - 1], arr[j]) <= 0 then
						leave
					else
						begin
							tmp := arr[j];
							arr[j] := arr[j - 1];
							arr[j - 1] := tmp;
						end;
			end;
	end;

end.