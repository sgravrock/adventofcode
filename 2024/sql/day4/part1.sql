select count(*) from deltas
cross join grid xc
where xc.c = 'X'
and exists (
	select * from grid
	where x = xc.x + deltas.dx
	and y = xc.y + deltas.dy
	and c = 'M'
)
and exists (
	select * from grid
	where x = xc.x + 2 * deltas.dx
	and y = xc.y + 2 * deltas.dy
	and c = 'A'
)
and exists (
	select * from grid
	where x = xc.x + 3 * deltas.dx
	and y = xc.y + 3 * deltas.dy
	and c = 'S'
);

