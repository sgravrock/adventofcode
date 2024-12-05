select count(*) from grid ac
inner join grid tl on tl.x = ac.x - 1 and tl.y = ac.y - 1
inner join grid tr on tr.x = ac.x + 1 and tr.y = ac.y - 1
inner join grid bl on bl.x = ac.x - 1 and bl.y = ac.y + 1
inner join grid br on br.x = ac.x + 1 and br.y = ac.y + 1
where ac.c = 'A'
and (
	(tl.c = 'M' and br.c = 'S')
	or
	(tl.c = 'S' and br.c = 'M')
)
and (
	(tr.c = 'M' and bl.c = 'S')
	or
	(tr.c = 'S' and bl.c = 'M')
)
