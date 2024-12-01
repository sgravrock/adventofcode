with similarity as (
	select count(*) * l.a as score
	from input l
	inner join input r on r.b = l.a
	group by l.a
)
select sum(score) from similarity;
