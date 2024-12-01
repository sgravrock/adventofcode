create temporary table similarity (a integer primary key, score integer not null);

insert into similarity (a, score)
select l.a, count(*) * l.a 
from input l
inner join input r on r.b = l.a
group by l.a;

select sum(score) from similarity;
