create temporary table leftlist (id integer PRIMARY KEY AUTOINCREMENT, val integer not null);
create temporary table rightlist (id integer PRIMARY KEY AUTOINCREMENT, val integer not null);

insert into leftlist (val) select a from input order by a;
insert into rightlist (val) select b from input order by b;

select sum(abs(leftlist.val - rightlist.val))
from leftlist
inner join rightlist on rightlist.id = leftlist.id;
