with deltas as (
	select succ.report_id, succ.value - pred.value as delta
	from level succ
	inner join level pred on pred.report_id = succ.report_id
		and pred.level_num = succ.level_num - 1
)
select count(*) from report
where (
	not exists (select * from deltas where delta < 0 and report_id = report.id)
	or
	not exists (select * from deltas where delta > 0 and report_id = report.id)
)
and
not exists (select * from deltas where delta = 0 and report_id = report.id)
and
not exists (select * from deltas where abs(delta) > 3 and report_id = report.id);
