select count(*) from report
where exists (
	select level_num as skip from level
	where report_id = report.id
	and exists (
		with deltas as (
			select succ.level_num, succ.value - pred.value as delta, pred.value, succ.value
			from level succ
			inner join level pred
				on pred.report_id = report.id
				and succ.report_id = report.id
				and pred.level_num <> skip
				and succ.level_num <> skip
				and (
					(pred.level_num + 1 <> skip and succ.level_num = pred.level_num + 1)
					or
					(pred.level_num + 1 = skip and succ.level_num = pred.level_num + 2)
				)

		)
		select * from deltas
		where (
			not exists (select * from deltas where delta < 0)
			or 
			not exists (select * from deltas where delta > 0)
		)
		and
		not exists (select * from deltas where delta = 0)
		and
		not exists (select * from deltas where abs(delta) > 3)
	)
)
