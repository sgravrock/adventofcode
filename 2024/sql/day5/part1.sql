select sum(middle.page_num) from upd
inner join upd_page middle
	on middle.upd_id = upd.id
	and middle.page_ix = (select max(page_ix) / 2 from upd_page where upd_id = upd.id)
where not exists (
	select * from upd_page pred_page
	inner join upd_page succ_page
		on pred_page.upd_id = succ_page.upd_id
		and pred_page.page_ix < succ_page.page_ix
	inner join rule
		on rule.pred = succ_page.page_num
		and rule.succ = pred_page.page_num
	where pred_page.upd_id = upd.id
);
