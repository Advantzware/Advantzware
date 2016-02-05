/* --------------------------------------------------- oe/invpostj.i 10/94 gb */
/* Invoicing  - Post Invoicing Transactions - Job Costing                     */
/* -------------------------------------------------------------------------- */

if "{1}" > "" and
   "{2}" ne ? then do:
  find first work-job where work-job.actnum = {1} no-error.
  if not available work-job then do:
    create work-job.
    assign work-job.actnum = {1}
	   work-job.fg     = {3}.
  end.
  work-job.amt = work-job.amt + ((inv-line.inv-qty / 1000) * decimal({2})).
end.

/* END ---------------------------------- copr. 1994  Advanced Software, Inc. */
