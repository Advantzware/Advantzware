/* -------------------------------------------------- oe/invpostd.p 02/99 FWK */
/* Invoicing  - Delete Work Files                                             */
/* -------------------------------------------------------------------------- */

def input parameter v-term as char.

{oe/invwork.i}


for each report where report.term-id eq v-term:
  delete report.
end.

for each work-job:
  delete work-job.
end.

for each w-inv-line:
  delete w-inv-line.
end.

for each w-ord-misc:
  delete w-ord-misc.
end.

for each tmp-work-job:
  delete tmp-work-job.
end.

assign
 v-post-total   = 0
 v-post-freight = 0
 v-post-cash    = 0
 v-post-disc    = 0.

/* END ---------------------------------- copr. 1999  Advanced Software, Inc. */
