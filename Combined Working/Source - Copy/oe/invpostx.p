/* --------------------------------------------------- oe/invpostx.p 08/95 gb */
/* Invoicing  - Post Invoicing Transactions - Job Costing                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def input parameter v-actnum as char.
def input parameter v-cost as dec.
def input parameter v-qty as int.
def input parameter v-fg as log.
def input parameter v-i-no as char.
def input parameter v-inv-no as int.
def input parameter v-uom like itemfg.prod-uom.

def var v-cost-ea as dec.

{oe/invwork.i}


if v-actnum gt "" and v-cost ne ? then do:
  create tmp-work-job.
  assign
   tmp-work-job.actnum = v-actnum
   tmp-work-job.i-no   = v-i-no
   tmp-work-job.inv-no = v-inv-no.
   tmp-work-job.fg     = v-fg.

  if v-uom eq "EA" then
    v-cost-ea = v-cost.
  else
    run sys/ref/convcuom.p(v-uom, "EA", 0, 0, 0, 0,
                           v-cost, output v-cost-ea).

  tmp-work-job.amt = v-qty * dec(v-cost-ea).

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ v-i-no
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN
    tmp-work-job.weight = itemfg.weight-100 * v-qty / 100.

  IF tmp-work-job.weight EQ ? THEN tmp-work-job.weight = 0.

  find first work-job where work-job.actnum eq v-actnum no-error.
  if not available work-job then do:
    create work-job.
    assign
     work-job.actnum = v-actnum
     work-job.fg     = v-fg.
  end.

  assign
   work-job.amt     = work-job.amt + tmp-work-job.amt
   work-job.weight  = work-job.weight + tmp-work-job.weight.

  if not v-fg then v-cgs-test = v-cgs-test + tmp-work-job.amt.
end.

/* END ---------------------------------- copr. 1994  Advanced Software, Inc. */
