/* -------------------------------------------------- oe/invpostj.p 08/95 CAH */
/* Invoicing  - Post Invoicing Transactions - Job Costing                     */
/* -------------------------------------------------------------------------- */

DEF SHARED WORKFILE work-job NO-UNDO
  FIELD actnum   LIKE account.actnum
  FIELD amt      LIKE inv-line.t-price
  FIELD fg       AS LOGICAL.

def input param ws_actnum as char no-undo.
def input param ws_amt    as decimal no-undo.
def input param ws_fg     as logical no-undo.
def input param ws_qty    as decimal no-undo.

if ws_actnum > "" and
   ws_amt ne ? then do:
  find first work-job where work-job.actnum = ws_actnum no-error.
  if not available work-job then do:
    create work-job.
    assign work-job.actnum = ws_actnum
	   work-job.fg     = ws_fg.
  end.
  work-job.amt = work-job.amt + ((ws_qty / 1000) * decimal(ws_amt)).
end.

/* END ---------------------------------- copr. 1994  Advanced Software, Inc. */
