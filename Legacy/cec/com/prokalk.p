/* ------------------------------------------------ ce/com/prokalk.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def new shared buffer xop for est-op.

{cec/print4.i shared shared}

def new shared var maxco as int no-undo.
def new shared var v-n-out as int init 1 no-undo.

def new shared var lm-spo as de NO-UNDO.
def new shared var lm-waste as de NO-UNDO.
def new shared var lm-qty as de NO-UNDO.

def var cumul as de NO-UNDO.
def var eb_id as recid NO-UNDO.
def var sav-spo as de extent 99 NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.


fil_id = recid(xef).
find xef where recid(xef) eq fil_id no-error.
xef.op-lock = YES.
find xef where recid(xef) eq fil_id no-lock no-error.

run cec/com/localk.p (500, ?, YES).  /*Changed from NO to YES for ticket 33762 - setting this to no, 
                                       causes a reset of op-lock.val[1] and .val[2] (the user checkbox 
                                       on the recalc to use machine standards).  Problem was that
                                       forms 2-n used the est-op values instead of recalculating from standards*/

for each est-op
    where est-op.company = xest.company
      AND est-op.est-no eq xest.est-no
      and est-op.s-num eq xef.form-no
      and est-op.line  GT 500
    no-lock
    BY est-op.LINE:
  cumul = est-op.num-sh.
  leave.
end.

fil_id = recid(xef).
find xef where recid(xef) eq fil_id no-error.
xef.gsh-qty = cumul.
qty = save-qty.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
