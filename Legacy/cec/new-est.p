
DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.
DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER recalc-mr FOR reftable.

def var li-new-estnum as int no-undo.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMetric AS LOGICAL     NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode, "METRIC", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
ASSIGN lMetric = LOGICAL(cRtnChar) .
REPEAT:

find first ce-ctrl where
     ce-ctrl.company = cocode and
     ce-ctrl.loc = locode
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:
   ASSIGN
   li-new-estnum = ce-ctrl.e-num + 1
   ce-ctrl.e-num = li-new-estnum.
   FIND CURRENT ce-ctrl NO-LOCK.
   LEAVE.
END.
END.

CREATE est.
assign est.est-type = ip-est-type
       est.company = cocode
       est.loc = locode
       est.est-no = string(li-new-estnum,">>>>>>>>")
       est.form-qty = 1
       est.est-date = today
       est.mod-date = ?
       est.metric = IF lMetric THEN TRUE ELSE FALSE 
       .
{sys/ref/est-add.i est C}

CREATE est-qty.
ASSIGN 
 est-qty.company  = cocode
 est-qty.est-no   = est.est-no
 est-qty.eqty     = 0
 est-qty.qty-date = est.est-date.

RUN cec/new-form.p (ROWID(est), OUTPUT op-rowid).
