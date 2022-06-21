/* ---------------------------------------------------- ce/prokalk.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

define shared buffer xest for est.
define shared buffer xef  for ef.
define shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def new shared var maxco as int no-undo.
def var mess as ch format "x(80)" extent 2 NO-UNDO.
def new shared var v-n-out as int init 1 no-undo.
def var v-spo as dec NO-UNDO.
def var cumul as dec NO-UNDO.
def var v-num-up like xeb.num-up NO-UNDO.
DEF VAR ip-rowid AS ROWID NO-UNDO.

DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEFINE VARIABLE glAssignUnitsForInk AS LOGICAL NO-UNDO.
DEFINE VARIABLE lUnitSetup AS LOGICAL NO-UNDO.

RUN pGetSettingValue (xest.company).

{ce/mach-ink.i new}


run ce/mach-ink.p.

assign
 spo      = 0
 r-spo[1] = 0
 save-qty = qty
 v-num-up = xeb.num-up
 v-n-out  = (if xef.n-out   eq 0 then 1 else xef.n-out) *
            (if xef.n-out-l eq 0 then 1 else xef.n-out-l)
 cumul    = qty / v-num-up / v-n-out.

for each est-op
    where est-op.company = xest.company 
      AND est-op.est-no eq xest.est-no
      and est-op.qty   eq v-op-qty
      and est-op.line  ge 500
    by line descending:

  ip-rowid = ROWID(est-op).

  {ce/prokalk.i xeb}
end.

qty = save-qty.

{sys/inc/roundup.i r-spo[1]}
fil_id = recid(xef).
find xef where recid(xef) = fil_id no-error.
xef.gsh-qty = cumul.
release est-op.


PROCEDURE pGetSettingValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to the session
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "CEInksWithUnits", "L" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glAssignUnitsForInk = cReturn EQ "YES".
    
END PROCEDURE.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
