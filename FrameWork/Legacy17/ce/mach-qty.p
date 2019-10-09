/* --------------------------------------------------- ce/mach-qty.p 9/94 gb  */
/* Determine the Run Quantity Type                                            */
/* -------------------------------------------------------------------------- */

def input param ip-rowid as rowid no-undo.

/*def input parameter p-type like mach.p-type no-undo.
def input parameter therm like mach.therm no-undo.
def input parameter seq as int no-undo.*/

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.
def shared var v-chk-qty as dec no-undo.
def shared var v-sht-qty as dec no-undo.
def shared var sh-len  as de NO-UNDO.
def shared var v-rc-seq as int no-undo.

DEF VAR ll-tandem AS LOG NO-UNDO.


FIND mach WHERE ROWID(mach) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL mach THEN DO:
  v-chk-qty = 0.

  IF xest.est-type GE 3 THEN DO:
    IF xest.est-type EQ 4 THEN
      RUN ce/com/istandem.p (ROWID(xest), OUTPUT ll-tandem).

    IF ll-tandem THEN
    FOR EACH eb NO-LOCK
        WHERE eb.company EQ xest.company
          AND eb.est-no  EQ xest.est-no:
      v-chk-qty = v-chk-qty + eb.bl-qty.
    END.

    ELSE v-chk-qty = IF xest.est-type EQ 3 THEN xeb.bl-qty ELSE xeb.yld-qty.
  END.

  ELSE
    v-chk-qty = IF xest.est-type EQ 2 THEN xest.est-qty[1] ELSE qty.
.
  if p-type ne "B" then do:
    v-chk-qty = v-chk-qty / xeb.num-up.

    if 10 * mach.d-seq gt v-rc-seq then v-chk-qty = v-chk-qty / xef.n-out.

    if (mach.p-type eq "R" OR mach.dept[1] EQ "LM") and mach.therm then v-chk-qty = v-chk-qty * (sh-len / 12).

    {sys/inc/roundup.i v-chk-qty} 
  end.
END.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
