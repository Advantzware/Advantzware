/* --------------------------------------------------- oe/closchk.p 10/01 JLF */
/*                                                                            */
/* Order Close - Check to see if order for invoice line should be closed      */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-ord-no LIKE oe-ord.ord-no NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF BUFFER b-oe-ordl FOR oe-ordl.

{oe/closchk.i}

DEF VAR v-ord-closed AS LOG NO-UNDO.
DEF VAR v-lin-closed AS LOG NO-UNDO.
DEF VAR v-rel-closed AS LOG NO-UNDO.
DEF VAR v-job-qty LIKE job-hdr.qty NO-UNDO.
DEF VAR v-rec-qty LIKE fg-rdtlh.qty NO-UNDO.
DEF VAR v-bin-qty LIKE fg-bin.qty NO-UNDO.
DEF VAR v-add-overrn AS LOG NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "JOB QTY"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "JOB QTY"
    sys-ctrl.descrip = "Create Job Quantity with overrun % from OE?"
    sys-ctrl.log-fld = NO.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
END.
v-add-overrn = sys-ctrl.log-fld.

DO TRANSACTION:
  {sys/inc/oereleas.i}
  {sys/inc/oeclose.i}
END.

FOR EACH w-ord BREAK BY w-ord.ord-no:
  IF NOT FIRST-OF(w-ord.ord-no) THEN DELETE w-ord.
END.

IF v-ord-no NE 0 THEN DO:
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ v-ord-no
      NO-LOCK NO-ERROR.

  CREATE w-ord.
  ASSIGN
   w-ord.ord-no = oe-ord.ord-no
   w-ord.rec-id = RECID(oe-ord).
END.

FOR EACH w-ord:

FIND FIRST oe-ord
    WHERE oe-ord.company EQ cocode
      AND oe-ord.ord-no  EQ w-ord.ord-no
    NO-LOCK NO-ERROR.

v-ord-closed = AVAIL oe-ord.
                  
IF v-ord-closed THEN
FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ oe-ord.company
      AND oe-ordl.ord-no  EQ oe-ord.ord-no
      AND oe-ordl.stat    NE "C":

  RUN oe/clslnchk.p (BUFFER oe-ordl, OUTPUT v-lin-closed).     
  IF NOT v-lin-closed THEN DO:
    v-ord-closed = NO.
    LEAVE.
  END.
END.

{oe/closeaud.i oe-ord}

IF v-ord-closed THEN reftable.val[1] = 2.

ELSE DO:
  reftable.val[1] = 1.
  DELETE w-ord.
END.

END.   /* for each w-ord */
