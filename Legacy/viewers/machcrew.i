 
DEF VAR lv-ref-rec-qty AS RECID NO-UNDO.
DEF VAR lv-ref-rec-cst AS RECID NO-UNDO. 


FIND FIRST reftable
    WHERE reftable.reftable EQ "MACH-CREW"
	  AND reftable.company  EQ mach.company
	  AND reftable.loc      EQ mach.loc
	  AND reftable.code     EQ mach.m-code
	  AND reftable.code2    EQ "{1}-QTY"
	NO-LOCK NO-ERROR.
IF NOT AVAIL reftable THEN DO TRANSACTION:
  CREATE reftable.
  ASSIGN
   reftable.reftable = "MACH-CREW"
   reftable.company  = mach.company
   reftable.loc      = mach.loc
   reftable.code     = mach.m-code
   reftable.code2    = "{1}-QTY"
   reftable.val[1]   = 1.
END.
lv-ref-rec-qty = RECID(reftable).

FIND FIRST reftable
    WHERE reftable.reftable EQ "MACH-CREW"
	  AND reftable.company  EQ mach.company
	  AND reftable.loc      EQ mach.loc
	  AND reftable.code     EQ mach.m-code
	  AND reftable.code2    EQ "{1}-CST"
	NO-LOCK NO-ERROR.
IF NOT AVAIL reftable THEN DO TRANSACTION:
  CREATE reftable.
  ASSIGN
   reftable.reftable = "MACH-CREW"
   reftable.company  = mach.company
   reftable.loc      = mach.loc
   reftable.code     = mach.m-code
   reftable.code2    = "{1}-CST"
   reftable.val[1]   = DEC(SELF:SCREEN-VALUE).
END.
lv-ref-rec-cst = RECID(reftable).

RUN cec/d-refest.w (lv-ref-rec-qty, lv-ref-rec-cst, "{1}").

FIND reftable WHERE RECID(reftable) EQ lv-ref-rec-cst NO-LOCK.

SELF:SCREEN-VALUE = STRING(reftable.val[1]).
