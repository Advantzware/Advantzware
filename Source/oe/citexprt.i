/* oe/citexprt.i */

DEF VAR v-assign-num AS INT NO-UNDO.
DEF VAR v-assign-date AS DATE NO-UNDO.

  /* Assignment # */
FIND FIRST reftable WHERE reftable.reftable EQ "CITEXPORT"
          AND reftable.company  EQ cocode
          AND reftable.loc    EQ locode NO-LOCK NO-ERROR.
IF NOT AVAIL reftable THEN DO TRANSACTION:
   CREATE reftable.
   ASSIGN reftable.reftable = "CITEXPORT"
          reftable.company = cocode
          reftable.loc = locode
          reftable.val[1] = 1.
END.
ELSE IF reftable.val[1] >= 9999 THEN DO TRANSACTION : 
    FIND CURRENT reftable .
    reftable.val[1] = 1.
END.
FIND CURRENT reftable NO-LOCK.
v-assign-num = reftable.val[1] + 1.
v-assign-date = TODAY.
