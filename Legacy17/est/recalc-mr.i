
FIND FIRST recalc-mr
    WHERE recalc-mr.reftable EQ "est.recalc-mr"
      AND recalc-mr.company  EQ {1}.company
      AND recalc-mr.loc      EQ {1}.loc
      AND recalc-mr.code     EQ TRIM({1}.est-no)
    NO-ERROR.

IF NOT AVAIL recalc-mr THEN DO:
  CREATE recalc-mr.
  ASSIGN
   recalc-mr.reftable = "est.recalc-mr"
   recalc-mr.company  = {1}.company
   recalc-mr.loc      = {1}.loc
   recalc-mr.code     = TRIM({1}.est-no).
END.
