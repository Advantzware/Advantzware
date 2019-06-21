
DEF BUFFER calcpcts FOR reftable.


FIND FIRST calcpcts
    WHERE calcpcts.reftable EQ "est.calcpcts"
      AND calcpcts.company  EQ {1}.company
      AND calcpcts.loc      EQ {1}.loc
      AND calcpcts.code     EQ TRIM({1}.est-no)
    NO-ERROR.

IF NOT AVAIL calcpcts THEN DO:
  CREATE calcpcts.
  ASSIGN
   calcpcts.reftable = "est.calcpcts"
   calcpcts.company  = {1}.company
   calcpcts.loc      = {1}.loc
   calcpcts.code     = TRIM({1}.est-no).
END.
