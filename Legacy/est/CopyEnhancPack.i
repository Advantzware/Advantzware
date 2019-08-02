DEFINE VARIABLE lPackCodeCopy AS LOGICAL NO-UNDO .
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE BUFFER bf-estPacking FOR estPacking .

  RUN sys/ref/nk1look.p (INPUT eb.company, "CePackEnhanced", "L" /* Logical */, NO /* check by cust */, 
                         INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                         OUTPUT cRtnChar, OUTPUT lRecFound).

  IF lRecFound THEN
      lPackCodeCopy = LOGICAL(cRtnChar) NO-ERROR.

  IF lPackCodeCopy THEN DO:
      FOR EACH estPacking NO-LOCK 
          WHERE estPacking.company EQ eb.company 
           AND estPacking.estimateNo EQ eb.est-no 
           AND estPacking.FormNo  EQ eb.form-no 
           AND estPacking.BlankNo  EQ eb.blank-No :

         CREATE bf-estPacking .
         ASSIGN
            bf-estPacking.company      = eb.company 
            bf-estPacking.estimateNo   = eb.est-no
            bf-estPacking.FormNo       = b-eb.form-no
            bf-estPacking.BlankNo      = b-eb.blank-No
            bf-estPacking.estPackingID = NEXT-VALUE(estPackingId_seq)  .
         BUFFER-COPY estPacking EXCEPT company estimateNo FormNo  BlankNo estPackingID TO bf-estPacking.
      END.
      RELEASE bf-estPacking NO-ERROR .
  END.
