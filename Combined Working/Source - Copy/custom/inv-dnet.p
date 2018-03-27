
DEF INPUT  PARAM ip-rowid AS   ROWID NO-UNDO.
DEF OUTPUT PARAM op-amt   AS   DEC   NO-UNDO.


DEF VAR cocode AS CHAR NO-UNDO.


FIND inv-head WHERE ROWID(inv-head) EQ ip-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL inv-head THEN
FIND ar-inv WHERE ROWID(ar-inv) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL inv-head THEN DO:
  cocode = inv-head.company.

  FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

  FOR EACH inv-line NO-LOCK WHERE inv-line.r-no EQ inv-head.r-no:
    RELEASE fgcat.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ inv-line.company
          AND itemfg.i-no    EQ inv-line.i-no
        NO-ERROR.

    IF AVAIL itemfg THEN
    FIND FIRST fgcat
        WHERE fgcat.company EQ itemfg.company
          AND fgcat.procat  EQ itemfg.procat
        NO-LOCK NO-ERROR.

    RUN calc-net (IF AVAIL fgcat AND fgcat.glacc NE "" THEN fgcat.glacc
                  ELSE ar-ctrl.sales,
                  inv-line.t-price).
  END.

  FOR EACH inv-misc NO-LOCK
      WHERE inv-misc.r-no EQ inv-head.r-no
        AND inv-misc.bill EQ "Y":

    RUN calc-net (IF inv-misc.actnum NE "" THEN inv-misc.actnum
                  ELSE ar-ctrl.sales,
                  inv-misc.amt).
  END.
END.

ELSE
IF AVAIL ar-inv THEN DO:
  cocode = ar-inv.company.

  FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

  FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ ar-inv.x-no:
    RELEASE fgcat.

    IF ar-invl.i-no NE "" AND ar-invl.actnum EQ "" THEN DO:
      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ ar-invl.company
            AND itemfg.i-no    EQ ar-invl.i-no
          NO-ERROR.

      IF AVAIL itemfg THEN
      FIND FIRST fgcat NO-LOCK
          WHERE fgcat.company eq itemfg.company
            AND fgcat.procat  eq itemfg.procat
          NO-ERROR.
    END.

    RUN calc-net (IF AVAIL fgcat THEN fgcat.glacc ELSE
                  IF ar-invl.actnum NE "" THEN ar-invl.actnum
                  ELSE ar-ctrl.sales,
                  ar-invl.amt).
  END.
END.

RETURN.

PROCEDURE calc-net.
  DEF INPUT PARAM ip-actnum LIKE account.actnum NO-UNDO.
  DEF INPUT PARAM ip-amt    AS   DEC            NO-UNDO.


  RELEASE account.  

  IF ip-actnum NE "" THEN
  FIND FIRST account NO-LOCK
      WHERE account.company EQ cocode
        AND account.actnum  EQ ip-actnum
      NO-ERROR.

  IF AVAIL account THEN  
  IF account.terms-discount EQ NO THEN
    op-amt = op-amt + ip-amt.

END PROCEDURE.

