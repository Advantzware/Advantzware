
DEF VAR li AS INT NO-UNDO.
DEF VAR lv AS CHAR NO-UNDO.
                  
                  
FOR EACH ar-ledger
    WHERE CAN-FIND(FIRST period
                   WHERE period.company EQ ar-ledger.company
                     AND period.pst     LE ar-ledger.ref-date
                     AND period.pend    GE ar-ledger.ref-date)
      AND CAN-FIND(FIRST period
                   WHERE period.company EQ ar-ledger.company
                     AND period.pst     LE ar-ledger.tr-date
                     AND period.pend    GE ar-ledger.tr-date)
      AND NOT CAN-FIND(FIRST period
                       WHERE period.company EQ ar-ledger.company
                         AND period.pst     LE ar-ledger.ref-date
                         AND period.pst     LE ar-ledger.tr-date
                         AND period.pend    GE ar-ledger.ref-date
                         AND period.pend    GE ar-ledger.tr-date):

  IF ar-ledger.ref-num BEGINS "INV# " THEN
  FOR EACH ar-inv
      WHERE ar-inv.company EQ ar-ledger.company
        AND ar-inv.inv-no  EQ INT(SUBSTR(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num)))
      NO-LOCK:
    ar-ledger.ref-date = ar-inv.inv-date.
  END.

  ELSE
  IF ar-ledger.ref-num BEGINS "Memo#" THEN
  FOR EACH ar-cash
      WHERE ar-cash.company  EQ ar-ledger.company
        AND ar-cash.memo     EQ YES
        AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,8))
      NO-LOCK:
    ar-ledger.ref-date = ar-cash.check-date.
  END.

  ELSE
  IF ar-ledger.ref-num BEGINS "CHK# " OR ar-ledger.ref-num BEGINS "DISC " THEN
  FOR EACH ar-cash
      WHERE ar-cash.company  EQ ar-ledger.company
        AND ar-cash.memo     EQ NO
        AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,10))
      NO-LOCK:
    ar-ledger.ref-date = ar-cash.check-date.
  END.

  ELSE
  IF ar-ledger.cust-no EQ "" THEN DO:
    lv = "".
    DO li = 1 TO LENGTH(ar-ledger.ref-num):
      IF SUBSTR(ar-ledger.ref-num,li,1) EQ " " THEN LEAVE.
      lv = lv + SUBSTR(ar-ledger.ref-num,li,1).
    END.
    li = INT(lv) NO-ERROR.
    IF ERROR-STATUS:ERROR EQ NO AND li NE 0 THEN
    FOR EACH ar-mcash
        WHERE ar-mcash.company EQ ar-ledger.company
          AND ar-mcash.m-no    EQ li
          AND ar-mcash.payer   EQ SUBSTR(ar-ledger.ref-num,LENGTH(lv) + 2,LENGTH(ar-ledger.ref-num))
        NO-LOCK:
      ar-ledger.ref-date = ar-mcash.check-date.
    END.
  END.
END.

FOR EACH ap-ledger
    WHERE CAN-FIND(FIRST period
                   WHERE period.company EQ ap-ledger.company
                     AND period.pst     LE ap-ledger.ref-date
                     AND period.pend    GE ap-ledger.ref-date)
      AND CAN-FIND(FIRST period
                   WHERE period.company EQ ap-ledger.company
                     AND period.pst     LE ap-ledger.tr-date
                     AND period.pend    GE ap-ledger.tr-date)
      AND NOT CAN-FIND(FIRST period
                       WHERE period.company EQ ap-ledger.company
                         AND period.pst     LE ap-ledger.ref-date
                         AND period.pst     LE ap-ledger.tr-date
                         AND period.pend    GE ap-ledger.ref-date
                         AND period.pend    GE ap-ledger.tr-date):

  IF ap-ledger.refnum BEGINS "INV# " THEN
  FOR EACH ap-inv
      WHERE ap-inv.company EQ ap-ledger.company
        AND ap-inv.vend-no EQ ap-ledger.vend-no
        AND ap-inv.inv-no  EQ TRIM(SUBSTR(ap-ledger.refnum,6,LENGTH(ap-ledger.refnum)))
      NO-LOCK:
    ap-ledger.ref-date = ap-inv.inv-date.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "CHK# " THEN DO:
    lv = "".
    DO li = 6 TO LENGTH(ap-ledger.refnum):
      IF SUBSTR(ap-ledger.refnum,li,1) EQ " " THEN LEAVE.
      lv = lv + SUBSTR(ap-ledger.refnum,li,1).
    END.
    li = INT(lv) NO-ERROR.
    IF ERROR-STATUS:ERROR EQ NO AND li NE 0 THEN
    FOR EACH ap-pay
        WHERE ap-pay.company  EQ ap-ledger.company
          AND ap-pay.vend-no  EQ ap-ledger.vend-no
          AND ap-pay.check-no EQ li
        NO-LOCK:
      ap-ledger.ref-date = ap-pay.check-date.
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "AC " THEN
  FOR EACH ap-pay
      WHERE ap-pay.company  EQ ap-ledger.company
        AND ap-pay.vend-no  EQ ap-ledger.vend-no
        AND ap-pay.check-no EQ INT(SUBSTR(ap-ledger.refnum,4,6))
      NO-LOCK:
    ap-ledger.ref-date = ap-pay.check-date.
  END.

  /*ELSE
  IF ap-ledger.refnum BEGINS "MEMO#" THEN
  FOR EACH ap-payl
      WHERE ap-payl.company EQ ap-ledger.company
        AND ap-payl.vend-no EQ ap-ledger.vend-no
        AND ap-payl.inv-no  EQ TRIM(SUBSTR(ap-ledger.refnum,6,LENGTH(ap-ledger.refnum))
      USE-INDEX inv-no NO-LOCK,
      FIRST ap-pay WHERE ap-pay.c-no EQ ap-payl.c-no NO-LOCK:
    ap-ledger.ref-date = ap-pay.check-date.
  END.*/
END.
