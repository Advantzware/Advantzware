
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.


FIND cust WHERE ROWID(cust) EQ ip-rowid NO-ERROR.

IF AVAIL cust THEN DO:
  ASSIGN
   cust.ptd-msf = 0
   cust.sales   = 0.

  FOR EACH period
      WHERE period.company EQ cust.company
        AND period.pstat   EQ YES
        AND period.pnum    GT 0
        AND period.pnum    LE EXTENT(cust.sales)
      NO-LOCK,

      EACH ar-ledger
      WHERE ar-ledger.company  EQ period.company
        AND ar-ledger.cust-no  EQ cust.cust-no
        AND ar-ledger.tr-date  GE period.pst
        AND ar-ledger.tr-date  LE period.pend
        AND (ar-ledger.ref-num BEGINS "Memo#" OR
             ar-ledger.ref-num BEGINS "INV#")
      NO-LOCK:
                           
    li = INT(SUBSTRING(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN li = 0.

    IF li NE 0 THEN
      IF ar-ledger.ref-num BEGINS "INV#" THEN
      FOR EACH ar-inv
          WHERE ar-inv.company EQ ar-ledger.company
            AND ar-inv.cust-no EQ ar-ledger.cust-no
            AND ar-inv.inv-no  EQ li
            AND ar-inv.posted  EQ YES
          USE-INDEX ar-inv NO-LOCK:

        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
            USE-INDEX x-no NO-LOCK:
        
          IF ar-invl.amt-msf NE 0 THEN
            cust.ptd-msf[period.pnum] = cust.ptd-msf[period.pnum] +
                                        ar-invl.amt-msf.
         
          ELSE DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ ar-invl.company
                  AND itemfg.i-no    EQ ar-invl.i-no
                USE-INDEX i-no NO-LOCK NO-ERROR.
            
            IF AVAIL itemfg THEN
              cust.ptd-msf[period.pnum] = cust.ptd-msf[period.pnum] +
                                          (ar-invl.inv-qty * itemfg.t-sqft / 1000).
          END.
        END.

        ld = (IF ar-inv.gross GT ar-inv.net THEN ar-inv.gross
              ELSE ar-inv.net) - ar-inv.tax-amt.

        IF ld EQ ? THEN ld = 0.

        cust.sales[period.pnum] = cust.sales[period.pnum] + ld.
      END.

      ELSE
      FOR EACH ar-cash
          WHERE ar-cash.company    EQ ar-ledger.company
            AND ar-cash.cust-no    EQ ar-ledger.cust-no
            AND ar-cash.check-date EQ ar-ledger.ref-date
            AND ar-cash.posted     EQ YES
            AND ar-cash.check-no   EQ li
          USE-INDEX ar-cash NO-LOCK, 

          EACH ar-cashl
          WHERE ar-cashl.c-no EQ ar-cash.c-no
          USE-INDEX c-no NO-LOCK:

        cust.sales[period.pnum] = cust.sales[period.pnum] +
                                  (ar-cashl.amt-paid - ar-cashl.amt-disc).
      END.
  END.
END.

