
assign
 t-credits = 0
 t-debits  = 0
 t-balance = 0.  /* resets so balance is for current invoice only */
     
ll-valid = ar-inv.due NE 0 OR NOT v-open.

IF ll-valid AND (begin_chk NE 0 OR end_chk NE 2147483647) THEN DO:
  ll-valid = NO.

  FOR EACH ar-cashl
      WHERE ar-cashl.company EQ cocode
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.cust-no EQ cust.cust-no
        AND ar-cashl.inv-no  EQ ar-inv.inv-no
      NO-LOCK,      
      FIRST ar-cash
      WHERE ar-cash.c-no     EQ ar-cashl.c-no 
        AND ar-cash.check-no GE INT(begin_chk)
        AND ar-cash.check-no LE end_chk
      NO-LOCK:
    ll-valid = YES.
    LEAVE.
  END.
END.
 
IF ll-valid THEN DO:
  if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
    t-debits = ar-inv.net.
  else
    t-debits = ar-inv.gross.

  assign
   t-check-no = "Invoice"
   t-balance  = t-balance + t-debits
   v-tot-due  = v-tot-due + t-balance.

  CREATE tt-arinq.
  ASSIGN li-seq            = li-seq + 1
         tt-arinq.inv-no   = ar-inv.inv-no
         tt-arinq.tr-date  = ar-inv.inv-date
         tt-arinq.tr-dscr  = t-check-no
         tt-arinq.tr-damt  = t-debits
         tt-arinq.balance  = t-balance
         tt-arinq.tr-from  = "AR Invoice"
         tt-arinq.applied  = YES
         tt-arinq.seq      = li-seq
         tt-arinq.printed  = IF AVAILABLE ar-inv THEN ar-inv.printed ELSE NO
         tt-arinq.posted   = IF AVAILABLE ar-inv THEN ar-inv.posted ELSE NO.

  if "{1}" eq "2" then do:
    find first ar-invl
        where ar-invl.x-no  eq ar-inv.x-no
          and ar-invl.po-no ne ""
        no-lock no-error.
  
    v-pay-stat2 = trim(string(today - ar-inv.inv-date,"->>>>")).

    if ar-inv.due eq 0 then
    for each ar-cashl
        where ar-cashl.company eq cocode
          and ar-cashl.posted  eq yes
          and ar-cashl.cust-no eq cust.cust-no
          and ar-cashl.inv-no  eq ar-inv.inv-no
        no-lock,      
        first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock
        BY ar-cash.check-date BY ar-cash.c-no:
      v-pay-stat2 = trim(string(ar-cash.check-date - ar-inv.inv-date,"->>>>")).
    END.

    ASSIGN tt-arinq.ref-num = IF AVAIL ar-invl THEN ar-invl.po-no ELSE ""
           tt-arinq.ageapp = v-pay-stat2
           tt-arinq.balance = v-tot-due.
  end.
  
  for each ar-cashl
      where ar-cashl.company eq cocode
        and ar-cashl.posted  eq yes
        and ar-cashl.cust-no eq cust.cust-no
        and ar-cashl.inv-no  eq ar-inv.inv-no
      no-lock,      
      first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock
      BY ar-cash.check-date BY ar-cash.c-no:
      
    ASSIGN
     v-pay-stat1 = yes
     v-pay-stat2 = "yes"
     t-check-no  = if ar-cashl.memo then "CR Memo" else "Discount".
     
    if ar-cashl.amt-disc ne 0 then do with frame a2-{1}:
      assign
       t-check-no = "Discount"
       t-credits  = ar-cashl.amt-disc
       t-balance  = t-balance - t-credits
       x-check-no = ar-cashl.check-no.

      if ar-cash.memo and ar-cashl.amt-disc gt 0 then t-check-no = "Return".
      CREATE tt-arinq.
      ASSIGN li-seq            = li-seq + 1
             tt-arinq.ref-num  = x-check-no
             tt-arinq.inv-no   = ar-cashl.inv-no
             tt-arinq.tr-date  = ar-cash.check-date
             tt-arinq.tr-dscr  = t-check-no             
             tt-arinq.tr-camt  = t-credits
             tt-arinq.balance  = t-balance
             tt-arinq.tr-from  = "AR Cash"
             tt-arinq.applied  = YES
             tt-arinq.seq      = li-seq
             tt-arinq.printed  = IF AVAILABLE ar-inv THEN ar-inv.printed ELSE NO
             tt-arinq.posted   = IF AVAILABLE ar-inv THEN ar-inv.posted ELSE NO.

      IF "{1}" = "1" THEN tt-arinq.ageapp = STRING(v-pay-stat1).
      ELSE tt-arinq.ageapp = v-pay-stat2.
      
      v-tot-due = v-tot-due - t-credits.
              
      if "{1}" eq "2" then
         tt-arinq.balance = v-tot-due.
    end.

    if ar-cashl.amt-paid ne 0 then DO:
    
      t-check-no = "Payment".
      
      if ar-cashl.memo then do:
        if ar-cashl.amt-paid lt 0 then
          t-credits = ar-cashl.amt-paid * -1.
        ELSE
          t-debits  = ar-cashl.amt-paid.        
      end.
      
      else
      DO:
         ASSIGN t-credits = ar-cashl.amt-paid
                t-check-no = "Payment"
                v-gltrans-desc = "VOID " + cust.cust-no + " " +
                               STRING(ar-cash.check-no,"9999999999") +
                               " Inv# " + STRING(ar-cashl.inv-no).
         IF t-credits LT 0 AND
            (CAN-FIND(FIRST reftable WHERE
            reftable.reftable = "ARCASHLVDDATE" AND
            reftable.rec_key = ar-cashl.rec_key
            USE-INDEX rec_key)) OR
            can-find(FIRST gltrans WHERE
               gltrans.company EQ cocode AND
               gltrans.jrnl EQ "CASHRVD" AND
               gltrans.tr-dscr EQ v-gltrans-desc) THEN
               t-check-no = "Void".

         IF ar-cashl.inv-no EQ 0 AND t-check-no EQ "Payment" AND t-credits LT 0 THEN
            t-credits = t-credits * -1.
      END.

      if not ar-cashl.memo then
        assign
         v-tot-due = v-tot-due - t-credits
         t-balance = t-balance - t-credits.

      x-check-no = ar-cashl.check-no.

      if ar-cashl.memo then do:
        if ar-cashl.amt-paid lt 0 then
          assign
           v-tot-due  = v-tot-due - t-credits
           t-balance  = t-balance - t-credits
           t-debits   = 0
           t-check-no = "CR Memo".
        else
         assign
          v-tot-due  = v-tot-due + t-debits
          t-check-no = "DB Memo"
          t-credits  = 0
          t-balance  = t-balance + t-debits.
      
        CREATE tt-arinq.
        ASSIGN li-seq            = li-seq + 1
               tt-arinq.ref-num  = x-check-no
               tt-arinq.inv-no   = ar-cashl.inv-no
               tt-arinq.tr-dscr  = t-check-no             
               tt-arinq.balance  = t-balance
               tt-arinq.tr-from  = "AR Cash"
               tt-arinq.applied  = YES
               tt-arinq.seq      = li-seq
               tt-arinq.printed  = IF AVAILABLE ar-inv THEN ar-inv.printed ELSE NO
               tt-arinq.posted   = IF AVAILABLE ar-inv THEN ar-inv.posted ELSE NO.

        IF t-check-no EQ "Payment" THEN
           tt-arinq.tr-date  = ar-cash.check-date.
        ELSE
        DO:
           FIND FIRST reftable WHERE
                reftable.reftable EQ "ARCASHLVDDATE" AND
                reftable.rec_key EQ ar-cashl.rec_key
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.

           IF AVAIL reftable THEN
              tt-arinq.tr-date = DATE(reftable.CODE).
           ELSE
           DO:
              FIND FIRST gltrans WHERE
                   gltrans.company EQ cocode AND
                   gltrans.jrnl EQ "CASHRVD" AND
                   gltrans.tr-dscr EQ v-gltrans-desc
                   NO-LOCK NO-ERROR.

              IF AVAIL gltrans THEN
                 tt-arinq.tr-date = gltrans.tr-date.
              ELSE
                 tt-arinq.tr-date = ar-cash.check-date.
           END.
        END.

        IF "{1}" = "1" THEN tt-arinq.ageapp = STRING(v-pay-stat1).
        ELSE tt-arinq.ageapp = v-pay-stat2.

        IF t-check-no = "CR Memo" THEN tt-arinq.tr-camt = t-credits.
        IF t-debits <> 0 THEN tt-arinq.tr-damt = t-debits.
      end.
      
      else do:
         CREATE tt-arinq.
         ASSIGN li-seq            = li-seq + 1
                tt-arinq.ref-num  = x-check-no
                tt-arinq.inv-no   = ar-cashl.inv-no
                tt-arinq.tr-dscr  = t-check-no        
                tt-arinq.tr-camt  = t-credits
                tt-arinq.balance  = t-balance
                tt-arinq.tr-from  = "AR Cash"
                tt-arinq.applied  = YES
                tt-arinq.seq      = li-seq
                tt-arinq.printed  = IF AVAILABLE ar-inv THEN ar-inv.printed ELSE NO
                tt-arinq.posted   = IF AVAILABLE ar-inv THEN ar-inv.posted ELSE NO.

        IF t-check-no EQ "Payment" THEN
           tt-arinq.tr-date  = ar-cash.check-date.
        ELSE
        DO:
           FIND FIRST reftable WHERE
                reftable.reftable EQ "ARCASHLVDDATE" AND
                reftable.rec_key EQ ar-cashl.rec_key
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.

           IF AVAIL reftable THEN
              tt-arinq.tr-date = DATE(reftable.CODE).
           ELSE
           DO:
              FIND FIRST gltrans WHERE
                   gltrans.company EQ cocode AND
                   gltrans.jrnl EQ "CASHRVD" AND
                   gltrans.tr-dscr EQ v-gltrans-desc
                   NO-LOCK NO-ERROR.

              IF AVAIL gltrans THEN
                 tt-arinq.tr-date = gltrans.tr-date.
              ELSE
                 tt-arinq.tr-date = ar-cash.check-date.
           END.
        END.

        IF "{1}" = "1" THEN tt-arinq.ageapp = STRING(v-pay-stat1).
        ELSE tt-arinq.ageapp = v-pay-stat2.
      end.
      
      if "{1}" eq "2" then
         tt-arinq.balance = v-tot-due.
    end.
  end. /* ar-cashl */
end.

