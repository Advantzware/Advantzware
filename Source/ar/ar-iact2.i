
assign
 v-pay-stat1 = no
 v-pay-stat2 = "".

if ar-cashl.memo then t-check-no = "CR Memo".


if ar-cashl.amt-disc ne 0 then DO:
   assign
   t-check-no = "Discount"
   t-credits  = ar-cashl.amt-disc
   t-balance  = t-credits
   x-check-no = ar-cashl.check-no.  

   CREATE tt-arinq.
   ASSIGN li-seq            = li-seq + 1
          tt-arinq.ref-num  = x-check-no
          tt-arinq.tr-date  = ar-cash.check-date
          tt-arinq.tr-dscr  = t-check-no
          tt-arinq.tr-camt  = t-credits
          tt-arinq.balance  = t-balance
          tt-arinq.tr-from  = "AR Cash"
          tt-arinq.applied  = NO
          tt-arinq.seq      = li-seq
          tt-arinq.printed  = NO
          tt-arinq.posted   = YES
          .
   IF "{1}" = "1" THEN tt-arinq.ageapp = STRING(v-pay-stat1).
   ELSE tt-arinq.ageapp = v-pay-stat2.
       
  assign
   t-balance = t-balance * -1
   v-tot-due = v-tot-due + t-balance.
  
  if "{1}" eq "2" then tt-arinq.balance = v-tot-due.
end.

if ar-cashl.amt-paid ne 0 then do:
  if ar-cashl.memo then
    t-debits  = ar-cashl.amt-paid .
  else
    t-credits = ar-cashl.amt-paid - ar-cashl.amt-disc.
    
  if not ar-cashl.memo then
  DO:
    assign
     t-credits  = ar-cashl.amt-paid
     t-balance  = t-credits
     t-check-no = "Payment"
     v-gltrans-desc = "VOID " + cust.cust-no + " "
                    +  STRING(ar-cash.check-no,"9999999999")
                    + " Inv# " + STRING(ar-cashl.inv-no).

     IF t-credits LT 0 AND
        ar-cashl.voided EQ YES OR
        can-find(FIRST gltrans WHERE
           gltrans.company EQ cocode AND
           gltrans.jrnl EQ "CASHRVD" AND
           gltrans.tr-dscr EQ v-gltrans-desc) THEN
        t-check-no = "Void".
  END.
     
  x-check-no = ar-cashl.check-no.
   
  if ar-cashl.memo and ar-cashl.dscr begins "debit" then do:
    assign
     t-check-no = "DB Memo"
     t-balance  = t-debits.

    CREATE tt-arinq.
    ASSIGN li-seq            = li-seq + 1
           tt-arinq.ref-num  = x-check-no
           tt-arinq.tr-dscr  = t-check-no             
           tt-arinq.tr-damt  = t-debits
           tt-arinq.balance  = t-balance
           tt-arinq.tr-from  = "AR Cash"
           tt-arinq.applied  = NO
           tt-arinq.seq      = li-seq
           tt-arinq.printed  = NO
           tt-arinq.posted   = YES.

      IF t-check-no EQ "Payment" THEN
         tt-arinq.tr-date  = ar-cash.check-date.
      ELSE
      DO:
         IF ar-cashl.voided THEN
            tt-arinq.tr-date = ar-cashl.voidDate.
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
   
  else do:

     IF t-check-no = "Payment" AND
        ar-cashl.inv-no EQ 0 AND ar-cashl.amt-paid LT 0 THEN
        ASSIGN
           t-credits = ar-cashl.amt-paid
           t-balance = t-credits.
     ELSE
     DO:
        ASSIGN
           t-credits = ar-cashl.amt-paid * -1
           t-balance = t-credits.
       
        IF t-check-no = "Payment" AND t-credits LT 0 THEN
           ASSIGN
              t-credits = t-credits * -1
              t-balance = t-credits.
     END.

     CREATE tt-arinq.
     ASSIGN li-seq            = li-seq + 1
            tt-arinq.ref-num  = x-check-no
            tt-arinq.tr-dscr  = t-check-no             
            tt-arinq.tr-camt  = t-credits
            tt-arinq.balance  = t-balance
            tt-arinq.tr-from  = "AR Cash"
            tt-arinq.applied  = NO
            tt-arinq.seq      = li-seq
            tt-arinq.printed  = NO
            tt-arinq.posted   = YES.

     IF t-check-no EQ "Payment" THEN
        tt-arinq.tr-date  = ar-cash.check-date.
     ELSE
     DO:
        IF ar-cashl.voided THEN
             tt-arinq.tr-date = ar-cashl.voidDate.
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
     t-balance = t-balance * -1.
  end.
  
  v-tot-due = v-tot-due + t-balance.
  
  if "{1}" eq "2" then tt-arinq.balance = v-tot-due.
end. /* if ar-cashl.amt-paid ne 0 */
