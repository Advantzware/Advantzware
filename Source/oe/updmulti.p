
DEF PARAM BUFFER io-inv-head FOR inv-head.
DEF PARAM BUFFER old-io-inv-head FOR inv-head.

DEF BUFFER inv-mult FOR inv-head.

DEF VAR ll-mult AS LOG NO-UNDO.
DEF VAR lv-r-no LIKE inv-head.r-no NO-UNDO.
DEF VAR ll-any-freight AS LOG NO-UNDO.
DEF VAR llBolMatched AS LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF inv-mult.
DISABLE TRIGGERS FOR LOAD OF inv-head.
DISABLE TRIGGERS FOR LOAD OF io-inv-head.
DISABLE TRIGGERS FOR LOAD OF old-io-inv-head.

IF AVAIL io-inv-head THEN DO:
  ll-mult = CAN-FIND(FIRST cust
                     WHERE cust.company  EQ io-inv-head.company
                       AND cust.cust-no  EQ io-inv-head.cust-no
                       AND cust.inv-meth EQ ?).

  FIND FIRST inv-mult
      WHERE inv-mult.company       EQ io-inv-head.company
        AND inv-mult.inv-no        EQ io-inv-head.inv-no
        AND inv-mult.cust-no       EQ io-inv-head.cust-no
        AND inv-mult.multi-invoice EQ YES
      NO-ERROR.

  IF AVAIL inv-mult THEN DO:
     IF io-inv-head.multi-invoice                    AND
        io-inv-head.inv-no NE old-io-inv-head.inv-no THEN
     FOR EACH inv-head
         WHERE inv-head.company       EQ io-inv-head.company
           AND inv-head.inv-no        EQ old-io-inv-head.inv-no
           AND inv-head.cust-no       EQ io-inv-head.cust-no
           AND inv-head.multi-invoice EQ NO
           AND inv-head.stat NE "H":
       inv-head.inv-no = io-inv-head.inv-no.
     END.
    
     IF (NOT ll-mult OR
         CAN-FIND(FIRST inv-head
                  WHERE inv-head.company       EQ io-inv-head.company
                    AND inv-head.inv-no        EQ io-inv-head.inv-no
                    AND inv-head.cust-no       EQ io-inv-head.cust-no
                    AND inv-head.multi-invoice EQ YES
                    AND ROWID(inv-head)        NE ROWID(inv-mult))) THEN
     FOR EACH inv-head
         WHERE inv-head.company       EQ io-inv-head.company
           AND inv-head.inv-no        EQ io-inv-head.inv-no
           AND inv-head.cust-no       EQ io-inv-head.cust-no
           AND inv-head.multi-invoice EQ YES
           AND (ROWID(inv-head)       NE ROWID(inv-mult) OR ll-mult):
       DELETE inv-head.
     END.
  END.
END.

IF ll-mult THEN DO:
  IF NOT AVAIL inv-mult THEN DO:

    lv-r-no = next-value(inv_r_no_seq).

    CREATE inv-mult.
    BUFFER-COPY io-inv-head EXCEPT rec_key TO inv-mult
    ASSIGN
     inv-mult.r-no          = lv-r-no
     inv-mult.multi-invoice = YES
     inv-mult.stat          = "H".
  END.

  ELSE
  IF NOT io-inv-head.multi-invoice            AND
     old-io-inv-head.r-no NE 0                AND
     io-inv-head.stat NE old-io-inv-head.stat AND
     index(PROGRAM-NAME(3),"hold-invoice oe/v-oeinv") EQ 0 THEN
    inv-mult.stat = io-inv-head.stat.

  ASSIGN
   inv-mult.t-comm        = 0
   inv-mult.t-inv-freight = 0
   inv-mult.t-inv-fuel    = 0
   inv-mult.t-inv-tax     = 0
   inv-mult.t-inv-cost    = 0
   inv-mult.t-inv-rev     = 0
   inv-mult.t-inv-weight  = 0
   ll-any-freight         = NO.

  IF CAN-FIND(FIRST inv-head
              WHERE inv-head.company       EQ inv-mult.company
                AND inv-head.inv-no        EQ inv-mult.inv-no
                AND inv-head.cust-no       EQ inv-mult.cust-no
                AND inv-head.multi-invoice EQ NO
                AND (TRIM(PROGRAM-NAME(2)) NE "delete.trg/inv-head.p" OR
                     (ROWID(inv-head)      NE ROWID(io-inv-head)))) THEN DO:
      
      llBolMatched = NO.
      FOR EACH inv-head
          WHERE inv-head.company       EQ inv-mult.company
            AND inv-head.inv-no        EQ inv-mult.inv-no
            AND inv-head.cust-no       EQ inv-mult.cust-no
            AND inv-head.multi-invoice EQ NO
            AND inv-head.stat NE "H":
          IF inv-head.bol-no EQ inv-mult.bol-no THEN
              llBolMatched = TRUE.
      END.

      IF NOT llBolMatched THEN DO:
        /* Make the master have the sold-no of one of the released invs */
        FIND FIRST inv-head
          WHERE inv-head.company       EQ inv-mult.company
            AND inv-head.inv-no        EQ inv-mult.inv-no
            AND inv-head.cust-no       EQ inv-mult.cust-no
            AND inv-head.multi-invoice EQ NO
            AND inv-head.stat NE "H"
          NO-LOCK NO-ERROR.
        IF AVAIL inv-head THEN DO:
        
           BUFFER-COPY inv-head 
               EXCEPT rec_key r-no stat TO inv-mult.
           ASSIGN
               inv-mult.t-comm        = 0
               inv-mult.t-inv-freight = 0
               inv-mult.t-inv-fuel    = 0
               inv-mult.t-inv-tax     = 0
               inv-mult.t-inv-cost    = 0
               inv-mult.t-inv-rev     = 0
               inv-mult.t-inv-weight  = 0
               inv-mult.multi-invoice = TRUE
               ll-any-freight         = NO.
        END.
      END.

      FOR EACH inv-head
          WHERE inv-head.company       EQ inv-mult.company
            AND inv-head.inv-no        EQ inv-mult.inv-no
            AND inv-head.cust-no       EQ inv-mult.cust-no
            AND inv-head.multi-invoice EQ NO
            AND inv-head.stat NE "H":
        ASSIGN
         
         inv-head.printed       = inv-mult.printed
         inv-head.inv-no        = inv-mult.inv-no
         inv-mult.t-comm        = inv-mult.t-comm        + inv-head.t-comm
         
         inv-mult.t-inv-fuel    = inv-mult.t-inv-fuel    + inv-head.t-inv-fuel
         inv-mult.t-inv-tax     = inv-mult.t-inv-tax     + inv-head.t-inv-tax
         inv-mult.t-inv-cost    = inv-mult.t-inv-cost    + inv-head.t-inv-cost
         inv-mult.t-inv-rev     = inv-mult.t-inv-rev     + inv-head.t-inv-rev
         inv-mult.t-inv-weight  = inv-mult.t-inv-weight  + inv-head.t-inv-weight.
        IF inv-head.f-bill THEN
          inv-mult.t-inv-freight = inv-mult.t-inv-freight + inv-head.t-inv-freight.
        IF index(PROGRAM-NAME(3),"hold-invoice oe/v-oeinv") EQ 0 THEN
           inv-head.stat          = inv-mult.stat.
        IF inv-head.f-bill THEN
          ll-any-freight = TRUE.
      END.
      inv-mult.f-bill = ll-any-freight.
  END.

  ELSE IF NOT io-inv-head.multi-invoice THEN DELETE inv-mult.


  FOR EACH inv-mult
      WHERE inv-mult.company       EQ io-inv-head.company
        AND inv-mult.cust-no       EQ io-inv-head.cust-no
        AND inv-mult.multi-invoice EQ YES
        AND NOT CAN-FIND(FIRST inv-head
                         WHERE inv-head.company       EQ inv-mult.company
                           AND inv-head.inv-no        EQ inv-mult.inv-no
                           AND inv-head.multi-invoice EQ NO):
    DELETE inv-mult.
  END.
END.

