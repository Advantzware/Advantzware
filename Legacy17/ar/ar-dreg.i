/* --------------------------------------------------- ar/ar-dreg.i 09/12 JLF */
/* A/R Credit/Debit Memo Edit Register Program - A/R Module  (post include)   */
/* -------------------------------------------------------------------------- */
      DEF BUFFER b-reftable FOR reftable.


      ASSIGN ar-cash.posted = YES.
      /* if ar-cashl.amt-disc = 0 then ar-cashl.dscr = "debit". */
      FIND FIRST cust {ar/ar-custW.i} AND
        cust.cust-no = ar-cash.cust-no EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAILABLE cust THEN
      DO:
        MESSAGE "Invalid Customer Number, Post Will Be ABORTED! " VIEW-AS ALERT-BOX ERROR.
        UNDO postit, LEAVE postit.
      END.

      ASSIGN t1           = 0
        v-on-act-amt = 0.

      FOR EACH ar-cashl WHERE ar-cashl.c-no = ar-cash.c-no USE-INDEX c-no:
        ASSIGN
        ar-cashl.posted = YES
        ar-cashl.dscr   = TRIM(IF ar-cashl.amt-disc - ar-cashl.amt-paid LT 0
                               THEN "Debit" ELSE "Credit") +
                          " - " + ar-cashl.dscr.

        FIND FIRST ar-inv EXCLUSIVE-LOCK
            WHERE ar-inv.company EQ cocode
              AND ar-inv.cust-no EQ ar-cash.cust-no
              AND ar-inv.inv-no  EQ ar-cashl.inv-no
            USE-INDEX ar-inv NO-ERROR.
        IF AVAILABLE ar-inv THEN
          ASSIGN
           ar-inv.paid = ar-inv.paid - ar-cashl.amt-paid + ar-cashl.amt-disc
           ar-inv.due  = (IF ar-inv.net EQ ar-inv.gross + ar-inv.freight +
                                           ar-inv.tax-amt THEN ar-inv.net ELSE
                                                               ar-inv.gross) -
                         ar-inv.paid.

        ELSE DO:
          ASSIGN
           v-on-act-amt        = v-on-act-amt + ar-cashl.amt-disc - ar-cashl.amt-paid
           ar-cashl.on-account = YES.

          /*FIND FIRST b-reftable
              WHERE b-reftable.reftable EQ "ar-cashl.net-paid-due"
                AND b-reftable.company  EQ ar-cash.company
                AND b-reftable.loc      EQ ""
                AND b-reftable.code     EQ STRING(ar-cashl.c-no,"9999999999")
                AND b-reftable.code2    EQ STRING(ar-cashl.line,"9999999999")
              NO-ERROR.
          IF NOT AVAIL b-reftable THEN CREATE b-reftable.
          ASSIGN
           b-reftable.reftable = "ar-cashl.net-paid-due"
           b-reftable.company  = ar-cash.company
           b-reftable.loc      = ""
           b-reftable.code     = STRING(ar-cashl.c-no,"9999999999")
           b-reftable.code2    = STRING(ar-cashl.line,"9999999999").*/
        END.

        RELEASE ar-inv.

        t1 = t1 + ar-cashl.amt-paid - ar-cashl.amt-disc.

        CREATE gltrans.
        ASSIGN
          gltrans.company = cocode
          gltrans.actnum  = ar-cashl.actnum
          gltrans.jrnl    = "CRMEM"
          gltrans.tr-dscr = cust.name  + " " +
          STRING(ar-cash.check-no,"99999999")
          gltrans.tr-date = tran-date
          gltrans.tr-amt  = ar-cashl.amt-disc - ar-cashl.amt-paid
          gltrans.period  = tran-period
          gltrans.trnum   = xtrnum.
        IF gltrans.tr-amt < 0 THEN
          ASSIGN  gltrans.jrnl = "DBMEM".
               /* gltrans.actnum = xar-acct    DAR */

        find first bank where bank.company = cocode and
                              bank.actnum = ar-cashl.actnum no-error.
        if avail bank then
          assign bank.bal = bank.bal + gltrans.tr-amt.
        RELEASE bank.
      END.  /* each line */

      ASSIGN cust.sales[tran-period] = cust.sales[tran-period] + t1
/*
        cust.sales[13]      = cust.sales[13]      + t1
*/
       cust.ytd-sales      = cust.ytd-sales      + t1
        cust.acc-bal        = cust.acc-bal        + t1
        cust.on-account     = cust.on-account     + v-on-act-amt.
      IF cust.acc-bal >= cust.hibal
        THEN
      ASSIGN cust.hibal      = cust.acc-bal
             cust.hibal-date = ar-cash.check-date.
      RELEASE cust.

      g2 = g2 + t1.
      CREATE ar-ledger.
      ASSIGN
        ar-ledger.company  = cocode
        ar-ledger.cust-no  = ar-cash.cust-no
        ar-ledger.amt      = t1
        ar-ledger.ref-num  = "Memo#" +
        STRING(ar-cash.check-no,"99999999") +
        "A/R"
        ar-ledger.ref-date = ar-cash.check-date
        ar-ledger.tr-date  = tran-date
        ar-ledger.tr-num   = xtrnum
        t1                 = 0.

/* End ---------------------------------- Copr. 1996  Advanced Software, Inc. */

