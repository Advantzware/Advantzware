/* --------------------------------------------------- ar/ar-creg.i 03/97 JLF */
/* A/R Cash Receipts Edit Register Print Program - A/R Module                 */
/* -------------------------------------------------------------------------- */

  FOR EACH ar-cash
      WHERE ar-cash.company    EQ cocode
        AND ar-cash.posted     EQ NO
        AND ar-cash.memo       EQ NO
        AND ar-cash.cust-no    GE begin_cust
        AND ar-cash.cust-no    LE END_cust
        AND ar-cash.check-date GE v-from-date 
        AND ar-cash.check-date LE v-to-date 
        AND CAN-FIND(FIRST cust
                     {sys/ref/custW.i}
                       AND cust.cust-no EQ ar-cash.cust-no)
        AND CAN-FIND(FIRST bank
                     WHERE bank.company   EQ cocode
                       AND bank.bank-code EQ ar-cash.bank-code)
        AND CAN-FIND(FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no)
      USE-INDEX posted NO-LOCK
      BREAK BY ar-cash.{1} BY ar-cash.check-no WITH FRAME a1{2}:

      IF FIRST-OF(ar-cash.{1}) THEN DO:
         FIND FIRST cust {sys/ref/custW.i} AND cust.cust-no = ar-cash.cust-no
         NO-LOCK NO-ERROR.
         IF AVAIL cust THEN
            PUT cust.cust-no SPACE(1) cust.name FORMAT "x(25)".
         ELSE
            PUT ar-cash.cust-no SPACE(1) "Error - Customer Not On File".
      END.
      ELSE IF FIRST-OF(ar-cash.check-no) AND sort-by-cust THEN PUT SKIP(1).

      CREATE tt-post.
      ASSIGN
       tt-post.row-id   = ROWID(ar-cash)
       tt-post.curr-amt = ar-cash.check-amt.
    
      RELEASE currency.
      IF lv-comp-curr NE "" AND lv-comp-curr NE ar-cash.curr-code[1] THEN
      FIND FIRST currency NO-LOCK
          WHERE currency.company     EQ ar-cash.company
            AND currency.c-code      EQ ar-cash.curr-code[1]
            AND currency.ar-ast-acct NE ""
            AND currency.ex-rate     GT 0
          NO-ERROR.

      IF AVAIL currency THEN
        ASSIGN
         tt-post.actnum   = currency.ar-ast-acct
         tt-post.ex-rate  = currency.ex-rate
         tt-post.curr-amt = tt-post.curr-amt * tt-post.ex-rate.

      PUT ar-cash.check-no      AT 38 FORMAT ">>>>>>>>>>"
          ar-cash.check-date    AT 49
          tt-post.curr-amt      AT 57 FORMAT "->>>,>>9.99".
      v2 = v2 + tt-post.curr-amt.

      for each ar-cashl where ar-cashl.c-no = ar-cash.c-no NO-LOCK
         break by ar-cashl.inv-no with frame a2{2} no-box no-labels width 200:

         if ar-cashl.inv-no ne 0 then do:
            put ar-cashl.inv-no at 70 format ">>>>>9".
            find first ar-inv where ar-inv.company = ar-cashl.company and
                                    ar-inv.inv-no  = ar-cashl.inv-no
                                    use-index inv-no no-lock no-error.
            if available ar-inv then do:
               put ar-inv.due * tt-post.ex-rate at 80 format "->>>,>>9.99".
               assign
                v-amt-due-sub = v-amt-due-sub + (ar-inv.due * tt-post.ex-rate).
/*
                v-on-act-amt  = ar-cashl.amt-paid - ar-inv.due.
*/
            end.
         end. /* if ar-cashl.inv-no ne 0 */
         else v-on-act-amt = ar-cashl.amt-paid * tt-post.ex-rate.

         if ar-cashl.amt-paid ne 0 then
            put ar-cashl.amt-paid * tt-post.ex-rate at 94 format "->>>,>>9.99".
         if ar-cashl.amt-disc ne 0 then
            put ar-cashl.amt-disc * tt-post.ex-rate at 109 format "->>,>>9.99".
         if v-on-act-amt ne 0 then do:
            put v-on-act-amt to 131.
            assign v-on-act-sub = v-on-act-sub + v-on-act-amt
                   v-on-act-amt = 0.
         end.
         put skip.
         assign
          v-amt-paid-sub = v-amt-paid-sub +
                           (ar-cashl.amt-paid * tt-post.ex-rate)
          v-disc-sub     = v-disc-sub     +
                           (ar-cashl.amt-disc * tt-post.ex-rate)
          dsc            = dsc            +
                           (ar-cashl.amt-disc * tt-post.ex-rate)
          net-cr         = net-cr         +
                           ((ar-cashl.amt-paid - ar-cashl.amt-disc) * tt-post.ex-rate).
      end. /* each ar-cashl */

      if last-of(ar-cash.{1}) then do:
         if sort-by-cust then
            display skip(1)
                    "***** CUSTOMER TOTALS:"        to 52
                                     v2             at 54
                                     v-amt-due-sub  at 78
                                     v-amt-paid-sub at 92
                                     v-disc-sub     at 108
                                     v-on-act-sub   to 131
                                     skip(1)
                with frame vtot{2} no-box no-labels width 200 STREAM-IO.

         assign
          g1             = g1 + v1
          g2             = g2 + v2
          v-amt-due-tot  = v-amt-due-tot + v-amt-due-sub
          v-amt-due-sub  = 0
          v-amt-paid-tot = v-amt-paid-tot + v-amt-paid-sub
          v-amt-paid-sub = 0
          v-disc-tot     = v-disc-tot + v-disc-sub
          v-disc-sub     = 0
          v-on-act-tot   = v-on-act-tot + v-on-act-sub
          v-on-act-sub   = 0
          v1             = 0
          v2             = 0.
      end.
  end. /* for each ar-cash */

  display skip(1)
          "***** GRAND TOTALS:"        to 52
          g2             at 54
          v-amt-due-tot  at 78
          v-amt-paid-tot at 92
          v-disc-tot     at 108
          v-on-act-tot   to 131
          skip(1)
          with frame gtot{2} no-box no-labels width 200 STREAM-IO.

/* end ----------------------------------- Copr. 1997  Advanced Software Inc. */

