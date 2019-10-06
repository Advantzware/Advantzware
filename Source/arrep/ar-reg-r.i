/* --------------------------------------------------- ar/ar-creg.i 03/97 JLF */
/* A/R Cash Receipts Edit Register Print Program - A/R Module                 */
/* -------------------------------------------------------------------------- */
FOR EACH ar-cash
      WHERE ar-cash.company    EQ cocode
        AND ar-cash.posted     EQ is-print-posted
        AND ar-cash.memo       EQ NO
        AND ar-cash.cust-no  GE fcust
        AND ar-cash.cust-no  LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-cash.cust-no
        AND ttCustList.log-fld no-lock) else true)
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

         IF tb_excel THEN
           DO:
             IF AVAIL cust THEN
             PUT STREAM excel UNFORMATTED
                 '"' cust.cust-no     '",'
                 '"' cust.NAME         '",'.
             ELSE
               PUT STREAM excel UNFORMATTED
                   '"' ""  '",'
                   '"' "Error - Customer Not On File"  '",'.
         END.
      END.
      ELSE IF FIRST-OF(ar-cash.check-no) AND sort-by-cust THEN DO:
          PUT SKIP(1).
          IF tb_excel THEN DO:
              PUT STREAM excel SKIP(1).
              PUT STREAM excel ",,".
          END. 
      END.
          

      

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
          tt-post.curr-amt      AT 59 FORMAT /* "->>>,>>9.99"*/
          "->>>,>>9.99".
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
           "" + STRING(ar-cash.check-no, ">>>>>>>>>>") + "," +
           "" + (IF ar-cash.check-date = ? THEN "" ELSE
                 STRING(ar-cash.check-date)) + "," +
           "" + STRING(tt-post.curr-amt, "->>>>>9.99") + "," /* skip */.

      v2 = v2 + tt-post.curr-amt.
      iLine-count = 1.
      for each ar-cashl where ar-cashl.c-no = ar-cash.c-no NO-LOCK
         break by ar-cashl.inv-no with frame a2{2} no-box no-labels width 200:

         if ar-cashl.inv-no ne 0 then do:
            put ar-cashl.inv-no at 71 format ">>>>>9".
            IF tb_excel THEN DO:
              IF iLINE-COUNT > 1 THEN
                PUT STREAM excel UNFORMATTED ",,,,," .
              PUT STREAM excel UNFORMATTED
               /* ",,,,," + */ STRING(ar-cashl.inv-no, ">>>>>9") + ",".
            END.
            find first ar-inv where ar-inv.company = ar-cashl.company and
                                    ar-inv.inv-no  = ar-cashl.inv-no
                                    use-index inv-no no-lock no-error.
            if available ar-inv then do:
               put ar-inv.net at 80 format "->>>,>>9.99".
               IF tb_excel THEN
                 PUT STREAM excel UNFORMATTED
                    "" + STRING(ar-inv.net, "->>>>>9.99") + ",".
               assign
                v-amt-due-sub = v-amt-due-sub + (ar-inv.net).
/*
                v-on-act-amt  = ar-cashl.amt-paid - ar-inv.due.
*/
            end.
            ELSE 
            IF tb_excel THEN
              PUT STREAM excel UNFORMATTED ",".

         end. /* if ar-cashl.inv-no ne 0 */
         ELSE DO: 
            IF tb_excel THEN DO:
              IF iLine-count = 1 THEN
                PUT STREAM excel UNFORMATTED ",," /* ",,,,,,," */.
              ELSE 
                PUT STREAM excel UNFORMATTED ",,,,,,,".
            END.
             v-on-act-amt = ar-cashl.amt-paid * tt-post.ex-rate.
         END.

         iLINE-COUNT = iLINE-COUNT + 1.

         if ar-cashl.amt-paid + (ar-cashl.amt-disc * tt-post.ex-rate)
             + v-on-act-amt ne 0 THEN DO:
         
            put (ar-cashl.amt-paid * tt-post.ex-rate )
                + (ar-cashl.amt-disc * tt-post.ex-rate)
                /*+ v-on-act-amt */
                at 94 format "->>>,>>9.99".

            IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                 "" + STRING((ar-cashl.amt-paid * tt-post.ex-rate )
                           + (ar-cashl.amt-disc * tt-post.ex-rate)
                          /* + v-on-act-amt, "->>>>>9.99" */) + ",".
         END.
         ELSE 
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED ",".

         if ar-cashl.amt-disc ne 0 THEN DO:
           put ar-cashl.amt-disc * tt-post.ex-rate at 109 format "->>,>>9.99".
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                "" + STRING(ar-cashl.amt-disc * tt-post.ex-rate, "->>>>9.99") + ",".
         END.
         ELSE 
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED ",".
         assign
          v-amt-paid-sub = v-amt-paid-sub 
                          + (ar-cashl.amt-paid * tt-post.ex-rate )
                          + (ar-cashl.amt-disc * tt-post.ex-rate)
                          /* + v-on-act-amt */.

         if v-on-act-amt ne 0 then do:
            put v-on-act-amt to 131.
            IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                 "" + STRING(v-on-act-amt, "->>>>>9.99") + ",".
            assign v-on-act-sub = v-on-act-sub + v-on-act-amt
                   v-on-act-amt = 0.
         end.
         ELSE 
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED ",".

         put skip.
         IF tb_excel THEN
             PUT STREAM excel SKIP.
         
         ASSIGN
           v-disc-sub     = v-disc-sub     +
                            (ar-cashl.amt-disc * tt-post.ex-rate)
           dsc            = dsc            +
                            (ar-cashl.amt-disc * tt-post.ex-rate)
           net-cr          = net-cr         +
                            ((ar-cashl.amt-paid - ar-cashl.amt-disc) * tt-post.ex-rate).
         
      end. /* each ar-cashl */

      if last-of(ar-cash.{1}) then do:
         if sort-by-cust then
            display skip(1)
                    "***** CUSTOMER TOTALS:"        to 52
                                     v2             at 56
                                     v-amt-due-sub  at 78
                                     v-amt-paid-sub at 92
                                     v-disc-sub     at 108
                                     v-on-act-sub   to 131
                                     skip(1)
                with frame vtot{2} no-box no-labels width 200 STREAM-IO.

         IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                "," + "***** CUSTOMER TOTALS:" + ",,,"
                "" + STRING(v2 ) + ",,"
                "" + STRING(v-amt-due-sub ) + ","
                 "" + STRING(v-amt-paid-sub ) + ","
                 "" + STRING(v-disc-sub ) + ","
                 "" + STRING(v-on-act-sub ) + ","  SKIP.


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
          g2             at 56
          v-amt-due-tot  at 78
          v-amt-paid-tot at 92
          v-disc-tot     at 108
          v-on-act-tot   to 131
          skip(1)
          with frame gtot{2} no-box no-labels width 200 STREAM-IO.

  IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                "," + "***** GRAND TOTALS:" + ",,,"
                "" + STRING(g2 ) + ",,"
                "" + STRING(v-amt-due-tot ) + ","
                 "" + STRING(v-amt-paid-tot ) + ","
                 "" + STRING(v-disc-tot ) + ","
                 "" + STRING(v-on-act-tot ) + ","  SKIP.

/* end ----------------------------------- Copr. 1997  Advanced Software Inc. */

