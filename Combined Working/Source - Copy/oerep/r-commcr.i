
    FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ cocode.

    for EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ cocode
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  GE v-cust[1]
          AND ar-inv.cust-no  LE v-cust[2]
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
          AND ttCustList.log-fld no-lock) else true)
          /*AND ar-inv.cust-no  LE v-cust[2]*/
          AND ar-inv.inv-date GE v-date[1]
          AND ar-inv.inv-date LE v-date[2],

        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no:

        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

      ld-date = ?.
      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
          USE-INDEX inv-no,

          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
          USE-INDEX c-no            
          BREAK BY ar-cash.check-date:
        /* Duplicate loop below just to get correct ld-date */
        IF NOT ar-cashl.memo THEN
          ASSIGN
           ld-date = ar-cash.check-date.
      END.

      IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
        v-amt = ar-inv.net.
      ELSE
        v-amt = ar-inv.gross.
      IF ld-date NE ? AND ld-date LE ar-inv.inv-date - ar-inv.disc-days THEN
        v-amt = v-amt - ROUND(v-amt * (ar-inv.disc-% / 100),2).

      IF v-amt EQ ? THEN v-amt = 0.

      ASSIGN
       v-amtd  = v-amt
       v-amtp  = v-amt
       ld-date = ?.

      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
          USE-INDEX inv-no,

          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
          USE-INDEX c-no
            
          BREAK BY ar-cash.check-date:

        IF ar-cashl.memo THEN    /* Just in case we change FOR EACH */
          IF ar-cashl.amt-disc NE 0 AND ar-cashl.amt-paid EQ 0 THEN
            ASSIGN
             v-amtd = v-amtd - ar-cashl.amt-disc
             v-amtp = v-amtp - ar-cashl.amt-disc.
          ELSE 
          IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
            ASSIGN
             v-amtd = v-amtd + (ar-cashl.amt-paid + ar-cashl.amt-disc)
             v-amtp = v-amtp + (ar-cashl.amt-paid + ar-cashl.amt-disc).
          ELSE
            ASSIGN
             v-amtd = v-amtd + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc)))
             v-amtp = v-amtp + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
        ELSE
          ASSIGN
           v-amtd  = v-amtd + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1))
           v-amtp  = v-amtp + (ar-cashl.amt-paid * -1)
           ld-date = ar-cash.check-date.
      END.

      ASSIGN
       v-amtd = v-amt - v-amtd
       v-amtp = v-amt - v-amtp.

      IF rd_inv EQ "All"                          OR
         (v-amtd LT v-amt AND rd_inv EQ "Unpaid") OR
         (v-amtd GE v-amt AND rd_inv EQ "Paid")   THEN DO:
        
        ld-inv-pct = 0.
        FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-inv.x-no NO-LOCK:
          ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
        END.
        ld-inv-pct = ld-inv-pct / v-amt.
        IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

        ASSIGN
         v-amt  = v-amt  * ld-inv-pct
         v-amtd = v-amtd * ld-inv-pct
         v-amtp = v-amtp * ld-inv-pct.

        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc):

          RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT ll-comp).
          IF ll-comp THEN NEXT.

          ld-inv-pct = 0.
          FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
            ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
            ACCUMULATE 1 (TOTAL).
          END.
          ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                          (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                       ELSE (ACCUM TOTAL 1))
                       ELSE (ar-invl.amt / ld-inv-pct).

          IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

          DO i = 1 TO 3:
            v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 AND NOT ar-invl.misc THEN
                          cust.sman ELSE ar-invl.sman[i].
          
            IF v-slsm[1]   LT v-sman[1]                     OR
               v-slsm[1]   GT v-sman[2]                     OR
               ((i NE 1 OR ar-invl.misc) AND
                (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

            ld-slsp = IF ar-invl.sman[i] EQ ""              OR
                         (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                      ELSE ar-invl.s-pct[i].

            FIND FIRST sman NO-LOCK
                WHERE sman.company EQ cocode
                  AND sman.sman    EQ v-slsm[1]
                NO-ERROR.

            CREATE tt-report.
            ASSIGN
             tt-report.key-01    = v-slsm[1]
             tt-report.key-02    = ar-inv.cust-no
             tt-report.key-03    = STRING(ar-inv.inv-no,"999999")
             tt-report.key-10    = "ar-invl"
             tt-report.rec-id    = RECID(ar-invl)
             tt-report.row-id    = ROWID(ar-invl)
             tt-report.qty       = ar-invl.inv-qty * ld-slsp / 100
             tt-report.amt       = v-amt * ld-inv-pct * ld-slsp / 100
             tt-report.amtd      = v-amtd * ld-inv-pct * ld-slsp / 100
             tt-report.amtp      = v-amtp * ld-inv-pct * ld-slsp / 100
             tt-report.delta     = tt-report.amt - tt-report.amtd
             tt-report.cost      = ar-invl.t-cost * ld-slsp / 100
             tt-report.comm      = IF ar-invl.sman[i] EQ "" AND AVAIL sman
                                   THEN sman.scomm ELSE ar-invl.s-comm[i]
             tt-report.cash-date = ld-date.

            /*IF ar-invl.misc AND NOT oe-ctrl.prep-comm THEN tt-report.comm = 0.*/
          END.
        END.
      END.
    END.

    ASSIGN
     str-tit3 = /*(IF v-per-rpt THEN "P" ELSE "Y") + "TD "*/
                "(" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +
                ") - By Sales Rep By Customer"
     {sys/inc/ctrtext.i str-tit3 132}

     excelheader = "Sman,Customer,Inv#,Inv Date,Cust Part#,Order#,Quantity,Inv Amt," +
                   "Date Inv Paid,Amt Paid,Delta,Comm Amt,Comm Pct".
    
    DISPLAY "" WITH FRAME r-top.

    IF tb_excel THEN DO:
    /*PUT STREAM excel UNFORMATTED excelheader SKIP.*/
        EXPORT STREAM excel DELIMITER ","
            "Sman"
            "Customer"
            "Inv#"
            "Inv Date"
            "Cust Part#"
            "Order#"
            "Quantity"
            "Inv Amt" 
            "Date Inv Paid"
            "Amt Paid"
            (IF rd_gp = "Delta" THEN "Delta"  ELSE "GrossProfit%")
            "Comm Amt"
            "Comm Pct".

    END.

    FOR EACH tt-report,

        FIRST ar-invl NO-LOCK WHERE RECID(ar-invl) EQ tt-report.rec-id,

        FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no,

        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no

        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.row-id
              BY tt-report.key-10
              BY tt-report.rec-id
              BY ROWID(tt-report):

        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

      IF FIRST(tt-report.key-01)    THEN v-frst[1] = YES.
      IF FIRST-OF(tt-report.key-01) THEN v-frst[2] = YES.

      FIND FIRST sman NO-LOCK
          WHERE sman.company EQ cocode
            AND sman.sman    EQ tt-report.key-01
          NO-ERROR.

      RUN custom/combasis.p (cocode, tt-report.key-01, cust.type, "", 0,
                             cust.cust-no,
                             OUTPUT v-basis).

      RELEASE prep.
      RELEASE itemfg.

      IF ar-invl.misc THEN
      FIND FIRST prep NO-LOCK
          WHERE prep.company EQ cocode
            AND prep.code    EQ ar-invl.i-name
          NO-ERROR.
      ELSE
      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ ar-invl.i-no
          NO-ERROR.

      RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                             (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                             cust.cust-no,
                             OUTPUT v-basis).

      IF tt-report.cost EQ ? THEN tt-report.cost = 0.
      IF tt-report.comm EQ ? THEN tt-report.comm = 0.
v-comm = 0.
      ASSIGN
       v-prof = tt-report.amt - tt-report.cost
       v-camt = IF v-basis EQ "G" THEN (v-prof * tt-report.comm / 100)
                                  ELSE (tt-report.amt * (tt-report.comm / 100))
       v-comm = tt-report.comm
       v-gp   = ROUND(v-prof / tt-report.amt * 100,2)
       v-camt = ROUND(v-camt,2).

      IF tt-report.delta LE .99 THEN tt-report.delta = 0.

      IF tb_calc AND tt-report.delta GT 0 THEN v-camt = 0.

      ASSIGN
       v-tot-samt[1] = v-tot-samt[1] + tt-report.amt
       v-tot-pamt[1] = v-tot-pamt[1] + tt-report.amtp
       v-tot-damt[1] = v-tot-damt[1] + tt-report.delta
       v-tot-camt[1] = v-tot-camt[1] + v-camt
       v-tot-cost[1] = v-tot-cost[1] + tt-report.cost.

      IF v-comm EQ ? THEN v-comm = 0.
      IF v-gp   EQ ? THEN v-gp   = 0.

      IF FIRST-OF(tt-report.key-01) THEN DO:
        IF NOT FIRST(tt-report.key-01) THEN PAGE.
        p-sman = tt-report.key-01.
      END.
      
      {sys/inc/roundup.i tt-report.qty}

      IF tb_detailed THEN DO:
        IF rd_gp = "Delta" THEN DO:
           DISPLAY tt-report.key-01        WHEN FIRST-OF(tt-report.key-01)
                                        FORMAT "x(3)"
                                        COLUMN-LABEL "Sman"
                ar-inv.cust-no          WHEN FIRST-OF(tt-report.key-02)
                                        COLUMN-LABEL "Customer"
                ar-inv.inv-no           COLUMN-LABEL "Inv#"
                ar-inv.inv-date         COLUMN-LABEL "Inv Date"
                ar-invl.part-no         COLUMN-LABEL "Cust Part#"
                ar-invl.ord-no          COLUMN-LABEL "Order#"
                tt-report.qty           COLUMN-LABEL "Quantity"
                tt-report.amt           COLUMN-LABEL "Inv Amt"
                tt-report.cash-date     COLUMN-LABEL "Date Inv Paid"
                tt-report.amtp          COLUMN-LABEL "Amount Paid"
                tt-report.delta         COLUMN-LABEL "Delta"
                v-comm                  COLUMN-LABEL "Comm Pct"
                                        FORMAT "->>>9.99"
                v-camt                  COLUMN-LABEL "Comm Amt"
                                        FORMAT "->>>>>9.99"
            WITH FRAME detail DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 200.

            DOWN WITH FRAME detail.
        END.
        ELSE DO:
           DISPLAY tt-report.key-01        WHEN FIRST-OF(tt-report.key-01)
                                        FORMAT "x(3)"
                                        COLUMN-LABEL "Sman"
                ar-inv.cust-no          WHEN FIRST-OF(tt-report.key-02)
                                        COLUMN-LABEL "Customer"
                ar-inv.inv-no           COLUMN-LABEL "Inv#"
                ar-inv.inv-date         COLUMN-LABEL "Inv Date"
                ar-invl.part-no         COLUMN-LABEL "Cust Part#"
                ar-invl.ord-no          COLUMN-LABEL "Order#"
                tt-report.qty           COLUMN-LABEL "Quantity"
                tt-report.amt           COLUMN-LABEL "Inv Amt"
                tt-report.cash-date     COLUMN-LABEL "Date Inv Paid"
                tt-report.amtp          COLUMN-LABEL "Amount Paid"
                v-gp                    COLUMN-LABEL "Gross!Profit%"   
                                        FORMAT "->>>9.99"
                v-comm                  COLUMN-LABEL "Comm Pct"
                                        FORMAT "->>>9.99"
                v-camt                  COLUMN-LABEL "Comm Amt"
                                        FORMAT "->>>>>9.99"
            WITH FRAME detail2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 200.

            DOWN WITH FRAME detail2.

        END.

        IF tb_excel THEN 
          PUT STREAM excel UNFORMATTED
              '"' tt-report.key-01    '",'
              '"' ar-inv.cust-no      '",'
              '"' ar-inv.inv-no       '",'
              '"' ar-inv.inv-date     '",'
              '"' REPLACE(ar-invl.part-no, '"', "")     '",'
              '"' ar-invl.ord-no      '",'
              '"' tt-report.qty       '",'
              '"' tt-report.amt       '",'
              '"' tt-report.cash-date '",'
              '"' tt-report.amtp      '",'
              '"' (IF rd_gp = "Delta" THEN tt-report.delta ELSE v-gp ) '",'
              '"' v-comm              '",'
              '"' v-camt              '",'
              SKIP.
      END.

      IF LAST-OF(tt-report.key-02) THEN DO:
        ASSIGN
         v-comm = v-tot-camt[1] / v-tot-samt[1] * 100
         v-cost = (v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100
         v-gp   = ROUND((v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100,2).

        IF v-comm = ? THEN v-comm = 0.
        IF v-cost = ? THEN v-cost = 0.
        IF v-gp   = ? THEN v-gp   = 0.
                
        IF tb_detailed THEN PUT SKIP(1).

        IF rd_gp = "Delta" THEN DO:
        
          DISPLAY p-sman                  WHEN NOT tb_detailed
                                        @ tt-report.key-01
                ar-inv.cust-no          WHEN NOT tb_detailed
                "Customer Totals"       WHEN tb_detailed
                                        @ ar-invl.part-no
                v-tot-samt[1]           @ tt-report.amt
                v-tot-pamt[1]           @ tt-report.amtp
                v-tot-damt[1]           @ tt-report.delta
                v-comm
                v-tot-camt[1]           @ v-camt
            WITH FRAME detail.

          DOWN WITH FRAME detail.
        END.
        ELSE DO:
          DISPLAY p-sman                  WHEN NOT tb_detailed
                                        @ tt-report.key-01
                ar-inv.cust-no          WHEN NOT tb_detailed
                "Customer Totals"       WHEN tb_detailed
                                        @ ar-invl.part-no
                v-tot-samt[1]           @ tt-report.amt
                v-tot-pamt[1]           @ tt-report.amtp
                v-gp                    @ v-gp
                v-comm
                v-tot-camt[1]           @ v-camt
            WITH FRAME detail2.

          DOWN WITH FRAME detail2.
        END.

        IF tb_detailed THEN PUT SKIP(1).

        IF tb_excel AND NOT tb_detailed THEN 
          PUT STREAM excel UNFORMATTED
              '"' tt-report.key-01  '",'
              '"' ar-inv.cust-no    '",'
              '"'                   '",'
              '"'                   '",'
              '"'                   '",'
              '"'                   '",'
              '"'                   '",'
              '"' v-tot-samt[1]     '",'
              '"'                   '",'
              '"' v-tot-pamt[1]     '",'
              '"' (IF rd_gp = "Delta" THEN v-tot-damt[1] ELSE v-gp )     '",'
              '"' v-comm            '",'
              '"' v-tot-camt[1]     '",'
              SKIP.

        ASSIGN
         p-sman        = ""
         v-tot-samt[2] = v-tot-samt[2] + v-tot-samt[1]
         v-tot-pamt[2] = v-tot-pamt[2] + v-tot-pamt[1]
         v-tot-damt[2] = v-tot-damt[2] + v-tot-damt[1]
         v-tot-camt[2] = v-tot-camt[2] + v-tot-camt[1]
         v-tot-cost[2] = v-tot-cost[2] + v-tot-cost[1]
         v-tot-samt[1] = 0
         v-tot-pamt[1] = 0
         v-tot-damt[1] = 0
         v-tot-camt[1] = 0
         v-tot-cost[1] = 0.
      END.

      IF LAST-OF(tt-report.key-01) THEN DO:
        ASSIGN
         v-comm = v-tot-camt[2] / v-tot-samt[2] * 100
         v-cost = (v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100
         v-gp   = ROUND((v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100,2).

        IF v-comm = ? THEN v-comm = 0.
        IF v-cost = ? THEN v-cost = 0.
        IF v-gp   = ? THEN v-gp   = 0.

        IF NOT tb_detailed THEN PUT SKIP(1).

        IF rd_gp = "Delta" THEN DO:
            DISPLAY "SalesRep Totals"       @ ar-invl.part-no
                    v-tot-samt[2]           @ tt-report.amt
                    v-tot-pamt[2]           @ tt-report.amtp
                    v-tot-damt[2]           @ tt-report.delta
                    v-comm
                    v-tot-camt[2]           @ v-camt
                WITH FRAME detail.
    
            DOWN WITH FRAME detail.
        END.
        ELSE DO:
            DISPLAY "SalesRep Totals"       @ ar-invl.part-no
                    v-tot-samt[2]           @ tt-report.amt
                    v-tot-pamt[2]           @ tt-report.amtp
                    v-gp                    @ v-gp
                    v-comm
                    v-tot-camt[2]           @ v-camt
                WITH FRAME detail2.
    
            DOWN WITH FRAME detail2.

        END.

        PUT SKIP(1).

        ASSIGN
         v-frst[1]     = NO
         v-tot-samt[3] = v-tot-samt[3] + v-tot-samt[2]
         v-tot-pamt[3] = v-tot-pamt[3] + v-tot-pamt[2]
         v-tot-damt[3] = v-tot-damt[3] + v-tot-damt[2]
         v-tot-camt[3] = v-tot-camt[3] + v-tot-camt[2]
         v-tot-cost[3] = v-tot-cost[3] + v-tot-cost[2]
         v-tot-samt[2] = 0
         v-tot-pamt[2] = 0
         v-tot-damt[2] = 0
         v-tot-camt[2] = 0
         v-tot-cost[2] = 0.
      END.

      IF LAST-OF(tt-report.key-02) THEN v-frst[2] = NO. 

      IF LAST(tt-report.key-01) THEN DO:
        ASSIGN
         v-comm = v-tot-camt[3] / v-tot-samt[3] * 100
         v-cost = (v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100
         v-gp   = ROUND((v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100,2).

        IF v-comm = ? THEN v-comm = 0.
        IF v-cost = ? THEN v-cost = 0.
        IF v-gp   = ? THEN v-gp   = 0.

        PUT SKIP(1).

        IF rd_gp = "Delta" THEN DO:
            DISPLAY "   Grand Totals"       @ ar-invl.part-no
                    v-tot-samt[3]           @ tt-report.amt
                    v-tot-pamt[3]           @ tt-report.amtp
                    v-tot-damt[3]           @ tt-report.delta
                    v-comm
                    v-tot-camt[3]           @ v-camt
                WITH FRAME detail.
    
            DOWN WITH FRAME detail.
        END.
        ELSE DO:
            DISPLAY "   Grand Totals"       @ ar-invl.part-no
                    v-tot-samt[3]           @ tt-report.amt
                    v-tot-pamt[3]           @ tt-report.amtp
                    v-gp                    @ v-gp
                    v-comm
                    v-tot-camt[3]           @ v-camt
                WITH FRAME detail2.
    
            DOWN WITH FRAME detail2.
        END.                       

      END.

      DELETE tt-report.
    END.  /* input-work */
