
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
        
    for each cust
        where cust.company eq cocode
          and cust.cust-no ge v-cust[1]
          and cust.cust-no le v-cust[2] AND LOOKUP(cust.cust-no,custcount) <> 0
        no-lock:

      for each ar-inv
          where ar-inv.company  eq cocode
            and ar-inv.posted   eq yes
            and ar-inv.cust-no  eq cust.cust-no
            and ar-inv.inv-date ge v-date[1]
            and ar-inv.inv-date le v-date[2]
          no-lock,

          each ar-invl
          where ar-invl.x-no eq ar-inv.x-no
            and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
          no-lock:

        RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT ll-comp).
        IF ll-comp THEN NEXT.

        if v-cat ne "" then do:
          release itemfg.
          if not ar-invl.misc then
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

          IF (ar-invl.misc AND v-cat NE "MISC")             OR
             (NOT ar-invl.misc AND
              (NOT AVAIL itemfg OR itemfg.procat NE v-cat)) THEN NEXT.
        end.

        do i = 1 to 3:
          v-slsm[1] = if ar-invl.sman[i] eq "" and i eq 1 then
                        cust.sman else ar-invl.sman[i].
          
          if v-slsm[1]   lt v-sman[1]                     or
             v-slsm[1]   gt v-sman[2]                     or
             (i ne 1 and
              (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

          create tt-report.
          assign
           tt-report.key-01  = v-slsm[1]
           tt-report.key-02  = cust.cust-no
           tt-report.key-03  = string(ar-inv.inv-no,"999999")
           tt-report.key-10  = "ar-invl"
           tt-report.rec-id  = recid(ar-invl)
           tt-report.row-id  = ROWID(ar-invl).
        end.
      end.

      FOR EACH ar-cash
          WHERE ar-cash.company    EQ cocode
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE v-date[1]
            AND ar-cash.check-date LE v-date[2]
            AND ar-cash.posted     EQ YES
          NO-LOCK,

          EACH ar-cashl
          WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
                         WHERE account.company EQ ar-cashl.company
                           AND account.actnum  EQ ar-cashl.actnum
                           AND account.type    EQ "R")
          NO-LOCK:

        RELEASE tt-report.
        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        if avail oe-retl then 
        find first ar-invl
            where ar-invl.company eq ar-cashl.company
              and ar-invl.cust-no eq cust.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
            no-lock no-error.

        IF ar-cashl.inv-no NE 0                                                       AND
           (AVAIL ar-invl                             OR
            (NOT AVAIL reftable AND
             NOT ar-cashl.dscr MATCHES "*oe return*") OR
            SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
        FOR EACH b-ar-invl
            WHERE b-ar-invl.company EQ ar-cashl.company
              AND b-ar-invl.cust-no EQ cust.cust-no
              AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
              AND ((tb_prep AND b-ar-invl.billable) OR NOT b-ar-invl.misc)
              AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
            NO-LOCK:

          IF AVAIL ar-invl THEN DO:
            RUN oe/invlcomp.p (ROWID(b-ar-invl), OUTPUT ll-comp).
            IF ll-comp THEN NEXT.
          END.

          IF v-cat NE "" THEN DO:
            RELEASE itemfg.
            IF NOT b-ar-invl.misc THEN
            FIND FIRST itemfg
                WHERE itemfg.company eq b-ar-invl.company
                  AND itemfg.i-no    eq b-ar-invl.i-no
                NO-LOCK NO-ERROR.

            IF (b-ar-invl.misc AND v-cat NE "MISC")           OR
               (NOT b-ar-invl.misc AND
                (NOT AVAIL itemfg OR itemfg.procat NE v-cat)) THEN NEXT.
          END.

          DO i = 1 TO 3:
            v-slsm[1] = IF b-ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                          cust.sman ELSE b-ar-invl.sman[i].

            IF v-slsm[1]   LT v-sman[1]                       OR
               v-slsm[1]   GT v-sman[2]                       OR
               (i NE 1 AND
                (v-slsm[1] EQ "" OR b-ar-invl.s-pct[i] EQ 0)) THEN NEXT.

            CREATE tt-report.
            ASSIGN
             tt-report.key-01 = v-slsm[1]
             tt-report.key-03 = STRING(b-ar-invl.inv-no,"999999")
             tt-report.row-id = ROWID(b-ar-invl)
             tt-report.key-02 = cust.cust-no
             tt-report.key-10 = "ar-cashl"
             tt-report.rec-id = RECID(ar-cashl).
          END.
        END.

        ELSE DO:
          if v-cat ne "" and v-cat ne "CRMEM" then next.

          else
          if cust.sman ge v-sman[1] and
             cust.sman le v-sman[2] then do:

            create tt-report.
            assign
             tt-report.key-01 = cust.sman
             tt-report.key-03 = string(ar-cashl.inv-no,"999999").
          end.

          if avail tt-report then
            assign
             tt-report.key-02  = cust.cust-no
             tt-report.key-10  = "ar-cashl"
             tt-report.rec-id  = recid(ar-cashl).
        END.
      end.
    end.

    assign
    str-tit3 = (if v-per-rpt then "P" else "Y") +
               "TD (" + string(v-date[1]) + "-" + string(v-date[2]) +
               ") - By Sales Rep By Customer"
    {sys/inc/ctrtext.i str-tit3 132}

    v-head[1] = "".

    if v-sumdet then
      assign
       v-head[2] =
                 "Sales Rep  Customer Name                           Total Sa" +
                 "les $        Comm $    Comm %        Cost $       GP %"
       v-head[3] = fill("-",113)
       v-exp-head =  "Sales Rep,Customer,Name,Total Sales $, Comm $, Comm %, Cost $, GP %".

    else
      assign
       v-head[2] =
                 "SalRep Customer Name" + FILL(" ",16) + STRING(rd_part-fg,"x(16)") +
                 "Order#   Inv# Cat    Quantity   Sell Price   Total Cost      GP %   Co" +
                 "mm Amt Comm Pct"
       v-head[3] = fill("-",137)
       v-exp-head = "Sal Rep,Customer,Name," + TRIM(rd_part-fg) + ",Order#,Inv#," +
                    "Cat,Quantity,Sell Price,Total Cost,GP %,Comm Amt,Comm Pct".
    
    DISPLAY "" WITH frame r-top.
    IF tb_excel THEN PUT STREAM st-excell v-exp-head SKIP.

    input-work:
    for each tt-report,

        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-report.key-02
        no-lock

        break by tt-report.key-01
              by tt-report.key-02
              by tt-report.key-03
              BY tt-report.row-id
              BY tt-report.key-10
              BY tt-report.rec-id
              BY ROWID(tt-report):

      if FIRST(tt-report.key-01)    then v-frst[1] = yes.
      if first-of(tt-report.key-01) then v-frst[2] = yes.

      find first sman
          where sman.company eq cocode
            and sman.sman    eq tt-report.key-01
          no-lock no-error.

      release ar-invl.
      release ar-cashl.

      assign
       v-cust-part = ""
       v-job-no    = ""
       v-job-no2   = 0
       v-ord-no    = 0
       v-i-no      = ""
       v-amt       = 0
       v-cost      = 0
       v-qty       = 0.

      RUN custom/combasis.p (cocode, tt-report.key-01, cust.type, "", 0,
                             cust.cust-no,
                             OUTPUT v-basis).

      if tt-report.key-10 eq "ar-invl" then
        find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
        RELEASE prep.
        RELEASE itemfg.

        IF ar-invl.misc then
        FIND FIRST prep NO-LOCK
            WHERE prep.company EQ cocode
              AND prep.code    EQ ar-invl.i-name
            NO-ERROR.
        ELSE
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ ar-invl.i-no
            NO-ERROR.

        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report.key-01 or
             ar-invl.sman[1] eq ""            then leave.
          if i eq 3 then next input-work.
        end.

        assign
         v-inv-no  = ar-invl.inv-no
         v-procat  = IF ar-invl.misc THEN
                       IF AVAIL prep THEN prep.fgcat ELSE "MISC"
                     ELSE
                     IF AVAIL itemfg THEN itemfg.procat ELSE "ARINV"
         v-slsp[1] = if ar-invl.sman[i] eq ""              or
                        (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                     else ar-invl.s-pct[i]
         v-qty     = (if ar-invl.inv-qty ne 0 then ar-invl.inv-qty
                      else ar-invl.qty) * v-slsp[1] / 100
         v-amt     = ar-invl.amt * v-slsp[1] / 100
                       
         v-slsc[1] = /*IF ar-invl.sman[i] EQ "" AND AVAIL sman AND ar-invl.ord-no NE 0) THEN
                       sman.scomm ELSE*/ ar-invl.s-comm[i]
         v-cust-part = ar-invl.part-no
         v-job-no    = ar-invl.job-no
         v-job-no2   = ar-invl.job-no2
         v-ord-no    = ar-invl.ord-no
         v-i-no      = ar-invl.i-no
         v-cost      = 0.

        IF ar-invl.t-cost NE 0 THEN
           v-cost = ar-invl.t-cost * v-slsp[1] / 100.
        /*when updating cost via A-U-1, bug where t-cost
           was not being updated after posting*/
        ELSE IF ar-invl.cost NE 0 THEN
        DO:
           IF ar-invl.dscr[1] EQ "M" OR
              ar-invl.dscr[1] EQ "" THEN
              v-cost = ar-invl.cost * (ar-invl.inv-qty / 1000) * v-slsp[1] / 100.
           ELSE /*EA*/
              v-cost = ar-invl.cost * ar-invl.inv-qty * v-slsp[1] / 100.
        END.

        IF ar-invl.misc AND NOT oe-ctrl.prep-comm THEN v-slsc[1] = 0.

        RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                               (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                               cust.cust-no,
                               OUTPUT v-basis).
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

      if avail ar-cashl then do:
        RELEASE oe-retl.
        RELEASE ar-invl.

        FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        if avail oe-retl and not avail ar-invl then 
        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq cust.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
            no-lock no-error.

        IF AVAIL ar-invl THEN DO:
          DO i = 1 to 3:
            IF ar-invl.sman[i] EQ tt-report.key-01 OR
               ar-invl.sman[1] EQ ""               THEN LEAVE.
            IF i EQ 3 THEN NEXT input-work.
          END.

          RELEASE prep.
          RELEASE itemfg.

          IF ar-invl.misc then
          FIND FIRST prep NO-LOCK
              WHERE prep.company EQ cocode
                AND prep.code    EQ ar-invl.i-name
              NO-ERROR.
          ELSE
          FIND FIRST itemfg NO-LOCK
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
              NO-ERROR.

          ASSIGN
           v-inv-no    = ar-invl.inv-no
           v-slsp[1]   = if ar-invl.sman[i] eq ""              or
                          (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                         else ar-invl.s-pct[i]
           v-slsc[1]   = /*IF ar-invl.sman[i] EQ "" AND AVAIL sman AND ar-invl.ord-no NE 0) THEN
                           sman.scomm ELSE*/ ar-invl.s-comm[i]
           v-qty       = 0
           v-amt       = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                         (v-slsp[1] / 100)
           v-cost      = 0 
           v-procat    = IF ar-invl.misc THEN
                           IF AVAIL prep THEN prep.fgcat ELSE "MISC"
                         ELSE
                         IF AVAIL itemfg THEN itemfg.procat ELSE "CRMEMO"
           v-cust-part = ar-invl.part-no
           v-job-no    = ar-invl.job-no
           v-job-no2   = ar-invl.job-no2
           v-ord-no    = ar-invl.ord-no
           v-i-no      = ar-invl.i-no.

          IF AVAIL oe-retl THEN
            ASSIGN
             v-qty  = oe-retl.tot-qty-return * -1
             v-cost = oe-retl.cost * (oe-retl.tot-qty-return / 1000) *
                      (v-slsp[1] / 100) * -1.

          ELSE DO:
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

            ld-csh-pct = 0.
            FOR EACH b-ar-cashl
                WHERE b-ar-cashl.c-no   EQ ar-cashl.c-no
                  AND b-ar-cashl.inv-no EQ ar-cashl.inv-no
                NO-LOCK:
              ld-csh-pct = ld-csh-pct + (b-ar-cashl.amt-paid - b-ar-cashl.amt-disc).
            END.
            ld-csh-pct = (ar-cashl.amt-paid - ar-cashl.amt-disc) / ld-csh-pct.

            IF ld-csh-pct EQ ? THEN ld-csh-pct = 0.

            v-amt = v-amt * ld-inv-pct.
          END.

          RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                                 (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                                 cust.cust-no,
                                  OUTPUT v-basis).

          IF ar-invl.misc AND NOT oe-ctrl.prep-comm THEN v-slsc[1] = 0.
        END.

        else
          assign
           v-inv-no    = ar-cashl.inv-no
           v-procat    = "CRMEM"
           v-slsp[1]   = 100
           v-qty       = 0
           v-amt       = ar-cashl.amt-paid - ar-cashl.amt-disc
           v-cost      = 0
           v-slsc[1]   = if avail sman then sman.scomm else 0
           v-cust-part = "".
      end.

      IF v-i-no NE "" THEN
        IF v-cost1 EQ "E" THEN
          RUN sys/inc/bordcost.p (v-job-no, v-job-no2, v-i-no, v-qty, NO,
                                  OUTPUT v-cost).
        ELSE
        IF v-cost1 EQ "O" AND v-ord-no NE 0 THEN DO:
          FIND FIRST oe-ordl
              WHERE oe-ordl.company eq cocode
                AND oe-ordl.ord-no  eq v-ord-no
                AND oe-ordl.i-no    eq v-i-no
              NO-LOCK NO-ERROR.
          IF AVAIL oe-ordl THEN v-cost = oe-ordl.cost * v-qty / 1000.
        END.

      if v-cost    eq ? then v-cost    = 0.
      if v-slsc[1] eq ? then v-slsc[1] = 0.

      IF v-qty EQ 0 AND AVAIL ar-cashl THEN v-cost = v-amt.

      assign
       v-slsm[1] = tt-report.key-01

       v-prof    = v-amt - v-cost
       v-camt    = if v-basis eq "G" then
                     (v-prof * v-slsc[1] / 100)
                   else
                     (v-amt * (v-slsc[1] / 100)).

      IF v-qty EQ 0 AND AVAIL ar-cashl THEN
        ASSIGN
         v-camt = 0
         v-cost = 0
         v-prof = 0.

      assign
       v-comm    = round(v-camt / v-amt * 100,2)
       v-gp      = round(v-prof / v-amt * 100,2)
       v-camt    = round(v-camt,2)

       v-tot-samt[1] = v-tot-samt[1] + v-amt
       v-tot-camt[1] = v-tot-camt[1] + v-camt
       v-tot-cost[1] = v-tot-cost[1] + v-cost.

      if v-comm eq ? then v-comm = 0.
      if v-gp   eq ? then v-gp   = 0.

      if first-of(tt-report.key-01) then do:
        IF NOT FIRST(tt-report.key-01) THEN page.
        p-sman = tt-report.key-01.
      end.
      
      {sys/inc/roundup.i v-qty}

      v-part-fg = IF rd_part-fg BEGINS "Cust" THEN v-cust-part ELSE v-i-no.

      if not v-sumdet then DO:
         display tt-report.key-01       when first-of(tt-report.key-01)
                                        format "x(3)"
                space(4)
                tt-report.key-02        when first-of(tt-report.key-02)
               SPACE(1)
                cust.name               when first-of(tt-report.key-02)
                                        format "x(19)" 
                v-part-fg
                v-ord-no
                v-inv-no
                v-procat
                v-qty                   format "->>>>>>>9"
                v-amt                   format "->>>>>>>9.99"
                v-cost  WHEN v-print-cost  format "->>>>>>>9.99"
                v-gp    WHEN v-print-cost  format "->>>>9.99"
                v-camt                  format "->>>>>9.99"
                v-comm                  format "->>>9.99"
            with frame detail no-box no-labels stream-io width 200.

          IF tb_excel THEN 
             PUT STREAM st-excell tt-report.key-01 FORM "x(3)" v-comma
                        tt-report.key-02   v-comma
                REPLACE(cust.NAME,',','') FORM "x(30)" v-comma
                v-part-fg v-comma
                v-ord-no  v-comma
                v-inv-no  v-comma
                v-procat  v-comma
                v-qty     format "->>>>>>>9"        v-comma
                v-amt     format "->>>>>>>9.99"     v-comma
                (IF v-print-cost THEN string(v-cost,"->>>>>>>9.99") ELSE "")  FORM "x(20)"   v-comma
                (IF v-print-cost THEN STRING(v-gp,"->>>>9.99") ELSE "")       FORM "x(20)"   v-comma
                v-camt    format "->>>>>9.99"       v-comma
                v-comm    format "->>>9.99"      
               SKIP.
      END.

      if last-of(tt-report.key-02) then do:
        assign
         v-comm = v-tot-camt[1] / v-tot-samt[1] * 100
         v-cost = (v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100
         v-gp   = round((v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100,2).

        if v-comm = ? then v-comm = 0.
        if v-cost = ? then v-cost = 0.
        if v-gp   = ? then v-gp   = 0.

        if v-sumdet THEN DO:
        
          display p-sman                format "x(10)"
                  /*space(7)*/
                  tt-report.key-02
                  /*space(7)*/
                  cust.NAME    FORM "x(30)"
                  space(2)
                  v-tot-samt[1]
                  space(2)
                  v-tot-camt[1]
                  space(2)
                  v-comm                format "->>>9.99"
                  space(2)
                  v-tot-cost[1] WHEN v-print-cost
                  space(2)
                  v-cost        WHEN v-print-cost        format "->>>9.99"
              with frame summary no-box no-labels stream-io width 200.

          IF tb_excel THEN
            PUT STREAM st-excell
                  p-sman  format "x(3)" v-comma
                  tt-report.key-02 v-comma
                  cust.NAME  FORM "x(30)" v-comma
                  v-tot-samt[1] v-comma
                  v-tot-camt[1] v-comma
                  v-comm      format "->>>9.99" v-comma
                  (IF v-print-cost THEN string(v-tot-cost[1],"->>>>>>>9.99") ELSE "") FORM "x(20)" v-comma
                  (IF v-print-cost THEN string(v-cost,"->>>9.99") ELSE "")            FORM "x(20)"
                  SKIP.
        END.

        else do:
          find first w-comm
              where w-comm.sman eq tt-report.key-01
              no-error.

          if not avail w-comm then do:
            create w-comm.
            w-comm.sman = tt-report.key-01.
          end.

          assign
           w-comm.samt = w-comm.samt + v-tot-samt[1]
           w-comm.camt = w-comm.camt + v-tot-camt[1]
           w-comm.cost = w-comm.cost + v-tot-cost[1].

          if (not first-of(tt-report.key-01)) and
             (not first-of(tt-report.key-02)) then do:
             
            down with frame detail.
            
            put skip(1).
            
            display "Customer Totals:"    @ cust.name
                    v-tot-samt[1]               @ v-amt
                    v-tot-cost[1]   WHEN v-print-cost            @ v-cost      
                    v-gp            WHEN v-print-cost
                    v-tot-camt[1]               @ v-camt
                    v-comm

                with frame detail.
                
            put skip(1).
          end.
          
          else
            display skip(1) with frame skip-a-line no-box no-labels stream-io.
        end.

        assign
         p-sman        = ""
         v-tot-samt[2] = v-tot-samt[2] + v-tot-samt[1]
         v-tot-camt[2] = v-tot-camt[2] + v-tot-camt[1]
         v-tot-cost[2] = v-tot-cost[2] + v-tot-cost[1]
         v-tot-samt[1] = 0
         v-tot-camt[1] = 0
         v-tot-cost[1] = 0.
      end.

      if last-of(tt-report.key-01) then do:
        assign
         v-comm = v-tot-camt[2] / v-tot-samt[2] * 100
         v-cost = (v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100
         v-gp   = round((v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100,2).

        if v-comm = ? then v-comm = 0.
        if v-cost = ? then v-cost = 0.
        if v-gp   = ? then v-gp   = 0.

        if ((not v-frst[2]) and (not last(tt-report.key-01))) or
           ((not v-frst[1]) and last(tt-report.key-01))       then
           
          if v-sumdet THEN DO:
             display skip(1)
                    space(5)
                    "Sales Rep Totals:"
                    space(30)
                    v-tot-samt[2]
                    space(2)
                    v-tot-camt[2]
                    space(2)
                    v-comm              format "->>>9.99"
                    space(2)
                    v-tot-cost[2] WHEN v-print-cost
                    space(2)
                    v-cost        WHEN v-print-cost      format "->>>9.99"
                    skip(1)
                with frame Salesman-sum no-box no-labels stream-io width 200.
             
                   
          END.
          else do:
            down with frame detail.
            
            put skip(1).
            
            display "Sales Rep Totals:"    @ cust.name
                    v-tot-samt[2]               @ v-amt
                    v-tot-cost[2] WHEN v-print-cost              @ v-cost      
                    v-gp          WHEN v-print-cost
                    v-tot-camt[2]               @ v-camt
                    v-comm

                with frame detail.
          end.
          

        assign
         v-frst[1]     = no
         v-tot-samt[3] = v-tot-samt[3] + v-tot-samt[2]
         v-tot-camt[3] = v-tot-camt[3] + v-tot-camt[2]
         v-tot-cost[3] = v-tot-cost[3] + v-tot-cost[2]
         v-tot-samt[2] = 0
         v-tot-camt[2] = 0
         v-tot-cost[2] = 0.
      end.

      if last-of(tt-report.key-02) then v-frst[2] = no.

      delete tt-report.
    end.  /* input-work */

    if not v-sumdet then do:
      assign
       str-tit3 = (if v-per-rpt then "P" else "Y") +
                  "TD (" + string(v-date[1]) + "-" + string(v-date[2]) +
                  ") - By Sales Rep"
       {sys/inc/ctrtext.i str-tit3 132}

       v-head[2] =
                 "Sales Rep                                           Total Sa" +
                 "les $        Comm $    Comm %        Cost $      GP %"
       v-head[3] = fill("-",112).


      page.

      assign
       v-tot-samt[3] = 0
       v-tot-camt[3] = 0
       v-tot-cost[3] = 0.
     

      recap-work:
      for each w-comm
          break by w-comm.sman:

        assign
         v-comm = w-comm.camt / w-comm.samt * 100
         v-cost = (w-comm.samt - w-comm.cost) / w-comm.samt * 100

         v-tot-samt[3] = v-tot-samt[3] + w-comm.samt
         v-tot-camt[3] = v-tot-camt[3] + w-comm.camt
         v-tot-cost[3] = v-tot-cost[3] + w-comm.cost.

        if v-comm = ? then v-comm = 0.
        if v-cost = ? then v-cost = 0.

        display w-comm.sman         format "x(3)"
                space(49)
                w-comm.samt         format "->>>>>>>9.99"
                space(2)
                w-comm.camt         format "->>>>>>>9.99"
                space(2)
                v-comm              format "->>>9.99"
                space(2)
                w-comm.cost  WHEN v-print-cost       format "->>>>>>>9.99"
                space(2)
                v-cost       WHEN v-print-cost       format "->>>9.99"

            with frame Salesman-det no-box no-labels stream-io width 200.

      end.  /* recap-work */
    end.

    assign
     v-comm = v-tot-camt[3] / v-tot-samt[3] * 100.
     v-cost = (v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100.

    if v-comm = ? then v-comm = 0.
    if v-cost = ? then v-cost = 0.

    display skip(1)
            "Grand Totals:"
            space(39)
            v-tot-samt[3]
            space(2)
            v-tot-camt[3]
            space(2)
            v-comm                format "->>>9.99"
            space(2)
            v-tot-cost[3] WHEN v-print-cost
            space(2)
            v-cost        WHEN v-print-cost        format "->>>9.99"

        with frame grand-tot no-box no-labels stream-io width 200.
        
   
