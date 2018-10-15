    
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
        
    for each cust
        where cust.company eq cocode
          and cust.cust-no ge v-cust[1]
          and cust.cust-no le v-cust[2]
          AND cust.TYPE GE v-cust-typ[1]
          AND cust.TYPE LE v-cust-typ[2]
          AND cust.spare-char-2 GE begin_group
          AND cust.spare-char-2 LE end_group
        no-lock:

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"}
       
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
        v-calc-cat = "".
        IF ar-invl.misc THEN DO:
            FIND FIRST prep NO-LOCK
           WHERE prep.company EQ cocode
             AND prep.code    EQ ar-invl.i-name
          NO-ERROR.
          IF AVAIL prep THEN
              v-calc-cat = prep.fgcat.
        END.
        ELSE DO:
            find first itemfg
                where itemfg.company eq cocode
                      and itemfg.i-no    eq ar-invl.i-no
                no-lock no-error.
            IF AVAIL itemfg THEN
                v-calc-cat = itemfg.procat.
        END.

        if v-cat ne "" then do:
          release itemfg.
          if not ar-invl.misc then
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

          IF (ar-invl.misc AND v-cat NE "MISC")             OR
             (NOT ar-invl.misc AND
              (v-calc-cat NE v-cat)) THEN NEXT.
        end.

        do i = 1 to 3:
          v-slsm[1] = if ar-invl.sman[i] eq "" and i eq 1 then
                        cust.sman else ar-invl.sman[i].
          
          if v-slsm[1]   lt v-sman[1]                     or
             v-slsm[1]   gt v-sman[2]                     or
             (i ne 1 and
              (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

           IF tgChooseSalesReps AND LOOKUP(v-slsm[1], cSlsList) EQ 0 THEN
             NEXT.

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

      FOR EACH ar-cashl WHERE ar-cashl.company EQ cocode
                         AND ar-cashl.cust-no  EQ cust.cust-no
                         AND ar-cashl.posted   EQ YES
                         AND ar-cashl.memo     EQ YES
/*                          AND ar-cashl.inv-date GE v-date[1] */
/*                          AND ar-cashl.inv-date LE v-date[2] */
                         AND CAN-FIND(FIRST account WHERE account.company EQ ar-cashl.company
                                                   AND account.actnum  EQ ar-cashl.actnum
                                                   AND account.type    EQ "R") NO-LOCK,
          EACH ar-cash WHERE ar-cash.c-no       EQ ar-cashl.c-no
                         AND ar-cash.company    EQ cocode
                         AND ar-cash.cust-no    EQ ar-cashl.cust-no
                         AND ar-cash.check-date GE v-date[1]
                         AND ar-cash.check-date LE v-date[2]
                         AND ar-cash.posted     EQ YES NO-LOCK
                       USE-INDEX c-no:


        RELEASE tt-report.
        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

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
            IF tgChooseSalesReps AND LOOKUP(v-slsm[1], cSlsList) EQ 0 THEN
              NEXT.
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
             cust.sman le v-sman[2] AND
             (tgChooseSalesReps EQ NO OR LOOKUP(cust.sman, cSlsList) EQ 0) 
              then do:
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

  /*  if v-sumdet then
      assign
   v-head[2] =
                 "Sales Rep  Customer Name                           Total Sa" +
                 "les $        Comm $    Comm %        Cost $       GP %"
       v-head[3] = fill("-",113)
       v-exp-head =  "Sales Rep,Customer,Name,Total Sales $, Comm $, Comm %, Cost $, GP %".

    else
      assign
       v-head[2] =
                 "Rep Customer Name       Type     " +  STRING(rd_part-fg,"x(16)") +
                 "Order#   Inv# Cat    Quantity   Sell Price   Total Cost      GP %   Co" +
                 "mm Amt Comm Pct"
       v-head[3] = fill("-",134) /* 138/143 */
       v-exp-head = "Sal Rep,Customer,Name,Type," + TRIM(rd_part-fg) + ",Order#,Inv#," +
                    "Cat,Quantity,Sell Price,Total Cost,GP %,Comm Amt,Comm Pct,Currency,Invoice Date,Warehouse".
                       /*BV - 01071303 - added currency and invoice date to XL*/
       IF v-show-sls-cat THEN DO:
       
        IF v-print-cost THEN
              v-exp-head = v-exp-head + ",Category,Cat Sell,Cat. Comm,Cat. Comm%,Cat. Cost,Cat. GP%,".
          ELSE 
              v-exp-head = v-exp-head + ",Category,Cat Sell,Cat. Comm,Cat. Comm%".
      END. */
      
    DISPLAY "" WITH frame r-top.
    /*IF tb_excel THEN PUT STREAM st-excell UNFORMATTED v-exp-head SKIP.*/

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
     {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"}
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
       cWhse       = ""
       v-job-no    = ""
       v-job-no2   = 0
       v-ord-no    = 0
       v-i-no      = ""
       v-bol-no    = 0
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
        FIND FIRST ar-inv where ar-inv.x-no eq ar-invl.x-no NO-LOCK NO-ERROR.
        IF AVAIL ar-inv THEN  v-inv-date  = ar-inv.inv-date.
        ELSE v-inv-date = ar-invl.inv-date.
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
         v-qty     = (if ar-invl.inv-qty ne 0 then ar-invl.inv-qty * v-slsp[1] / 100
                      else 0) /*if invoice qty is 0 - i.e. sample - qty should be 0) 04011303 */
         v-amt     = ar-invl.amt * v-slsp[1] / 100
                       
         v-slsc[1] = ar-invl.s-comm[i]
         v-cust-part = ar-invl.part-no
         v-job-no    = ar-invl.job-no
         v-job-no2   = ar-invl.job-no2
         v-ord-no    = ar-invl.ord-no
         v-i-no      = ar-invl.i-no
         v-bol-no    = ar-invl.bol-no
         v-cost      = 0.

        IF ar-invl.loc NE "" THEN cWhse = ar-invl.loc.
        ELSE DO:
            FIND FIRST oe-boll
                WHERE oe-boll.company EQ ar-invl.company
                  AND oe-boll.bol-no EQ ar-invl.bol-no
                  AND oe-boll.i-no EQ ar-invl.i-no
                NO-LOCK NO-ERROR.
            IF AVAIL oe-boll THEN cWhse = oe-boll.loc.
        END.

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

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        if avail oe-retl and not avail ar-invl then 
        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq cust.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
            no-lock no-error.

        IF AVAIL ar-invl THEN DO:
            FIND FIRST ar-inv where ar-inv.x-no eq ar-invl.x-no NO-LOCK NO-ERROR.
            IF AVAIL ar-inv THEN  v-inv-date  = ar-inv.inv-date.
            ELSE v-inv-date = ar-invl.inv-date.
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
            v-slsc[1]   = ar-invl.s-comm[i]
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
             FOR EACH b-ar-invl FIELDS(amt billable misc i-no) WHERE
                 b-ar-invl.x-no EQ ar-invl.x-no
                 NO-LOCK:

                 IF NOT((tb_prep AND b-ar-invl.billable) OR NOT b-ar-invl.misc) THEN
                    NEXT.

                 if v-cat ne "" then do:
                    release b-itemfg.
                    if not b-ar-invl.misc then
                    find first b-itemfg WHERE
                         b-itemfg.company eq cocode AND
                         b-itemfg.i-no    eq b-ar-invl.i-no
                         no-lock no-error.
             
                    IF (b-ar-invl.misc AND v-cat NE "MISC")             OR
                       (NOT b-ar-invl.misc AND
                       (NOT AVAIL b-itemfg OR b-itemfg.procat NE v-cat)) THEN do:
                        NEXT.
                    END.

                 end.

                 ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                 ACCUMULATE 1 (TOTAL).
             END.
             
             ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                             (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                          ELSE (ACCUM TOTAL 1))
                          ELSE (ar-invl.amt / ld-inv-pct).


             IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.
          
             ld-csh-pct = 0.
             FOR EACH b-ar-cashl FIELDS(amt-paid amt-disc)
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

        ELSE DO:
            assign
             v-inv-no    = ar-cashl.inv-no
             v-inv-date  =  ar-cashl.inv-date
             v-procat    = "CRMEM"
             v-slsp[1]   = 100
             v-qty       = 0
             v-amt       = ar-cashl.amt-paid - ar-cashl.amt-disc
             v-cost      = 0
             v-slsc[1]   = if avail sman then sman.scomm else 0
             v-cust-part = "".

        END.
      end.

      IF v-i-no NE "" THEN
        IF v-cost1 EQ "E" THEN
            RUN sys/inc/bordcost.p (v-job-no, v-job-no2, v-i-no, v-bol-no, v-qty, NO,
                                    OUTPUT v-cost).
        ELSE
        IF v-cost1 EQ "O" AND v-ord-no NE 0 THEN DO:
          FIND FIRST oe-ordl
              WHERE oe-ordl.company eq cocode
                AND oe-ordl.ord-no  eq v-ord-no
                AND oe-ordl.i-no    eq v-i-no
              NO-LOCK NO-ERROR.
          IF AVAIL oe-ordl THEN
               v-cost = oe-ordl.cost * v-qty / 1000.
             
        END.

      if v-cost    eq ? then v-cost    = 0.
      if v-slsc[1] eq ? then v-slsc[1] = 0.

      IF v-qty EQ 0 AND AVAIL ar-cashl THEN
          v-cost = v-amt.

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

              
       FIND FIRST tt-slsrp WHERE tt-slsrp.sman = v-slsm[1]
                              AND tt-slsrp.scat = v-procat
                           NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-slsrp THEN DO:
         CREATE tt-slsrp.
         ASSIGN  tt-slsrp.sman = v-slsm[1]
                 tt-slsrp.scat = v-procat.
       END.
       ASSIGN 
         tt-slsrp.samt = tt-slsrp.samt + v-amt
         tt-slsrp.camt = tt-slsrp.camt + v-camt
         tt-slsrp.cost = tt-slsrp.cost + v-cost.

      if v-comm eq ? then v-comm = 0.
      if v-gp   eq ? then v-gp   = 0.

      if first-of(tt-report.key-01) then do:
        IF NOT FIRST(tt-report.key-01) THEN page.
        p-sman = tt-report.key-01.
      end.
      
      {sys/inc/roundup.i v-qty}

    /*  v-part-fg = IF rd_part-fg BEGINS "Cust" THEN v-cust-part ELSE v-i-no.*/

     /* If 'full cost' selected and history full cost > zero, 
        then print history full cost. */
     IF v-full-cost = YES AND ar-invl.spare-dec-1 > 0 THEN DO:
/*          ASSIGN v-cost = ar-invl.spare-dec-1. */
         IF ar-invl.dscr[1] EQ "M" OR
            ar-invl.dscr[1] EQ "" THEN
             v-cost = ar-invl.spare-dec-1 * (ar-invl.inv-qty / 1000) * v-slsp[1] / 100.
         ELSE /*EA*/
             v-cost = ar-invl.spare-dec-1 * ar-invl.inv-qty * v-slsp[1] / 100.
     END.
        

     /* if not v-sumdet then DO:
         display tt-report.key-01       when first-of(tt-report.key-01)
                                        format "x(3)"
/*                 space(1)                                                               */
/*                 tt-report.key-02        when first-of(tt-report.key-02) FORMAT "x(8)"  */
               SPACE(1)
                cust.name               when first-of(tt-report.key-02)
                                        format "x(19)" 
                cust.TYPE               when first-of(tt-report.key-02) format "x(8)"
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
                cust.TYPE FORMAT "x(8)" v-comma
                v-part-fg v-comma
                v-ord-no  v-comma
                v-inv-no  v-comma
                v-procat  v-comma
                v-qty     format "->>>>>>>9"        v-comma
                v-amt     format "->>>>>>>9.99"     v-comma
                (IF v-print-cost THEN string(v-cost,"->>>>>>>9.99") ELSE "")  FORM "x(20)"   v-comma
                (IF v-print-cost THEN STRING(v-gp,"->>>>9.99") ELSE "")       FORM "x(20)"   v-comma
                v-camt    format "->>>>>9.99"       v-comma
                v-comm    format "->>>9.99"         v-comma
                cust.curr-code                      v-comma
                v-inv-date                          v-comma
                cWhse                               
               SKIP.
      END. */

            ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
         
         IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "ar-inv" THEN hField = IF AVAIL bar-inv THEN BUFFER bar-inv:BUFFER-FIELD(cTmpField) ELSE ?.
                 
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     /*/IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-rcpth.job-no2,"99") ELSE "".                  */

                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
         END.
         ELSE DO: 
             
            CASE cTmpField: 
                 WHEN "sman" THEN cVarValue = IF AVAIL tt-report THEN string(tt-report.key-01,"x(3)") ELSE "". 
                 WHEN "cust-no" THEN cVarValue = IF AVAIL tt-report THEN string(tt-report.key-02) ELSE "".
                 WHEN "cust-nam" THEN cVarValue = IF AVAIL cust THEN string(cust.name,"x(19)") ELSE "".
                 WHEN "type" THEN cVarValue = IF AVAIL cust THEN string(cust.TYPE) ELSE "".
                 WHEN "i-no" THEN cVarValue = string(v-i-no).          
                 WHEN "part-no" THEN cVarValue = string(v-cust-part) .            
                 WHEN "ord" THEN cVarValue = string(v-ord-no).            
                 WHEN "inv" THEN cVarValue = STRING(v-inv-no).
                 WHEN "cat" THEN cVarValue = STRING(v-procat).
                 WHEN "qty" THEN cVarValue = string(v-qty,"->>>>>>>9"). 
                 WHEN "sel-pric" THEN cVarValue = string(v-amt,"->>>>>>>9.99") .
                 WHEN "totl-cst" THEN cVarValue = string(v-cost,"->>>>>>>9.99").
                 WHEN "v-gp" THEN cVarValue = string(v-gp,"->>>>9.99") .
                 WHEN "v-camt" THEN cVarValue = string(v-camt,"->>>>>9.99").
                 WHEN "v-comm" THEN cVarValue = string(v-comm,"->>>9.99") .
                 WHEN "grp" THEN cVarValue = IF AVAIL cust THEN string(cust.spare-char-2,"x(8)") ELSE "" .
                 WHEN "curr" THEN cVarValue = IF AVAIL cust THEN string(cust.curr-code) ELSE "".
                 WHEN "inv-date" THEN cVarValue = string(v-inv-date,"99/99/9999") .
                 WHEN "ware-house" THEN cVarValue = IF cWhse NE "" THEN cWhse ELSE "".
            END CASE.
            
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END. 
      END.
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
       cExcelDisplay = cExcelDisplay.
       PUT STREAM st-excell UNFORMATTED  
               cExcelDisplay SKIP.
      END.

      if last-of(tt-report.key-02) then do:
        assign
         v-comm = v-tot-camt[1] / v-tot-samt[1] * 100
         v-cost = (v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100
         v-gp   = round((v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100,2).

        if v-comm = ? then v-comm = 0.
        if v-cost = ? then v-cost = 0.
        if v-gp   = ? then v-gp   = 0.

     /*   if v-sumdet THEN DO:

          display p-sman                format "x(10)"
/*                   tt-report.key-02  */
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
        END. */
        
        /*else do:*/
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
            
          /*  display "Customer Totals:"    @ cust.name
                    v-tot-samt[1]               @ v-amt
                    v-tot-cost[1]   WHEN v-print-cost            @ v-cost      
                    v-gp            WHEN v-print-cost
                    v-tot-camt[1]               @ v-camt
                    v-comm

                with frame detail. */
                
          PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                 WHEN "sman" THEN cVarValue = "". 
                 WHEN "cust-no" THEN cVarValue = "".
                 WHEN "cust-nam" THEN cVarValue = "".
                 WHEN "type" THEN cVarValue = "".
                 WHEN "i-no" THEN cVarValue = "".          
                 WHEN "part-no" THEN cVarValue = "" .            
                 WHEN "ord" THEN cVarValue = "".            
                 WHEN "inv" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = "". 
                 WHEN "sel-pric" THEN cVarValue = string(v-tot-samt[1],"->>>>>>>9.99") .
                 WHEN "totl-cst" THEN cVarValue = string(v-tot-cost[1],"->>>>>>>9.99").
                 WHEN "v-gp" THEN cVarValue = string(v-gp,"->>>>9.99") .
                 WHEN "v-camt" THEN cVarValue = string(v-tot-camt[1],"->>>>>9.99").
                 WHEN "v-comm" THEN cVarValue = string(v-comm,"->>>9.99") .
                 WHEN "grp" THEN cVarValue = "" .
                 WHEN "curr" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "ware-house" THEN cVarValue = "".
              END CASE.
              
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       * Customer Totals *" substring(cDisplay,27,300) SKIP.
/*          IF tb_excel THEN DO:                                              */
/*              cExcelDisplay = cExcelDisplay.                                */
/*              PUT STREAM st-excell UNFORMATTED                              */
/*                  "Customer Totals " + substring(cExcelDisplay,3,300) SKIP. */
/*          END.                                                              */
                
            put skip(1).
          end.
          
          else
            display skip(1) with frame skip-a-line no-box no-labels stream-io.
       /* end.*/

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

        IF v-show-sls-cat THEN DO:

          FOR EACH tt-slsrp WHERE tt-slsrp.sman = tt-report.key-01
                BREAK BY tt-slsrp.sman BY tt-slsrp.scat.

              v-srs-comm = tt-slsrp.camt / tt-slsrp.samt * 100.
              v-srs-cost = tt-slsrp.cost.
              v-srs-gp   = round((tt-slsrp.samt - tt-slsrp.cost) / tt-slsrp.samt * 100,2).

              /*  if v-sumdet THEN DO:
                   IF FIRST(tt-slsrp.sman) THEN
                       PUT SKIP(1).
                   DISPLAY
                          space(5)
                          "Category  Totals for" WHEN FIRST(tt-slsrp.sman)
                          SPACE(1)
                          tt-slsrp.sman WHEN FIRST(tt-slsrp.sman)
                          space(1)
                          tt-slsrp.scat
                          SPACE(7)
                          tt-slsrp.samt
                          /* space(2) */
                          tt-slsrp.camt FORMAT "->>>>>>>>>>>>"
                          space(2)
                          v-srs-comm              format "->>>9.99"
                          space(4)
                          v-srs-cost WHEN v-print-cost
                          space(2)
                          v-srs-gp   WHEN v-print-cost      format "->>>9.99"
                          SKIP
                      with frame Salesman-cat-sum no-box no-labels stream-io width 200.

                  IF tb_excel THEN
                      IF v-print-cost THEN
                        PUT STREAM st-excell
                           ",,,,,,,,"
                           tt-slsrp.scat  format "x(8)" v-comma
                           tt-slsrp.samt FORMAT "->>>>>>>>.99" v-comma
                           tt-slsrp.camt   v-comma
                           v-srs-comm v-comma
                           v-srs-cost    FORMAT "->>>>>>>>.99" v-comma
                           v-srs-gp      format "->>>9.99" v-comma
                           SKIP.
                      ELSE
                         PUT STREAM st-excell
                             ",,,,,,,,"
                             tt-slsrp.scat  format "x(8)" v-comma
                             tt-slsrp.samt FORMAT "->>>>>>>>.99" v-comma
                             tt-slsrp.camt   v-comma
                             v-srs-comm v-comma
                             SKIP.
                END. /* if v-sumdet */
                else do:
                   down with frame detail.
                   IF FIRST(tt-slsrp.sman) THEN
                       PUT SKIP(1).


                     display " Category Totals" WHEN FIRST(tt-slsrp.sman) @ cust.name
                          "For " + tt-slsrp.sman WHEN FIRST(tt-slsrp.sman) @ v-part-fg
                          tt-slsrp.scat                @ v-procat
                          tt-slsrp.samt                @ v-amt
                          v-srs-cost WHEN v-print-cost @ v-cost      
                          v-srs-gp   WHEN v-print-cost @ v-gp
                          tt-slsrp.camt                @ v-camt
                          v-srs-comm                   @ v-comm
                      with frame detail.

                  IF tb_excel THEN DO:
                      IF  v-print-cost THEN
                        PUT STREAM st-excell
                           ",,,,,,,,,,,,,,,,,"
                           tt-slsrp.scat   v-comma
                           tt-slsrp.samt   FORMAT "->>>>>>>>.99" v-comma
                           tt-slsrp.camt   FORMAT "->>>>>>>.99" v-comma
                           v-srs-comm      v-comma
                           v-srs-cost      FORMAT "->>>>>>>>.99" v-comma
                           v-srs-gp      format "->>>9.99" v-comma
                           SKIP.
                      ELSE
                          PUT STREAM st-excell
                             ",,,,,,,,,,,,,,,,,"
                             tt-slsrp.scat   v-comma
                             tt-slsrp.samt   FORMAT "->>>>>>>>.99" v-comma
                             tt-slsrp.camt   FORMAT "->>>>>>>.99" v-comma
                             v-srs-comm      v-comma
                             SKIP.

                  END. /* tb_excel */
                END. /* not v-sumdet */  */

                PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                 WHEN "sman" THEN cVarValue = "". 
                 WHEN "cust-no" THEN cVarValue = "".
                 WHEN "cust-nam" THEN cVarValue = "".
                 WHEN "type" THEN cVarValue = "".
                 WHEN "i-no" THEN cVarValue = "".          
                 WHEN "part-no" THEN cVarValue = "" .            
                 WHEN "ord" THEN cVarValue = "".            
                 WHEN "inv" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = STRING(tt-slsrp.scat).
                 WHEN "qty" THEN cVarValue = "". 
                 WHEN "sel-pric" THEN cVarValue = string(tt-slsrp.samt,"->>>>>>>9.99") .
                 WHEN "totl-cst" THEN cVarValue = string(v-srs-cost,"->>>>>>>9.99").
                 WHEN "v-gp" THEN cVarValue = string(v-srs-gp,"->>>>9.99") .
                 WHEN "v-camt" THEN cVarValue = string(tt-slsrp.camt,"->>>>>9.99").
                 WHEN "v-comm" THEN cVarValue = string(v-srs-comm,"->>>9.99") .
                 WHEN "grp" THEN cVarValue = "" .
                 WHEN "curr" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "ware-house" THEN cVarValue = "".
              END CASE.
              
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       * Category  Totals *" substring(cDisplay,28,300) SKIP.
/*          IF tb_excel THEN DO:                                               */
/*              cExcelDisplay = cExcelDisplay.                                 */
/*              PUT STREAM st-excell UNFORMATTED                               */
/*                  "Category  Totals " + substring(cExcelDisplay,3,300) SKIP. */
/*          END.                                                               */
          

                DELETE tt-slsrp.
            end. /* for each */
        END.
        
        if ((not v-frst[2]) and (not last(tt-report.key-01))) or
           ((not v-frst[1]) and last(tt-report.key-01))       THEN DO:
        
           
           /* if v-sumdet THEN DO:

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
            ELSE do:
              down with frame detail.
              put skip(1).
            
              display "Sales Rep Totals:"    @ cust.name
                      v-tot-samt[2]               @ v-amt
                      v-tot-cost[2] WHEN v-print-cost              @ v-cost      
                      v-gp          WHEN v-print-cost
                      v-tot-camt[2]               @ v-camt
                      v-comm
                      with frame detail.

            end. */

         PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                 WHEN "sman" THEN cVarValue = "". 
                 WHEN "cust-no" THEN cVarValue = "".
                 WHEN "cust-nam" THEN cVarValue = "".
                 WHEN "type" THEN cVarValue = "".
                 WHEN "i-no" THEN cVarValue = "".          
                 WHEN "part-no" THEN cVarValue = "" .            
                 WHEN "ord" THEN cVarValue = "".            
                 WHEN "inv" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = "". 
                 WHEN "sel-pric" THEN cVarValue = string(v-tot-samt[2],"->>>>>>>9.99") .
                 WHEN "totl-cst" THEN cVarValue = string(v-tot-cost[2],"->>>>>>>9.99").
                 WHEN "v-gp" THEN cVarValue = string(v-gp,"->>>>9.99") .
                 WHEN "v-camt" THEN cVarValue = string(v-tot-camt[2],"->>>>>9.99").
                 WHEN "v-comm" THEN cVarValue = string(v-comm,"->>>9.99") .
                 WHEN "grp" THEN cVarValue = "" .
                 WHEN "curr" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "ware-house" THEN cVarValue = "".
              END CASE.
              
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       * Sales Rep Totals: *" substring(cDisplay,29,300) SKIP.
/*          IF tb_excel THEN DO:                                                */
/*              cExcelDisplay = cExcelDisplay.                                  */
/*              PUT STREAM st-excell UNFORMATTED                                */
/*                  "Sales Rep Totals: " + substring(cExcelDisplay,3,300) SKIP. */
/*          END.                                                                */

        END. /* sales rep totals */
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

  /*  if not v-sumdet then do:
      assign
       str-tit3 = (if v-per-rpt then "P" else "Y") +
                  "TD (" + string(v-date[1]) + "-" + string(v-date[2]) +
                  ") - By Sales Rep"
       {sys/inc/ctrtext.i str-tit3 132}

       v-head[2] =
                 "Sales Rep                                           Total Sa" +
                 "les $        Comm $    Comm %        Cost $      GP %"
       v-head[3] = fill("-",112).


      page. */

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

    /*    display w-comm.sman         format "x(3)"
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

            with frame Salesman-det no-box no-labels stream-io width 200. */

      end.  /* recap-work */
  /*  end.   */

    assign
     v-comm = v-tot-camt[3] / v-tot-samt[3] * 100.
     v-cost = (v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100.

    if v-comm = ? then v-comm = 0.
    if v-cost = ? then v-cost = 0.

  /*  display skip(1)
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

        with frame grand-tot no-box no-labels stream-io width 200.  */

    PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                 WHEN "sman" THEN cVarValue = "". 
                 WHEN "cust-no" THEN cVarValue = "".
                 WHEN "cust-nam" THEN cVarValue = "".
                 WHEN "type" THEN cVarValue = "".
                 WHEN "i-no" THEN cVarValue = "".          
                 WHEN "part-no" THEN cVarValue = "" .            
                 WHEN "ord" THEN cVarValue = "".            
                 WHEN "inv" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = "". 
                 WHEN "sel-pric" THEN cVarValue = string(v-tot-samt[3],"->>>>>>>9.99") .
                 WHEN "totl-cst" THEN cVarValue = string(v-tot-cost[3],"->>>>>>>9.99").
                 WHEN "v-gp" THEN cVarValue = string(v-cost,"->>>>9.99") .
                 WHEN "v-camt" THEN cVarValue = string(v-tot-camt[3],"->>>>>9.99").
                 WHEN "v-comm" THEN cVarValue = string(v-comm,"->>>9.99") .
                 WHEN "grp" THEN cVarValue = "" .
                 WHEN "curr" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "ware-house" THEN cVarValue = "".
              END CASE.
              
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       * Grand Totals: *" substring(cDisplay,25,300) SKIP.
/*          IF tb_excel THEN DO:                                            */
/*              cExcelDisplay = cExcelDisplay.                              */
/*              PUT STREAM st-excell UNFORMATTED                            */
/*                  "Grand Totals: " + substring(cExcelDisplay,3,300) SKIP. */
/*          END.                                                            */
    
        
   
