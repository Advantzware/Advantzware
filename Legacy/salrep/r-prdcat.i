  
  find first period
      where period.company eq cocode
        and period.pst     le tdate
        and period.pend    ge tdate
      NO-LOCK NO-ERROR.

  IF AVAIL period THEN
     assign
        v-period = period.pnum
        v-year   = period.yr
        fdate[2] = period.pst.

  ASSIGN
   edate[2] = tdate
   fdate[1] = tdate
   edate[1] = tdate
   fdate[3] = sdate
   edate[3] = tdate.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  for each ar-inv
      where ar-inv.company  eq cocode
        and ar-inv.posted   eq yes
        AND ar-inv.cust-no  GE fcust
        AND ar-inv.cust-no  LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
        AND ttCustList.log-fld no-lock) else true)
        and ar-inv.inv-date ge fdate[3]
        and ar-inv.inv-date le edate[3]
        and (ar-inv.type    ne "FC" or v-inc-fc)
      use-index inv-date no-lock,

      first cust
      where cust.company eq ar-inv.company
        and cust.cust-no eq ar-inv.cust-no
      no-lock,

      each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
        and (ar-invl.billable or not ar-invl.misc)
      no-lock:
     {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    {sa/sa-sman3.i ar-invl}
  end.

  for each cust
      where cust.company eq cocode
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
        AND ttCustList.log-fld no-lock) else true)
      no-lock,
      each ar-cash
      where ar-cash.company    eq cocode
        and ar-cash.cust-no    eq cust.cust-no
        and ar-cash.check-date ge fdate[3]
        and ar-cash.check-date le edate[3]
        and ar-cash.posted     eq yes
      no-lock,

      EACH ar-cashl
      WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
                     WHERE account.company EQ ar-cashl.company
                       AND account.actnum  EQ ar-cashl.actnum
                       AND account.type    EQ "R")
      NO-LOCK:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    RELEASE ar-invl.

    RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

    IF AVAIL oe-retl THEN
    find first ar-invl
        where ar-invl.company eq cocode
          and ar-invl.cust-no eq ar-cash.cust-no
          and ar-invl.inv-no  eq ar-cashl.inv-no
          and ar-invl.i-no    eq oe-retl.i-no
          and (ar-invl.billable or not ar-invl.misc)
        no-lock no-error.

    if avail ar-invl then do:
       find ar-inv where ar-inv.x-no eq ar-invl.x-no NO-LOCK.
       {sa/sa-sman3.i "ar-cashl"}
    end.

    else
    if cust.sman ge fsman and
       cust.sman le tsman and
       "MEMO"    ge fcat  and
       "MEMO"    le tcat  then do:
      
      IF AVAIL oe-retl THEN DO:
        cp-part-no = "".
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ oe-retl.company
              AND itemfg.i-no    EQ oe-retl.i-no
            NO-ERROR.
        IF AVAIL itemfg AND rd_fg-cp EQ "Cust Part#" THEN DO:
          cp-rowid = ROWID(itemfg).
          RUN custom/getcpart.p (oe-retl.company, cust.cust-no,
                                 INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
          IF TRIM(cp-part-no) EQ "" THEN cp-part-no = itemfg.part-no.
        END.
        IF TRIM(cp-part-no) EQ "" THEN cp-part-no = oe-retl.i-no.
      END.

      ELSE cp-part-no = "MEMO".

      create tt-report.
      assign
       tt-report.key-01  = IF AVAIL reftable                      OR
                              ar-cashl.dscr MATCHES "*oe return*" THEN "2"
                           ELSE "3"
       tt-report.key-02  = "MEMO"
       tt-report.key-03  = cust.sman
       tt-report.key-04  = cust.cust-no
       tt-report.key-05  = cp-part-no
       tt-report.key-10  = "ar-cashl"
       tt-report.rec-id  = recid(ar-cashl).
    end.
  end.

  for each tt-report

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05
      WITH FRAME detail:
      {custom/statusMsg.i " 'Processing Customer#  '  + tt-report.key-04 "}
    find first w-data no-error.

    if first-of(tt-report.key-05) then create w-data.

    find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

    if avail ar-invl then do:
      find ar-inv where ar-inv.x-no eq ar-invl.x-no NO-LOCK.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq ar-invl.i-no
          no-lock no-error.

      assign
       v-pct  = 1
       v-amt  = ar-invl.amt
       v-cost = ar-invl.t-cost
       v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                else
                if avail itemfg then
                  (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

      if v-amt  eq ? then v-amt  = 0.
      if v-cost eq ? then v-cost = 0.
      if v-sqft eq ? then v-sqft = 0.

      do i = 1 to 3:
        if ar-invl.sman[i] eq tt-report.key-02 then
          assign
           v-pct = ar-invl.s-pct[i] / 100
           i     = 3.
      end.

      if v-pct eq 0 then
      do i = 1 to 3:
        if i eq 1 then j = 0.
        if ar-invl.sman[i] ne "" then j = j + 1.
        if i eq 3 then v-pct = 1 / j.
      end.

      if v-pct le 0 or v-pct eq ? then v-pct = 1.

      do i = 1 to 3:
        if ar-inv.inv-date ge fdate[i] and
           ar-inv.inv-date le edate[i] then
          assign
           w-data.w-sqft[i] = w-data.w-sqft[i] + (v-sqft * v-pct)
           w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct)
           w-data.w-cost[i] = w-data.w-cost[i] + (v-cost * v-pct).
      end.
    end.

    else do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

      if avail ar-cashl then do:
        find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

        assign
         v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-cost = 0
         v-sqft = 0
         v-pct  = 1.

        RELEASE itemfg.
        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        IF AVAIL oe-retl THEN DO:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-retl.i-no
              no-lock no-error.

          v-sqft = if avail itemfg then
                     (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                   else 0.

          if v-sqft eq ? then v-sqft = 0.

          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
              no-lock no-error.

          if avail ar-invl then do:
            do i = 1 to 3:
              if ar-invl.sman[i] eq tt-report.key-02 then
                assign
                 v-pct = ar-invl.s-pct[i] / 100
                 i     = 3.
            end.

            if v-pct eq 0 then
            do i = 1 to 3:
              if i eq 1 then j = 0.
              if ar-invl.sman[i] ne "" then j = j + 1.
              if i eq 3 then v-pct = 1 / j.
            end.

            if v-pct le 0 or v-pct eq ? then v-pct = 1.

            RUN salrep/salecost.p (3,
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no,
                                   oe-retl.tot-qty-return,
                                   OUTPUT v-cost).
          end.
        end.

        do i = 1 to 3:
          if ar-cash.check-date ge fdate[i] and
             ar-cash.check-date le edate[i] then
            assign
             w-data.w-sqft[i] = w-data.w-sqft[i] - (v-sqft * v-pct)
             w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct)
             w-data.w-cost[i] = w-data.w-cost[i] - (v-cost * v-pct).
        end.
      end.
    end.

    if first-of(tt-report.key-02) and tt-report.key-01 eq "1" then put skip(2).

    if last-of(tt-report.key-05) then do:
      if tt-report.key-01 eq "1" and (not v-sum) then do:
        do i = 1 to 3:
          v-prof[i] = (w-data.w-amt[i] - w-data.w-cost[i]) /
                      w-data.w-amt[i] * 100.
          if v-prof[i] eq ? then v-prof[i] = 0.
        end.

        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report.key-03
            no-lock no-error.
        
        IF NOT tb_rem-cents THEN
           ASSIGN w-data.w-amt-txt[1] = STRING(w-data.w-amt[1],"->>>,>>9.99")
                  w-data.w-amt-txt[2] = STRING(w-data.w-amt[2],"->>>,>>9.99")
                  w-data.w-amt-txt[3] = STRING(w-data.w-amt[3],"->>>,>>9.99")
                  w-data.w-cost-txt[1] = STRING(w-data.w-cost[1],"->>,>>9.99")
                  w-data.w-cost-txt[2] = STRING(w-data.w-cost[2],"->>,>>9.99")
                  w-data.w-cost-txt[3] = STRING(w-data.w-cost[3],"->>,>>9.99").
        ELSE
           ASSIGN w-data.w-amt-txt[1] = FILL(" ",3) + STRING(w-data.w-amt[1],"->>>,>>9")
                  w-data.w-amt-txt[2] = FILL(" ",3) + STRING(w-data.w-amt[2],"->>>,>>9")
                  w-data.w-amt-txt[3] = FILL(" ",3) + STRING(w-data.w-amt[3],"->>>,>>9")
                  w-data.w-cost-txt[1] = FILL(" ",3) + STRING(w-data.w-cost[1],"->>,>>9")
                  w-data.w-cost-txt[2] = FILL(" ",3) + STRING(w-data.w-cost[2],"->>,>>9")
                  w-data.w-cost-txt[3] = FILL(" ",3) + STRING(w-data.w-cost[3],"->>,>>9").

        display tt-report.key-02
                tt-report.key-03   
                sman.sname              when avail sman
                tt-report.key-04
                tt-report.key-05
                w-data.w-sqft[1 for 3]
                w-data.w-amt-txt[1 for 3]
                w-data.w-cost-txt[1 for 3]  when v-cst
                v-prof[1 for 3]         when v-cst.
               
        down.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' tt-report.key-02                            '",'
               '"' tt-report.key-03                            '",'
               '"' IF avail sman THEN sman.sname ELSE ""       '",'
               '"' tt-report.key-04                            '",'
               '"' tt-report.key-05                            '",'
               '"' STRING(w-data.w-sqft[1],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(w-data.w-amt[1],"->>>,>>9.99")
                   ELSE STRING(w-data.w-amt[1],"->>>,>>9")      '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(w-data.w-cost[1],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(w-data.w-cost[1],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[1],"->>>9.99")
                   ELSE ""                                     '",'
               '"' STRING(w-data.w-sqft[2],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(w-data.w-amt[2],"->>>,>>9.99")
                   ELSE STRING(w-data.w-amt[2],"->>>,>>9")     '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(w-data.w-cost[2],"->>,>>9.99") 
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(w-data.w-cost[2],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[2],"->>>9.99")
                   ELSE ""                                     '",'
               '"' STRING(w-data.w-sqft[3],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(w-data.w-amt[3],"->>>,>>9.99")
                   ELSE STRING(w-data.w-amt[3],"->>>,>>9")       '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(w-data.w-cost[3],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(w-data.w-cost[3],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[3],"->>>9.99")
                   ELSE ""                                     '",'
               SKIP.
      end.

      do i = 1 to 3:
        assign
         v-tot-sqft[i] = v-tot-sqft[i] + w-data.w-sqft[i]
         v-tot-amt[i]  = v-tot-amt[i]  + w-data.w-amt[i]
         v-tot-cost[i] = v-tot-cost[i] + w-data.w-cost[i].
      end.

      delete w-data.
    end.

    if last-of(tt-report.key-04) then do:
      if tt-report.key-01 eq "1" and (not v-sum or v-sum1) then do:
        if not v-sum then do:
           underline w-data.w-sqft[1 for 3]
                     w-data.w-amt-txt[1 for 3]
                     w-data.w-cost-txt[1 for 3]
                     v-prof[1 for 3].

          down.
        end.
      
        do i = 1 to 3:
          v-prof[i] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
          if v-prof[i] eq ? then v-prof[i] = 0.
        end.

        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report.key-03
            no-lock no-error.

        RUN text-proc(INPUT 1).

        display tt-report.key-02    when v-sum
                tt-report.key-03    when v-sum
                sman.sname       when v-sum and avail sman
                "     CUSTOMER TOTALS" when not v-sum @ sman.sname 
                tt-report.key-04    when v-sum   
                "ALL"            when v-sum @ tt-report.key-05
                v-tot-sqft[1]               @ w-data.w-sqft[1]
                v-tot-amt-txt[1]            @ w-data.w-amt-txt[1]
                v-tot-cost-txt[1]    when v-cst @ w-data.w-cost-txt[1]
                v-prof[1]        when v-cst
                v-tot-sqft[2]               @ w-data.w-sqft[2]                  
                v-tot-amt-txt[2]                @ w-data.w-amt-txt[2]
                v-tot-cost-txt[2]    when v-cst @ w-data.w-cost-txt[2]
                v-prof[2]        when v-cst
                v-tot-sqft[3]               @ w-data.w-sqft[3]                  
                v-tot-amt-txt[3]                @ w-data.w-amt-txt[3]
                v-tot-cost-txt[3]    when v-cst @ w-data.w-cost-txt[3]
                v-prof[3]        when v-cst.
        
        down.

        IF tb_excel AND v-sum THEN
        DO:
           PUT STREAM excel UNFORMATTED
               '"' tt-report.key-02       '",'
               '"' tt-report.key-03       '",'
               '"' IF avail sman THEN sman.sname ELSE
                   "" '",'
               '"' tt-report.key-04       '",'
               '"' "ALL"                  '",'
               '"' STRING(v-tot-sqft[1],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(v-tot-amt[1],"->>>,>>9.99")
                   ELSE STRING(v-tot-amt[1],"->>>,>>9")       '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(v-tot-cost[1],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(v-tot-cost[1],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[1],"->>>9.99")
                   ELSE ""                                     '",'
               '"' STRING(v-tot-sqft[2],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(v-tot-amt[2],"->>>,>>9.99")
                   ELSE STRING(v-tot-amt[2],"->>>,>>9")        '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(v-tot-cost[2],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(v-tot-cost[2],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[2],"->>>9.99")
                   ELSE ""                                     '",'
               '"' STRING(v-tot-sqft[3],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(v-tot-amt[3],"->>>,>>9.99")
                   ELSE STRING(v-tot-amt[3],"->>>,>>9")       '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(v-tot-cost[3],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(v-tot-cost[3],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[3],"->>>9.99")
                   ELSE ""                                     '",'
                   SKIP.
        END.

        if not last-of(tt-report.key-03) then
        DO:
          put skip(1).
          IF tb_excel AND v-sum THEN
             PUT STREAM excel UNFORMATTED SKIP(1).
        END.
      end.

      do i = 4 to 6:
        assign
         v-tot-sqft[i] = v-tot-sqft[i] + v-tot-sqft[i - 3]
         v-tot-amt[i]  = v-tot-amt[i]  + v-tot-amt[i - 3]
         v-tot-cost[i] = v-tot-cost[i] + v-tot-cost[i - 3]

         v-tot-sqft[i - 3] = 0
         v-tot-amt[i - 3]  = 0
         v-tot-cost[i - 3] = 0.
      end.
    end.

    if last-of(tt-report.key-03) then do:
      if tt-report.key-01 eq "1" then do:
        if not v-sum or v-sum1 then do:
          
          underline w-data.w-sqft[1 for 3]
                     w-data.w-amt-txt[1 for 3]
                     w-data.w-cost-txt[1 for 3]
                     v-prof[1 for 3].
          down.
        end.  
      
        do i = 4 to 6:
          v-prof[i - 3] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
          if v-prof[i - 3] eq ? then v-prof[i - 3] = 0.
        end.

        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report.key-03
            no-lock no-error.

        RUN text-proc(INPUT 4).

        display tt-report.key-02    when not v-sum1 and v-sum
                tt-report.key-03    when not v-sum1 and v-sum
                sman.sname       when not v-sum1 and v-sum and avail sman
                "     SALESREP TOTALS" when v-sum1 or not v-sum @ sman.sname
                "ALL"            when not v-sum1 and v-sum @ tt-report.key-04
                "ALL"            when not v-sum1 and v-sum @ tt-report.key-05   
                v-tot-sqft[4]               @ w-data.w-sqft[1]
                v-tot-amt-txt[4]            @ w-data.w-amt-txt[1]
                v-tot-cost-txt[4]    when v-cst @ w-data.w-cost-txt[1]
                v-prof[1]        when v-cst
                v-tot-sqft[5]               @ w-data.w-sqft[2]
                v-tot-amt-txt[5]            @ w-data.w-amt-txt[2]
                v-tot-cost-txt[5]    when v-cst @ w-data.w-cost-txt[2]
                v-prof[2]        when v-cst
                v-tot-sqft[6]               @ w-data.w-sqft[3]
                v-tot-amt-txt[6]            @ w-data.w-amt-txt[3]
                v-tot-cost-txt[6]    when v-cst @ w-data.w-cost-txt[3]
                v-prof[3]        when v-cst.
        down.
      
        IF tb_excel AND NOT(v-sum1 OR NOT v-sum) THEN
        DO:
           IF v-sum1 or not v-sum THEN
              PUT STREAM excel UNFORMATTED SKIP(1).

           PUT STREAM excel UNFORMATTED
               '"' IF not v-sum1 AND v-sum THEN tt-report.key-02
                   ELSE ""      '",'
               '"' IF not v-sum1 AND v-sum THEN tt-report.key-03
                   ELSE ""      '",'
               '"' IF not v-sum1 AND v-sum AND avail sman THEN sman.sname
                   ELSE IF v-sum1 OR NOT v-sum THEN "     SALESREP TOTALS" ELSE "" '",'
               '"' IF NOT v-sum1 AND v-sum THEN "ALL" ELSE ""      '",'
               '"' IF NOT v-sum1 AND v-sum THEN "ALL" ELSE ""      '",'
               '"' STRING(v-tot-sqft[4],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(v-tot-amt[4],"->>>,>>9.99")
                   ELSE STRING(v-tot-amt[4],"->>>,>>9")       '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(v-tot-cost[4],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(v-tot-cost[4],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[1],"->>>9.99")
                   ELSE ""                                     '",'
               '"' STRING(v-tot-sqft[5],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(v-tot-amt[5],"->>>,>>9.99")
                   ELSE STRING(v-tot-amt[5],"->>>,>>9")       '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(v-tot-cost[5],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(v-tot-cost[5],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[2],"->>>9.99")
                   ELSE ""                                     '",'
               '"' STRING(v-tot-sqft[6],"->>>9.999")        '",'
               '"' IF NOT tb_rem-cents THEN
                      STRING(v-tot-amt[6],"->>>,>>9.99")
                   ELSE
                      STRING(v-tot-amt[6],"->>>,>>9")       '",'
               '"' IF v-cst AND NOT tb_rem-cents THEN
                      STRING(v-tot-cost[6],"->>,>>9.99")
                   ELSE IF v-cst AND tb_rem-cents THEN
                      STRING(v-tot-cost[6],"->>,>>9")
                   ELSE ""                                     '",'
               '"' IF v-cst THEN STRING(v-prof[3],"->>>9.99")
                   ELSE ""                                     '",'
                   SKIP.
        END.

        if not last-of(tt-report.key-02) and 
           (not v-sum or v-sum1)      THEN
        DO:
           put skip(1).
           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED SKIP(1).
        END.
      end.

      do i = 7 to 9:
        assign
         v-tot-sqft[i] = v-tot-sqft[i] + v-tot-sqft[i - 3]
         v-tot-amt[i]  = v-tot-amt[i]  + v-tot-amt[i - 3]
         v-tot-cost[i] = v-tot-cost[i] + v-tot-cost[i - 3]

         v-tot-sqft[i - 3] = 0
         v-tot-amt[i - 3]  = 0
         v-tot-cost[i - 3] = 0.
      end.
    end.

    if last-of(tt-report.key-02) then do:
      if tt-report.key-01 eq "1" then
         underline w-data.w-sqft[1 for 3]
                   w-data.w-amt-txt[1 for 3]
                   w-data.w-cost-txt[1 for 3]
                   v-prof[1 for 3].
      down.

      do i = 7 to 9:
        v-prof[i - 6] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
        if v-prof[i - 6] eq ? then v-prof[i - 6] = 0.
      end.

      if tt-report.key-01 eq "1" then
      DO:
         RUN text-proc(INPUT 7).

         display "     CATEGORY TOTALS"      @ sman.sname
                 v-tot-sqft[7]               @ w-data.w-sqft[1]
                 v-tot-amt-txt[7]            @ w-data.w-amt-txt[1]
                 v-tot-cost-txt[7]   when v-cst  @ w-data.w-cost-txt[1]
                 v-prof[1]       when v-cst
                 v-tot-sqft[8]               @ w-data.w-sqft[2]
                 v-tot-amt-txt[8]                @ w-data.w-amt-txt[2]
                 v-tot-cost-txt[8]   when v-cst  @ w-data.w-cost-txt[2]
                 v-prof[2]       when v-cst
                 v-tot-sqft[9]               @ w-data.w-sqft[3]
                 v-tot-amt-txt[9]                @ w-data.w-amt-txt[3]
                 v-tot-cost-txt[9]   when v-cst  @ w-data.w-cost-txt[3]
                 v-prof[3]       when v-cst.
      END.

      down.

      do i = 10 to 12:
        assign
         v-tot-sqft[i] = v-tot-sqft[i] + v-tot-sqft[i - 3]
         v-tot-amt[i]  = v-tot-amt[i]  + v-tot-amt[i - 3]
         v-tot-cost[i] = v-tot-cost[i] + v-tot-cost[i - 3]

         v-tot-sqft[i - 3] = 0
         v-tot-amt[i - 3]  = 0
         v-tot-cost[i - 3] = 0.
      end.
    end.

    if last-of(tt-report.key-01) then do:
      if tt-report.key-01 eq "1" then
         underline w-data.w-sqft[1 for 3]
                   w-data.w-amt-txt[1 for 3]
                   w-data.w-cost-txt[1 for 3]
                   v-prof[1 for 3].

      down.

      do i = 10 to 12:
        v-prof[i - 9] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
        if v-prof[i - 9] eq ? then v-prof[i - 9] = 0.
      end.

      if tt-report.key-01 eq "1" then
         display "         TOTAL SALES"  @ sman.sname.
      else
      if tt-report.key-01 eq "2" then
         display "                MISC"  @ sman.sname.
         
      else
        display "                MEMO"  @ sman.sname.
      
      RUN text-proc(INPUT 10).

      display v-tot-sqft[10]            @ w-data.w-sqft[1]
              v-tot-amt-txt[10]         @ w-data.w-amt-txt[1]
              v-tot-cost-txt[10] when v-cst @ w-data.w-cost-txt[1]
              v-prof[1]      when v-cst
              v-tot-sqft[11]            @ w-data.w-sqft[2]
              v-tot-amt-txt[11]             @ w-data.w-amt-txt[2]
              v-tot-cost-txt[11] when v-cst @ w-data.w-cost-txt[2]
              v-prof[2]      when v-cst
              v-tot-sqft[12]            @ w-data.w-sqft[3]
              v-tot-amt-txt[12]             @ w-data.w-amt-txt[3]
              v-tot-cost-txt[12] when v-cst @ w-data.w-cost-txt[3]
              v-prof[3]      when v-cst.
      
      down.

      if tt-report.key-01 eq "1" then do:
        underline w-data.w-sqft[1 for 3]
                  w-data.w-amt-txt[1 for 3]
                  w-data.w-cost-txt[1 for 3]
                  v-prof[1 for 3].
        down.
      end.

      do i = 13 to 15:
        assign
         v-tot-sqft[i] = v-tot-sqft[i] + (v-tot-sqft[i - 3] *
                                       if tt-report.key-01 eq "1" then 1 else -1)
         v-tot-amt[i]  = v-tot-amt[i]  + (v-tot-amt[i - 3] *
                                       if tt-report.key-01 eq "1" then 1 else -1)
         v-tot-cost[i] = v-tot-cost[i] + (v-tot-cost[i - 3] *
                                       if tt-report.key-01 eq "1" then 1 else -1)

         v-tot-sqft[i - 3] = 0
         v-tot-amt[i - 3]  = 0
         v-tot-cost[i - 3] = 0.
      end.
    end.

    if last(tt-report.key-01) then do:
      do i = 13 to 15:
        v-prof[i - 12] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
        if v-prof[i - 12] eq ? then v-prof[i - 12] = 0.
      end.

      RUN text-proc(INPUT 13).

      display "               SALES"     @ sman.sname
              v-tot-sqft[13]             @ w-data.w-sqft[1]
              v-tot-amt-txt[13]              @ w-data.w-amt-txt[1]
              v-tot-cost-txt[13] when v-cst  @ w-data.w-cost-txt[1]
              v-prof[1]      when v-cst
              v-tot-sqft[14]             @ w-data.w-sqft[2]
              v-tot-amt-txt[14]              @ w-data.w-amt-txt[2]
              v-tot-cost-txt[14] when v-cst  @ w-data.w-cost-txt[2]
              v-prof[2]      when v-cst
              v-tot-amt-txt[15]              @ w-data.w-amt-txt[3]
              v-tot-sqft[15]             @ w-data.w-sqft[3]
              v-tot-cost-txt[15] when v-cst  @ w-data.w-cost-txt[3]
              v-prof[3]      when v-cst.
      
      down.
    end.
  end.
