  
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
    {sa/sa-smanN3.i ar-invl}
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
       {sa/sa-smanN3.i "ar-cashl"}
    end.

    else
    if cust.sman ge fsman and
       cust.sman le tsman and
       "MEMO"    ge fcat  and
       "MEMO"    le tcat  then do:
      
      IF AVAIL oe-retl THEN DO:
        cp-part-no = "".
        cp-i-no = "" .
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ oe-retl.company
              AND itemfg.i-no    EQ oe-retl.i-no
            NO-ERROR.
        IF AVAIL itemfg /*AND rd_fg-cp EQ "Cust Part#"*/ THEN DO:
          cp-rowid = ROWID(itemfg).
          RUN custom/getcpart.p (oe-retl.company, cust.cust-no,
                                 INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
          IF TRIM(cp-part-no) EQ "" THEN cp-part-no = itemfg.part-no.
        END.
        cp-i-no = oe-retl.i-no.
      END.
      ELSE do:
          ASSIGN cp-part-no = "MEMO"
              cp-i-no = "MEMO" .
      END. 
      


      create tt-report.
      assign
       tt-report.key-01  = IF AVAIL reftable                      OR
                              ar-cashl.dscr MATCHES "*oe return*" THEN "2"
                           ELSE "3"
       tt-report.key-02  = "MEMO"
       tt-report.key-03  = cust.sman
       tt-report.key-04  = cust.cust-no
       tt-report.key-05  = cp-part-no
       tt-report.key-06  = cp-i-no
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
            by tt-report.key-06
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
      if tt-report.key-01 eq "1" /*and (not v-sum)*/ then do:
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
           ASSIGN w-data.w-amt-txt[1] = STRING(w-data.w-amt[1],"->>>>>>9.99")
                  w-data.w-amt-txt[2] = STRING(w-data.w-amt[2],"->>>>>>9.99")
                  w-data.w-amt-txt[3] = STRING(w-data.w-amt[3],"->>>>>>9.99")
                  w-data.w-cost-txt[1] = STRING(w-data.w-cost[1],"->>>>>>9.99")
                  w-data.w-cost-txt[2] = STRING(w-data.w-cost[2],"->>>>>>9.99")
                  w-data.w-cost-txt[3] = STRING(w-data.w-cost[3],"->>>>>>9.99").
        ELSE
           ASSIGN w-data.w-amt-txt[1] = FILL(" ",3) + STRING(w-data.w-amt[1],"->>>>>>9")
                  w-data.w-amt-txt[2] = FILL(" ",3) + STRING(w-data.w-amt[2],"->>>>>>9")
                  w-data.w-amt-txt[3] = FILL(" ",3) + STRING(w-data.w-amt[3],"->>>>>>9")
                  w-data.w-cost-txt[1] = FILL(" ",3) + STRING(w-data.w-cost[1],"->>>>>>9")
                  w-data.w-cost-txt[2] = FILL(" ",3) + STRING(w-data.w-cost[2],"->>>>>>9")
                  w-data.w-cost-txt[3] = FILL(" ",3) + STRING(w-data.w-cost[3],"->>>>>>9"). 

      /*  display tt-report.key-02
                tt-report.key-03   
                sman.sname              when avail sman
                tt-report.key-04
                tt-report.key-05
                w-data.w-sqft[1 for 3]
                w-data.w-amt-txt[1 for 3]
                w-data.w-cost-txt[1 for 3]  when v-cst
                v-prof[1 for 3]         when v-cst. 
               
        down.                                       */

       /* IF tb_excel THEN
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
               SKIP. */

           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"       THEN cVarValue = string(tt-report.key-02) .
                         WHEN "rep"       THEN cVarValue = string(tt-report.key-03).
                         WHEN "name"      THEN cVarValue = IF avail sman THEN sman.sname ELSE "" .
                         WHEN "cust"      THEN cVarValue = tt-report.key-04  .
                         WHEN "ino"       THEN cVarValue = tt-report.key-06  .
                         WHEN "cust-prt"  THEN cVarValue = tt-report.key-05  .
                         WHEN "dly-sf"    THEN cVarValue = STRING(w-data.w-sqft[1],"->>>9.999") .
                         WHEN "dly-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(w-data.w-amt[1],"->>>,>>9.99") ELSE STRING(w-data.w-amt[1],"->>>,>>9") .
                         WHEN "dly-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(w-data.w-cost[1],"->>,>>9.99")ELSE IF v-cst AND tb_rem-cents THEN STRING(w-data.w-cost[1],"->>,>>9") ELSE "") .
                         WHEN "dly-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[1],"->>>9.99") ELSE "" .
                         WHEN "ptd-sf"    THEN cVarValue = STRING(w-data.w-sqft[2],"->>>9.999") .                                                                                                                       
                         WHEN "ptd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(w-data.w-amt[2],"->>>,>>9.99") ELSE STRING(w-data.w-amt[2],"->>>,>>9") .                                                     
                         WHEN "ptd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(w-data.w-cost[2],"->>,>>9.99")ELSE IF v-cst AND tb_rem-cents THEN STRING(w-data.w-cost[2],"->>,>>9") ELSE "") .   
                         WHEN "ptd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[2],"->>>9.99") ELSE "" .                                                                                                         
                         WHEN "ytd-sf"    THEN cVarValue = STRING(w-data.w-sqft[3],"->>>9.999") .                                                                                                                       
                         WHEN "ytd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(w-data.w-amt[3],"->>>>>>9.99") ELSE STRING(w-data.w-amt[3],"->>>>>>9") .                                                     
                         WHEN "ytd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(w-data.w-cost[3],"->>>>>>9.99")ELSE IF v-cst AND tb_rem-cents THEN STRING(w-data.w-cost[3],"->>>>>>9") ELSE "") .   
                         WHEN "ytd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[3],"->>>>>9.99") ELSE "" .                                                                                                         
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
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
      if tt-report.key-01 eq "1" /*and (not v-sum or v-sum1)*/ then do:
       /* if not v-sum then do:
           underline w-data.w-sqft[1 for 3]
                     w-data.w-amt-txt[1 for 3]
                     w-data.w-cost-txt[1 for 3]
                     v-prof[1 for 3].

          down.
        end. */
      
        do i = 1 to 3:
          v-prof[i] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
          if v-prof[i] eq ? then v-prof[i] = 0.
        end.

        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report.key-03
            no-lock no-error.

        RUN text-proc(INPUT 1).

        PUT SKIP str-line FORMAT "x(250)" SKIP .
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"       THEN cVarValue = "" .
                         WHEN "rep"       THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "ino"       THEN cVarValue = "" .
                         WHEN "cust-prt"  THEN cVarValue = ""  .
                         WHEN "dly-sf"    THEN cVarValue = STRING(v-tot-sqft[1],"->>>9.999") .
                         WHEN "dly-amt"   THEN cVarValue = (IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[1]) ELSE STRING(v-tot-amt-txt[1])) .
                         WHEN "dly-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[1]) ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[1]) ELSE "") .
                         WHEN "dly-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[1],"->>>9.99") ELSE "" .
                         WHEN "ptd-sf"    THEN cVarValue = STRING(v-tot-sqft[2],"->>>9.999") .                                                                                                                                                                                                     
                         WHEN "ptd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[2]) ELSE STRING(v-tot-amt-txt[2]) .                                                                                                                              
                         WHEN "ptd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[2])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[2]) ELSE "") .                                                                            
                         WHEN "ptd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[2],"->>>9.99") ELSE "" .                                                                                                                                                                                    
                         WHEN "ytd-sf"    THEN cVarValue = STRING(v-tot-sqft[3],"->>>9.999") .                                                                                                                                                                                                     
                         WHEN "ytd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[3]) ELSE STRING(v-tot-amt-txt[3]) .                                                                                                                              
                         WHEN "ytd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[3])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[3]) ELSE "") .                                                                            
                         WHEN "ytd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[3],"->>>>>9.99") ELSE "" .                                                                                                                                                                                    
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " CUSTOMER TOTALS"  substring(cDisplay,17,250) SKIP.
            IF NOT LAST-OF(tt-report.key-03) THEN PUT SKIP(1) .
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' CUSTOMER TOTALS ,'
                        substring(cExcelDisplay,4,250) SKIP.
             END.

      /*  display tt-report.key-02    when v-sum
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
        
        down. */

      /*  IF tb_excel AND v-sum THEN
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
        END. */
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
       /* if not v-sum or v-sum1 then do:
          
          underline w-data.w-sqft[1 for 3]
                     w-data.w-amt-txt[1 for 3]
                     w-data.w-cost-txt[1 for 3]
                     v-prof[1 for 3].
          down.
        end.  */
      
        do i = 4 to 6:
          v-prof[i - 3] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
          if v-prof[i - 3] eq ? then v-prof[i - 3] = 0.
        end.

        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report.key-03
            no-lock no-error.

        RUN text-proc(INPUT 4).

        PUT SKIP str-line  FORMAT "x(250)" SKIP .
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"       THEN cVarValue = "" .
                         WHEN "rep"       THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "ino"       THEN cVarValue = "" .
                         WHEN "cust-prt"  THEN cVarValue = ""  .
                         WHEN "dly-sf"    THEN cVarValue = STRING(v-tot-sqft[4],"->>>9.999") .
                         WHEN "dly-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[4]) ELSE STRING(v-tot-amt-txt[4]) .
                         WHEN "dly-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[4])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[4]) ELSE "") .
                         WHEN "dly-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[1],"->>>9.99") ELSE "" .
                         WHEN "ptd-sf"    THEN cVarValue = STRING(v-tot-sqft[5],"->>>9.999") .                                                                                                                                                                                                     
                         WHEN "ptd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[5]) ELSE STRING(v-tot-amt-txt[5]) .                                                                                                                              
                         WHEN "ptd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[5])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[5]) ELSE "") .                                                                            
                         WHEN "ptd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[2],"->>>9.99") ELSE "" .                                                                                                                                                                                    
                         WHEN "ytd-sf"    THEN cVarValue = STRING(v-tot-sqft[6],"->>>9.999") .                                                                                                                                                                                                     
                         WHEN "ytd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[6]) ELSE STRING(v-tot-amt-txt[6]) .                                                                                                                              
                         WHEN "ytd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[6])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[6]) ELSE "") .                                                                            
                         WHEN "ytd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[3],"->>>>>9.99") ELSE "" .                                                                                                                                                                                    
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " SALESREP TOTALS"  substring(cDisplay,17,250) SKIP.
            IF NOT LAST-OF(tt-report.key-02) THEN PUT SKIP(1) .
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' SALESREP TOTALS ,'
                        substring(cExcelDisplay,4,250) SKIP.
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
        /* underline w-data.w-sqft[1 for 3]
                   w-data.w-amt-txt[1 for 3]
                   w-data.w-cost-txt[1 for 3]
                   v-prof[1 for 3].
      down. */

      do i = 7 to 9:
        v-prof[i - 6] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
        if v-prof[i - 6] eq ? then v-prof[i - 6] = 0.
      end.

      if tt-report.key-01 eq "1" then
      DO:
         RUN text-proc(INPUT 7).

         PUT SKIP str-line  FORMAT "x(250)" SKIP .
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"       THEN cVarValue = "" .
                         WHEN "rep"       THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "ino"       THEN cVarValue = "" .
                         WHEN "cust-prt"  THEN cVarValue = ""  .
                         WHEN "dly-sf"    THEN cVarValue = STRING(v-tot-sqft[7],"->>>9.999") .
                         WHEN "dly-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[7]) ELSE STRING(v-tot-amt-txt[7]) .
                         WHEN "dly-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[7])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[7]) ELSE "") .
                         WHEN "dly-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[1],"->>>9.99") ELSE "" .
                         WHEN "ptd-sf"    THEN cVarValue = STRING(v-tot-sqft[8],"->>>9.999") .                                                                                                                                                                                           
                         WHEN "ptd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[8]) ELSE STRING(v-tot-amt-txt[8]) .                                                                                                                    
                         WHEN "ptd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[8])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[8]) ELSE "") .                                                                            
                         WHEN "ptd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[2],"->>>9.99") ELSE "" .                                                                                                                                                                          
                         WHEN "ytd-sf"    THEN cVarValue = STRING(v-tot-sqft[8],"->>>9.999") .                                                                                                                                                                                           
                         WHEN "ytd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[9]) ELSE STRING(v-tot-amt-txt[9]) .                                                                                                                    
                         WHEN "ytd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[9])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[9]) ELSE "") .                                                                            
                         WHEN "ytd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[3],"->>>>>9.99") ELSE "" .                                                                                                                                                                                    
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " CATEGORY TOTALS"  substring(cDisplay,17,250) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' CATEGORY TOTALS ,'
                        substring(cExcelDisplay,4,250) SKIP.
             END.

      END.

     /* down.*/

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
        /* underline w-data.w-sqft[1 for 3]
                   w-data.w-amt-txt[1 for 3]
                   w-data.w-cost-txt[1 for 3]
                   v-prof[1 for 3]. 

      down.                         */

      do i = 10 to 12:
        v-prof[i - 9] = (v-tot-amt[i] - v-tot-cost[i]) / v-tot-amt[i] * 100.
        if v-prof[i - 9] eq ? then v-prof[i - 9] = 0.
      end.

     /* if tt-report.key-01 eq "1" then
         display "         TOTAL SALES"  @ sman.sname.
      else
      if tt-report.key-01 eq "2" then
         display "                MISC"  @ sman.sname.
         
      else
        display "                MEMO"  @ sman.sname. */
      
      RUN text-proc(INPUT 10).

       PUT SKIP str-line FORMAT "x(250)" SKIP .
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"       THEN cVarValue = "" .
                         WHEN "rep"       THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "ino"       THEN cVarValue = "" .
                         WHEN "cust-prt"  THEN cVarValue = ""  .
                         WHEN "dly-sf"    THEN cVarValue = STRING(v-tot-sqft[10],"->>>9.999") .
                         WHEN "dly-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[10]) ELSE STRING(v-tot-amt-txt[10]) .
                         WHEN "dly-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[10])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[10]) ELSE "") .
                         WHEN "dly-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[1],"->>>9.99") ELSE "" .
                         WHEN "ptd-sf"    THEN cVarValue = STRING(v-tot-sqft[11],"->>>9.999") .                                                                                                                                                                                           
                         WHEN "ptd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[11]) ELSE STRING(v-tot-amt-txt[11]) .                                                                                                                    
                         WHEN "ptd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[11])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[11]) ELSE "") .                                                                            
                         WHEN "ptd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[2],"->>>9.99") ELSE "" .                                                                                                                                                                          
                         WHEN "ytd-sf"    THEN cVarValue = STRING(v-tot-sqft[12],"->>>9.999") .                                                                                                                                                                                           
                         WHEN "ytd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[12]) ELSE STRING(v-tot-amt-txt[12]) .                                                                                                                    
                         WHEN "ytd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[12])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[12]) ELSE "") .                                                                            
                         WHEN "ytd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[3],"->>>>>9.99") ELSE "" .                                                                                                                                                                                    
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            if tt-report.key-01 eq "1" THEN do:
                PUT UNFORMATTED " TOTAL SALES"  substring(cDisplay,13,250) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  ' TOTAL SALES ,'
                        substring(cExcelDisplay,4,250) SKIP.
                END.
            END.
            ELSE if tt-report.key-01 eq "2" THEN do:
                PUT UNFORMATTED " MISC"  substring(cDisplay,6,250) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  ' MISC ,'
                        substring(cExcelDisplay,4,250) SKIP.
                END.
            END.
            ELSE DO:
                PUT UNFORMATTED " MEMO"  substring(cDisplay,6,250) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  ' MEMO ,'
                        substring(cExcelDisplay,4,250) SKIP.
                END.
            END.
            

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
   
      PUT SKIP str-line  FORMAT "x(250)" SKIP .
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"       THEN cVarValue = "" .
                         WHEN "rep"       THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "ino"       THEN cVarValue = "" .
                         WHEN "cust-prt"  THEN cVarValue = ""  .
                         WHEN "dly-sf"    THEN cVarValue = STRING(v-tot-sqft[13],"->>>9.999") .
                         WHEN "dly-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[13]) ELSE STRING(v-tot-amt-txt[13]) .
                         WHEN "dly-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[13])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[13]) ELSE "") .
                         WHEN "dly-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[1],"->>>9.99") ELSE "" .
                         WHEN "ptd-sf"    THEN cVarValue = STRING(v-tot-sqft[14],"->>>9.999") .                                                                                                                                                                                           
                         WHEN "ptd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[14]) ELSE STRING(v-tot-amt-txt[14]) .                                                                                                                    
                         WHEN "ptd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[14])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[14]) ELSE "") .                                                                            
                         WHEN "ptd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[2],"->>>9.99") ELSE "" .                                                                                                                                                                          
                         WHEN "ytd-sf"    THEN cVarValue = STRING(v-tot-sqft[15],"->>>9.999") .                                                                                                                                                                                           
                         WHEN "ytd-amt"   THEN cVarValue = IF NOT tb_rem-cents THEN STRING(v-tot-amt-txt[15]) ELSE STRING(v-tot-amt-txt[15]) .                                                                                                                    
                         WHEN "ytd-cst"   THEN cVarValue = (IF v-cst AND NOT tb_rem-cents THEN STRING(v-tot-cost-txt[15])ELSE IF v-cst AND tb_rem-cents THEN STRING(v-tot-cost-txt[15]) ELSE "") .                                                                            
                         WHEN "ytd-mar"   THEN cVarValue = IF v-cst THEN STRING(v-prof[3],"->>>>>9.99") ELSE "" .                                                                                                                                                                                    
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " SALES"  substring(cDisplay,7,250) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' SALES ,'
                        substring(cExcelDisplay,4,250) SKIP.
             END.
    end.
  end.
