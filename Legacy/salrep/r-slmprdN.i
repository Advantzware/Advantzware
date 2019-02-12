    
    if tdate eq ? then tdate = today.

    
    find first period
        where period.company eq cocode
          and period.pst     le tdate
          and period.pend    ge tdate
        no-lock.

    assign
     v-period = period.pnum
     v-year   = period.yr
     fdate[2] = period.pst
     edate[2] = tdate
     fdate[1] = tdate
     edate[1] = tdate.

    find first period
        where period.company eq cocode
          and period.yr      eq v-year
        no-lock.

    assign
     fdate[3] = period.pst
     edate[3] = tdate.

    for each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.inv-date ge fdate[3]
          and ar-inv.inv-date le edate[3]
          and ar-inv.posted   eq yes
          and (ar-inv.type    ne "FC" or v-inc-fc)
        use-index inv-date no-lock,

        first cust
        where cust.company eq cocode
          and cust.cust-no eq ar-inv.cust-no
        no-lock,

        each ar-invl
        where ar-invl.x-no eq ar-inv.x-no
          and ar-invl.i-no ge begin_i-no
          and ar-invl.i-no le end_i-no
          and (ar-invl.billable or not ar-invl.misc)
        no-lock:

      {sa/sa-sman4.i "ar-invl"}
    end.

    for each cust where cust.company eq cocode no-lock:
      for each ar-cash
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

        lv-i-no = IF ar-cashl.dscr MATCHES "*oe return*" THEN 
                    SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5)
                  ELSE "".

        release ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        if avail oe-retl then
        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq ar-cash.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock no-error.

        IF ar-cashl.inv-no NE 0                                                       AND
           (AVAIL ar-invl                             OR
            (NOT AVAIL reftable AND
             NOT ar-cashl.dscr MATCHES "*oe return*") OR
            lv-i-no EQ "items")                                                       THEN
        FOR EACH b-ar-invl
            WHERE b-ar-invl.company EQ ar-cashl.company
              AND b-ar-invl.cust-no EQ cust.cust-no
              AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
              AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
              AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
            NO-LOCK:
          {sa/sa-sman4.i "ar-cashl" "b-"}
        END.

        else
        if lv-i-no ge begin_i-no and
           lv-i-no le end_i-no   and
           cust.sman ge fsman    and
           cust.sman le tsman    then do:
          create tt-report.
          assign
           tt-report.key-01  = IF AVAIL reftable                      OR
                                  ar-cashl.dscr MATCHES "*oe return*" THEN "2" ELSE "4"
           tt-report.key-02  = cust.sman
           tt-report.key-09  = cust.cust-no
           tt-report.key-10  = "ar-cashl"
           tt-report.rec-id  = recid(ar-cashl).
        end.
      end.
    end.

    for each tt-report break by tt-report.key-02:
      find first w-data
          where w-data.w-type    eq tt-report.key-01
            and w-data.w-sman-no eq tt-report.key-02
          no-lock no-error.

      if not avail w-data then do:
        create w-data.
        assign
         w-data.w-type    = tt-report.key-01
         w-data.w-sman-no = tt-report.key-02.
      end.

      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq ar-invl.i-no
            no-lock no-error.

        assign
         v-pct  = 1
         v-amt  = ar-invl.amt
         v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                  else
                  if avail itemfg then
                    (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

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
             w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct).
        end.
      end.

      else do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

        if avail ar-cashl then do:
          find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

          assign
           v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
           v-sqft = 0
           v-pct  = 1.

          RELEASE ar-invl.
          RELEASE oe-retl.

          FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

          IF NOT AVAIL ar-invl THEN
            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

          IF AVAIL oe-retl THEN DO:
            find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no    eq oe-retl.i-no
                no-lock no-error.

            v-sqft = IF AVAIL itemfg THEN
                       (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                     ELSE 0.
          END.

          ELSE
          IF AVAIL ar-invl THEN DO:
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

            v-amt = v-amt * ld-inv-pct.

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
          end.

          do i = 1 to 3:
            if ar-cash.check-date ge fdate[i] and
               ar-cash.check-date le edate[i] then
              assign
               w-data.w-sqft[i] = w-data.w-sqft[i] - (v-sqft * v-pct)
               w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct).
          end.
        end.
      end.
      
      if last-of(tt-report.key-02) then do:
        create w-data1.
        w-data1.w-sman-no = tt-report.key-02.
        
        for each w-data where w-data.w-sman-no eq w-data1.w-sman-no:
          do i = 1 to 3:
            assign
             w-data1.w-sqft[i] = w-data1.w-sqft[i] + w-data.w-sqft[i]
             w-data1.w-amt[i]  = w-data1.w-amt[i]  + w-data.w-amt[i]
             
             v-tot-amt[i]  = v-tot-amt[i]  + w-data.w-amt[i]
             v-tot-sqft[i] = v-tot-sqft[i] + w-data.w-sqft[i].
          end.
        end.
      
        do i = 1 to 3:
          w-data1.w-pmsf[i] = if w-data1.w-sqft[i] eq 0 then 0
                              else (w-data1.w-amt[i] / w-data1.w-sqft[i]).
        end.
          
        put skip(1).
        find first sman
            where sman.company eq cocode
              and sman.sman    eq w-data1.w-sman-no
            no-lock no-error.
        if not called then do:
         /* display w-data1.w-sman-no
                  sman.sname when avail sman
                  w-data1.w-sqft[1]
                  w-data1.w-amt[1]
                  w-data1.w-pmsf[1]
                  w-data1.w-sqft[2]
                  w-data1.w-amt[2]
                  w-data1.w-pmsf[2]
                  w-data1.w-sqft[3]
                  w-data1.w-amt[3]
                  w-data1.w-pmsf[3]
              with frame f-prod down.
          down with frame f-prod.*/

         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "rep"    THEN cVarValue = string(w-data1.w-sman-no,"x(3)") .
                         WHEN "name"   THEN cVarValue = IF AVAIL sman THEN string(sman.sname,"x(20)") ELSE "".
                         WHEN "d-sqt"  THEN cVarValue = STRING(w-data1.w-sqft[1],"->>>>,>>9.999").
                         WHEN "d-amt"  THEN cVarValue = STRING(w-data1.w-amt[1],"->,>>>,>>9.99") .
                         WHEN "d-msf"  THEN cVarValue = STRING(w-data1.w-pmsf[1],"->>>,>>9.99") .
                         WHEN "ptd-sqt"  THEN cVarValue = STRING(w-data1.w-sqft[2],"->>>>,>>9.999") .
                         WHEN "ptd-amt"   THEN cVarValue = STRING(w-data1.w-amt[2],"->,>>>,>>9.99") .
                         WHEN "ptd-msf"  THEN cVarValue = STRING(w-data1.w-pmsf[2],"->>,>>9.99") .
                         WHEN "ytd-sqt"  THEN cVarValue = STRING(w-data1.w-sqft[3],"->>>>,>>9.999") .
                         WHEN "ytd-amt"   THEN cVarValue = STRING(w-data1.w-amt[3],"->,>>>,>>9.99") .
                         WHEN "ytd-msf"  THEN cVarValue = STRING(w-data1.w-pmsf[3],"->>,>>9.99") .
                         
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
      end.
    end.
    
    for each w-data break by w-data.w-type:
      if w-data.w-type ne "1" then
      do i = 1 to 3:
        assign
         v-tot-amt[i]  = v-tot-amt[i]  + w-data.w-amt[i]
         v-tot-sqft[i] = v-tot-sqft[i] + w-data.w-sqft[i].
      end.

      if last-of(w-data.w-type) then do:
        if w-data.w-type eq "1" then
          if not called then do with frame f-prod:
            
              PUT str-line SKIP.

          end.

        do i = 1 to 3:
          v-tot-pmsf[i] = v-tot-amt[i] / v-tot-sqft[i].
          if v-tot-pmsf[i] eq ? then v-tot-pmsf[i] = 0.
        end.

        if not called then do with frame f-prod:
          if w-data.w-type eq "1" then
            v-head-line = "  TOTAL SALES" .
          else
          if w-data.w-type eq "2" then
            v-head-line = "         MISC" .
          else
          if w-data.w-type eq "3" then
            v-head-line = "         PREP".
          else
            v-head-line = "         MEMO" .

          /*display v-tot-amt[1]  @ w-data1.w-amt[1]
                  v-tot-sqft[1] @ w-data1.w-sqft[1]
                  v-tot-pmsf[1] @ w-data1.w-pmsf[1]
                  v-tot-amt[2]  @ w-data1.w-amt[2]
                  v-tot-sqft[2] @ w-data1.w-sqft[2]
                  v-tot-pmsf[2] @ w-data1.w-pmsf[2]
                  v-tot-amt[3]  @ w-data1.w-amt[3]
                  v-tot-sqft[3] @ w-data1.w-sqft[3]
                  v-tot-pmsf[3] @ w-data1.w-pmsf[3].
          down 1.*/

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "rep"    THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "d-sqt"  THEN cVarValue = STRING(v-tot-sqft[1],"->>>>,>>9.999").
                         WHEN "d-amt"  THEN cVarValue = STRING(v-tot-amt[1],"->,>>>,>>9.99") .
                         WHEN "d-msf"  THEN cVarValue = STRING(v-tot-pmsf[1],"->>>,>>9.99") .
                         WHEN "ptd-sqt"  THEN cVarValue = STRING(v-tot-sqft[2],"->>>>,>>9.999") .
                         WHEN "ptd-amt"   THEN cVarValue = STRING(v-tot-amt[2],"->,>>>,>>9.99") .
                         WHEN "ptd-msf"  THEN cVarValue = STRING(v-tot-pmsf[2],"->>,>>9.99") .
                         WHEN "ytd-sqt"  THEN cVarValue = STRING(v-tot-sqft[3],"->>>>,>>9.999") .
                         WHEN "ytd-amt"   THEN cVarValue = STRING(v-tot-amt[3],"->,>>>,>>9.99") .
                         WHEN "ytd-msf"  THEN cVarValue = STRING(v-tot-pmsf[3],"->>,>>9.99") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED v-head-line  substring(cDisplay,14,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED v-head-line ',' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.

          if w-data.w-type eq "1" then do:
           
              PUT str-line SKIP.
          end.
        end.

        do i = 4 to 6:
          assign
           v-tot-amt[i]  = v-tot-amt[i]  + (v-tot-amt[i - 3] *
                                         if w-data.w-type eq "1" then 1 else -1)
           v-tot-sqft[i] = v-tot-sqft[i] + (v-tot-sqft[i - 3] *
                                         if w-data.w-type eq "1" then 1 else -1)

           v-tot-amt[i - 3]  = 0
           v-tot-sqft[i - 3] = 0.
        end.
      end.

      if last(w-data.w-type) then do:
        do i = 4 to 6:
          v-tot-pmsf[i] = v-tot-amt[i] / v-tot-sqft[i].
          if v-tot-pmsf[i] eq ? then v-tot-pmsf[i] = 0.
        end.

        if not called then do with frame f-prod:
          /*display "  SALES"       @ sman.sname
                  v-tot-amt[4]    @ w-data1.w-amt[1]
                  v-tot-sqft[4]   @ w-data1.w-sqft[1]
                  v-tot-pmsf[4]   @ w-data1.w-pmsf[1]
                  v-tot-amt[5]    @ w-data1.w-amt[2]
                  v-tot-sqft[5]   @ w-data1.w-sqft[2]
                  v-tot-pmsf[5]   @ w-data1.w-pmsf[2]
                  v-tot-amt[6]    @ w-data1.w-amt[3]
                  v-tot-sqft[6]   @ w-data1.w-sqft[3]
                  v-tot-pmsf[6]   @ w-data1.w-pmsf[3].
          down 1.*/

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "rep"    THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "d-sqt"  THEN cVarValue = STRING(v-tot-sqft[4],"->>>>,>>9.999").
                         WHEN "d-amt"  THEN cVarValue = STRING(v-tot-amt[4],"->,>>>,>>9.99") .
                         WHEN "d-msf"  THEN cVarValue = STRING(v-tot-pmsf[4],"->>>,>>9.99") .
                         WHEN "ptd-sqt"  THEN cVarValue = STRING(v-tot-sqft[5],"->>>>,>>9.999") .
                         WHEN "ptd-amt"   THEN cVarValue = STRING(v-tot-amt[5],"->,>>>,>>9.99") .
                         WHEN "ptd-msf"  THEN cVarValue = STRING(v-tot-pmsf[5],"->>,>>9.99") .
                         WHEN "ytd-sqt"  THEN cVarValue = STRING(v-tot-sqft[6],"->>>>,>>9.999") .
                         WHEN "ytd-amt"   THEN cVarValue = STRING(v-tot-amt[6],"->,>>>,>>9.99") .
                         WHEN "ytd-msf"  THEN cVarValue = STRING(v-tot-pmsf[6],"->>,>>9.99") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "        SALES"  substring(cDisplay,14,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                      'SALES , '  substring(cExcelDisplay,4,350) SKIP.
             END.

         
        end.
      end.
    end.
