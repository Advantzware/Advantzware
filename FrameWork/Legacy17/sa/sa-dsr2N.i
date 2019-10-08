
        break by tt-report.key-01
              by tt-report.key-02

        with frame itemx

        transaction:

      find first w-data no-error.

      if first-of(tt-report.key-02) then do:
        create w-data.
        w-procat = tt-report.key-02.
      end.

      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq ar-invl.i-no
            no-lock no-error.

        assign
         v-amt  = ar-invl.amt
         v-msf  = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                  else
                  if avail itemfg then
                    (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

        RUN salrep/salecost.p (LOOKUP(rd_show1,"Board,Order,Invoice"),
                               ROWID(ar-invl),
                               ar-invl.job-no,
                               ar-invl.job-no2,
                               ar-invl.ship-qty,
                               OUTPUT v-cost).

        if v-msf eq ? then v-msf = 0.

        if v-amt eq ? then v-amt = 0.

        if v-cost eq ? then v-cost = 0.

        assign
         w-ptd-msf = w-ptd-msf + v-msf
         w-ptd-amt  = w-ptd-amt  + v-amt
         w-ptd-cost = w-ptd-cost + v-cost.

        if tdate eq ar-inv.inv-date then
          assign
           w-msf  = w-msf  + v-msf
           w-amt  = w-amt  + v-amt
           w-cost = w-cost + v-cost.
      end.

      else do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

        if avail ar-cashl then do:
          find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

          assign
           v-amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc)
           w-ptd-amt = w-ptd-amt + v-amt.

          if tdate eq ar-cash.check-date then
            w-amt = w-amt + v-amt.

          RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

          IF AVAIL oe-retl THEN DO:
            find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no    eq oe-retl.i-no
                no-lock no-error.

            assign
             v-msf  = if avail itemfg then
                        (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                      else 0.

            RUN salrep/salecost.p (LOOKUP(rd_show1,"Board,Order,Invoice"),
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no2,
                                   oe-retl.tot-qty-return,
                                   OUTPUT v-cost).

            if v-msf eq ? then v-msf = 0.

            if v-amt eq ? then v-amt = 0.

            if v-cost eq ? then v-cost = 0.

            assign
             w-ptd-msf  = w-ptd-msf - v-msf
             w-ptd-cost = w-ptd-cost - v-cost.

            if tdate eq ar-cash.check-date then
              assign
               w-msf  = w-msf - v-msf
               w-cost = w-cost - v-cost.
          end.
        end.
      end.

      if last-of(tt-report.key-02) then do:
        /* 9507 CAH: following code added to detect and prevent division by a
        number very near 0, which will cause overflow in $/MSF */

        abs-msf = if w-msf lt 0 then -1 * w-msf else w-msf.
        if abs-msf lt eps then w-msf = 0.

        abs-msf = if w-ptd-msf lt 0 then -1 * w-ptd-msf else w-ptd-msf.
        if abs-msf lt eps then w-ptd-msf = 0.

        put skip(1).
        
        if v-misc and tt-report.key-01 eq "MISC" then do:
          put w-procat format "x(25)" space(2).
          
          find first account
              where account.company eq cocode
                and account.actnum  eq tt-report.key-02
              no-lock no-error.
              
          if avail account then put account.dscr.
          
          put skip.
        end.
        
        assign
         w-$msf     = if w-msf ne 0 then
                        (w-amt / w-msf)
                      else 0
         w-ptd-$msf = if w-ptd-msf ne 0 then
                        (w-ptd-amt / w-ptd-msf)
                      else 0
         w-prof     = if w-amt ne 0 then
                        ((w-amt - w-cost) / w-amt)
                      else 0
         w-ptd-prof = if w-ptd-amt ne 0 then
                        ((w-ptd-amt - w-ptd-cost) / w-ptd-amt)
                      else 0
         w-prof     = w-prof     * 100
         w-ptd-prof = w-ptd-prof * 100.

       /* display w-procat    when not (v-misc and tt-report.key-01 eq "MISC")
                w-msf
                w-$msf
                w-amt
                w-cost
                w-prof
                w-ptd-msf
                w-ptd-$msf
                w-ptd-amt
                w-ptd-cost
                w-ptd-prof
                
            with frame itemx down.
        down with frame itemx.*/

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cat"    THEN cVarValue = IF not (v-misc and tt-report.key-01 eq "MISC") THEN string(w-procat,"x(8)") ELSE "" .
                         WHEN "d-msf"   THEN cVarValue = string(w-msf,"->,>>>.999").
                         WHEN "d-$msf"   THEN cVarValue = STRING(w-$msf,"->>>,>>9.99").
                         WHEN "d-amt"  THEN cVarValue = STRING(w-amt,"->,>>>,>>9.99") .
                         WHEN "d-cost"   THEN cVarValue = STRING(w-cost,"->,>>>,>>9.99") .
                         WHEN "d-pro"  THEN cVarValue = STRING(w-prof,"->>>>,>>9.99") .
                         WHEN "ptd-msf"   THEN cVarValue = string(w-ptd-msf,"->,>>>.999").
                         WHEN "ptd-$msf"   THEN cVarValue = STRING(w-ptd-$msf,"->>,>>9.99").
                         WHEN "ptd-amt"  THEN cVarValue = STRING(w-ptd-amt,"->,>>>,>>9.99") .
                         WHEN "ptd-cost"   THEN cVarValue = STRING(w-ptd-cost,"->,>>>,>>9.99") .
                         WHEN "ptd-pro"  THEN cVarValue = STRING(w-ptd-prof,"->>,>>9.99") .
                         
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

        
        assign
         v-{1}tot-msf      = v-{1}tot-msf      + w-msf
         v-{1}tot-amt      = v-{1}tot-amt      + w-amt
         v-{1}tot-cost     = v-{1}tot-cost     + w-cost
         v-{1}tot-ptd-msf  = v-{1}tot-ptd-msf  + w-ptd-msf
         v-{1}tot-ptd-amt  = v-{1}tot-ptd-amt  + w-ptd-amt
         v-{1}tot-ptd-cost = v-{1}tot-ptd-cost + w-ptd-cost.

        delete w-data.
      end.

      if avail tt-report then delete tt-report.
    end.
    
