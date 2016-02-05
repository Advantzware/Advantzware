    
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.inv-date ge fdate[2]
          and ar-inv.inv-date le tdate[1]
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  EQ ttCustList.cust-no /*fcust*/
        /*  and ar-inv.cust-no  le tcust*/
          and (ar-inv.type    ne "FC" or v-inc-fc)
        use-index inv-date no-lock:

      create tt-report.
      assign
       tt-report.key-09 = ar-inv.cust-no
       tt-report.key-10 = "ar-inv"
       tt-report.rec-id = recid(ar-inv).
    end.

    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        each cust
        where cust.company eq cocode
          and cust.cust-no EQ ttCustList.cust-no /*fcust*/
         /* and cust.cust-no le tcust*/
        no-lock,
       
        each ar-cash
        where ar-cash.company    eq cocode
          and ar-cash.cust-no    eq cust.cust-no
          and ar-cash.check-date ge fdate[2]
          and ar-cash.check-date le tdate[1]
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

      create tt-report.
      assign
       tt-report.key-09 = cust.cust-no
       tt-report.key-10 = "ar-cashl"
       tt-report.rec-id = recid(ar-cashl).
    end.
     
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq ""
          and tt-report.key-02  eq ""
          and tt-report.key-03  eq ""
          and tt-report.key-04  eq ""
          and tt-report.key-05  eq ""
          and tt-report.key-06  eq ""
          and tt-report.key-07  eq ""
          and tt-report.key-08  eq "",
          
        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-report.key-09
        no-lock:

      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

        for each ar-invl
            where ar-invl.x-no eq ar-inv.x-no
              and ar-invl.i-no ge begin_i-no
              and ar-invl.i-no le end_i-no
              and (ar-invl.billable or not ar-invl.misc)
            use-index x-no no-lock:

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

          do i = 1 to 3:
            v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                        else ar-invl.sman[i].

            if v-sman-no  lt fsman                          or
               v-sman-no  gt tsman                          or
               (i ne 1 and
                (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
       
            assign
             v-amt[1] = 0
             v-amt[2] = 0
             v-msf[1] = 0
             v-msf[2] = 0
             v-pct    = ar-invl.s-pct[i] / 100.

            if v-pct eq 0 then
            do v = 1 to 3:
              if v eq 1 then j = 0.
              if ar-invl.sman[v] ne "" then j = j + 1.
              if v eq 3 then v-pct = 1 / j.
            end.

            if v-pct le 0 or v-pct eq ? then v-pct = 1.
            
            do v = 1 to 2:
              if ar-inv.inv-date ge fdate[v] and
                 ar-inv.inv-date le tdate[v] then
                assign
                 v-amt[v] = ar-invl.amt * v-pct
                 v-msf[v] = (if ar-invl.amt-msf ne 0 then
                               ar-invl.amt-msf
                             else
                             if avail itemfg then
                               (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0) * v-pct.
            end.

            create xtt-report.
       
            assign
             xtt-report.key-01  = "2" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = string(v-amt[1],"-9999999999999.99")
             xtt-report.key-05  = string(v-amt[2],"-9999999999999.99")
             xtt-report.key-06  = string(v-msf[1],"-9999999999999.99")
             xtt-report.key-07  = string(v-msf[2],"-9999999999999.99").
          end.
        end.  

        delete tt-report.
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

        assign
         v-amt[1] = 0
         v-amt[2] = 0
         v-msf[1] = 0
         v-msf[2] = 0
         lv-i-no  = "".
         
        do v = 1 to 2:
          if ar-cash.check-date ge fdate[v] and
             ar-cash.check-date le tdate[v] then
            v-amt[v] = ar-cashl.amt-paid - ar-cashl.amt-disc. 
        end.
        
        assign
         tt-report.key-01 = "2"
         tt-report.key-02 = tt-report.key-09
         tt-report.key-03 = cust.sman
         tt-report.key-04 = string(v-amt[1],"-9999999999999.99")
         tt-report.key-05 = string(v-amt[2],"-9999999999999.99")
         tt-report.key-06 = string(v-msf[1],"-9999999999999.99")
         tt-report.key-07 = string(v-msf[2],"-9999999999999.99").

        release ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
         lv-r-no = 0
         lv-type = "".
          
        IF AVAIL reftable THEN
          ASSIGN
           lv-r-no = reftable.val[1]
           lv-type = reftable.dscr.
        ELSE
        IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
          ASSIGN
           lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
           lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

        IF lv-r-no NE 0 THEN DO:
          find first oe-reth
              where oe-reth.company eq cocode
                and oe-reth.r-no    eq lv-r-no
              no-lock no-error.
          if avail oe-reth then
          find first ar-inv
               where ar-inv.company eq cocode
                 and ar-inv.cust-no eq oe-reth.cust-no
                 and ar-inv.inv-no  eq oe-reth.inv-no
               no-lock no-error.

          if lv-type eq "items" then do:
            release ar-invl.
            find first oe-retl
                where oe-retl.company eq cocode
                  and oe-retl.r-no    eq oe-reth.r-no
                  and oe-retl.line    eq ar-cashl.line
                  and oe-retl.i-no    ge begin_i-no
                  and oe-retl.i-no    le end_i-no
                no-lock no-error.

            if avail oe-retl then do:
              find first itemfg
                  where itemfg.company eq cocode
                    and itemfg.i-no    eq oe-retl.i-no
                  no-lock no-error.

              do v = 1 to 2:
                if ar-cash.check-date ge fdate[v] and
                   ar-cash.check-date le tdate[v] then
                  v-msf[v] = if avail itemfg then
                               (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                             else 0.
                if v-msf[v] eq ? then v-msf[v] = 0.
              end.
            end.

            if avail oe-retl then
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                  and (ar-invl.billable or not ar-invl.misc)
                no-lock no-error.
            if avail ar-invl then do:
              do i = 1 to 3:
                v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                            else ar-invl.sman[i].

                if v-sman-no  lt fsman                          or
                   v-sman-no  gt tsman                          or
                   (i ne 1 and
                    (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

                v-pct = ar-invl.s-pct[i] / 100.

                if v-pct eq 0 then
                do v = 1 to 3:
                  if v eq 1 then j = 0.
                  if ar-invl.sman[v] ne "" then j = j + 1.
                  if v eq 3 then v-pct = 1 / j.
                end.
                
                if v-pct le 0 or v-pct eq ? then v-pct = 1.
                
                create xtt-report.

                assign
                 xtt-report.key-01  = "2"
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = string(v-amt[1] * v-pct,"-9999999999999.99")
                 xtt-report.key-05  = string(v-amt[2] * v-pct,"-9999999999999.99")
                 xtt-report.key-06  = string(v-msf[1] * v-pct,"-9999999999999.99")
                 xtt-report.key-07  = string(v-msf[2] * v-pct,"-9999999999999.99").
              end.

              delete tt-report.
            end.
          end.

          else
          if lv-type eq "freight" or
             lv-type eq "tax"     then
            lv-i-no = lv-type.
        end.  

        if avail tt-report then
          if tt-report.key-03 lt fsman      or
             tt-report.key-03 gt tsman      or
             lv-i-no          lt begin_i-no or
             lv-i-no          gt end_i-no   then delete tt-report.
      end.     
    end.

    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "2"
        no-lock
        break by tt-report.key-02:

      if first-of(tt-report.key-02) then
        assign
         v-amt = 0
         v-msf = 0.

      assign
       v-amt[1] = v-amt[1] + dec(tt-report.key-04)
       v-amt[2] = v-amt[2] + dec(tt-report.key-05)
       v-msf[1] = v-msf[1] + dec(tt-report.key-06)
       v-msf[2] = v-msf[2] + dec(tt-report.key-07).

      if last-of(tt-report.key-02) then do:
        create xtt-report.
        assign
         xtt-report.key-01  = "1"
         xtt-report.key-02  = if v-sort eq "C" then tt-report.key-02 else "" 
         xtt-report.key-03  = string(v-amt[1],"-9999999999999.99")
         xtt-report.key-04  = string(v-amt[2],"-9999999999999.99")
         xtt-report.key-05  = tt-report.key-02
         xtt-report.key-06  = string(v-msf[1],"-9999999999999.99")
         xtt-report.key-07  = string(v-msf[2],"-9999999999999.99").
      end.
    end.

    put skip(1).

    FOR EACH xtt-report
        WHERE xtt-report.term-id EQ ""
          AND xtt-report.key-01  EQ "1",

        first cust
        where cust.company eq cocode
          and cust.cust-no eq xtt-report.key-05
        no-lock

        break by xtt-report.key-02
              by (if v-sort eq "M" then dec(xtt-report.key-06)
                                   else dec(xtt-report.key-03)) desc
              by (if v-sort eq "M" then dec(xtt-report.key-07)
                                   else dec(xtt-report.key-04)) desc
              by xtt-report.key-05:      

      assign
       v-one     = yes
       ll-first  = YES
       v-sman-no = "xxxxxx".

      for each tt-report
          where tt-report.term-id eq ""
            and tt-report.key-01  eq "2"
            and tt-report.key-02  eq xtt-report.key-05

          break by tt-report.key-03

          transaction:

        if first-of(tt-report.key-03) then
          assign
           v-amt[1] = 0
           v-amt[2] = 0
           v-msf[1] = 0
           v-msf[2] = 0.

        assign
         v-amt[1] = v-amt[1] + dec(tt-report.key-04)
         v-amt[2] = v-amt[2] + dec(tt-report.key-05)
         v-msf[1] = v-msf[1] + dec(tt-report.key-06)
         v-msf[2] = v-msf[2] + dec(tt-report.key-07).

        IF LAST-OF(tt-report.key-03)            AND
           (NOT tb_exc-zero OR
            v-amt[1] NE 0   OR
            v-amt[2] NE 0   OR
            (tb_msf AND
             (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN DO:

          if v-sman-no eq "xxxxxx" then v-sman-no = tt-report.key-03.

          if v-sman-no ne tt-report.key-03 then v-one = no.

          assign
           v-diff = v-amt[1] - v-amt[2]
           v-pct  = v-amt[1] / v-amt[2] * 100.

          if v-pct eq ? then v-pct = 0.

          if tb_msf then do with frame detail-msf:   
            display cust.cust-no     WHEN ll-first
                    cust.NAME        WHEN ll-first
                    tt-report.key-03 @ cust.sman
                    v-amt[1]
                    v-msf[1]
                    v-amt[2]
                    v-msf[2]
                    v-diff
                    v-pct.
            down.
          end.

          else
          do with frame detail:
            display cust.cust-no     WHEN ll-first
                    cust.NAME        WHEN ll-first
                    tt-report.key-03 @ cust.sman
                    v-amt[1]
                    v-amt[2]
                    v-diff
                    v-pct.
            down.
          END.
          
          IF tb_excel THEN  
            PUT STREAM excel UNFORMATTED
               '"' cust.cust-no '",' 
               '"' cust.name '",' 
               '"' tt-report.key-03 '",'
               '"' v-amt[1] '",'
               '"' v-msf[1] '",'
               '"' v-amt [2] '",' 
               '"' v-msf[2] '",'
               '"' v-diff '",' 
               '"' v-pct '",'  
               SKIP. 

          ll-first = NO.
        end.
      end.
      
      assign
       v-amt[1] = dec(xtt-report.key-03)
       v-amt[2] = dec(xtt-report.key-04)
       v-msf[1] = dec(xtt-report.key-06)
       v-msf[2] = dec(xtt-report.key-07)
       v-diff   = v-amt[1] - v-amt[2]
       v-pct    = v-amt[1] / v-amt[2] * 100.

      if v-pct eq ? then v-pct = 0.

      if not v-one                            AND
         (NOT tb_exc-zero OR
          v-amt[1] NE 0   OR
          v-amt[2] NE 0   OR
          (tb_msf AND
           (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN DO:

        if tb_msf then do with frame detail-msf:
          underline v-amt[1]
                    v-msf[1]
                    v-amt[2]
                    v-msf[2]
                    v-diff
                    v-pct.
                  
          display cust.cust-no
                  cust.name
                  v-amt[1]
                  v-msf[1]
                  v-amt[2]
                  v-msf[2]
                  v-diff
                  v-pct.
          down.
        end.

        else
        do with frame detail:
          underline v-amt[1]
                    v-amt[2]
                    v-diff
                    v-pct.
                  
          display cust.cust-no
                  cust.name
                  v-amt[1]
                  v-amt[2]
                  v-diff
                  v-pct.
          down.
        end.
          
/*         IF tb_excel THEN                 */
/*           PUT STREAM excel UNFORMATTED   */
/*                '"' cust.cust-no '",'     */
/*                '"' cust.name '",'        */
/*                '"' tt-report.key-03 '",' */
/*                '"' v-amt[1] '",'         */
/*                '"' v-msf[1] '",'         */
/*                '"' v-amt [2] '",'        */
/*                '"' v-msf[2] '",'         */
/*                '"' v-diff '",'           */
/*                '"' v-pct '",'            */
/*                SKIP.                     */
      end.  
      
      do v = 3 to 4:
        assign
         v-amt[v] = v-amt[v] + v-amt[v - 2]
         v-msf[v] = v-msf[v] + v-msf[v - 2].
      end.
              
      /* display cust totals */
      if last(xtt-report.key-02) THEN DO:
        assign
         v-diff = v-amt[3] - v-amt[4]
         v-pct  = v-amt[3] / v-amt[4] * 100.

        if v-pct eq ? then v-pct = 0.

        if tb_msf then do with frame detail-msf:
          underline v-amt[1]
                    v-msf[1]
                    v-amt[2]
                    v-msf[2]
                    v-diff
                    v-pct.
          down.          
          underline v-amt[1]
                    v-msf[1]
                    v-amt[2]
                    v-msf[2]
                    v-diff
                    v-pct.

          display "Grand Totals"
                           @ cust.name
                  v-amt[3] @ v-amt[1]
                  v-msf[3] @ v-msf[1]
                  v-amt[4] @ v-amt[2]
                  v-msf[4] @ v-msf[2]
                  v-diff
                  v-pct.
          down.
        end.

        else
        do with frame detail:
          underline v-amt[1]
                    v-amt[2]
                    v-diff
                    v-pct.
          down.          
          underline v-amt[1]
                    v-amt[2]
                    v-diff
                    v-pct.

          display "Grand Totals"
                           @ cust.name
                  v-amt[3] @ v-amt[1]
                  v-amt[4] @ v-amt[2]
                  v-diff
                  v-pct.
          down.
        end.
      end.

      else
      IF (NOT tb_exc-zero OR
          v-amt[1] NE 0   OR
          v-amt[2] NE 0   OR
          (tb_msf AND
           (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN PUT SKIP(1).
    END.
   
