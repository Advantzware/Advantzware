    dFilterStartDate = fdate[2].
    IF index(cSelectedList, "YTD Sales") GT 0 OR index(cSelectedList, "MTD Sales") GT 0 THEN DO:
      IF index(cSelectedList, "YTD Sales") GT 0 AND fdate[4] LT dFilterStartDate THEN
        dFilterStartDate = fdate[4].
       IF index(cSelectedList, "MTD Sales") GT 0 AND fdate[6] LT dFilterStartDate then
       dFilterStartDate = fdate[6].
    END.
          
    /* fdate[4] is 1/1 of prior year */
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.inv-date ge dFilterStartDate
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
          and ar-cash.check-date ge dFilterStartDate
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
          /*and tt-report.key-08  eq ""*/,
          
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
                              /*LOOKUP(rd_show1,"Board,Order,Invoice")*/
          
          
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
             v-amt = 0             
             v-msf = 0             
             v-pct    = ar-invl.s-pct[i] / 100
             ld-cost = 0
             v-cost = 0.

            if v-pct eq 0 then
            do v = 1 to 3:
              if v eq 1 then j = 0.
              if ar-invl.sman[v] ne "" then j = j + 1.
              if v eq 3 then v-pct = 1 / j.
            end.

            if v-pct le 0 or v-pct eq ? then v-pct = 1.
            
            do v = 1 to 6:
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
            
            IF ar-inv.inv-date GE fdate[1] AND
               ar-inv.inv-date LE tdate[1] THEN DO:            
              RUN salrep/salecost.p (3,
                                 ROWID(ar-invl),
                                 ar-invl.job-no,
                                 ar-invl.job-no2,
                                 ar-invl.inv-qty,
                                 OUTPUT ld-cost).              
            END.
            ASSIGN v-cost = ld-cost * v-pct
                   v-costYtd = v-cost
                   v-costMtd = v-cost
                   v-pctYtd = v-pct
                   v-pctMtd = v-pct.
            DEF VAR tt-report-key AS RECID.
            create xtt-report.
       
            assign
             xtt-report.key-01  = "2" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = string(v-amt[1],"-9999999999999.99")
             xtt-report.key-05  = string(v-amt[2],"-9999999999999.99")
             xtt-report.key-06  = string(v-msf[1],"-9999999999999.99")
             xtt-report.key-07  = string(v-msf[2],"-9999999999999.99") 
             xtt-report.key-08  = string(v-cost,"-9999999999999.99").
             tt-report-key = recid(xtt-report)
            .
             /* Handle YTD */
            create xtt-report.
       
            assign
             xtt-report.key-01  = "2.1" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = string(v-amt[3],"-9999999999999.99")
             xtt-report.key-05  = string(v-amt[4],"-9999999999999.99")
             xtt-report.key-06  = string(v-msf[3],"-9999999999999.99")
             xtt-report.key-07  = string(v-msf[4],"-9999999999999.99") 
             xtt-report.key-08  = string(v-costYtd,"-9999999999999.99")
             xtt-report.rec-id   = tt-report-key
             xtt-report.rec_key   = string(tt-report-key).             
            
             /* Handle MTD */
            create xtt-report.
       
            assign
             xtt-report.key-01  = "2.2" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = string(v-amt[5],"-9999999999999.99")
             xtt-report.key-05  = string(v-amt[6],"-9999999999999.99")
             xtt-report.key-06  = string(v-msf[5],"-9999999999999.99")
             xtt-report.key-07  = string(v-msf[6],"-9999999999999.99") 
             xtt-report.key-08  = string(v-costMtd,"-9999999999999.99")
             xtt-report.rec-id   = tt-report-key
             xtt-report.rec_key   = string(tt-report-key).                 
          end.
        end.  

        delete tt-report.
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

        assign
         v-amt = 0         
         v-msf = 0         
         lv-i-no  = "".
         
        do v = 1 to 6:
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
        FIND FIRST xtt-reportYtd 
          WHERE /*xtt-report.term-id EQ "" 
            AND */ xtt-reportYtd.key-01 = "2.1"
            AND xtt-reportYtd.rec_key = string(recid(tt-report))
            USE-INDEX rec_key
            NO-ERROR.
        IF NOT AVAIL xtt-reportYtd THEN DO:
            CREATE xtt-reportYtD.
            ASSIGN xtt-reportYtd.key-01 = "2.1"
                   xtt-reportYtd.rec-id = recid(tt-report)
                   xtt-reportYtd.rec_key = STRING(recid(tt-report)).
        END.
        ASSIGN 
             xtt-reportYtd.key-02 = tt-report.key-09
             xtt-reportYtd.key-03 = cust.sman
             xtt-reportYtd.key-04 = string(v-amt[3],"-9999999999999.99")
             xtt-reportYtd.key-05 = string(v-amt[4],"-9999999999999.99")
             xtt-reportYtd.key-06 = string(v-msf[3],"-9999999999999.99")
             xtt-reportYtd.key-07 = string(v-msf[4],"-9999999999999.99").
             
        FIND FIRST xtt-reportMtd 
          WHERE /* xtt-reportMtd.term-id EQ ""
           AND */ xtt-reportMtd.key-01 = "2.2"
            AND xtt-reportMtd.rec-id = recid(tt-report)
          AND xtt-reportMtd.rec_key = string(recid(tt-report))
            USE-INDEX rec_key
            NO-ERROR.        
        IF NOT AVAIL xtt-reportMtd THEN DO:
            CREATE xtt-reportMtd.
            ASSIGN xtt-reportMtd.key-01 = "2.2"
                   xtt-reportMtd.rec-id = recid(tt-report)
                   xtt-reportMtd.rec_key = STRING(recid(tt-report)).
        END.
        
        ASSIGN 
             xtt-reportMtd.key-02 = tt-report.key-09
             xtt-reportMtd.key-03 = cust.sman
             xtt-reportMtd.key-04 = string(v-amt[5],"-9999999999999.99")
             xtt-reportMtd.key-05 = string(v-amt[6],"-9999999999999.99")
             xtt-reportMtd.key-06 = string(v-msf[5],"-9999999999999.99")
             xtt-reportMtd.key-07 = string(v-msf[6],"-9999999999999.99").        
          
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

              do v = 1 to 6:
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
                RUN salrep/salecost.p (3,
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no2,
                                   oe-retl.tot-qty-return,
                                   OUTPUT ld-cost).
                ld-cost = ld-cost * v-pct.
                create xtt-report.

                assign
                 xtt-report.key-01  = "2"
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = string(v-amt[1] * v-pct,"-9999999999999.99")
                 xtt-report.key-05  = string(v-amt[2] * v-pct,"-9999999999999.99")
                 xtt-report.key-06  = string(v-msf[1] * v-pct,"-9999999999999.99")
                 xtt-report.key-07  = string(v-msf[2] * v-pct,"-9999999999999.99")
                 xtt-report.key-08  = STRING(ld-cost * (-1),"-9999999999999.99").
                 tt-report-key = recid(xtt-report)
                 .
                 /* Handle YTD */
                create xtt-report.           
                assign
                 xtt-report.key-01  = "2.1" 
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = string(v-amt[3],"-9999999999999.99")
                 xtt-report.key-05  = string(v-amt[4],"-9999999999999.99")
                 xtt-report.key-06  = string(v-msf[3],"-9999999999999.99")
                 xtt-report.key-07  = string(v-msf[4],"-9999999999999.99") 
                 xtt-report.key-08  = STRING(ld-cost * (-1),"-9999999999999.99")
                 xtt-report.rec-id   = tt-report-key
                 xtt-report.rec_key   = string(tt-report-key).             
                
                 /* Handle MTD */
                create xtt-report.
           
                assign
                 xtt-report.key-01  = "2.2" 
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = string(v-amt[5],"-9999999999999.99")
                 xtt-report.key-05  = string(v-amt[6],"-9999999999999.99")
                 xtt-report.key-06  = string(v-msf[5],"-9999999999999.99")
                 xtt-report.key-07  = string(v-msf[6],"-9999999999999.99") 
                 xtt-report.key-08  = STRING(ld-cost * (-1),"-9999999999999.99")
                 xtt-report.rec-id   = tt-report-key
                xtt-report.rec_key   = string(tt-report-key).                    
                
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
         v-msf = 0
         v-cost = 0
         v-costYtd = 0
         v-costMtd = 0 .
         
      FIND FIRST xtt-reportYtd 
        WHERE  /*xtt-reportYtd.term-id eq ""
          and  */ xtt-reportYtd.key-01  eq "2.1"
          AND xtt-reportYtd.rec-id = recid(tt-report) 
          AND xtt-reportYtd.rec_key = string(recid(tt-report))
        USE-INDEX rec_key  no-error.
      FIND FIRST xtt-reportMtd 
      WHERE  /*xtt-reportMtd.term-id eq "" 
          and  */ xtt-reportMtd.key-01  eq "2.2"
          AND xtt-reportMtd.rec-id = recid(tt-report)
          AND xtt-reportMtd.rec_key = string(RECID(tt-report))
        USE-INDEX rec_key  no-error.
      
        
      assign
       v-amt[1] = v-amt[1] + dec(tt-report.key-04)
       v-amt[2] = v-amt[2] + dec(tt-report.key-05)
       v-msf[1] = v-msf[1] + dec(tt-report.key-06)
       v-msf[2] = v-msf[2] + dec(tt-report.key-07)
       v-cost = v-cost + DEC(tt-report.key-08)
       
       v-amt[3] = v-amt[3] + dec(xtt-reportYtd.key-04)
       v-amt[4] = v-amt[4] + dec(xtt-reportYtd.key-05)
       v-msf[3] = v-msf[3] + dec(xtt-reportYtd.key-06)
       v-msf[4] = v-msf[4] + dec(xtt-reportYtd.key-07)
       v-costYtd = v-costYtd + DEC(xtt-reportYtd.key-08)
       
       v-amt[5] = v-amt[5] + dec(xtt-reportMtd.key-04)
       v-amt[6] = v-amt[6] + dec(xtt-reportMtd.key-05)
       v-msf[5] = v-msf[5] + dec(xtt-reportMtd.key-06)
       v-msf[6] = v-msf[6] + dec(xtt-reportMtd.key-07)
       v-costMtd = v-costMtd + DEC(xtt-reportMtd.key-08)       
       .

      if last-of(tt-report.key-02) then do:
        create xtt-report.
        assign
         xtt-report.key-01  = "1"
         xtt-report.key-02  = if v-sort eq "C" then tt-report.key-02 else "" 
         xtt-report.key-03  = string(v-amt[1],"-9999999999999.99")
         xtt-report.key-04  = string(v-amt[2],"-9999999999999.99")
         xtt-report.key-05  = tt-report.key-02
         xtt-report.key-06  = string(v-msf[1],"-9999999999999.99")
         xtt-report.key-07  = string(v-msf[2],"-9999999999999.99")
         xtt-report.key-08  = string(v-cost,"-9999999999999.99")    
             .
        create xtt-reportYtd.
        assign
         xtt-reportYtd.key-01  = "1.1"
         xtt-reportYtd.key-02  = if v-sort eq "C" then tt-report.key-02 else "" 
         xtt-reportYtd.key-03  = string(v-amt[3],"-9999999999999.99")
         xtt-reportYtd.key-04  = string(v-amt[4],"-9999999999999.99")
         xtt-reportYtd.key-05  = tt-report.key-02
         xtt-reportYtd.key-06  = string(v-msf[3],"-9999999999999.99")
         xtt-reportYtd.key-07  = string(v-msf[4],"-9999999999999.99")
         xtt-reportYtd.key-08  = string(v-costYtd,"-9999999999999.99")    
         xtt-reportYtd.rec-id  = recid(xtt-report)
         xtt-reportYtd.rec_key  = string(recid(xtt-report))
             .
             
        create xtt-reportMtd.
        assign
         xtt-reportMtd.key-01  = "1.2"
         xtt-reportMtd.key-02  = if v-sort eq "C" then tt-report.key-02 else "" 
         xtt-reportMtd.key-03  = string(v-amt[5],"-9999999999999.99")
         xtt-reportMtd.key-04  = string(v-amt[6],"-9999999999999.99")
         xtt-reportMtd.key-05  = tt-report.key-02
         xtt-reportMtd.key-06  = string(v-msf[5],"-9999999999999.99")
         xtt-reportMtd.key-07  = string(v-msf[6],"-9999999999999.99")
         xtt-reportMtd.key-08  = string(v-costMtd,"-9999999999999.99")    
         xtt-reportMtd.rec-id  = recid(xtt-report)
         xtt-reportMtd.rec_key  = string(RECID(xtt-report))
             .
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
              
        FIND FIRST xtt-reportYtd 
        WHERE /*xtt-reportYtd.term-id eq ""
          and */ xtt-reportYtd.key-01  eq "2.1"
          AND xtt-reportYtd.rec-id = recid(tt-report) 
          AND xtt-reportYtd.rec_key = string(recid(tt-report))
          USE-INDEX rec_key no-error.
          
        FIND FIRST xtt-reportMtd 
        WHERE /* xtt-reportMtd.term-id eq ""
          and */ xtt-reportMtd.key-01  eq "2.2"
          AND xtt-reportMtd.rec-id = recid(tt-report) 
          AND xtt-reportMtd.rec_key = string(recid(tt-report))
          USE-INDEX rec_key no-error.                   
         
        if first-of(tt-report.key-03) then
          assign
           v-amt = 0           
           v-msf = 0           
           v-cost = 0
           v-costYtd = 0
           v-costMtd = 0
           .

        assign
         v-amt[1] = v-amt[1] + dec(tt-report.key-04)
         v-amt[2] = v-amt[2] + dec(tt-report.key-05)
         v-msf[1] = v-msf[1] + dec(tt-report.key-06)
         v-msf[2] = v-msf[2] + dec(tt-report.key-07)
         v-cost = v-cost + dec(tt-report.key-08) 
         v-costTotal = v-costTotal + DEC(tt-report.key-08)  
         .
        assign
         v-amt[3] = v-amt[3] + dec(xtt-reportYtd.key-04)
         v-amt[4] = v-amt[4] + dec(xtt-reportYtd.key-05)
         v-msf[3] = v-msf[3] + dec(xtt-reportYtd.key-06)
         v-msf[4] = v-msf[4] + dec(xtt-reportYtd.key-07)
         v-costYtd = v-costYtd + dec(xtt-reportYtd.key-08) 
         v-costTotalYtd = v-costTotalYtd + DEC(xtt-reportYtd.key-08)  
         .
        assign
         v-amt[5] = v-amt[5] + dec(xtt-reportmtd.key-04)
         v-amt[6] = v-amt[6] + dec(xtt-reportMtd.key-05)
         v-msf[5] = v-msf[5] + dec(xtt-reportMtd.key-06)
         v-msf[6] = v-msf[6] + dec(xtt-reportMtd.key-07)
         v-costMtd = v-costMtd + dec(xtt-reportMtd.key-08) 
         v-costTotalMtd = v-costTotalMtd + DEC(xtt-reportMtd.key-08)  
         .        
         
        IF LAST-OF(tt-report.key-03)            AND
           (NOT tb_exc-zero OR
            v-amt[1] NE 0   OR
            v-amt[2] NE 0   /*OR
            (/*tb_msf AND
             (v-msf[1] NE 0 OR v-msf[2] NE 0)*/ )*/  ) THEN DO:

          if v-sman-no eq "xxxxxx" then v-sman-no = tt-report.key-03.

          if v-sman-no ne tt-report.key-03 then v-one = no.

          assign
           v-diff = v-amt[1] - v-amt[2]
           v-pct  = v-amt[1] / v-amt[2] * 100
           v-diffMSF = v-msf[1] - v-msf[2]
           v-pctMSF = v-msf[1] / v-msf[2] * 100  
           v-profit = v-amt[1] - v-cost   
           v-diffYtd = v-amt[3] - v-amt[4]
           v-pctYtd  = v-amt[3] / v-amt[4] * 100
           v-diffMtd = v-amt[5] - v-amt[6]
           v-pctMtd  = v-amt[5] / v-amt[6] * 100
          .

          if v-pct eq ? then v-pct = 0.
          IF v-pctMSF = ? THEN v-pctMSF = 0.
          IF v-pctYtd = ? THEN v-pctYtd = 0.
          IF v-pctMtd = ? THEN v-pctMtd = 0.
          
          ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  IF INDEX(cTmpField,".") > 0 THEN DO:
                            cFieldName = cTmpField.
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
                            IF hField <> ? THEN DO:                 
                                cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                                cDisplay = cDisplay + 
                                          IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                            (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                                          ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                            (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                                          ELSE cTmpField.
                                cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                            END.
                            ELSE DO:
                               cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                               cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                               cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                            END.
                  END.
                  ELSE DO: 
                       CASE cTmpField:                                          
                            WHEN "v-salesRep" THEN cVarValue = tt-report.key-03.
                            WHEN "v-amt[1]" THEN cVarValue = IF v-amt[1] = 0 THEN "" ELSE string(v-amt[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[2]" THEN cVarValue = IF v-amt[2] = 0 THEN "" ELSE string(v-amt[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[3]" THEN cVarValue = IF v-amt[3] = 0 THEN "" ELSE string(v-amt[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[4]" THEN cVarValue = IF v-amt[4] = 0 THEN "" ELSE string(v-amt[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[5]" THEN cVarValue = IF v-amt[5] = 0 THEN "" ELSE string(v-amt[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[6]" THEN cVarValue = IF v-amt[6] = 0 THEN "" ELSE string(v-amt[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[1]" THEN cVarValue = IF v-msf[1] = 0 THEN "" ELSE string(v-msf[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[2]" THEN cVarValue = IF v-msf[2] = 0 THEN "" ELSE string(v-msf[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[3]" THEN cVarValue = IF v-msf[3] = 0 THEN "" ELSE string(v-msf[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[4]" THEN cVarValue = IF v-msf[4] = 0 THEN "" ELSE string(v-msf[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[5]" THEN cVarValue = IF v-msf[5] = 0 THEN "" ELSE string(v-msf[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[6]" THEN cVarValue = IF v-msf[6] = 0 THEN "" ELSE string(v-msf[6],"->,>>>,>>>,>>>.99").                            
                            WHEN "v-diff" THEN   cVarValue = IF v-diff = 0 THEN "" ELSE string(v-diff,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffYtd" THEN   cVarValue = IF v-diffYTD = 0 THEN "" ELSE string(v-diffYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMtd" THEN   cVarValue = IF v-diffMTD = 0 THEN "" ELSE string(v-diffMTD,"->,>>>,>>>,>>>.99").                            
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSF = 0 THEN "" ELSE string(v-diffMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-pct" THEN    cVarValue = IF v-pct = 0 THEN "" ELSE string(v-pct,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctYtd" THEN    cVarValue = IF v-pctYTD = 0 THEN "" ELSE string(v-pctYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMtd" THEN    cVarValue = IF v-pctMTD = 0 THEN "" ELSE string(v-pctMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSF" THEN cVarValue = IF v-pctMSF = 0 THEN "" ELSE string(v-pctMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-profit" THEN cVarValue = IF v-profit = 0 THEN "" ELSE string(v-profit,"->,>>>,>>>,>>>.99"). 
                            WHEN "v-cost" THEN cVarValue = IF v-cost = 0 THEN "" ELSE string(v-cost,"->,>>>,>>>,>>>.99"). 
                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                  END.
              END.
              
              PUT UNFORMATTED cDisplay SKIP.
              IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                          cExcelDisplay SKIP.
              END.
              /*=============================*/

          ll-first = NO.
        end.
      end.
    
      FIND FIRST xtt-reportYtd 
        WHERE /*xtt-reportYtd.term-id eq ""
          and */ xtt-reportYtd.key-01  eq "1.1"
          AND xtt-reportYtd.rec-id = recid(xtt-report)
          AND xtt-reportYtd.rec_key = string(recid(xtt-report))
      USE-INDEX rec_key no-error.
      FIND FIRST xtt-reportMtd 
        WHERE /* xtt-reportMtd.term-id eq ""
          and */ xtt-reportMtd.key-01  eq "1.2"
          AND xtt-reportMtd.rec-id = recid(xtt-report)
        AND xtt-reportMtd.rec_key = STRING(RECID(xtt-report)) 
        USE-INDEX rec_key no-error.
       
      assign
       v-amt[1] = dec(xtt-report.key-03)
       v-amt[2] = dec(xtt-report.key-04)
       v-msf[1] = dec(xtt-report.key-06)
       v-msf[2] = dec(xtt-report.key-07)       
       v-cost = DEC(xtt-report.key-08)
       v-diff   = v-amt[1] - v-amt[2]
       v-pct    = v-amt[1] / v-amt[2] * 100              
       v-diffMSF = v-msf[1] - v-msf[2]
       v-pctMSF = v-msf[1] / v-msf[2] * 100       
       v-profit = v-amt[1] - v-cost   
       .
      assign
       v-amt[3] = dec(xtt-reportYtd.key-03)
       v-amt[4] = dec(xtt-reportYtd.key-04)
       v-msf[3] = dec(xtt-reportYtd.key-06)
       v-msf[4] = dec(xtt-reportYtd.key-07)       
       v-costYtd = DEC(xtt-reportYtd.key-08)
       v-diffYtd   = v-amt[3] - v-amt[4]
       v-pctYtd    = v-amt[3] / v-amt[4] * 100              
       v-diffMSFYtd = v-msf[3] - v-msf[4]
       v-pctMSFYtd = v-msf[3] / v-msf[4] * 100       
       v-profitYtd = v-amt[3] - v-costYtd   
       .       
      
      assign
       v-amt[5] = dec(xtt-reportMtd.key-03)
       v-amt[6] = dec(xtt-reportMtd.key-04)
       v-msf[5] = dec(xtt-reportMtd.key-06)
       v-msf[6] = dec(xtt-reportMtd.key-07)       
       v-costMtd = DEC(xtt-reportMtd.key-08)
       v-diffMtd   = v-amt[5] - v-amt[6]
       v-pctMtd    = v-amt[5] / v-amt[6] * 100              
       v-diffMSFMtd = v-msf[5] - v-msf[6]
       v-pctMSFMtd = v-msf[5] / v-msf[6] * 100       
       v-profitMtd = v-amt[5] - v-costMtd   
       .       

              
      if v-pct eq ? then v-pct = 0.
      if v-pctYtd eq ? then v-pctYtd = 0.
      if v-pctMtd eq ? then v-pctMtd = 0.
      IF v-pctMSF = ? THEN v-pctMSF = 0.
      IF v-pctMSFYtd = ? THEN v-pctMSFYtd = 0.
      IF v-pctMSFmTD = ? THEN v-pctMSFMtd = 0.
      
      /* customer total */
      if not v-one                            AND
         (NOT tb_exc-zero OR
          v-amt[1] NE 0   OR
          v-amt[2] NE 0   /*OR
          (/*tb_msf AND
           (v-msf[1] NE 0 OR v-msf[2] NE 0)*/ )*/ ) THEN DO:

          PUT   
            SKIP SPACE(36)  str-line  /*  Task 12041302 */
            /*fill("-",int(entry(4,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(5,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(6,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(7,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(8,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(9,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
              fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "*/
             skip.

          ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  IF INDEX(cTmpField,".") > 0 THEN DO:
                            cFieldName = cTmpField.
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
                            IF hField <> ? THEN DO:                 
                                cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                                cDisplay = cDisplay + 
                                          IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                            (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                                          ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                            (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                                          ELSE cTmpField.
                                cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                            END.
                            ELSE DO:
                               cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                               cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                               cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                            END.
                  END.
                  ELSE DO:      
                       CASE cTmpField:                                          
                            WHEN "v-salesRep" THEN cVarValue = "" /*tt-report.key-03*/.
                            WHEN "v-amt[1]" THEN cVarValue = IF v-amt[1] = 0 THEN "" ELSE string(v-amt[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[2]" THEN cVarValue = IF v-amt[2] = 0 THEN "" ELSE string(v-amt[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[1]" THEN cVarValue = IF v-msf[1] = 0 THEN "" ELSE string(v-msf[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[2]" THEN cVarValue = IF v-msf[2] = 0 THEN "" ELSE string(v-msf[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-diff" THEN   cVarValue = IF v-diff = 0 THEN "" ELSE string(v-diff,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSF = 0 THEN "" ELSE string(v-diffMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-pct" THEN    cVarValue = IF v-pct = 0 THEN "" ELSE string(v-pct,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSF" THEN cVarValue = IF v-pctMSF = 0 THEN "" ELSE string(v-pctMSF,"->,>>>,>>>,>>>.99").
                            
                            WHEN "v-amt[3]" THEN cVarValue = IF v-amt[3] = 0 THEN "" ELSE string(v-amt[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[4]" THEN cVarValue = IF v-amt[4] = 0 THEN "" ELSE string(v-amt[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[3]" THEN cVarValue = IF v-msf[3] = 0 THEN "" ELSE string(v-msf[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[4]" THEN cVarValue = IF v-msf[4] = 0 THEN "" ELSE string(v-msf[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-diffYtd" THEN   cVarValue = IF v-diffYtd = 0 THEN "" ELSE string(v-diffYtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSFYtd" THEN cVarValue = IF v-diffMSFYtd = 0 THEN "" ELSE string(v-diffMSFYtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctYtd" THEN    cVarValue = IF v-pctYtd = 0 THEN "" ELSE string(v-pctYtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFYtd" THEN cVarValue = IF v-pctMSFYtd = 0 THEN "" ELSE string(v-pctMSFYtd,"->,>>>,>>>,>>>.99").
                            
                            WHEN "v-amt[5]" THEN cVarValue = IF v-amt[5] = 0 THEN "" ELSE string(v-amt[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[6]" THEN cVarValue = IF v-amt[6] = 0 THEN "" ELSE string(v-amt[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[5]" THEN cVarValue = IF v-msf[5] = 0 THEN "" ELSE string(v-msf[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[6]" THEN cVarValue = IF v-msf[6] = 0 THEN "" ELSE string(v-msf[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMtd" THEN   cVarValue = IF v-diffMtd = 0 THEN "" ELSE string(v-diffMtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSFMtd = 0 THEN "" ELSE string(v-diffMSFMtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMtd" THEN    cVarValue = IF v-pctMtd = 0 THEN "" ELSE string(v-pctMtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFMtd" THEN cVarValue = IF v-pctMSFMtd = 0 THEN "" ELSE string(v-pctMSFMtd,"->,>>>,>>>,>>>.99").
                            
                            WHEN "v-profit" THEN cVarValue = IF v-profit = 0 THEN "" ELSE string(v-profit,"->,>>>,>>>,>>>.99"). 
                            WHEN "v-cost" THEN cVarValue = IF v-cost = 0 THEN "" ELSE string(v-cost,"->,>>>,>>>,>>>.99"). 
                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                  END.
              END.
              
              PUT UNFORMATTED cDisplay SKIP.
/*               IF tb_excel THEN DO:               */
/*                     PUT STREAM excel UNFORMATTED */
/*                           cExcelDisplay SKIP.    */
/*               END.                               */

      end.  
      
      do v = 1 to 6:
    
        assign
         v-t-amt[v] = v-t-amt[v] + v-amt[v]
         v-t-msf[v] = v-t-msf[v] + v-msf[v].
      end.
              
      /* display cust totals */
      if last(xtt-report.key-02) THEN DO:
         
        assign
         v-diff = v-t-amt[1] - v-t-amt[2]
         v-pct  = v-t-amt[1] / v-t-amt[2] * 100
         v-diffMSF = v-t-msf[1] - v-t-msf[2]
         v-pctMSF = v-t-msf[1] / v-t-msf[2] * 100   
         
         v-diffYtd = v-t-amt[3] - v-t-amt[4]         
         v-pctYtd  = v-t-amt[3] / v-t-amt[4] * 100
         v-diffMSFYTD = v-t-msf[3] - v-t-msf[4]
         v-pctMSFYTD = v-t-msf[3] / v-t-msf[4] * 100 
         
         v-diffMtd = v-t-amt[5] - v-t-amt[6]
         v-pctMtd  = v-t-amt[5] / v-t-amt[6] * 100
         v-diffMSFMTD = v-t-msf[5] - v-t-msf[6]
         v-pctMSFMTD = v-t-msf[5] / v-t-msf[6] * 100 
         .

        if v-pct eq ? then v-pct = 0.
        if v-pctMSF eq ? then v-pctMSF = 0.
        if v-pctYtd eq ? then v-pctYtd = 0.
        if v-pctMSFYtd eq ? then v-pctMSFYtd = 0.
        if v-pctMtd eq ? then v-pctMtd = 0.
        if v-pctMSFMtd eq ? then v-pctMSFMtd = 0.
        
        PUT   
            SKIP SPACE(36) str-line SKIP   /*  Task 12041302 */
            /*fill("-",int(entry(4,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(5,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(6,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(7,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(8,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(9,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " " 
            skip*/
            /*"Grand Totals" 
            SPACE(28)              
                v-amt[3] FORM "->>>>>>>>>>>>>.99" " "
                v-amt[4] FORM "->>>>>>>>>>>>>.99" " "
                v-diff   FORM "->>>>>>>>>>>>>.99" " "
                v-pct  FORM "->>>>>>>>>>>>>.99" " "
                v-msf[3] FORM "->>>>>>>>>>>>>.99" " "                
                v-msf[4] FORM "->>>>>>>>>>>>>.99" " "
                v-diffMSF  FORM "->>>>>>>>>>>>>.99" " "
                v-pctmsf  FORM "->>>>>>>>>>>>>.99" " "
                v-costTotal FORM "->>>>>>>>>>>>>.99" " "
                v-amt[3] - v-costTotal FORM "->>>>>>>>>>>>>.99"  
            SKIP*/
            .
       /*  IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED 
                "Grand Totals," ",,"
                v-amt[3] ","
                v-amt[4] ","
                v-diff   ","
                v-pct   ","
                v-msf[3] ","                
                v-msf[4] ","
                v-diffMSF  ","
                v-pctmsf   ","
                v-costTotal ","
                v-amt[3] - v-costTotal.*/
        /*  Task 12041302 .. */
         ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   CASE cTmpField:                                          
                            WHEN "cust.cust-no" THEN cVarValue = "" /*tt-report.key-03*/.
                            WHEN "cust.NAME" THEN cVarValue = "Grand Totals" .
                            WHEN "v-salesRep" THEN cVarValue = "" .
                            WHEN "v-amt[1]" THEN cVarValue = IF v-t-amt[1] = 0 THEN "" ELSE string(v-t-amt[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[2]" THEN cVarValue = IF v-t-amt[2] = 0 THEN "" ELSE string(v-t-amt[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[1]" THEN cVarValue = IF v-t-msf[1] = 0 THEN "" ELSE string(v-t-msf[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[2]" THEN cVarValue = IF v-t-msf[2] = 0 THEN "" ELSE string(v-t-msf[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[3]" THEN cVarValue = IF v-t-amt[3] = 0 THEN "" ELSE string(v-t-amt[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[4]" THEN cVarValue = IF v-t-amt[4] = 0 THEN "" ELSE string(v-t-amt[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[3]" THEN cVarValue = IF v-t-msf[3] = 0 THEN "" ELSE string(v-t-msf[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[4]" THEN cVarValue = IF v-t-msf[4] = 0 THEN "" ELSE string(v-t-msf[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[5]" THEN cVarValue = IF v-t-amt[5] = 0 THEN "" ELSE string(v-t-amt[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[6]" THEN cVarValue = IF v-t-amt[6] = 0 THEN "" ELSE string(v-t-amt[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[5]" THEN cVarValue = IF v-t-msf[5] = 0 THEN "" ELSE string(v-t-msf[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[6]" THEN cVarValue = IF v-t-msf[6] = 0 THEN "" ELSE string(v-t-msf[6],"->,>>>,>>>,>>>.99").
                                    
                            WHEN "v-diff" THEN   cVarValue = IF v-diff = 0 THEN "" ELSE string(v-diff,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSF = 0 THEN "" ELSE string(v-diffMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffYTD" THEN   cVarValue = IF v-diffYTD = 0 THEN "" ELSE string(v-diffYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMTD" THEN   cVarValue = IF v-diffMTD = 0 THEN "" ELSE string(v-diffMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pct" THEN    cVarValue = IF v-pct = 0 THEN "" ELSE string(v-pct,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctYTD" THEN    cVarValue = IF v-pctYTD = 0 THEN "" ELSE string(v-pctYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMTD" THEN    cVarValue = IF v-pctMTD = 0 THEN "" ELSE string(v-pctMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSF" THEN cVarValue = IF v-pctmsf = 0 THEN "" ELSE string(v-pctmsf,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFYTD" THEN cVarValue = IF v-pctmsfYTD = 0 THEN "" ELSE string(v-pctmsfYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFMTD" THEN cVarValue = IF v-pctmsfMTD = 0 THEN "" ELSE string(v-pctmsfMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-profit" THEN cVarValue = IF v-t-amt[1] = 0 AND v-costTotal = 0  THEN "" ELSE string(v-t-amt[1] - v-costTotal,"->,>>>,>>>,>>>.99"). 
                            WHEN "v-cost" THEN cVarValue = IF v-costTotal = 0 THEN "" ELSE string(v-costTotal,"->,>>>,>>>,>>>.99"). 
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
              END.    /*  Task 12041302 */


/*         if tb_msf then do with frame detail-msf: */
/*           underline v-amt[1]                     */
/*                     v-msf[1]                     */
/*                     v-amt[2]                     */
/*                     v-msf[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*           down.                                  */
/*           underline v-amt[1]                     */
/*                     v-msf[1]                     */
/*                     v-amt[2]                     */
/*                     v-msf[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*                                                  */
/*           display "Grand Totals"                 */
/*                            @ cust.name           */
/*                   v-amt[3] @ v-amt[1]            */
/*                   v-msf[3] @ v-msf[1]            */
/*                   v-amt[4] @ v-amt[2]            */
/*                   v-msf[4] @ v-msf[2]            */
/*                   v-diff                         */
/*                   v-pct.                         */
/*           down.                                  */
/*         end.                                     */
/*                                                  */
/*         else                                     */
/*         do with frame detail:                    */
/*           underline v-amt[1]                     */
/*                     v-amt[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*           down.                                  */
/*           underline v-amt[1]                     */
/*                     v-amt[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*                                                  */
/*           display "Grand Totals"                 */
/*                            @ cust.name           */
/*                   v-amt[3] @ v-amt[1]            */
/*                   v-amt[4] @ v-amt[2]            */
/*                   v-diff                         */
/*                   v-pct.                         */
/*           down.                                  */
/*         end.                                     */
      end.

/*       else                                                      */
/*       IF (NOT tb_exc-zero OR                                    */
/*           v-amt[1] NE 0   OR                                    */
/*           v-amt[2] NE 0   OR                                    */
/*           (/*tb_msf AND*/                                       */
/*            (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN PUT SKIP(1). */
    END.
   
