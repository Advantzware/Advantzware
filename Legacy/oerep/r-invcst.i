    
    for each cust
        where cust.company eq cocode
          and cust.cust-no ge fcust
          and cust.cust-no le tcust
        no-lock:

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

      {sa/sa-sls03.i "fdate" "tdate"}
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

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

        IF tb_freight AND ar-inv.f-bill AND ar-inv.freight NE 0 THEN DO:
          RUN salrep/invfrate.p (ROWID(ar-inv), fsman, tsman).

          FOR EACH tt-report2
              WHERE tt-report2.key-10 EQ "ar-invf"
                AND tt-report2.inv-no EQ ar-inv.inv-no
              BREAK BY tt-report2.sman:

            ACCUMULATE tt-report2.freight (TOTAL BY tt-report2.sman).

            IF LAST-OF(tt-report2.sman) THEN DO:
              ASSIGN
               tt-report2.key-01  = IF v-sort THEN tt-report2.sman ELSE ""
               tt-report2.key-02  = STRING(YEAR(ar-inv.inv-date),"9999") +
                                    STRING(MONTH(ar-inv.inv-date),"99")  +
                                    STRING(DAY(ar-inv.inv-date),"99")
               tt-report2.key-03  = STRING(ar-inv.inv-no,"9999999999")
               tt-report2.key-04  = "FREIGHT"
               tt-report2.rec-id  = RECID(ar-inv)
               tt-report2.freight = ACCUM TOTAL BY tt-report2.sman tt-report2.freight.
            END.
            ELSE DELETE tt-report2.
          END.
        END.

        for each ar-invl
            where ar-invl.x-no eq ar-inv.x-no
              and ar-invl.i-no ge fitem
              and ar-invl.i-no le titem
              and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
            no-lock:

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
                and itemfg.procat  ge fpcat
                and itemfg.procat  le tpcat
              no-lock no-error.

          IF AVAIL itemfg OR ("" GE fpcat AND "" LE tpcat) THEN
          do i = 1 to 3:
            v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                        else ar-invl.sman[i].

            if v-sman-no  lt fsman                          or
               v-sman-no  gt tsman                          or
               (i ne 1 and
                (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

            create tt-report2.

            assign
             tt-report2.term-id = ""
             tt-report2.rec-id  = recid(ar-invl)
             tt-report2.key-01  = if v-sort then v-sman-no else ""
             tt-report2.key-02  = string(year(ar-inv.inv-date),"9999") +
                                  string(month(ar-inv.inv-date),"99")  +
                                  string(day(ar-inv.inv-date),"99")
             tt-report2.key-03  = string(ar-inv.inv-no,"9999999999")
             tt-report2.key-04  = ar-invl.i-no
             tt-report2.key-05  = ar-invl.job-no + STRING(ar-invl.job-no2,"99")
             tt-report2.key-09  = tt-report.key-09
             tt-report2.key-10  = "ar-invl".
          end.
        end.

        DELETE tt-report.
      end.
      
      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
        
        assign
         v-exc            = yes
         tt-report.key-02 = string(year(ar-cash.check-date),"9999") +
                            string(month(ar-cash.check-date),"99")  +
                            string(day(ar-cash.check-date),"99")
         tt-report.key-03 = string(ar-cashl.inv-no,"9999999999").

        release ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

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
                  and oe-retl.i-no    ge fitem
                  and oe-retl.i-no    le titem
                no-lock no-error.
            if avail oe-retl then
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                  and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
                no-lock no-error.
            if avail ar-invl then do:
              find first itemfg
                  where itemfg.company eq cocode
                    and itemfg.i-no    eq ar-invl.i-no
                    and itemfg.procat  ge fpcat
                    and itemfg.procat  le tpcat
                  no-lock no-error.

              IF AVAIL itemfg OR ("" GE fpcat AND "" LE tpcat) THEN
              do i = 1 to 3:
                v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                            else ar-invl.sman[i].

                if v-sman-no  lt fsman                          or
                   v-sman-no  gt tsman                          or
                   (i ne 1 and
                    (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

                create tt-report2.

                assign
                 v-exc              = no
                 tt-report2.term-id = ""
                 tt-report2.rec-id  = recid(ar-cashl)
                 tt-report2.key-01  = if v-sort then v-sman-no else ""
                 tt-report2.key-02  = tt-report.key-02
                 tt-report2.key-03  = tt-report.key-03
                 tt-report2.key-04  = oe-retl.i-no
                 tt-report2.key-09  = tt-report.key-09
                 tt-report2.key-10  = tt-report.key-10.
              end.

              DELETE tt-report.
            end.
          end.

          else
          if lv-type   eq "freight"                  and
             tb_freight                              and
             cust.sman ge fsman                      and
             cust.sman le tsman                      then
            assign
             v-exc            = no
             tt-report.key-01 = if v-sort then cust.sman else ""
             tt-report.key-04 = "FREIGHT".

          else
          if lv-type   eq "tax"                  and
             "tax"     ge fitem                  and
             "tax"     le titem                  and
             "tax"     ge fpcat                  and
             "tax"     le tpcat                  and
             cust.sman ge fsman                  and
             cust.sman le tsman                  then
            assign
             v-exc            = no
             tt-report.key-01 = if v-sort then cust.sman else ""
             tt-report.key-04 = "TAX".

          else
          if ""        ge fitem and
             ""        le titem and
             ""        ge fpcat and
             ""        le fpcat and
             cust.sman ge fsman and
             cust.sman le tsman then
            assign
             v-exc            = no
             tt-report.key-01 = if v-sort then cust.sman else "".
        end.

        else
        if ""        ge fitem and
           ""        le titem and
           ""        ge fpcat and
           ""        le fpcat and
           cust.sman ge fsman and
           cust.sman le tsman then
          assign
           v-exc            = no
           tt-report.key-01 = if v-sort then cust.sman else "".
           
        IF AVAIL tt-report AND v-exc THEN DELETE tt-report.
      end.
    end.

    for each tt-report2
        where tt-report2.term-id eq ""

        break by tt-report2.key-01
              by tt-report2.key-02
              by tt-report2.key-03
              by tt-report2.key-04
              by tt-report2.key-05

        with frame itemx down:
        
      if v-sort and first-of(tt-report2.key-01) then do:
        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report2.key-01
            no-lock no-error.
            
        v-slsmn-hdr = "SalesRep: " + trim(tt-report2.key-01) + " " +
                      (if avail sman then sman.sname else "Not on file").
      
        if first(tt-report2.key-01) then do:
          hide frame r-top2 no-pause.
          view frame r-top2.
        end.
        
        page.
      end.

      create w-data.
      assign
       w-data.inv-no = int(tt-report2.key-03)
       w-data.i-no   = tt-report2.key-04
       w-data.job-no = TRIM(tt-report2.key-05)
       w-data.rec-id = tt-report2.rec-id.

      assign
       v-job-no   = ""
       v-job-no2  = 0
       v-msf[1]   = 0
       v-po-no-po = 0
       v-pct      = 1.

      DO li = 1 TO 5:
        v-cst[(5 * (li - 1)) + 1] = 0.
      END.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-data.i-no
          no-lock no-error.

      if tt-report2.key-10 eq "ar-invl" then do:
        find first ar-invl where recid(ar-invl) eq w-data.rec-id no-lock.

        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
        assign
         v-cust-no  = ar-inv.cust-no
         v-date     = ar-inv.inv-date
         v-amt[1]   = ar-invl.unit-pr
         v-uom      = ar-invl.pr-uom
         v-job-no   = ar-invl.job-no
         v-job-no2  = ar-invl.job-no2
         v-po-no-po = ar-invl.po-no-po
         v-qty[1]   = ar-invl.ship-qty
         v-amt[1]   = ar-invl.amt
         v-msf[1]   = ar-invl.amt-msf
         v-cst[1]   = v-qty[1] / 1000 * ar-invl.std-mat-cost
         v-cst[6]   = v-qty[1] / 1000 * ar-invl.std-lab-cost
         v-cst[11]  = v-qty[1] / 1000 * ar-invl.std-fix-cost
         v-cst[16]  = v-qty[1] / 1000 * ar-invl.std-var-cost.

        IF ar-invl.dscr[1] = "M" OR ar-invl.dscr[1] EQ ""  THEN
           v-cst[21]  = v-qty[1] / 1000 * ar-invl.cost.
        ELSE
           v-cst[21]  = v-qty[1] * ar-invl.cost.
        
        if v-sort then
        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report2.key-01 then
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

      ELSE
      IF tt-report2.key-10 EQ "ar-invf" THEN DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ w-data.rec-id NO-LOCK.

        ASSIGN
         v-cust-no = ar-inv.cust-no
         v-date    = ar-inv.inv-date
         v-amt[1]  = tt-report2.freight
         v-uom     = ""
         v-qty[1]  = 0.

        DO li = 1 TO 5:
          v-cst[(5 * (li - 1)) + 1] = 0.
        END.
      END.

      else
      if tt-report2.key-10 eq "ar-cashl" then do:
        find first ar-cashl where recid(ar-cashl) eq w-data.rec-id no-lock.
        find first ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
        assign
         v-cust-no = ar-cash.cust-no
         v-date    = ar-cash.check-date
         v-amt[1]  = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom     = ""
         v-qty[1]  = 0.

        DO li = 1 TO 5:
          v-cst[(5 * (li - 1)) + 1] = 0.
        END.

        RELEASE ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

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
          for each ar-inv
              where ar-inv.company eq cocode
                and ar-inv.cust-no eq oe-reth.cust-no
                and ar-inv.inv-no  eq oe-reth.inv-no
              no-lock,
              each ar-invl
              where ar-invl.x-no eq ar-inv.x-no
                and ar-invl.i-no eq w-data.i-no
              no-lock:
            v-po-no-po = ar-invl.po-no-po.
            leave.
          end.

          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq int(substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 25,12))
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.

          if avail oe-retl then do:
            assign
             v-uom     = oe-retl.uom
             v-job-no  = oe-retl.job-no
             v-job-no2 = oe-retl.job-no2
             v-qty[1]  = - oe-retl.tot-qty-return.
             
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                no-lock no-error.
            if avail ar-invl then do:
              if v-sort then
              do i = 1 to 3:
                if ar-invl.sman[i] eq tt-report2.key-01 then
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

            ASSIGN
             v-cst[1]   = v-qty[1] / 1000 * ar-invl.std-mat-cost
             v-cst[6]   = v-qty[1] / 1000 * ar-invl.std-lab-cost
             v-cst[11]  = v-qty[1] / 1000 * ar-invl.std-fix-cost
             v-cst[16]  = v-qty[1] / 1000 * ar-invl.std-var-cost.

            IF ar-invl.dscr[1] = "M" OR ar-invl.dscr[1] EQ ""  THEN
               v-cst[21]  = v-qty[1] / 1000 * ar-invl.cost.
            ELSE
               v-cst[21]  = v-qty[1] * ar-invl.cost.
          end.
        end.
      end.

      if v-msf[1] eq 0 and avail itemfg then
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).
        
      ASSIGN
       v-qty[1] = v-qty[1] * v-pct
       v-amt[1] = v-amt[1] * v-pct
       v-msf[1] = v-msf[1] * v-pct
       
       v-qty[2] = v-qty[1]
       v-amt[2] = v-amt[1]
       v-msf[2] = v-msf[1].

      IF v-qty[2] EQ ? THEN v-qty[2] = 0.
      IF v-amt[2] EQ ? THEN v-amt[2] = 0.
      IF v-msf[2] EQ ? THEN v-msf[2] = 0.

      DO li = 1 TO 5:
        ASSIGN
         lj            = (5 * (li - 1)) + 1
         v-cst[lj]     = v-cst[lj] * v-pct
         v-cst[lj + 1] = v-cst[lj].

        IF v-cst[lj + 1] EQ ? THEN v-cst[lj + 1] = 0.

        v-brdc[li] = v-cst[lj + 1].

        IF v-brdc[li] EQ ? THEN v-brdc[li] = 0.
      END.
       
      ASSIGN
       v-marg = v-amt[2] - v-cst[2]
       v-$msf = v-amt[2] / v-msf[2].

      if v-marg eq ? then v-marg = 0.
      if v-$msf eq ? then v-$msf = 0.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq v-cust-no
          no-lock no-error.

      DISPLAY cust.name         WHEN AVAIL cust
              w-data.inv-no
              w-data.i-no
              w-data.job-no
              v-brdc[1]         WHEN v-cost2
              v-brdc[2]         WHEN v-cost2
              v-brdc[3]         WHEN v-cost2
              v-brdc[4]         WHEN v-cost2
              v-brdc[5]         WHEN v-cost2
              v-amt[2].
      DOWN.

      IF tb_excel THEN DO:  
          PUT STREAM excel UNFORMATTED
              '"' (IF AVAIL cust THEN cust.name ELSE "") '",'
              '"' w-data.inv-no '",' 
              '"' w-data.i-no '",' 
              '"' w-data.job-no '",'
              '"' (IF v-cost2 THEN v-brdc[1] ELSE 0) '",'
              '"' (IF v-cost2 THEN v-brdc[2] ELSE 0) '",'
              '"' (IF v-cost2 THEN v-brdc[3] ELSE 0) '",'
              '"' (IF v-cost2 THEN v-brdc[4] ELSE 0) '",'
              '"' (IF v-cost2 THEN v-brdc[5] ELSE 0) '",'
              '"' v-amt[2] '",'
              SKIP.
      END.

      ASSIGN
       v-qty[3] = v-qty[3] + v-qty[2]
       v-msf[3] = v-msf[3] + v-msf[2]
       v-amt[3] = v-amt[3] + v-amt[2]
         
       v-qty[2] = 0
       v-msf[2] = 0
       v-amt[2] = 0.
          
      DO li = 1 TO 5:
        ASSIGN
         v-cst[(5 * (li - 1)) + 3] = v-cst[(5 * (li - 1)) + 3] +
                                     v-cst[(5 * (li - 1)) + 2]
         v-cst[(5 * (li - 1)) + 2] = 0.
      END.

      IF LAST-OF(tt-report2.key-02) THEN DO:
        IF v-cost2 THEN
          UNDERLINE v-brdc[1]
                    v-brdc[2]
                    v-brdc[3]
                    v-brdc[4]
                    v-brdc[5]
                    v-amt[2]
              WITH FRAME itemx.

        DISPLAY "    DATE TOTALS FOR " + SUBSTR(tt-report2.key-02,5,2) + "/" +
                                         SUBSTR(tt-report2.key-02,7,2) + "/" +
                                         SUBSTR(tt-report2.key-02,1,4)
                                                @ cust.name
                v-cst[3]           WHEN v-cost2 @ v-brdc[1]
                v-cst[8]           WHEN v-cost2 @ v-brdc[2]
                v-cst[13]          WHEN v-cost2 @ v-brdc[3]
                v-cst[18]          WHEN v-cost2 @ v-brdc[4]
                v-cst[23]          WHEN v-cost2 @ v-brdc[5]
                v-amt[3]                        @ v-amt[2]

            WITH FRAME itemx.

        DOWN WITH FRAME itemx.
        
        PUT SKIP(1).

        ASSIGN
         v-qty[4] = v-qty[4] + v-qty[3]
         v-msf[4] = v-msf[4] + v-msf[3]
         v-amt[4] = v-amt[4] + v-amt[3]

         v-qty[3] = 0
         v-msf[3] = 0
         v-amt[3] = 0.
          
        DO li = 1 TO 5:
          ASSIGN
           v-cst[(5 * (li - 1)) + 4] = v-cst[(5 * (li - 1)) + 4] +
                                       v-cst[(5 * (li - 1)) + 3]
           v-cst[(5 * (li - 1)) + 3] = 0.
        END.
      END.

      IF LAST-OF(tt-report2.key-01) THEN DO:
        IF v-sort THEN DO:
          IF v-cost2 THEN
            UNDERLINE v-brdc[1]
                      v-brdc[2]
                      v-brdc[3]
                      v-brdc[4]
                      v-brdc[5]
                      v-amt[2]
                WITH FRAME itemx.

          DISPLAY "               SALESREP TOTALS" @ cust.name
                  v-cst[4]            WHEN v-cost2 @ v-brdc[1]
                  v-cst[9]            WHEN v-cost2 @ v-brdc[2]
                  v-cst[14]           WHEN v-cost2 @ v-brdc[3]
                  v-cst[19]           WHEN v-cost2 @ v-brdc[4]
                  v-cst[24]           WHEN v-cost2 @ v-brdc[5]
                  v-amt[4]                         @ v-amt[2]

              WITH FRAME itemx.

          DOWN WITH FRAME itemx.
        
          PUT SKIP(1).
        END.

        ASSIGN
         v-qty[5] = v-qty[5] + v-qty[4]
         v-msf[5] = v-msf[5] + v-msf[4]
         v-amt[5] = v-amt[5] + v-amt[4]

         v-qty[4] = 0
         v-msf[4] = 0
         v-amt[4] = 0.
          
        DO li = 1 TO 5:
          ASSIGN
           v-cst[(5 * (li - 1)) + 5] = v-cst[(5 * (li - 1)) + 5] +
                                       v-cst[(5 * (li - 1)) + 4]
           v-cst[(5 * (li - 1)) + 4] = 0.
        END.
      END.
      
      DELETE w-data.
    END.

    /* display final totals */
    IF v-qty[5]  NE 0 OR
       v-cst[5]  NE 0 OR
       v-cst[10] NE 0 OR
       v-cst[15] NE 0 OR
       v-cst[20] NE 0 OR
       v-cst[25] NE 0 OR
       v-amt[5]  NE 0 THEN DO:

      PUT SKIP(1).

      IF v-cost2 THEN
        UNDERLINE v-brdc[1]
                  v-brdc[2]
                  v-brdc[3]
                  v-brdc[4]
                  v-brdc[5]
                  v-amt[2]
            WITH FRAME itemx.

      DISPLAY "                  GRAND TOTALS"  @ cust.name
              v-cst[5]             WHEN v-cost2 @ v-brdc[1]
              v-cst[10]            WHEN v-cost2 @ v-brdc[2]
              v-cst[15]            WHEN v-cost2 @ v-brdc[3]
              v-cst[20]            WHEN v-cost2 @ v-brdc[4]
              v-cst[25]            WHEN v-cost2 @ v-brdc[5]
              v-amt[5]                          @ v-amt[2]

          WITH FRAME itemx.

      DOWN WITH FRAME itemx.
    END.
