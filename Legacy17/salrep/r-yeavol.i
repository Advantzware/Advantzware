   
   EMPTY TEMP-TABLE tt-report.
   EMPTY TEMP-TABLE tt-custsort.

   FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
       each cust
        where cust.company eq cocode
          and cust.cust-no EQ ttCustList.cust-no /*fcust*/
          /*and cust.cust-no le tcust*/
        no-lock:

      STATUS DEFAULT "Processing Customer#: " + TRIM(cust.cust-no).

      {sa/sa-sls03.i "fdate[4]" "tdate[2]"}
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
          and tt-report.key-08  eq ""

        break by tt-report.key-09:

      STATUS DEFAULT "Sorting Customer#: " + TRIM(tt-report.key-09).

      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

        for each ar-invl
            where ar-invl.x-no    eq ar-inv.x-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock:

          v-prodc = "MISC".

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.
          if avail itemfg and itemfg.procat ne "" then v-prodc = itemfg.procat.

          else do:
            find first fgcat
                where fgcat.company eq cocode
                  and fgcat.glacc   eq ar-invl.actnum
                no-lock no-error.
            if avail fgcat then v-prodc = fgcat.procat.
          end.

          if v-prodc ge begin_fg-cat and
             v-prodc le end_fg-cat   then
          do v = 1 to 4:
            if ar-inv.inv-date ge fdate[v] and
               ar-inv.inv-date le tdate[v] then
              assign
               v-tot[5] = v-tot[5] + (if v eq 1 then ar-invl.amt else 0)
               v-tot[6] = v-tot[6] + (if v eq 2 then ar-invl.amt else 0)
               v-tot[v] = v-tot[v] + ar-invl.amt.
          end.
        end.
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

        v-prodc = "MEMO".

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        IF AVAIL oe-retl THEN DO:
          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
                and (ar-invl.billable or not ar-invl.misc)
              no-lock no-error.

          if avail ar-invl then do:
            v-prodc = "MISC".

            find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no    eq ar-invl.i-no
                no-lock no-error.
            if avail itemfg and itemfg.procat ne "" then v-prodc = itemfg.procat.

            else do:
              find first fgcat
                  where fgcat.company eq cocode
                    and fgcat.glacc   eq ar-invl.actnum
                  no-lock no-error.
              if avail fgcat then v-prodc = fgcat.procat.
            end.
          end.
        end.

        if v-prodc ge begin_fg-cat and
           v-prodc le end_fg-cat   then
        do v = 1 to 4:
          if ar-cash.check-date ge fdate[v] and
             ar-cash.check-date le tdate[v] then
            assign
             v-tot[5] = v-tot[5] + (if v eq 1 then
                                      (ar-cashl.amt-paid - ar-cashl.amt-disc)
                                    else 0)
             v-tot[6] = v-tot[6] + (if v eq 2 then
                                      (ar-cashl.amt-paid - ar-cashl.amt-disc)
                                    else 0)
             v-tot[v] = v-tot[v] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
        end.
      end.

      if last-of(tt-report.key-09) then do:
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq tt-report.key-09
            no-lock no-error.
        create tt-custsort.
        assign
         tt-custsort.tot-01  = v-tot[5]
         tt-custsort.tot-02  = v-tot[6]
         tt-custsort.cust-no = tt-report.key-09
         v-tot[5]           = 0
         v-tot[6]           = 0.
        if v-sort-by-name eq "N" then
          tt-custsort.sorter = if avail cust then cust.name else tt-custsort.cust-no.
        else
        if v-sort-by-name eq "C" then
          tt-custsort.sorter = if avail cust then cust.cust-no else tt-custsort.cust-no.
      end.
    end.

    put skip(1).

    for each tt-custsort USE-INDEX sorter,

        each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq ""
          and tt-report.key-02  eq ""
          and tt-report.key-03  eq ""
          and tt-report.key-04  eq ""
          and tt-report.key-05  eq ""
          and tt-report.key-06  eq ""
          and tt-report.key-07  eq ""
          and tt-report.key-08  eq ""
          and tt-report.key-09  eq tt-custsort.cust-no

        break by tt-custsort.sorter
              by tt-custsort.tot-01 desc
              by tt-custsort.tot-02 desc
              by tt-custsort.cust-no

        with frame custx:

      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

        STATUS DEFAULT "Printing Customer#/Inv#: " +
                       TRIM(ar-inv.cust-no) + "/"  +
                       TRIM(STRING(ar-inv.inv-no,">>>>>>>>>>")).

        for each ar-invl
            where ar-invl.x-no    eq ar-inv.x-no
            no-lock:

          v-prodc = "MISC".

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.
          if avail itemfg and itemfg.procat ne "" then v-prodc = itemfg.procat.

          else do:
            find first fgcat
                where fgcat.company eq cocode
                  and fgcat.glacc   eq ar-invl.actnum
                no-lock no-error.
            if avail fgcat then v-prodc = fgcat.procat.
          end.

          RUN salrep/salecost.p (LOOKUP(rd_show1,"Board,Order,Invoice"),
                                 ROWID(ar-invl),
                                 ar-invl.job-no,
                                 ar-invl.job-no2,
                                 ar-invl.ship-qty,
                                 OUTPUT ld-cost).

          if v-prodc ge begin_fg-cat and
             v-prodc le end_fg-cat   then
          do v = 1 to 4:
            if ar-inv.inv-date ge fdate[v] and
               ar-inv.inv-date le tdate[v] then
              assign
               v-msf[v] = v-msf[v] +
                          (if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                           else
                           if avail itemfg then
                             (ar-invl.ship-qty * itemfg.t-sqft / 1000)
                           else 0)
               v-cst[v] = v-cst[v] + ld-cost
               v-amt[v] = v-amt[v] + ar-invl.amt
               v-ton[v] = v-ton[v] +
                          ((if ar-invl.t-weight ne 0 then ar-invl.t-weight
                            else
                            if avail itemfg then
                              (ar-invl.ship-qty * itemfg.weight-100 / 100)
                            else 0) / 2000).
          end.
        end.
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

        STATUS DEFAULT "Printing Customer#/Check#: " +
                       TRIM(ar-cash.cust-no) + "/"  +
                       TRIM(STRING(ar-cash.check-no,">>>>>>>>>>")).

        v-prodc = "MEMO".

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        IF AVAIL oe-retl THEN DO:
          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
                and (ar-invl.billable or not ar-invl.misc)
              no-lock no-error.

          if avail ar-invl then do:
            v-prodc = "MISC".

            find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no    eq ar-invl.i-no
                no-lock no-error.
            if avail itemfg and itemfg.procat ne "" then v-prodc = itemfg.procat.

            else do:
              find first fgcat
                  where fgcat.company eq cocode
                    and fgcat.glacc   eq ar-invl.actnum
                  no-lock no-error.
              if avail fgcat then v-prodc = fgcat.procat.
            end.

            RUN salrep/salecost.p (LOOKUP(rd_show1,"Board,Order,Invoice"),
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no2,
                                   oe-retl.tot-qty-return,
                                   OUTPUT ld-cost).

            if v-prodc ge begin_fg-cat and
               v-prodc le end_fg-cat   then
            do v = 1 to 4:
              if ar-cash.check-date ge fdate[v] and
                 ar-cash.check-date le tdate[v] then
                assign
                 v-msf[v] = v-msf[v] -
                            (if avail itemfg then
                               (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                             else 0)
                 v-cst[v] = v-cst[v] - ld-cost
                 v-ton[v] = v-ton[v] +
                            ((if avail itemfg then
                                (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                              else 0) / 2000).
            end.
          end.
        end.

        if v-prodc ge begin_fg-cat and
           v-prodc le end_fg-cat   then
        do v = 1 to 4:
          if ar-cash.check-date ge fdate[v] and
             ar-cash.check-date le tdate[v] then
            v-amt[v] = v-amt[v] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
        end.
      end.

      if last-of(tt-custsort.cust-no) then do:
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq tt-report.key-09
            no-lock.

        if v-msf[1] ne 0 or v-cst[1] ne 0 or v-amt[1] ne 0 or
           v-msf[2] ne 0 or v-cst[2] ne 0 or v-amt[2] ne 0 or
           v-msf[3] ne 0 or v-cst[3] ne 0 or v-amt[3] ne 0 or
           v-msf[4] ne 0 or v-cst[4] ne 0 or v-amt[4] ne 0 then do:

          if line-counter + 5 gt page-size then page.

          assign
           v-a-m = v-amt[1] / v-msf[1]
           v-pc1 = v-amt[1] / v-tot[1] * 100
           v-ret = v-amt[1] - v-cst[1]
           v-pc2 = v-ret    / v-amt[1] * 100
           v-a-t = v-amt[1] / v-ton[1].

          if v-a-m eq ? then v-a-m = 0.
          if v-pc1 eq ? then v-pc1 = 0.
          if v-ret eq ? then v-ret = 0.
          if v-pc2 eq ? then v-pc2 = 0.
          if v-a-t eq ? then v-a-t = 0.

          display cust.cust-no
                  cust.name
                  cust.sman
                  "  PTD:" @ v-lab
                  v-a-m
                  v-msf[1]
                  v-cst[1]
                  v-amt[1]
                  v-pc1
                  v-ret
                  v-pc2
                  v-a-t     WHEN tb_ton
                  v-ton[1]  WHEN tb_ton.
          down.

          IF tb_excel THEN DO:

            ASSIGN
              ptd-v-a-m = v-a-m
              ptd-v-pc1 = v-pc1
              ptd-v-ret = v-ret
              ptd-v-pc2 = v-pc2.

            IF tb_ton THEN
               ptd-v-a-t = v-a-t.
          END.

          assign
           v-a-m = v-amt[2] / v-msf[2]
           v-pc1 = v-amt[2] / v-tot[2] * 100
           v-ret = v-amt[2] - v-cst[2]
           v-pc2 = v-ret    / v-amt[2] * 100
           v-a-t = v-amt[2] / v-ton[2].

          if v-a-m eq ? then v-a-m = 0.
          if v-pc1 eq ? then v-pc1 = 0.
          if v-ret eq ? then v-ret = 0.
          if v-pc2 eq ? then v-pc2 = 0.
          if v-a-t eq ? then v-a-t = 0.

          display string(cust.city + ", " + cust.state,"x(30)")
                           @ cust.name
                  "  YTD:" @ v-lab
                  v-a-m
                  v-msf[2] @ v-msf[1]
                  v-cst[2] @ v-cst[1]
                  v-amt[2] @ v-amt[1]
                  v-pc1
                  v-ret
                  v-pc2
                  v-a-t     WHEN tb_ton
                  v-ton[2]  WHEN tb_ton @ v-ton[1].
          down.

          IF tb_excel THEN DO:

            ASSIGN
              ytd-v-a-m = v-a-m
              ytd-v-pc1 = v-pc1
              ytd-v-ret = v-ret
              ytd-v-pc2 = v-pc2.

            IF tb_ton THEN
               ytd-v-a-t = v-a-t.
          END.

          assign
           v-a-m = v-amt[3] / v-msf[3]
           v-pc1 = v-amt[3] / v-tot[3] * 100
           v-ret = v-amt[3] - v-cst[3]
           v-pc2 = v-ret    / v-amt[3] * 100
           v-dif = v-amt[1] - v-amt[3]
           v-a-t = v-amt[3] / v-ton[3].

          if v-a-m eq ? then v-a-m = 0.
          if v-pc1 eq ? then v-pc1 = 0.
          if v-ret eq ? then v-ret = 0.
          if v-pc2 eq ? then v-pc2 = 0.
          if v-dif eq ? then v-dif = 0.
          if v-a-t eq ? then v-a-t = 0.

          display string("PTD Sales Diff: " +
                  string(v-dif,">>>>>>>>9.99-"),"x(30)")
                         @ cust.name
                  "PTDLY:" @ v-lab
                  v-a-m
                  v-msf[3] @ v-msf[1]
                  v-cst[3] @ v-cst[1]
                  v-amt[3] @ v-amt[1]
                  v-pc1
                  v-ret
                  v-pc2
                  v-a-t     WHEN tb_ton
                  v-ton[3]  WHEN tb_ton @ v-ton[1].
          down.

          IF tb_excel THEN DO:

            ASSIGN
              ptdly-v-a-m = v-a-m
              ptdly-v-pc1 = v-pc1
              ptdly-v-ret = v-ret
              ptdly-v-pc2 = v-pc2
              ptd-v-dif   = v-dif.

            IF tb_ton THEN
               ptdly-v-a-t = v-a-t.
          END.

          assign
           v-a-m = v-amt[4] / v-msf[4]
           v-pc1 = v-amt[4] / v-tot[4] * 100
           v-ret = v-amt[4] - v-cst[4]
           v-pc2 = v-ret    / v-amt[4] * 100
           v-dif = v-amt[2] - v-amt[4]
           v-a-t = v-amt[4] / v-ton[4].

          if v-a-m eq ? then v-a-m = 0.
          if v-pc1 eq ? then v-pc1 = 0.
          if v-ret eq ? then v-ret = 0.
          if v-pc2 eq ? then v-pc2 = 0.
          if v-dif eq ? then v-dif = 0.
          if v-a-t eq ? then v-a-t = 0.

          display string("YTD Sales Diff: " +
                  string(v-dif,">>>>>>>>9.99-"),"x(30)")
                         @ cust.name
                  "YTDLY:" @ v-lab
                  v-a-m
                  v-msf[4] @ v-msf[1]
                  v-cst[4] @ v-cst[1]
                  v-amt[4] @ v-amt[1]
                  v-pc1
                  v-ret
                  v-pc2
                  v-a-t     WHEN tb_ton
                  v-ton[4]  WHEN tb_ton @ v-ton[1].
          down.
          
          IF tb_excel THEN DO:
            PUT STREAM excel UNFORMATTED
                '"' cust.cust-no '",'
                '"' cust.NAME + " " + cust.city + ", " + cust.state '",'
                '"' cust.sman '",'
                '"' STRING(ptd-v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[1],"->>>>9.9") '",'
                '"' STRING(v-cst[1],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[1],"->>>>>>>>>9.99") '",'
                '"' STRING(ptd-v-pc1,"->>>9.9") '",'
                '"' STRING(ptd-v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(ptd-v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(ptd-v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[1],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' STRING(ytd-v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[2],"->>>>9.9") '",'
                '"' STRING(v-cst[2],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[2],"->>>>>>>>>9.99") '",'
                '"' STRING(ytd-v-pc1,"->>>9.9") '",'
                '"' STRING(ytd-v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(ytd-v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(ytd-v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[2],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' STRING(ptdly-v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[3],"->>>>9.9") '",'
                '"' STRING(v-cst[3],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[3],"->>>>>>>>>9.99") '",'
                '"' STRING(ptdly-v-pc1,"->>>9.9") '",'
                '"' STRING(ptdly-v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(ptdly-v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(ptdly-v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[3],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' STRING(v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[4],"->>>>9.9") '",'
                '"' STRING(v-cst[4],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[4],"->>>>>>>>>9.99") '",'
                '"' STRING(v-pc1,"->>>9.9") '",'
                '"' STRING(v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[4],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' string(ptd-v-dif,">>>>>>>>9.99-") '",'
                '"' string(v-dif,">>>>>>>>9.99-") '",'
                SKIP(1).
          END.

          put skip(1).
        end.

        do v = 5 to 8:
          assign
           v-msf[v] = v-msf[v] + v-msf[v - 4]
           v-cst[v] = v-cst[v] + v-cst[v - 4]
           v-amt[v] = v-amt[v] + v-amt[v - 4]
           v-ton[v] = v-ton[v] + v-ton[v - 4]

           v-msf[v - 4] = 0
           v-cst[v - 4] = 0
           v-amt[v - 4] = 0
           v-ton[v - 4] = 0.
        end.
      end.

      /* display final totals */
      if last(tt-custsort.sorter) then do:
        underline cust.cust-no
                  cust.name
                  cust.sman
                  v-lab
                  v-a-m
                  v-msf[1]
                  v-cst[1]
                  v-amt[1]
                  v-pc1
                  v-ret
                  v-pc2
                  v-a-t     WHEN tb_ton
                  v-ton[1]  WHEN tb_ton.

        put skip(1).

        if line-counter + 5 gt page-size then page.

        assign
         v-a-m = v-amt[5] / v-msf[5]
         v-pc1 = v-amt[5] / v-tot[1] * 100
         v-ret = v-amt[5] - v-cst[5]
         v-pc2 = v-ret    / v-amt[5] * 100
         v-a-t = v-amt[5] / v-ton[5].

        if v-a-m eq ? then v-a-m = 0.
        if v-pc1 eq ? then v-pc1 = 0.
        if v-ret eq ? then v-ret = 0.
        if v-pc2 eq ? then v-pc2 = 0.
        if v-a-t eq ? then v-a-t = 0.

        IF tb_excel THEN DO:

           ASSIGN
             ptd-v-a-m = v-a-m
             ptd-v-pc1 = v-pc1
             ptd-v-ret = v-ret
             ptd-v-pc2 = v-pc2.

           IF tb_ton THEN
              ptd-v-a-t = v-a-t.
        END.

        display "Grand Totals"
                         @ cust.name
                "  PTD:" @ v-lab
                v-a-m
                v-msf[5] @ v-msf[1]
                v-cst[5] @ v-cst[1]
                v-amt[5] @ v-amt[1]
                v-pc1
                v-ret
                v-pc2
                v-a-t     WHEN tb_ton
                v-ton[5]  WHEN tb_ton @ v-ton[1].
        down.

        assign
         v-a-m = v-amt[6] / v-msf[6]
         v-pc1 = v-amt[6] / v-tot[2] * 100
         v-ret = v-amt[6] - v-cst[6]
         v-pc2 = v-ret    / v-amt[6] * 100
         v-a-t = v-amt[6] / v-ton[6].

        if v-a-m eq ? then v-a-m = 0.
        if v-pc1 eq ? then v-pc1 = 0.
        if v-ret eq ? then v-ret = 0.
        if v-pc2 eq ? then v-pc2 = 0.
        if v-a-t eq ? then v-a-t = 0.

        IF tb_excel THEN DO:

           ASSIGN
             ytd-v-a-m = v-a-m
             ytd-v-pc1 = v-pc1
             ytd-v-ret = v-ret
             ytd-v-pc2 = v-pc2.

           IF tb_ton THEN
              ytd-v-a-t = v-a-t.
        END.

        display "  YTD:" @ v-lab
                v-a-m
                v-msf[6] @ v-msf[1]
                v-cst[6] @ v-cst[1]
                v-amt[6] @ v-amt[1]
                v-pc1
                v-ret
                v-pc2
                v-a-t     WHEN tb_ton
                v-ton[6]  WHEN tb_ton @ v-ton[1].
        down.
        
        assign
         v-a-m = v-amt[7] / v-msf[7]
         v-pc1 = v-amt[7] / v-tot[3] * 100
         v-ret = v-amt[7] - v-cst[7]
         v-pc2 = v-ret    / v-amt[7] * 100
         v-dif = v-amt[5] - v-amt[7]
         v-a-t = v-amt[7] / v-ton[7].

        if v-a-m eq ? then v-a-m = 0.
        if v-pc1 eq ? then v-pc1 = 0.
        if v-ret eq ? then v-ret = 0.
        if v-pc2 eq ? then v-pc2 = 0.
        if v-dif eq ? then v-dif = 0.
        if v-a-t eq ? then v-a-t = 0.

        IF tb_excel THEN DO:

           ASSIGN
             ptdly-v-a-m = v-a-m
             ptdly-v-pc1 = v-pc1
             ptdly-v-ret = v-ret
             ptdly-v-pc2 = v-pc2
             ptd-v-dif   = v-dif.

           IF tb_ton THEN
              ptdly-v-a-t = v-a-t.
        END.

        display string("PTD Sales Diff: " +
                  string(v-dif,">>>>>>>>9.99-"),"x(30)")
                         @ cust.name
                "PTDLY:" @ v-lab
                v-a-m
                v-msf[7] @ v-msf[1]
                v-cst[7] @ v-cst[1]
                v-amt[7] @ v-amt[1]
                v-pc1
                v-ret
                v-pc2
                v-a-t     WHEN tb_ton
                v-ton[7]  WHEN tb_ton @ v-ton[1].
        down.

        assign
         v-a-m = v-amt[8] / v-msf[8]
         v-pc1 = v-amt[8] / v-tot[4] * 100
         v-ret = v-amt[8] - v-cst[8]
         v-pc2 = v-ret    / v-amt[8] * 100
         v-dif = v-amt[6] - v-amt[8]
         v-a-t = v-amt[8] / v-ton[8].

        if v-a-m eq ? then v-a-m = 0.
        if v-pc1 eq ? then v-pc1 = 0.
        if v-ret eq ? then v-ret = 0.
        if v-pc2 eq ? then v-pc2 = 0.
        if v-dif eq ? then v-dif = 0.
        if v-a-m eq ? then v-a-m = 0.

        display string("YTD Sales Diff: " +
                string(v-dif,">>>>>>>>9.99-"),"x(30)")
                         @ cust.name
                "YTDLY:" @ v-lab
                v-a-m
                v-msf[8] @ v-msf[1]
                v-cst[8] @ v-cst[1]
                v-amt[8] @ v-amt[1]
                v-pc1
                v-ret
                v-pc2
                v-a-t     WHEN tb_ton
                v-ton[8]  WHEN tb_ton @ v-ton[1].
        down.

        IF tb_excel THEN DO:
           PUT STREAM excel UNFORMATTED
               '"' "" '",'
               '"' "Grand Totals" '",'
               '"' "" '",'
               '"' STRING(ptd-v-a-m,"->>>9.99") '",'
               '"' STRING(v-msf[5],"->>>>9.9") '",'
               '"' STRING(v-cst[5],"->>>>>>>>>9.99") '",'
               '"' STRING(v-amt[5],"->>>>>>>>>9.99") '",'
               '"' STRING(ptd-v-pc1,"->>>9.9") '",'
               '"' STRING(ptd-v-ret,"->>>>>>>>>9.99") '",'
               '"' STRING(ptd-v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(ptd-v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[5],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' STRING(ytd-v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[6],"->>>>9.9") '",'
                '"' STRING(v-cst[6],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[6],"->>>>>>>>>9.99") '",'
                '"' STRING(ytd-v-pc1,"->>>9.9") '",'
                '"' STRING(ytd-v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(ytd-v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(ytd-v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[6],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' STRING(ptdly-v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[7],"->>>>9.9") '",'
                '"' STRING(v-cst[7],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[7],"->>>>>>>>>9.99") '",'
                '"' STRING(ptdly-v-pc1,"->>>9.9") '",'
                '"' STRING(ptdly-v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(ptdly-v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(ptdly-v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[7],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' STRING(v-a-m,"->>>9.99") '",'
                '"' STRING(v-msf[8],"->>>>9.9") '",'
                '"' STRING(v-cst[8],"->>>>>>>>>9.99") '",'
                '"' STRING(v-amt[8],"->>>>>>>>>9.99") '",'
                '"' STRING(v-pc1,"->>>9.9") '",'
                '"' STRING(v-ret,"->>>>>>>>>9.99") '",'
                '"' STRING(v-pc2,"->>>9.9") '",'.

            IF tb_ton THEN
              PUT STREAM excel UNFORMATTED
                  '"' STRING(v-a-t,"->>>>>9.99") '",'
                  '"' STRING(v-ton[8],"->>>9.9") '",'.

            PUT STREAM excel UNFORMATTED
                '"' string(ptd-v-dif,">>>>>>>>9.99-") '",'
                '"' string(v-dif,">>>>>>>>9.99-") '",'.
        END.
      end.
    end.

    STATUS DEFAULT.
