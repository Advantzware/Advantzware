for each cust
        where cust.company eq cocode
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)
        no-lock:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
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
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

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
             tt-report2.key-01  = IF rd_sort BEGINS "S" THEN v-sman-no
                                                        ELSE ar-invl.i-no
             tt-report2.key-02  = string(year(ar-inv.inv-date),"9999") +
                                  string(month(ar-inv.inv-date),"99")  +
                                  string(day(ar-inv.inv-date),"99")
             tt-report2.key-03  = string(ar-inv.inv-no,"9999999999")
             tt-report2.key-04  = ar-invl.i-no
             tt-report2.key-08  = v-sman-no
             tt-report2.key-09  = tt-report.key-09
             tt-report2.key-10  = "ar-invl".

            IF ar-inv.f-bill AND ar-inv.freight NE 0 THEN DO:
              RUN salrep/invfrate.p (ROWID(ar-invl), v-sman-no, v-sman-no).

              FOR EACH b-tt-report
                  WHERE b-tt-report.key-10 EQ "ar-invf"
                    AND b-tt-report.inv-no EQ ar-inv.inv-no
                    AND b-tt-report.rec-id EQ RECID(ar-invl):

                tt-report2.freight = tt-report2.freight + b-tt-report.freight.

                DELETE b-tt-report.
              END.
            END.

            IF NOT rd_sort BEGINS "S" AND ar-invl.s-pct[i] GE 100 THEN LEAVE.
            LEAVE.
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

        if ar-cashl.dscr matches "*oe return*" then do:
          release ar-inv.
          find first oe-reth
              where oe-reth.company eq cocode
                and oe-reth.r-no    eq int(substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 25,12))
              no-lock no-error.
          if avail oe-reth then
          find first ar-inv
               where ar-inv.company eq cocode
                 and ar-inv.cust-no eq oe-reth.cust-no
                 and ar-inv.inv-no  eq oe-reth.inv-no
               no-lock no-error.

          if substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 12,5) eq "items" then do:
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
                 tt-report2.key-01  = IF rd_sort BEGINS "S" THEN v-sman-no
                                                            ELSE ar-invl.i-no
                 tt-report2.key-02  = tt-report.key-02
                 tt-report2.key-03  = tt-report.key-03
                 tt-report2.key-04  = oe-retl.i-no
                 tt-report2.key-08  = v-sman-no
                 tt-report2.key-09  = tt-report.key-09
                 tt-report2.key-10  = tt-report.key-10.

                IF NOT rd_sort BEGINS "S" AND ar-invl.s-pct[i] GE 100 THEN LEAVE.
                LEAVE.
              end.

              DELETE tt-report.
            end.
          end.

          else
          if substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 12,7) eq "freight" and
             cust.sman ge fsman                      and
             cust.sman le tsman                      then
            assign
             v-exc            = no
             tt-report.key-01 = IF rd_sort BEGINS "S" THEN cust.sman
                                                      ELSE "FREIGHT"
             tt-report.key-04 = "FREIGHT".

          else
          if substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 12,3) eq "tax" and
             "tax"     ge fitem                  and
             "tax"     le titem                  and
             "tax"     ge fpcat                  and
             "tax"     le tpcat                  and
             cust.sman ge fsman                  and
             cust.sman le tsman                  then
            assign
             v-exc            = no
             tt-report.key-01 = IF rd_sort BEGINS "S" THEN cust.sman
                                                      ELSE "TAX"
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
             tt-report.key-01 = IF rd_sort BEGINS "S" THEN cust.sman ELSE "".
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
           tt-report.key-01 = IF rd_sort BEGINS "S" THEN cust.sman ELSE "".
           
        IF AVAIL tt-report AND v-exc THEN DELETE tt-report.
      end.
    end.

    for each tt-report2
        where tt-report2.term-id eq ""

        break by tt-report2.key-01
              by tt-report2.key-02
              by tt-report2.key-03
              by tt-report2.key-04

        with frame itemx down:

        {custom/statusMsg.i " 'Processing Customer#  '  +  tt-report2.key-09 "}
        
      if rd_sort BEGINS "S" and first-of(tt-report2.key-01) then do:
        find first sman
            where sman.company eq cocode
              and sman.sman    eq tt-report2.key-01
            no-lock no-error.
            
        v-slsmn-hdr = "SalesRep: " + trim(tt-report2.key-01) + " " +
                      (if avail sman then sman.sname else "Not on file").
      
        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' "Salesrep: " trim(tt-report2.key-01) + " " +
                   (if avail sman then sman.sname else "Not on file") '",'
               SKIP(1).

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
       w-data.rec-id = tt-report2.rec-id.

      assign
       v-job-no   = ""
       v-job-no2  = 0
       v-frt[1]   = tt-report2.freight
       v-cst[1]   = 0
       v-po-no-po = 0
       v-pct      = 1.

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
         v-amt[1]   = ar-invl.amt.
         
        if v-cost1 eq "O" then do:
          find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq ar-invl.ord-no
                and oe-ordl.i-no    eq ar-invl.i-no
              no-lock no-error.
          if avail oe-ordl then
            assign
             v-cst[1] = oe-ordl.cost
             v-cuom   = "M".
        end.
        
        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report2.key-01 then
            assign
             v-pct = ar-invl.s-pct[i] / 100
             i     = 3.
        end.

        if v-pct eq 0 then
        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report2.key-08 then
            assign
             v-pct = ar-invl.s-pct[i] / 100
             i     = 3.
        end.

      /*  if v-pct eq 0 then
        do i = 1 to 3:
          if i eq 1 then j = 0.
          if ar-invl.sman[i] NE "" then j = j + 1.
          if i eq 3 then v-pct = 1 / j.
        end.*/

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
         v-qty[1]  = 0
         v-cst[1]  = 0.
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
         v-qty[1]  = 0
         v-cst[1]  = 0.
         
        if ar-cashl.dscr matches "*oe return*"   and
           substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 12,5) eq "items" then do:
          release ar-inv.
          find first oe-reth
              where oe-reth.company eq cocode
                and oe-reth.r-no    eq int(substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 25,12))
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
             v-amt[1]  = oe-retl.unit-pr
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
              v-amt[1] = ar-invl.unit-pr.
            
              do i = 1 to 3:
                if ar-invl.sman[i] eq tt-report2.key-01 then
                  assign
                   v-pct = ar-invl.s-pct[i] / 100
                   i     = 3.
              end.

              if v-pct eq 0 then
                  do i = 1 to 3:
                  if ar-invl.sman[i] eq tt-report2.key-08 then
                      assign
                      v-pct = ar-invl.s-pct[i] / 100
                      i     = 3.
              end.

              /*if v-pct eq 0 then
              do i = 1 to 3:
                if i eq 1 then j = 0.
                if ar-invl.sman[i] ne "" then j = j + 1.
                if i eq 3 then v-pct = 1 / j.
              end.*/

              if v-pct le 0 or v-pct eq ? then v-pct = 1.
            end.
          end.

          if v-cost1 eq "O" then do:
            find first oe-ordl
                where oe-ordl.company eq cocode
                  and oe-ordl.ord-no  eq oe-retl.ord-no
                  and oe-ordl.i-no    eq oe-retl.i-no
                no-lock no-error.
            if avail oe-ordl then
              assign
               v-cst[1] = oe-ordl.cost
               v-cuom   = "M".
          end.
        end.
      end.

      if v-job-no ne "" and v-cost1 eq "B" then do:
        find first job
            where job.company eq cocode
              and job.job-no  eq v-job-no
              and job.job-no2 eq v-job-no2
            no-lock no-error.

        if avail job then do:
          find first job-hdr
              where job-hdr.company eq cocode
                and job-hdr.job     eq job.job
                and job-hdr.job-no  eq job.job-no
                and job-hdr.job-no2 eq job.job-no2
                and job-hdr.i-no    eq w-data.i-no
              no-lock no-error.

          if avail job-hdr then do:
            v-job-qty = 0.
            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.job-no    eq job-hdr.job-no
                  and fg-rcpth.job-no2   eq job-hdr.job-no2
                  and fg-rcpth.i-no      eq job-hdr.i-no
                  and fg-rcpth.rita-code eq "R"
                no-lock,

                each fg-rdtlh
                where fg-rdtlh.r-no eq fg-rcpth.r-no
                no-lock:

               v-job-qty = v-job-qty + fg-rdtlh.qty.
            end.

            for each mat-act
                where mat-act.company eq cocode
                  and mat-act.job     eq job-hdr.job
                  and mat-act.job-no  eq job-hdr.job-no
                  and mat-act.job-no2 eq job-hdr.job-no2
                  and mat-act.s-num   eq job-hdr.frm
                no-lock,

                first job-mat
                where job-mat.company  eq cocode
                  and job-mat.job      eq mat-act.job
                  and job-mat.frm      eq mat-act.s-num
                  and job-mat.blank-no eq mat-act.b-num
                  and job-mat.i-no     eq mat-act.i-no
                no-lock,

                first item
                where item.company  eq cocode
                  and item.i-no     eq mat-act.i-no
                  and item.mat-type eq "B"
                no-lock:

              if item.r-wid eq 0 then
                run sys/ref/convcuom.p(job-mat.sc-uom, mat-act.qty-uom,
                                       (if job-mat.basis-w  ne 0 then
                                          job-mat.basis-w else item.basis-w),
                                       (if job-mat.len      ne 0 then
                                          job-mat.len else item.s-len),
                                       (if job-mat.wid      ne 0 then
                                          job-mat.wid else item.s-wid),
                                       item.s-dep,   
                                       mat-act.cost, output v-cost).

              else
                run sys/ref/convcuom.p(job-mat.sc-uom, mat-act.qty-uom,
                                       (if job-mat.basis-w  ne 0 then
                                          job-mat.basis-w else item.basis-w),
                                       job-mat.len,
                                       (if job-mat.wid      ne 0 then
                                          job-mat.wid else item.r-wid),
                                       item.s-dep,   
                                       mat-act.cost, output v-cost).

              IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
                v-cst[1] = v-cst[1] + (v-cost * mat-act.qty).
              ELSE
                v-cst[1] = v-cst[1] + mat-act.ext-cost.
            end.

            assign
             v-cst[1] = v-cst[1] * (job-hdr.qty / v-job-qty)
             v-cst[1] = v-cst[1] / job-hdr.qty * v-qty[1].
          end.
        end.
      end.

      if v-cst[1] eq 0 or v-cost1 eq "O" then do:
        if v-cst[1] eq 0 or v-cost1 ne "O" then do:
          find first po-ordl
              where po-ordl.company   eq cocode
                and po-ordl.po-no     eq v-po-no-po
                and po-ordl.i-no      eq w-data.i-no
                and po-ordl.deleted   eq no
                and po-ordl.item-type eq no
                and po-ordl.job-no    eq v-job-no
                and po-ordl.job-no2   eq v-job-no2
              use-index po-no no-lock no-error.
          if not avail po-ordl then
          for each po-ordl
              where po-ordl.company   eq cocode
                and po-ordl.i-no      eq w-data.i-no
                and po-ordl.deleted   eq no
                and po-ordl.item-type eq no
                and po-ordl.job-no    eq v-job-no
                and po-ordl.job-no2   eq v-job-no2
              use-index item no-lock
              by po-ordl.po-no desc:
            leave.
          end.
          if not avail po-ordl then
          for each po-ordl
              where po-ordl.company   eq cocode
                and po-ordl.i-no      eq w-data.i-no
                and po-ordl.deleted   eq no
                and po-ordl.item-type eq no
              use-index item no-lock
              by po-ordl.po-no desc:
            leave.
          end.
          if avail po-ordl then
            assign
             v-cst[1] = po-ordl.cons-cost
             v-cuom   = po-ordl.cons-uom.

          if v-cst[1] eq 0 and avail itemfg then
            assign
             v-cst[1] = if itemfg.i-code eq "C" and v-cost1 eq "B" then
                          itemfg.std-mat-cost else
                        if fg-ctrl.inv-meth eq "A" then itemfg.avg-cost
                                                   else itemfg.last-cost
             v-cuom   = itemfg.prod-uom.
        end.

        v-cst[1] = v-cst[1] * v-qty[1] /
                   (if v-cuom eq "C"  then 100               else
                    if v-cuom eq "M"  then 1000              else
                    if v-cuom eq "CS" and avail itemfg and
                       itemfg.case-count ne 0 then itemfg.case-count else 1).
      end.
        
      assign
       v-qty[1] = v-qty[1] * v-pct
       v-amt[1] = v-amt[1] * v-pct
       v-cst[1] = v-cst[1] * v-pct
       
       v-qty[2] = v-qty[1]
       v-amt[2] = v-amt[1]
       v-frt[2] = v-frt[1]
       v-cst[2] = v-cst[1].

      IF v-qty[2] EQ ? THEN v-qty[2] = 0.
      IF v-frt[2] EQ ? THEN v-frt[2] = 0.
      IF v-cst[2] EQ ? THEN v-cst[2] = 0.
      IF v-amt[2] EQ ? THEN v-amt[2] = 0.
       
      assign
       v-brdc = v-cst[2]
       v-marg = v-amt[2] - v-cst[2].

      if v-brdc eq ? then v-brdc = 0.
      if v-marg eq ? then v-marg = 0.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq v-cust-no
          no-lock no-error.

      display cust.name         when avail cust
              w-data.inv-no
              w-data.i-no
              itemfg.procat     when avail itemfg
              v-qty[2]
              v-frt[2]
              v-amt[2]
              v-brdc            when v-cost2
              v-marg            when v-cost2.

      down.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' IF AVAIL cust THEN cust.name ELSE "" '",'
             '"' w-data.inv-no    '",'
             '"' w-data.i-no      '",'
             '"' IF AVAIL itemfg THEN itemfg.procat ELSE "" '",'
             '"' STRING(v-qty[2],"->>>,>>>,>>>")      '",'
             '"' STRING(v-frt[2],"->>>,>>>,>>9.99")      '",'
             '"' STRING(v-amt[2],"->>>,>>>,>>9.99<<")      '",'
             '"' IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE ""   '",'
             '"' IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE ""   '",'
                 SKIP.


      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-frt[3] = v-frt[3] + v-frt[2]
       v-cst[3] = v-cst[3] + v-cst[2]
       v-amt[3] = v-amt[3] + v-amt[2]
         
       v-qty[2] = 0
       v-frt[2] = 0
       v-cst[2] = 0
       v-amt[2] = 0.

      if last-of(tt-report2.key-02) then do:
        IF NOT rd_sort BEGINS "F" THEN DO:
          underline v-qty[2] v-frt[2] with frame itemx.

          if v-cost2 then underline v-brdc v-marg v-amt[2] with frame itemx.

          assign
           v-brdc = v-cst[3]
           v-marg = v-amt[3] - v-cst[3].

          if v-brdc eq ? then v-brdc = 0.
          if v-marg eq ? then v-marg = 0.

          display "    DATE TOTALS FOR " + substr(tt-report2.key-02,5,2) + "/" +
                                           substr(tt-report2.key-02,7,2) + "/" +
                                           substr(tt-report2.key-02,1,4)
                                                    @ cust.name
                  v-qty[3]                          @ v-qty[2]
                  v-frt[3]                          @ v-frt[2]
                  v-amt[3]                          @ v-amt[2]
                  v-brdc                            when v-cost2
                  v-marg                            when v-cost2

              with frame itemx.

          down with frame itemx.
        
          put skip(1).

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' "DATE TOTALS FOR " + substr(tt-report2.key-02,5,2) + "/" +
                     substr(tt-report2.key-02,7,2) + "/" +
                     substr(tt-report2.key-02,1,4) '",'
                 '"' ""    '",'
                 '"' ""    '",'
                 '"' ""    '",'
                 '"' STRING(v-qty[3],"->>>,>>>,>>>")      '",'
                 '"' STRING(v-frt[3],"->>>,>>>,>>9.99")      '",'
                 '"' STRING(v-amt[3],"->>>,>>>,>>9.99<<")      '",'
                 '"' IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE ""   '",'
                 '"' IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE ""   '",'
                     SKIP(1).
        END.

        assign
         v-qty[4] = v-qty[4] + v-qty[3]
         v-frt[4] = v-frt[4] + v-frt[3]
         v-cst[4] = v-cst[4] + v-cst[3]
         v-amt[4] = v-amt[4] + v-amt[3]

         v-qty[3] = 0
         v-frt[3] = 0
         v-cst[3] = 0
         v-amt[3] = 0.
      end.

      if last-of(tt-report2.key-01) then do:
        underline v-qty[2] v-frt[2] with frame itemx.

        if v-cost2 then underline v-brdc v-marg v-amt[2] with frame itemx.

        assign
         v-brdc = v-cst[4]
         v-marg = v-amt[4] - v-cst[4].

        if v-brdc eq ? then v-brdc = 0.
        if v-marg eq ? then v-marg = 0.

        display "     FG ITEM TOTALS "          @ cust.name
                   "    SALESREP TOTALS " WHEN rd_sort BEGINS "S"
                                                @ cust.name
                v-qty[4]                        @ v-qty[2]
                v-frt[4]                        @ v-frt[2]
                v-amt[4]                        @ v-amt[2]
                v-brdc                          when v-cost2
                v-marg                          when v-cost2
      
            with frame itemx.

        down with frame itemx.
          
        put skip(1).

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' IF rd_sort BEGINS "S" THEN "SALESREP TOTALS"
                   ELSE "FG ITEMS TOTAL"                      '",'
               '"' ""    '",'
               '"' ""    '",'
               '"' ""    '",'
               '"' STRING(v-qty[4],"->>>,>>>,>>>")      '",'
               '"' STRING(v-frt[4],"->>>,>>>,>>9.99")      '",'
               '"' STRING(v-amt[4],"->>>,>>>,>>9.99<<")      '",'
               '"' IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE ""   '",'
               '"' IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE ""   '",'
                   SKIP(1).

        assign
         v-qty[5] = v-qty[5] + v-qty[4]
         v-frt[5] = v-frt[5] + v-frt[4]
         v-cst[5] = v-cst[5] + v-cst[4]
         v-amt[5] = v-amt[5] + v-amt[4]

         v-qty[4] = 0
         v-frt[4] = 0
         v-cst[4] = 0
         v-amt[4] = 0.
      end.
      
      delete w-data.
    end.

    /* display final totals */
    if v-qty[5] ne 0 or v-cst[5] ne 0 or v-amt[5] ne 0 then do:
      put skip(1).

      underline v-qty[2] v-frt[2] with frame itemx.

      if v-cost2 then underline v-brdc v-marg v-amt[2] with frame itemx.

      assign
       v-brdc = v-cst[5]
       v-marg = v-amt[5] - v-cst[5].

      if v-brdc eq ? then v-brdc = 0.
      if v-marg eq ? then v-marg = 0.

      display "                  GRAND TOTALS"  @ cust.name
              v-qty[5]                          @ v-qty[2]
              v-frt[5]                          @ v-frt[2]
              v-amt[5]                          @ v-amt[2]
              v-brdc                            when v-cost2
              v-marg                            when v-cost2

          with frame itemx.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' "GRAND TOTAL"                      '",'
             '"' ""    '",'
             '"' ""    '",'
             '"' ""    '",'
             '"' STRING(v-qty[5],"->>>,>>>,>>>")      '",'
             '"' STRING(v-frt[5],"->>>,>>>,>>9.99")      '",'
             '"' STRING(v-amt[5],"->>>,>>>,>>9.99<<")      '",'
             '"' IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE ""   '",'
             '"' IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE ""   '",'
                 SKIP(1).
    end.
