
      /* display work-file and accumulate final totals */
      for each w-data
          break by w-data.sorter
                by w-data.i-no
                by w-data.inv-no

          with frame itemx down:

        assign
         v-job-no  = ""
         v-job-no2 = 0
         v-msf[1]  = 0
         v-cst[1]  = 0.

        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq w-data.i-no
            no-lock no-error.

        find first ar-invl
            where recid(ar-invl) eq w-data.rec-id
            no-lock no-error.

        if avail ar-invl then do:
          find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
          assign
           v-cust-no = ar-inv.cust-no
           v-date    = ar-inv.inv-date
           v-pric    = ar-invl.unit-pr
           v-uom     = ar-invl.pr-uom
           v-job-no  = ar-invl.job-no
           v-job-no2 = ar-invl.job-no2
           v-qty[1]  = ar-invl.ship-qty
           v-amt[1]  = ar-invl.amt
           v-msf[1]  = ar-invl.amt-msf.
        end.

        else do:
          find first ar-cashl
              where recid(ar-cashl) eq w-data.rec-id
              no-lock no-error.
          if avail ar-cashl then do:
            find first ar-cash
                where ar-cash.c-no eq ar-cashl.c-no
                no-lock.
            assign
             v-cust-no = ar-cash.cust-no
             v-date    = ar-cash.check-date
             v-pric    = if ar-cashl.dscr begins "debit"
                           then ar-cashl.amt-paid else (- ar-cashl.amt-disc)
             v-uom     = ""
             v-qty[1]  = 0
             v-cst[1]  = 0
             v-amt[1]  = if ar-cashl.dscr begins "debit"
                           then ar-cashl.amt-paid else (- ar-cashl.amt-disc).
          end.

          else do:
            find first oe-retl
                where recid(oe-retl) eq w-data.rec-id
                no-lock.
            find first oe-reth
                where oe-reth.company eq oe-retl.company
                  and oe-reth.r-no    eq oe-retl.r-no
                no-lock.

            assign
             v-fac     = if oe-retl.uom       eq "CS" and
                            avail itemfg              and
                            itemfg.case-count ne 0    then itemfg.case-count
                         else if oe-retl.uom  eq "C"  then 100
                         else if oe-retl.uom  eq "M"  then 1000 else 1

             v-cust-no = oe-reth.cust-no
             v-date    = oe-reth.return-date
             v-pric    = oe-retl.unit-pr
             v-uom     = oe-retl.uom
             v-job-no  = oe-retl.job-no
             v-job-no2 = oe-retl.job-no2
             v-qty[1]  = - oe-retl.tot-qty-return
             v-amt[1]  = oe-retl.unit-pr * (v-qty[1] / v-fac).
             
             find first ar-invl where ar-invl.company eq cocode
                                  and ar-invl.cust-no eq oe-reth.cust-no
                                  and ar-invl.inv-no  eq oe-reth.inv-no
                                  and ar-invl.i-no    eq oe-retl.i-no
                                no-lock no-error.
             if avail ar-invl then
               assign v-pric   = ar-invl.unit-pr
                      v-amt[1] = ar-invl.unit-pr * (v-qty[1] / v-fac).
                      
          end.
        end.

        run sys/inc/bordcost.p (v-job-no, v-job-no2, w-data.i-no, v-qty[1],
                                output v-cst[1]).

        if v-msf[1] eq 0 and avail itemfg then
          v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

        assign
         v-brdc = v-cst[1] / (v-qty[1] / 1000)
         v-marg = (v-amt[1] - v-cst[1]) / (v-qty[1] / 1000)
         v-brdp = v-cst[1] / v-amt[1] * 100
         v-$msf = v-amt[1] / v-msf[1].

        if v-brdc eq ? then v-brdc = 0.
        if v-marg eq ? then v-marg = 0.
        if v-brdp eq ? then v-brdp = 0.
        if v-$msf eq ? then v-$msf = 0.

        display v-cust-no
                w-data.inv-no
                w-data.i-no
                itemfg.procat     when avail itemfg
                v-qty[1]
                itemfg.t-sqft     when avail itemfg
                v-msf[1]
                v-$msf
                v-pric
                v-uom
                v-brdc
                v-marg
                v-brdp.

        down.

        assign
         v-qty[2] = v-qty[2] + v-qty[1]
         v-msf[2] = v-msf[2] + v-msf[1]
         v-cst[2] = v-cst[2] + v-cst[1]
         v-amt[2] = v-amt[2] + v-amt[1].

        if last-of(w-data.i-no) then do:
          underline v-qty[1] v-msf[1] v-brdc v-marg v-brdp with frame itemx.

          if (not first-of(w-data.i-no)) then do:
            assign
             v-brdc = v-cst[2] / (v-qty[2] / 1000)
             v-marg = (v-amt[2] - v-cst[2]) / (v-qty[2] / 1000)
             v-brdp = v-cst[2] / v-amt[2] * 100
             v-$msf = v-amt[2] / v-msf[2].

            if v-brdc eq ? then v-brdc = 0.
            if v-marg eq ? then v-marg = 0.
            if v-brdp eq ? then v-brdp = 0.
            if v-$msf eq ? then v-$msf = 0.

            display "    ITEM" @ v-cust-no
                    "TOTALS"   @ w-data.i-no
                    v-qty[2]   @ v-qty[1]
                    v-msf[2]   @ v-msf[1]
                    v-$msf
                    v-brdc
                    v-marg
                    v-brdp

                with frame itemx.

            down with frame itemx.
            put skip(1).
          end.

          assign
           v-qty[3] = v-qty[3] + v-qty[2]
           v-msf[3] = v-msf[3] + v-msf[2]
           v-cst[3] = v-cst[3] + v-cst[2]
           v-amt[3] = v-amt[3] + v-amt[2]

           v-qty[2] = 0
           v-msf[2] = 0
           v-cst[2] = 0
           v-amt[2] = 0.
        end.

        if last-of(w-data.sorter) then do:
          underline v-qty[1] v-msf[1] v-brdc v-marg v-brdp with frame itemx.

          if (not first-of(w-data.sorter)) then do:
            assign
             v-brdc = v-cst[3] / (v-qty[3] / 1000)
             v-marg = (v-amt[3] - v-cst[3]) / (v-qty[3] / 1000)
             v-brdp = v-cst[3] / v-amt[3] * 100
             v-$msf = v-amt[3] / v-msf[3].

            if v-brdc eq ? then v-brdc = 0.
            if v-marg eq ? then v-marg = 0.
            if v-brdp eq ? then v-brdp = 0.
            if v-$msf eq ? then v-$msf = 0.

            display "PROD CAT" @ v-cust-no
                    "CUSTOMER" when sort-by-cust @ v-cust-no
                    "TOTALS"   @ w-data.i-no
                    v-qty[3]   @ v-qty[1]
                    v-msf[3]   @ v-msf[1]
                    v-brdc
                    v-marg
                    v-brdp

                with frame itemx.

            down with frame itemx.
            put skip(1).
          end.

          assign
           v-qty[4] = v-qty[4] + v-qty[3]
           v-msf[4] = v-msf[4] + v-msf[3]
           v-cst[4] = v-cst[4] + v-cst[3]
           v-amt[4] = v-amt[4] + v-amt[3]

           v-qty[3] = 0
           v-msf[3] = 0
           v-cst[3] = 0
           v-amt[3] = 0.
        end.
      end.

      for each w-data:
        delete w-data.
      end.

