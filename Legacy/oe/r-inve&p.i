

        if v-post then do:
          v-xno = 1.
          find last ar-inv use-index x-no no-lock no-error.
          if avail ar-inv then
            assign
             v-xno   = ar-inv.x-no + 1
             v-xline = 0.

          create ar-inv.
          {oe/invhpost.i}

          if v-export eq "Sonoco" then do:
            run oe/sonofile.p (1,recid(ar-inv)).
            run ar/sonoinv.p ("inv-head", recid(inv-head),
                              output v-rec-written).
                              
            t-rec-written = t-rec-written + v-rec-written.
          end.
        end.

        RELEASE currency.
        IF inv-head.terms NE "CASH"              AND
           lv-comp-curr NE ""                    AND
           lv-comp-curr NE inv-head.curr-code[1] THEN
        FIND FIRST currency NO-LOCK
            WHERE currency.company     EQ inv-head.company
              AND currency.c-code      EQ inv-head.curr-code[1]
              AND currency.ar-ast-acct NE ""
              AND currency.ex-rate     GT 0
            NO-ERROR.

        ASSIGN
         v-postable       = YES
         v-reduce-ord-bal = 0
         v-inv-qty        = 0
         v-inv-disc       = 0
         v-line-tot       = 0
         v-misc-tot       = 0
         v-line-tot-w     = 0
         v-inv-disc-w     = 0
         v-ord-no         = 0
         v-ord-date       = ?.

        FOR EACH w-inv-line:
          DELETE w-inv-line.
        END.
        
/************ line ITEMS ************************************************/
        for each inv-line
            where inv-line.r-no eq inv-head.r-no
            use-index r-no break by inv-line.ord-no:

          find first itemfg
              {sys/look/itemfgrlW.i}
                and itemfg.i-no eq inv-line.i-no
              no-error.

          find first uom
              where uom.uom  eq inv-line.pr-uom
                and uom.mult ne 0
              no-lock no-error.

          find first oe-ordl
               where oe-ordl.company eq cocode
                 and oe-ordl.ord-no  eq inv-line.ord-no
                 and oe-ordl.line    eq inv-line.line
                 and oe-ordl.i-no    eq inv-line.i-no
               use-index ord-no no-error.

          RELEASE oe-ord.
          IF inv-line.ord-no NE 0 AND AVAIL oe-ordl THEN
          FIND FIRST oe-ord
              WHERE oe-ord.company EQ oe-ordl.company
                AND oe-ord.ord-no  EQ oe-ordl.ord-no
              NO-LOCK NO-ERROR.

          IF AVAIL oe-ord AND v-post THEN DO:
            {oe/closeaud.i oe-ord}
            CREATE w-ord.
            ASSIGN
             w-ord.ord-no = oe-ord.ord-no
             w-ord.rec-id = RECID(oe-ord).
          END.
          
          assign
           v-inv-qty = v-inv-qty + inv-line.inv-qty
           v-cas-cnt = if inv-line.cas-cnt ne 0 then
                         inv-line.cas-cnt
                       else
                       if avail oe-ordl and oe-ordl.cas-cnt ne 0 then
                         oe-ordl.cas-cnt
                       else
                       if avail itemfg and itemfg.case-count ne 0 then
                         itemfg.case-count
                       else 1.

          if first(inv-line.ord-no) then
            assign
             v-ord-no = inv-line.ord-no
             v-ord-date = inv-line.ord-date.

          if first-of(inv-line.ord-no) and v-detail and not v-post then
            put skip.

          /*if v-detail and not v-post then do:*/
            create w-inv-line.
            assign
             w-inv-line.ord-no   = inv-line.ord-no
             w-inv-line.i-no     = inv-line.i-no
             w-inv-line.i-name   = inv-line.i-name
             w-inv-line.qty      = inv-line.qty
             w-inv-line.inv-qty  = inv-line.inv-qty
             w-inv-line.ship-qty = inv-line.ship-qty
             w-inv-line.price    = inv-line.price
             w-inv-line.uom      = inv-line.pr-uom
             w-inv-line.t-price  = inv-line.t-price
             w-inv-line.cost     = inv-line.cost.
          /*end.*/

          if avail itemfg then
          find first fgcat
              where fgcat.company eq cocode
                and fgcat.procat  eq itemfg.procat
              no-lock no-error.
          else
          if v-post then undo ordblock, next ordblock.

          run oe/invlcost.p (ROWID(inv-line),
                             output v-cost[1], output v-cost[2],
                             output v-cost[3], output v-cost[4],
                             output inv-line.cost, output inv-line.t-cost).
          w-inv-line.t-cost = inv-line.t-cost.
          if inv-line.inv-qty ne 0 and
             inv-line.t-cost eq 0  and 
             not v-post-zero-cgs   then undo ordblock, next ordblock.

          run oe/invposty.p (inv-head.inv-no, inv-line.i-no, inv-line.inv-qty,
                             "M", v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

          if avail itemfg and v-post then do:
            assign
             v-invline = recid(inv-line)
             v-invhead = recid(inv-head).

            run oe/invpost3.p (tran-date, tran-period).
          end. /* avail itemfg & v-post */

          RUN calc-tons (w-inv-line.i-no, w-inv-line.inv-qty, OUTPUT w-inv-line.weight).

          IF inv-line.t-price NE 0 THEN DO:
            ld-temp-amt = 0.
            IF inv-line.disc NE 0 THEN DO:
              ld-temp-amt = ROUND((IF inv-line.pr-uom BEGINS "L" AND
                                      inv-line.pr-uom NE "LB"    THEN
                                     IF inv-line.inv-qty LT 0 THEN -1 ELSE 1
                                   ELSE
                                   IF inv-line.pr-uom EQ "CS" THEN
                                     inv-line.inv-qty / v-cas-cnt
                                   ELSE
                                   IF AVAIL uom THEN
                                     inv-line.inv-qty / uom.mult
                                   ELSE
                                     inv-line.inv-qty / 1000) *
                                   inv-line.price,2) -
                            inv-line.t-price.

              IF AVAIL currency THEN
                ld-temp-amt = ld-temp-amt * currency.ex-rate.

              ASSIGN
               v-inv-disc   = v-inv-disc + ld-temp-amt
               v-inv-disc-w = v-inv-disc-w + w-inv-line.weight.
            END.

            CREATE tt-report.
            ASSIGN
             tt-report.term-id = ""
             tt-report.key-01  = "work-line"
             tt-report.key-02  = if avail fgcat and fgcat.glacc ne ""
                                 then fgcat.glacc else v-ar-sales
             tt-report.key-03  = string(inv-head.inv-no,"999999")
             tt-report.key-04  = inv-line.i-no
             tt-report.weight  = w-inv-line.weight
             ld-temp-amt       = ld-temp-amt +
                                 (inv-line.t-price *
                                  (IF AVAIL currency THEN currency.ex-rate ELSE 1))
             tt-report.key-05  = STRING(ld-temp-amt).
          END.

          if v-post then do:
            /*** Calculate the amount of dollars to take out of the
                 customer's on order balance ***/
            FIND CURRENT oe-ordl NO-ERROR.
            if avail oe-ordl then do:
                  
              run ar/calctax2.p (oe-ord.tax-gr,
                                 no,
                                 oe-ordl.t-price,
                                 oe-ordl.company, 
                                 oe-ordl.i-no,
                                 output v-tax).

              v-uninv-ordl-amt = oe-ordl.t-price +
                                 (if oe-ordl.tax then v-tax else 0).

              for each ar-invl
                  where ar-invl.company eq cocode
                    and ar-invl.posted  eq yes
                    and ar-invl.cust-no eq inv-head.cust-no
                    and ar-invl.ord-no  eq inv-line.ord-no
                    and ar-invl.line    eq inv-line.line
                    and ar-invl.i-no    eq inv-line.i-no
                  use-index inv-status no-lock:
                  
                run ar/calctax2.p (ar-inv.tax-code, 
                                  no,
                                  ar-invl.amt,
                                  ar-invl.company,
                                  ar-invl.i-no,
                                  output v-tax).
                
                v-uninv-ordl-amt = v-uninv-ordl-amt - ar-invl.amt -
                                   (if ar-invl.tax then v-tax else 0).
              end.
            end.

            else
              v-uninv-ordl-amt = 0.

            v-tax = 0.
            if inv-line.tax then
            run ar/calctax2.p (inv-head.tax-gr, 
                              no,
                              inv-line.t-price, 
                              inv-line.company,
                              inv-line.i-no,
                              output v-tax).
                                  
            if inv-line.t-price + v-tax lt v-uninv-ordl-amt then
              v-reduce-ord-bal = v-reduce-ord-bal + inv-line.t-price + v-tax.
            else
              v-reduce-ord-bal = v-reduce-ord-bal + v-uninv-ordl-amt.

            /*find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq inv-head.cust-no
                  and ar-invl.inv-no  eq inv-head.inv-no
                  and ar-invl.ord-no  eq inv-line.ord-no
                  and ar-invl.line    eq inv-line.line
                  and ar-invl.i-no    eq inv-line.i-no
                  and ar-invl.b-no    eq inv-line.b-no
                use-index inv-no no-error.
                
            if not avail ar-invl then*/ create ar-invl.
            
            {oe/invlpost.i}.

            /* gdm - 09290908 */ RUN get-lot-no.

            if v-export eq "Sonoco" then run oe/sonofile.p (2,recid(ar-invl)).
            else
               if v-export eq "Inland" then run ar/jdedward.p (recid(ar-invl)).
            ELSE DO:
               if v-export eq "Excel" THEN DO: 
                  run ar/jdedwrdx.p (recid(ar-invl), INPUT v-first).
                  v-first = NO.
               END.
            END.
          end. /* v-post */
          
          IF AVAIL tt-report THEN
            ASSIGN
             v-line-tot   = v-line-tot   + inv-line.t-price
             v-line-tot-w = v-line-tot-w + tt-report.weight.

          if v-post then do:
            if inv-line.ord-no ne 0 then do:
              /* Sum all release qty */
              v-sum-rel-qty = 0.

              FOR EACH oe-boll
                  WHERE oe-boll.company EQ inv-line.company
                    AND oe-boll.b-no    EQ inv-line.b-no
                    AND oe-boll.ord-no  EQ inv-line.ord-no
                    AND oe-boll.i-no    EQ inv-line.i-no
                    AND oe-boll.line    EQ inv-line.line
                    AND oe-boll.po-no   EQ inv-line.po-no
                    AND CAN-FIND(FIRST oe-bolh
                                 WHERE oe-bolh.b-no   EQ oe-boll.b-no
                                   AND oe-bolh.posted EQ YES)
                  NO-LOCK
                  BREAK BY oe-boll.r-no
                        BY oe-boll.rel-no
                        BY oe-boll.b-ord-no:

                IF FIRST-OF(oe-boll.b-ord-no) THEN
                FOR EACH oe-rell
                    WHERE oe-rell.company  EQ oe-boll.company
                      AND oe-rell.ord-no   EQ oe-boll.ord-no
                      AND oe-rell.line     EQ oe-boll.line
                      AND oe-rell.i-no     EQ oe-boll.i-no
                      AND oe-rell.r-no     EQ oe-boll.r-no
                      AND oe-rell.rel-no   EQ oe-boll.rel-no
                      AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
                      AND CAN-FIND(FIRST oe-relh
                                   WHERE oe-relh.r-no   EQ oe-boll.r-no
                                     AND oe-relh.posted EQ YES)
                    USE-INDEX ord-no NO-LOCK:
                  v-sum-rel-qty = v-sum-rel-qty + oe-rell.qty.
                END.
              END.

              if v-sum-rel-qty ge oe-ordl.qty and 
                 (CAN-FIND(oe-boll WHERE oe-boll.company EQ inv-line.company
                                     AND oe-boll.b-no   EQ inv-line.b-no
                                     AND oe-boll.ord-no EQ inv-line.ord-no
                                     AND oe-boll.i-no   EQ inv-line.i-no
                                     AND oe-boll.line   EQ inv-line.line
                                     AND oe-boll.po-no  EQ inv-line.po-no
                                     AND oe-boll.p-c    EQ TRUE) or
                  inv-line.p-c eq true) then
              for each oe-ordl where oe-ordl.company eq cocode
                                 and oe-ordl.ord-no  eq inv-line.ord-no
                                 and oe-ordl.i-no    eq inv-line.i-no:
                /* previous runs may have overstated the shipped qty.  re-acquire the "truth" */
                RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT oe-ordl.inv-qty, 
                                   OUTPUT oe-ordl.ship-qty).
                ASSIGN oe-ordl.t-ship-qty = oe-ordl.ship-qty.
                assign v-close-qty = oe-ordl.qty - oe-ordl.t-ship-qty.
                if v-close-qty lt 0 
                    /*10021404 - also do not reduce allocated for Invoice Only*/
                    OR (CAN-FIND(oe-boll WHERE oe-boll.company EQ inv-line.company
                                     AND oe-boll.b-no   EQ inv-line.b-no
                                     AND oe-boll.ord-no EQ inv-line.ord-no
                                     AND oe-boll.i-no   EQ inv-line.i-no
                                     AND oe-boll.line   EQ inv-line.line
                                     AND oe-boll.po-no  EQ inv-line.po-no
                                     AND oe-boll.s-code EQ "I"))
                    then v-close-qty = 0.

                find first xoe-ord where xoe-ord.company eq cocode
                                     and xoe-ord.ord-no  eq oe-ordl.ord-no
                                   no-lock no-error.

                if avail itemfg then do:
                  IF xoe-ord.type NE "T" THEN
                    itemfg.q-alloc = itemfg.q-alloc - v-close-qty.
                  if itemfg.q-alloc lt 0 then itemfg.q-alloc = 0.

                  itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
                  if itemfg.q-avail lt 0 then itemfg.q-avail = 0.

                  RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xoe-ord.loc).

                  FIND FIRST itemfg-loc 
                      WHERE itemfg-loc.company EQ itemfg.company
                        AND itemfg-loc.i-no    EQ itemfg.i-no
                        AND itemfg-loc.loc     EQ xoe-ord.loc
                      EXCLUSIVE-LOCK NO-ERROR.

                  assign
                    itemfg.q-ptd     = itemfg.q-ptd     - v-close-qty
                    itemfg.q-ord-ytd = itemfg.q-ord-ytd - v-close-qty.

                  IF AVAIL itemfg-loc THEN
                    itemfg-loc.q-alloc = itemfg-loc.q-alloc - v-close-qty.
                          
                  IF AVAIL(itemfg-loc) THEN DO:
                      if itemfg-loc.q-alloc lt 0 then itemfg-loc.q-alloc = 0.
    
                      itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
                      if itemfg-loc.q-avail lt 0 then itemfg-loc.q-avail = 0.
    
                      assign
                        itemfg-loc.q-ptd     = itemfg-loc.q-ptd     - v-close-qty
                        itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - v-close-qty.
                  END.
                  FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                end.

                if v-close-qty gt 0 then do:
                  assign
                   v-uom-rate = if oe-ordl.pr-uom eq "M"  then 1000
                                else
                                if oe-ordl.pr-uom eq "C"  then 100
                                else
                                if avail itemfg           and
                                   oe-ordl.pr-uom  eq "CS"
                                                          then itemfg.case-count
                                else 1

                   v-dcr-val  = (v-close-qty / v-uom-rate) * oe-ordl.price
                   v-dcr-val  = v-dcr-val - (v-dcr-val * oe-ordl.disc / 100).

                  if oe-ordl.tax then do:
                    run ar/calctax2.p (oe-ord.tax-gr, 
                                       no,
                                       v-dcr-val,
                                       oe-ordl.company,
                                       oe-ordl.i-no,
                                       output v-tax).
                                      
                    v-dcr-val = v-dcr-val + v-tax.
                  end.
                  
                  if avail cust then cust.ord-bal = cust.ord-bal - v-dcr-val.
                end.

                /*RUN oe/clslnchkinv.p (BUFFER oe-ordl,OUTPUT v-close-line-ok).
                IF v-close-line-ok THEN
                   RUN oe/closelin.p (ROWID(oe-ordl),YES).*/
                
              end. /* for each oe-ordl */
            end.
          end.
          STATUS DEFAULT "Posting for Invoic#: " + string(inv-head.inv-no) +
                         ", Item: " + inv-line.i-no.
        end. /* each inv-line */

  /******************* MISCELLANEOUS ITEMS ***********************************/
  /* Be aware that job nos are not stroed in ar-invl records for misc charges*/

        for each inv-misc
            where inv-misc.r-no eq inv-head.r-no
            use-index r-no:

          if v-detail and not v-post and inv-misc.bill eq "Y" then do:
            create w-ord-misc.
            assign
             w-ord-misc.ord-no = inv-misc.ord-no
             w-ord-misc.charge = inv-misc.charge
             w-ord-misc.dscr   = inv-misc.dscr
             w-ord-misc.amt    = inv-misc.amt
             w-ord-misc.tax    = inv-misc.tax
             w-ord-misc.bill   = inv-misc.bill.
          end. /* v-detail */

          IF inv-misc.bill EQ "Y" AND inv-misc.amt NE 0 THEN DO:
            CREATE tt-report.
            ASSIGN
             tt-report.term-id = ""
             tt-report.key-01  = "work-misc"
             tt-report.key-02  = IF inv-misc.actnum NE ""
                                 THEN inv-misc.actnum ELSE v-ar-sales
             tt-report.key-03  = STRING(inv-head.inv-no,"999999")
             tt-report.key-04  = inv-misc.charge
             tt-report.key-05  = STRING(inv-misc.amt *
                                        (IF AVAIL currency  THEN
                                           currency.ex-rate ELSE 1)).

            v-misc-tot = v-misc-tot + inv-misc.amt.
                
            RELEASE oe-ord.
            IF inv-misc.ord-no NE 0 THEN
            FIND FIRST oe-ord
                WHERE oe-ord.company EQ inv-misc.company
                  AND oe-ord.ord-no  EQ inv-misc.ord-no
                NO-LOCK NO-ERROR.

            IF AVAIL oe-ord AND v-post THEN DO:
              {oe/closeaud.i oe-ord}
              CREATE w-ord.
              ASSIGN
               w-ord.ord-no = oe-ord.ord-no
               w-ord.rec-id = RECID(oe-ord).
            END.
          END.

          if v-post then do:
            find first oe-ordm
                where oe-ordm.company eq inv-misc.company
                  and oe-ordm.ord-no  eq inv-misc.ord-no
                  and oe-ordm.line    eq inv-misc.line
                  and oe-ordm.charge  eq inv-misc.charge
                no-error.
            if avail oe-ordm then do:
              IF oe-ordm.bill eq "P" THEN oe-ordm.bill = if inv-misc.bill eq "Y" then "I" else "Y".

              FOR EACH reftable
                  WHERE reftable.reftable EQ "oe/ordlmisc.p"
                    AND reftable.company  EQ oe-ordm.company
                    AND reftable.loc      EQ STRING(oe-ordm.ord-no,"9999999999")
                    AND reftable.code     EQ STRING(oe-ordm.line,"9999999999")
                    AND reftable.code2    EQ oe-ordm.charge
                  NO-LOCK:

                IF reftable.val[1] EQ 1 THEN
                FOR EACH est-prep
                    WHERE est-prep.company EQ oe-ordm.company
                      AND est-prep.est-no  EQ oe-ordm.est-no
                      AND est-prep.eqty    EQ reftable.val[2]
                      AND est-prep.line    EQ INT(reftable.val[3])
                      AND est-prep.code    EQ oe-ordm.charge
                      AND est-prep.simon   EQ "S"
                      AND est-prep.amtz    EQ 100:
                  IF oeprep-log THEN DELETE est-prep.
                                ELSE est-prep.simon = "N".
                END.

                /*ELSE
                IF reftable.val[4] GE 1                   AND
                   reftable.val[4] LE EXTENT(ef.mis-cost) THEN
                FOR EACH ef
                    WHERE ef.company EQ oe-ordm.company
                      AND ef.est-no  EQ oe-ordm.est-no
                      AND ef.eqty    EQ reftable.val[2]
                      AND ef.form-no EQ INT(reftable.val[3])
                      AND ef.mis-cost[INT(reftable.val[4])]
                                     EQ oe-ordm.charge
                      AND ef.mis-simon[INT(reftable.val[4])]
                                     EQ "S":

                  ef.mis-simon[INT(reftable.val[4])] = "N".
                END.*/

                LEAVE.
              END.
            end.

            if inv-misc.bill eq "Y" then do:
              run ar/calctax2.p (inv-head.tax-gr, 
                                 no,
                                 inv-misc.amt,
                                 inv-misc.company,
                                 inv-misc.inv-i-no,
                                 output v-tax).
                
              v-reduce-ord-bal = v-reduce-ord-bal + inv-misc.amt +
                                 (if inv-misc.tax then v-tax else 0).
            end.
              
            find first ar-invl
                where ar-invl.x-no eq v-xno
                  and ar-invl.line eq v-xline + 1
                no-lock no-error.
            if not avail ar-invl then create ar-invl.
            {oe/invmpost.i}.

            /* gdm - 09290908 */ RUN get-lot-no.

            if inv-misc.bill ne "Y" then ar-invl.bill = no.
            if v-export eq "Sonoco" then run oe/sonofile.p (3,recid(ar-invl)).
          end. /* v-post */
          
          down with frame invm.
        end. /* each inv-misc */

  /******************* DISCOUNT ITEMS ****************************************/
        ASSIGN
         v-post-disc   = v-post-disc   + v-inv-disc
         v-post-disc-w = v-post-disc-w + v-inv-disc-w.

        IF v-inv-disc NE 0 THEN DO:
          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-disc"
           tt-report.key-02  = string(inv-head.inv-no,"999999")
           tt-report.key-05  = string(v-inv-disc)
           tt-report.weight  = v-inv-disc-w.
        END.

  /******************* TAX ITEMS *********************************************/
        if inv-head.t-inv-tax ne 0 then do:
          if inv-head.tax-gr ne "" then do:
            IF inv-head.multi-invoice THEN DO:
                FOR EACH b-inv-head
                    WHERE b-inv-head.company       EQ inv-head.company
                      AND b-inv-head.cust-no       EQ inv-head.cust-no
                      AND b-inv-head.inv-no        EQ inv-head.inv-no
                      AND b-inv-head.multi-invoice EQ NO:
                   RUN calc-tax-gr (ROWID(b-inv-head), inv-head.inv-no).
                END.

            END.
            ELSE 
              RUN calc-tax-gr (ROWID(inv-head), inv-head.inv-no).

          end.

          else do:
            find first account
                where account.company eq cocode
                  and account.actnum  eq v-ar-stax
                no-lock no-error.
            create tt-report.
            assign
             tt-report.term-id = ""
             tt-report.key-01  = "work-tax"
             tt-report.key-02  = account.actnum
             tt-report.key-03  = string(inv-head.inv-no,"999999")
             tt-report.key-05  = string(inv-head.t-inv-tax *
                                        (IF AVAIL currency  THEN
                                           currency.ex-rate ELSE 1))
             tt-report.weight  = v-line-tot-w.
          end.
        end.

        if not v-post then do:
          ASSIGN
           ld-t[2] = v-line-tot-w / 2000
           ld-t[3] = ld-t[3] + v-line-tot-w
           ld-pton = inv-head.t-inv-rev / ld-t[2].

          IF ld-pton EQ ? THEN ld-pton = 0.

          display inv-head.inv-no inv-head.inv-date
                  inv-head.cust-no inv-head.cust-name v-ord-no
                  v-inv-qty inv-head.t-inv-freight inv-head.t-inv-tax
                  v-misc-tot v-line-tot inv-head.t-inv-rev
                  ld-pton FORMAT "->>>>>>9.999" WHEN tb_ton
                  ld-t[2] WHEN tb_ton
              with frame inv.
          down with fram inv.
          
          if v-detail then do:
            for each w-inv-line break by w-inv-line.ord-no:
              IF tb_ton THEN DO WITH FRAME invlt:
                ASSIGN
                 ld-t[1] = w-inv-line.weight / 2000
                 ld-pton = w-inv-line.t-price / ld-t[1].

                IF ld-pton EQ ? THEN ld-pton = 0.
                v-prof = (w-inv-line.t-price - w-inv-line.t-cost) / w-inv-line.t-price * 100.
                display w-inv-line.i-no w-inv-line.i-name w-inv-line.qty
                        w-inv-line.inv-qty w-inv-line.ship-qty 
                        w-inv-line.price w-inv-line.uom w-inv-line.t-price WHEN w-inv-line.t-price GT 0
                        ld-pton FORMAT "->>>>>>9.999" WHEN tb_ton 
                        ld-t[1] WHEN tb_ton
                        v-prof WHEN v-prof NE ?.
                DOWN.
              END.
              ELSE
              DO WITH FRAME invl:
                v-prof = (w-inv-line.t-price - w-inv-line.t-cost) / w-inv-line.t-price * 100.
                display w-inv-line.i-no w-inv-line.i-name w-inv-line.qty
                        w-inv-line.inv-qty w-inv-line.ship-qty w-inv-line.t-cost
                        w-inv-line.price w-inv-line.uom w-inv-line.t-price WHEN w-inv-line.t-price GT 0
                        v-prof WHEN v-prof NE ?.
                DOWN.
              END.
              delete w-inv-line.
              if last(w-inv-line.ord-no) then
              put skip(1).
            end.

            for each w-ord-misc break by w-ord-misc.ord-no with frame invm:
              if first(w-ord-misc.ord-no) then
              put "Miscellaneous" at 10 skip.
              display w-ord-misc.charge w-ord-misc.dscr w-ord-misc.amt.
              if w-ord-misc.bill eq "N" then
              display "       N/C" @ w-ord-misc.amt.
              down.
              delete w-ord-misc.
              if last(w-ord-misc.ord-no) then
              put skip(1).
            end. /* each w-inv-line */
          end.

          else
            down with frame inv.
        end. /* not v-post */

        ASSIGN
         v-post-total   = v-post-total   + inv-head.t-inv-rev
         v-post-total-w = v-post-total-w + v-line-tot-w.

        IF AVAIL currency THEN DO:
          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-curr"
           tt-report.key-02  = currency.ar-ast-acct
           tt-report.key-05  = STRING(((inv-head.t-inv-rev * currency.ex-rate) -
                                       inv-head.t-inv-rev) * -1).
        END.

        v-tot-frt = 0.
        IF inv-head.multi-invoice THEN
          FOR EACH b-inv-head
              WHERE b-inv-head.company       EQ inv-head.company
                AND b-inv-head.cust-no       EQ inv-head.cust-no
                AND b-inv-head.inv-no        EQ inv-head.inv-no
                AND b-inv-head.multi-invoice EQ NO:
  
            IF b-inv-head.f-bill AND b-inv-head.t-inv-freight NE 0 THEN 
               v-tot-frt = v-tot-frt + b-inv-head.t-inv-freight *
                          (IF AVAIL currency THEN currency.ex-rate ELSE 1).
          END.
        ELSE
          IF inv-head.f-bill THEN
            v-tot-frt = inv-head.t-inv-freight *
                        (IF AVAIL currency THEN currency.ex-rate ELSE 1).
        /** if Freight Is Billable then Post to GL **/
        IF v-tot-frt NE 0 THEN DO:
          ld-temp-amt = v-tot-frt.
          ASSIGN
           v-post-freight   = v-post-freight   - ld-temp-amt
           v-post-freight-w = v-post-freight-w - v-line-tot-w
           v-reduce-ord-bal = v-reduce-ord-bal + v-tot-frt.

          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-freight"
           tt-report.key-02  = STRING(inv-head.inv-no,"999999")
           tt-report.key-05  = STRING(- ld-temp-amt)
           tt-report.weight  = - v-line-tot-w.
        END.

        IF inv-head.terms EQ "CASH" AND inv-head.t-inv-rev NE 0 THEN DO:
          ASSIGN
           v-post-cash    = v-post-cash    + inv-head.t-inv-rev
           v-post-total   = v-post-total   - inv-head.t-inv-rev
           v-post-cash-w  = v-post-cash-w  + v-line-tot-w
           v-post-total-w = v-post-total-w - v-line-tot-w.

          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-cash"
           tt-report.key-02  = STRING(inv-head.inv-no,"999999")
           tt-report.key-05  = STRING(inv-head.t-inv-rev)
           tt-report.weight  = v-line-tot-w.
        END.

        IF v-post THEN DO:
          run oe/invcust.p (recid(inv-head), v-ord-no, tran-date, tran-period).

          if avail ar-inv then do:
            assign
             ar-inv.ord-no   = v-ord-no
             ar-inv.ord-date = v-ord-date.

            run oe/invcost.p (recid(ar-inv)).

            release ar-inv.
          end.

          /* update loadtag status - Bill of lading task#: 10190414 */
          FOR EACH inv-line OF inv-head NO-LOCK,
              EACH oe-boll
              WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.b-no    EQ inv-line.b-no
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                             WHERE oe-bolh.b-no   EQ inv-line.b-no
                               AND oe-bolh.posted EQ YES)
              NO-LOCK:

            IF oe-boll.tag GT "" THEN
              FIND FIRST loadtag EXCLUSIVE-LOCK
                WHERE loadtag.company EQ inv-head.company
                  AND loadtag.item-type EQ NO
                  AND loadtag.i-no    EQ inv-line.i-no
                  AND loadtag.job-no  EQ oe-boll.job-no
                  AND loadtag.job-no2 EQ oe-boll.job-no2
                  AND loadtag.tag-no  EQ oe-boll.tag
                USE-INDEX tag NO-ERROR.
            ELSE IF oe-boll.job-no GT "" THEN
              FIND FIRST loadtag EXCLUSIVE-LOCK
                WHERE loadtag.company EQ inv-head.company
                  AND loadtag.item-type EQ NO
                  AND loadtag.i-no    EQ inv-line.i-no
                  AND loadtag.job-no  EQ oe-boll.job-no
                  AND loadtag.job-no2 EQ oe-boll.job-no2
                  AND loadtag.tag-no  EQ oe-boll.tag
                USE-INDEX job-no NO-ERROR.
            ELSE 
              FIND FIRST loadtag EXCLUSIVE-LOCK
                WHERE loadtag.company EQ inv-head.company
                  AND loadtag.item-type EQ NO
                  AND loadtag.i-no    EQ inv-line.i-no
                  AND loadtag.job-no  EQ oe-boll.job-no
                  AND loadtag.job-no2 EQ oe-boll.job-no2
                  AND loadtag.tag-no  EQ oe-boll.tag
                USE-INDEX i-no NO-ERROR.

            IF AVAIL loadtag THEN
            DO:
               loadtag.sts = "Completed".
               FIND CURRENT loadtag NO-LOCK NO-ERROR.
            END.
          END.

          FOR EACH inv-line WHERE inv-line.r-no EQ inv-head.r-no USE-INDEX r-no
              BREAK BY inv-line.b-no:

            IF LAST-OF(inv-line.b-no) THEN
            FOR EACH oe-boll
                WHERE oe-boll.company EQ inv-line.company
                  AND oe-boll.b-no    EQ inv-line.b-no
                  AND CAN-FIND(FIRST oe-bolh
                               WHERE oe-bolh.b-no   EQ oe-boll.b-no
                                 AND oe-bolh.posted EQ YES)
                NO-LOCK
                BREAK BY oe-boll.r-no:

              IF LAST-OF(oe-boll.r-no) THEN
              FOR EACH oe-rell
                  WHERE oe-rell.company EQ oe-boll.company
                    AND oe-rell.r-no    EQ oe-boll.r-no
                    AND (oe-rell.posted EQ NO OR
                         NOT CAN-FIND(FIRST tmp-oe-boll
                                      WHERE tmp-oe-boll.company  EQ oe-rell.company
                                        AND tmp-oe-boll.r-no     EQ oe-rell.r-no
                                        AND tmp-oe-boll.ord-no   EQ oe-rell.ord-no
                                        AND tmp-oe-boll.i-no     EQ oe-rell.i-no
                                        AND tmp-oe-boll.line     EQ oe-rell.line
                                        AND tmp-oe-boll.rel-no   EQ oe-rell.rel-no
                                        AND tmp-oe-boll.b-ord-no EQ oe-rell.b-ord-no
                                        AND tmp-oe-boll.po-no    EQ oe-rell.po-no
                                     USE-INDEX ord-no))
                  USE-INDEX r-no,

                  FIRST oe-relh
                  WHERE oe-relh.r-no   EQ oe-rell.r-no
                    AND oe-relh.posted EQ YES
                  USE-INDEX r-no NO-LOCK:

                FOR EACH oe-rel
                    WHERE oe-rel.company  EQ oe-rell.company
                      AND oe-rel.link-no  EQ oe-rell.r-no
                      AND oe-rel.ord-no   EQ oe-rell.ord-no
                      AND oe-rel.i-no     EQ oe-rell.i-no
                      AND oe-rel.line     EQ oe-rell.line
                      AND oe-rel.rel-no   EQ oe-rell.rel-no
                      AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                      AND oe-rel.po-no    EQ oe-rell.po-no
                    USE-INDEX link:
                  ASSIGN
                   oe-rel.rel-no   = 0
                   oe-rel.b-ord-no = 0
                   oe-rel.link-no  = 0.
                END.

                DELETE oe-rell.
              END.
            END.

            DELETE inv-line.
          END.

          FOR EACH inv-misc WHERE inv-misc.r-no EQ inv-head.r-no USE-INDEX r-no:
            DELETE inv-misc.
          END.

          IF inv-head.multi-invoice THEN
          FOR EACH b-inv-head
              WHERE b-inv-head.company       EQ inv-head.company
                AND b-inv-head.cust-no       EQ inv-head.cust-no
                AND b-inv-head.inv-no        EQ inv-head.inv-no
                AND b-inv-head.multi-invoice EQ NO:

            DELETE b-inv-head.
          END.

          ELSE DELETE inv-head.

          IF v-print-fmt NE "Fibre" THEN RUN post-gl.
        END. /* v-post */
