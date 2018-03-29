
  EMPTY TEMP-TABLE tt-report.
  EMPTY TEMP-TABLE work-rel.
  EMPTY TEMP-TABLE work-rel-copy.

  FOR EACH inv-head
      WHERE inv-head.company   EQ cocode
        AND ((inv-head.printed EQ YES AND tb_printed) OR
             (inv-head.printed EQ NO  AND tb_unprinted))
        AND inv-head.posted    EQ NO
        AND inv-head.inv-date  GE begin_date
        AND inv-head.inv-date  LE end_date        
      NO-LOCK:

      v-sman-found = NO.

      for each inv-line FIELDS(sman s-pct) where
          inv-line.r-no = inv-head.r-no
          NO-LOCK:

          do i = 1 to 3:
             
             if NOT(inv-line.sman[i] = "" OR
                inv-line.sman[i] lt begin_slsmn or
                inv-line.sman[i] gt end_slsmn) THEN
                v-sman-found = YES.
          END.
      END.

      IF NOT v-sman-found AND 
         NOT CAN-FIND(FIRST inv-line WHERE inv-line.r-no = inv-head.r-no) THEN 
         for each inv-misc where inv-misc.r-no = inv-head.r-no NO-LOCK:
             ASSIGN v-sman-found = YES.
         LEAVE.
      END.      

      IF v-sman-found THEN DO:
         create tt-report.
         ASSIGN tt-report.term-id = ""
                tt-report.key-01  = if v-sort then inv-head.cust-no else ""
                tt-report.key-02  = string(inv-head.bol-no,"9999999999")
                tt-report.rec-id  = recid(inv-head).

      END.
  end.

  for each tt-report where tt-report.term-id eq "",
      first inv-head where recid(inv-head) eq tt-report.rec-id no-lock
      break by tt-report.key-01
            by tt-report.key-02:

    assign
     v-postable = yes
     v-tot-cas = 0
     v-tot-pallets = 0
     v-bol-cases = 0.


    /** CALCULATE toTAL PALLETS **/
    for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no:

      for each oe-bolh no-lock where oe-bolh.b-no = xinv-line.b-no and
          oe-bolh.ord-no = xinv-line.ord-no:
        for each oe-boll no-lock where oe-boll.company = oe-bolh.company and
            oe-boll.b-no = oe-bolh.b-no and
            oe-boll.i-no = xinv-line.i-no:

                                      /** Bill Of Lading toTAL CASES **/
          assign v-bol-cases = v-bol-cases + oe-boll.cases.
        end. /* each oe-boll */
      end. /* each oe-bolh */


        if xinv-line.est-no ne "" then do:
          find first eb where eb.company = xinv-line.company and
            eb.est-no = xinv-line.est-no and
            eb.e-num = xinv-line.e-num and
            eb.form-no = xinv-line.form-no and
            eb.blank-no = xinv-line.blank-no no-lock no-error.

          if xinv-line.form-no   eq 0                             and
             (xinv-line.est-type eq 2 or xinv-line.est-type eq 6) then do:
            for each fg-set no-lock where fg-set.company = xinv-line.company
                and fg-set.set-no = xinv-line.i-no:
              assign v-set-qty = v-set-qty + fg-set.qtyPerSet.
            end.
            if v-set-qty = 0 then
            assign v-set-qty = 1.
            for each eb no-lock where eb.company = xinv-line.company and
                eb.est-no = xinv-line.est-no and
                eb.e-num = xinv-line.e-num and
                eb.form-no ne 0:
              find fg-set where fg-set.company = xinv-line.company and
                fg-set.set-no = xinv-line.i-no  and
                fg-set.part-no = eb.stock-no no-lock no-error.

              if avail fg-set and fg-set.qtyPerSet ne 0 then
              assign v-part-qty = fg-set.qtyPerSet / v-set-qty.
              else
              assign v-part-qty = 1 / v-set-qty.

              IF eb.cas-cnt = 0 then
              assign v-cases = round((xinv-line.t-weight * v-part-qty) /
                eb.cas-wt, 2).
              else
              assign v-cases = round((xinv-line.ship-qty * v-part-qty) /
                eb.cas-cnt, 2).
              if v-bol-cases ne 0 then
               assign v-cases = v-bol-cases.
              assign v-tot-pallets = v-tot-pallets +
                round((v-cases  / eb.cas-pal) + .49, 0).
            end. /* each eb */
          end. /* do */
          else
          if avail eb then
          do:
            IF eb.cas-cnt = 0 then
            assign v-cases = round(xinv-line.t-weight / eb.cas-wt, 2).
            else
            assign v-cases = round(xinv-line.ship-qty / eb.cas-cnt, 2).
              if v-bol-cases ne 0 then
               assign v-cases = v-bol-cases.
            assign v-tot-pallets = v-tot-pallets +
              round((v-cases  / eb.cas-pal) + .49, 0).
          end. /* do */
        end. /* est-no ne "" */
        assign v-tot-weight = v-tot-weight + xinv-line.t-weight
               v-tot-cas = v-tot-cas + v-cases.
    end. /* each xoe-ordl */

    assign v-line-tot = 0
           v-misc-tot = 0
           dfreight =   0
           cfreightCode = "".

    /* Added by FWK 8/11/97 */
    for each inv-line where inv-line.r-no = inv-head.r-no no-lock:
      assign v-line-tot = v-line-tot + inv-line.t-price.
    end.

    for each inv-misc where inv-misc.r-no = inv-head.r-no and
            inv-misc.bill ne "I" no-lock:
      assign v-misc-tot = v-misc-tot + inv-misc.amt.
    end.
    /* Added by FWK 8/11/97 */
    ASSIGN dfreight = IF inv-head.frt-pay NE "p" THEN inv-head.t-inv-freight ELSE 0.
    ASSIGN  cfreightCode = IF inv-head.frt-pay EQ "B" THEN "Bill"
                           ELSE IF inv-head.frt-pay EQ "C" THEN "Collect"
                           ELSE IF inv-head.frt-pay EQ "P" THEN "Prepaid"
                           ELSE "3rd Party".


                                  /** Write header Line **/
    display
      inv-head.cust-no
      lv-dash
      inv-head.cust-name
      inv-head.t-inv-weight
      v-tot-pallets
      v-tot-cas
      cfreightCode
      dfreight
      inv-head.t-inv-tax
      v-misc-tot
      v-line-tot
      inv-head.t-inv-rev
    with frame ord.
    down with frame ord.

    /* gdm - 10130801*/
    IF tb_excel THEN DO:

       IF NOT v-detail THEN 
        PUT STREAM excel UNFORMATTED
          '"' REPLACE(inv-head.cust-no,'"','') + lv-dash + REPLACE(inv-head.cust-name,'"','') '",'
          '"' inv-head.t-inv-weight  '",'
          '"' v-tot-pallets          '",'
          '"' v-tot-cas              '",'
          '"' cfreightCode          '",'
          '"' dfreight '",'
          '"' inv-head.t-inv-tax     '",'
          '"' v-misc-tot             '",'
          '"' v-line-tot             '",'
          '"' inv-head.t-inv-rev     '",' 
         SKIP.
           
      
    END.
   
    ASSIGN
     ld-total-p = ld-total-p + inv-head.t-inv-rev
     ld-total-c = ld-total-c + inv-head.t-inv-cost.

    if inv-head.stat eq "H" then put "*** THIS INVOICE IS IN HOLD ***" skip.

    /************ LINE ITEMS ************************************************/
    /* gdm - 10130810 */
    FIND FIRST inv-line NO-LOCK 
        WHERE inv-line.r-no = inv-head.r-no NO-ERROR.
    IF NOT AVAIL inv-line THEN
       IF tb_excel THEN PUT STREAM excel UNFORMATTED SKIP.

    for each inv-line NO-LOCK where
        inv-line.r-no = inv-head.r-no
        break by inv-line.r-no by inv-line.ord-no:
      IF FIRST(inv-line.r-no) AND v-detail THEN PUT SKIP(1).

      assign 
       v-ext-price    = 0
       v-line-cost    = inv-line.cost * (inv-line.inv-qty / 1000)
       v-line-freight = 0.

      if v-detail then do:                           /** Write Detail Limes **/
        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq inv-line.ord-no
              and oe-ordl.i-no    eq inv-line.i-no
              and oe-ordl.line    eq inv-line.line
            no-lock no-error.

        IF tb_cost THEN DO:
          ld-margin = (inv-line.t-price - v-line-cost) / inv-line.t-price * 100.
          IF ld-margin EQ ? THEN ld-margin = 0.

          display inv-line.ord-no
                  inv-line.po-no @ oe-ordl.po-no
                  inv-line.i-no
                  inv-line.i-name
                  inv-line.qty
                  inv-line.inv-qty
                  inv-line.ship-qty
                  inv-line.price
                  inv-line.pr-uom
                  v-line-cost
                  inv-line.t-price
                  ld-margin
              with frame ordl-c.
          down with frame ordl-c.
        END.

        ELSE DO:
          display inv-line.ord-no
                  oe-ordl.po-no when avail oe-ordl
                    inv-line.po-no when not avail oe-ordl @ oe-ordl.po-no
                  inv-line.i-no
                  inv-line.i-name
                  inv-line.qty
                  inv-line.inv-qty
                  inv-line.ship-qty
                  inv-line.price
                  inv-line.pr-uom
                  inv-line.t-price
              with frame ordl.
          down with frame ordl.
        END.

        /* gdm - 10130810 */
        IF tb_excel THEN DO:

            PUT STREAM excel UNFORMATTED
                '"' REPLACE(inv-head.cust-no,'"','') + lv-dash + 
                    REPLACE(inv-head.cust-name,'"','') '",'
                '"' inv-head.t-inv-weight  '",'
                '"' v-tot-pallets          '",'
                '"' v-tot-cas              '",'
                '"' cfreightCode           '",'
                '"' dfreight '",'
                '"' inv-head.t-inv-tax     '",'
                '"' v-misc-tot             '",'
                '"' v-line-tot             '",'
                '"' inv-head.t-inv-rev     '",' .

            PUT STREAM excel UNFORMATTED
                '"' inv-line.ord-no   '",' 
                '"' inv-line.po-no    '",' 
                '"' inv-line.i-no     '",' 
                '"' inv-line.i-name   '",' 
                '"' inv-line.qty      '",' 
                '"' inv-line.inv-qty  '",' 
                '"' inv-line.ship-qty '",' 
                '"' inv-line.price    '",' 
                '"' inv-line.pr-uom   '",'.

            IF tb_cost THEN 
                PUT STREAM excel UNFORMATTED
                 '"' v-line-cost       '",'
                 '"' inv-line.t-price '",'
                 '"' STRING(ROUND(ld-margin,100)) '",'.              
              ELSE
                 PUT STREAM excel UNFORMATTED
                 '"' inv-line.t-price '",'.
                
        END.

        IF inv-line.stat = "I" or inv-line.stat = "B" then
        for each oe-relh no-lock where oe-relh.company = inv-line.company and
            oe-relh.ord-no = inv-line.ord-no and
            oe-relh.posted use-index relh:
          for each oe-rell no-lock where
              oe-rell.company = oe-relh.company and /* CTS added for speed */
              oe-rell.r-no = oe-relh.r-no and
              oe-rell.ord-no EQ inv-line.ord-no AND
              oe-rell.i-no = inv-line.i-no and
              oe-rell.line = inv-line.line and
              oe-rell.s-code =  "I":

            create work-rel.
            assign
              work-rel.company = oe-relh.company
              work-rel.loc = oe-rell.loc
              work-rel.r-no = oe-relh.r-no
              work-rel.carrier = oe-relh.carrier
              work-rel.cust-no = oe-relh.cust-no
              work-rel.ord-no = oe-relh.ord-no
              work-rel.rel-date = IF oe-relh.rel-date NE ? 
                                     THEN STRING(oe-relh.rel-date,'99/99/99')
                                     ELSE "" 
              work-rel.ship-id = oe-relh.ship-id
              work-rel.ship-i[1] = oe-relh.ship-i[1]
              work-rel.ship-i[2] = oe-relh.ship-i[2]
              work-rel.ship-i[3] = oe-relh.ship-i[3]
              work-rel.ship-i[4] = oe-relh.ship-i[4]
              work-rel.i-no = oe-rell.i-no
              work-rel.line = oe-rell.line
              work-rel.po-no = oe-rell.po-no
              work-rel.qty = oe-rell.qty
              work-rel.posted = oe-rell.posted
              work-rel.printed = oe-relh.printed.

            find first shipto where shipto.company = inv-line.company and
              shipto.cust-no = oe-relh.cust-no and
              shipto.ship-no = oe-relh.ship-no no-lock no-error.
            if avail shipto then
            assign
              work-rel.ship-addr = shipto.ship-addr[1]
              work-rel.ship-city = shipto.ship-city
              work-rel.ship-state = shipto.ship-state
              work-rel.ship-zip = shipto.ship-zip.
          end. /* each oe-rell */
        end. /* each oe-relh */

        if inv-line.stat = "S" or inv-line.stat = "B" then
        for each oe-bolh no-lock where oe-bolh.b-no = inv-line.b-no:

            v-tmp = 0.
            for each oe-boll FIELDS(qty) WHERE
                oe-boll.company = oe-bolh.company AND
                oe-boll.bol-no = oe-bolh.bol-no AND
                oe-boll.i-no = inv-line.i-no AND
                oe-boll.ord-no = inv-line.ord-no
                no-lock:
                v-tmp = v-tmp + oe-boll.qty.
            end.
            
            for each oe-boll no-lock where
                oe-boll.company = oe-bolh.company and
                oe-boll.b-no = oe-bolh.b-no and
                oe-boll.i-no = inv-line.i-no and
                oe-boll.ord-no = inv-line.ord-no AND
                NOT CAN-FIND(FIRST work-rel-copy WHERE
                    work-rel-copy.company EQ oe-bolh.company AND
                    work-rel-copy.i-no EQ oe-boll.i-no AND
                    work-rel-copy.LINE EQ oe-boll.LINE):

                find first work-rel where
                     work-rel.company = oe-bolh.company and
                     work-rel.i-no = oe-boll.i-no and
                     work-rel.line = oe-boll.line and
                     work-rel.loc = oe-boll.loc
                     no-error.

                if not avail work-rel then
                DO:
                  create work-rel.
                  ASSIGN
                     work-rel.qty = oe-boll.qty.
                end.
                else
                ASSIGN
                   work-rel.qty = work-rel.qty + oe-boll.qty.
                
                ASSIGN
                  work-rel.company = oe-bolh.company
                  work-rel.loc = oe-bolh.loc
                  work-rel.r-no = oe-bolh.r-no
                  work-rel.bol-no = oe-bolh.bol-no
                  work-rel.carrier = oe-bolh.carrier
                  work-rel.cust-no = oe-bolh.cust-no
                  work-rel.ord-no = oe-bolh.ord-no
                  work-rel.rel-date = IF oe-bolh.bol-date NE ? 
                                        THEN STRING(oe-bolh.bol-date,'99/99/99')
                                        ELSE ''
                  work-rel.ship-id = oe-bolh.ship-id
                  work-rel.ship-i[1] = oe-bolh.ship-i[1]
                  work-rel.ship-i[2] = oe-bolh.ship-i[2]
                  work-rel.ship-i[3] = oe-bolh.ship-i[3]
                  work-rel.ship-i[4] = oe-bolh.ship-i[4]
                  work-rel.i-no = oe-boll.i-no
                  work-rel.line = oe-boll.line
                  work-rel.po-no = oe-boll.po-no
                  work-rel.posted = oe-boll.posted
                  work-rel.printed = oe-bolh.printed.
                
                  find first oe-ordl
                    where oe-ordl.company = oe-boll.company
                      and oe-ordl.ord-no = oe-bolh.ord-no
                      and oe-ordl.i-no = oe-boll.i-no
                      and oe-ordl.line = oe-boll.line
                  no-lock no-error.
                  find oe-ord where oe-ord.company = oe-boll.company
                    and oe-ord.ord-no = oe-bolh.ord-no
                  no-lock no-error.
                  if available oe-ordl and available oe-ord then
                  work-rel.completed = if oe-ordl.ship-qty + v-tmp >=
                    oe-ordl.qty * (1 - oe-ordl.under-pct * .01) then "C" else "P".
                
                find first shipto where shipto.company = inv-line.company and
                  shipto.cust-no = oe-bolh.cust-no and
                  shipto.ship-no = oe-bolh.ship-no no-lock no-error.
                if avail shipto then
                assign
                  work-rel.ship-addr = shipto.ship-addr[1]
                  work-rel.ship-city = shipto.ship-city
                  work-rel.ship-state = shipto.ship-state
                  work-rel.ship-zip = shipto.ship-zip.

                RELEASE work-rel.
            end. /* each oe-boll */
        end. /* each oe-bolh */

        FOR EACH work-rel:
            CREATE work-rel-copy.
            BUFFER-COPY work-rel TO work-rel-copy.
            RELEASE work-rel-copy.
        END.

        /* gdm - 10130810 */
        IF tb_excel THEN DO:

            FIND FIRST inv-misc NO-LOCK 
                WHERE inv-misc.r-no = inv-line.r-no 
                  AND inv-misc.bill ne "I" NO-ERROR.
            IF AVAIL inv-misc THEN DO:
                IF v-detail THEN DO:
                   ASSIGN
                       v_misc-amt = IF inv-misc.bill = "N" THEN "       N/C" 
                                    ELSE STRING(inv-misc.amt).

                   PUT STREAM excel UNFORMATTED
                           '"' inv-misc.charge '",'
                           '"' inv-misc.dscr   '",'         
                           '"' inv-misc.po-no   '",'        .
                
                   IF tb_cost THEN DO:
                       ld-margin = (inv-misc.amt - inv-misc.cost) / inv-misc.amt * 100.
                       IF ld-margin EQ ? THEN ld-margin = 0.

                       PUT STREAM excel UNFORMATTED
                           '"' inv-misc.cost '",'
                           '"' v_misc-amt    '",'
                           '"'  ld-margin    '",' . 
                  
                   END.                        
                   ELSE DO:
                       PUT STREAM excel UNFORMATTED
                           '"' v_misc-amt      '",'.
                   END.
                END.
            END.
            ELSE DO:
                PUT STREAM excel UNFORMATTED
                            ',,,'.
                IF tb_cost THEN
                   PUT STREAM excel UNFORMATTED
                            ',,,'.
                ELSE 
                   PUT STREAM excel UNFORMATTED
                            ','.
            END.

            FIND LAST work-rel NO-LOCK NO-ERROR.
            IF AVAIL work-rel THEN DO:
            
                PUT STREAM excel UNFORMATTED
                  '"' work-rel.i-no      '",'
                  '"' work-rel.po-no     '",'
                  '"' work-rel.loc       '",'
                  '"' work-rel.rel-date  '",'
                  '"' work-rel.bol-no    '",'
                  '"' work-rel.completed '",'
                  '"' work-rel.r-no      '",'
                  '"' work-rel.carrier   '",'
                  '"' work-rel.ship-id   '",'
                  '"' work-rel.qty       '",'
                  SKIP.
   
            END.
            ELSE DO:
                PUT STREAM excel UNFORMATTED
                  ",,,,,,,,,"
                 SKIP.
            END.
        END.

      end. /* v-detail */

 /******************* MISCELLANEOUS ITEMS ***********************************/
      IF last-of(inv-line.r-no) then
      do:
         for each inv-misc where inv-misc.r-no = inv-head.r-no and
             inv-misc.bill ne "I"
             break by inv-misc.ord-no:
        
           if first(inv-misc.ord-no) and v-detail then
           put skip(1) "Miscellaneous" at 10 skip.
        
           if v-detail then
           do:
             IF tb_cost THEN DO:
               ld-margin = (inv-misc.amt - inv-misc.cost) / inv-misc.amt * 100.
               IF ld-margin EQ ? THEN ld-margin = 0.
        
               display inv-misc.charge inv-misc.dscr inv-misc.po-no inv-misc.cost inv-misc.amt
                       ld-margin
                   with frame ordm-c.
               if inv-misc.bill = "N" then
                 display "       N/C" @ inv-misc.amt with frame ordm-c.
               down with frame ordm-c.
             END.
        
             ELSE DO:
               display inv-misc.charge inv-misc.dscr inv-misc.po-no inv-misc.amt
                   with frame ordm.
               if inv-misc.bill = "N" then
                 display "       N/C" @ inv-misc.amt with frame ordm.
               down with frame ordm.
             END.
           end. /* v-detail */
        
           down with frame ordm.
         end. /* each inv-misc */
        
         if v-detail then do:
         for each work-rel break by work-rel.line by work-rel.i-no:
             if first(work-rel.line) then
                put skip(1).
             display
                work-rel.i-no work-rel.po-no work-rel.loc 
                DATE(work-rel.rel-date) @ work-rel.rel-date
                work-rel.bol-no work-rel.r-no work-rel.carrier work-rel.ship-id
                work-rel.qty work-rel.completed
             with frame rel.
             down with frame rel.
         end. /* each work-rel */
         put skip(1).
         /* gdm 111309*/
         EMPTY TEMP-TABLE work-rel.
         EMPTY TEMP-TABLE work-rel-copy.
        end.
      end. /* last-of(inv-line.r-no) */
    end. /* each oe-ordl */

    IF LAST(tt-report.key-01) THEN DO:
      PUT SKIP(2).

      IF tb_cost THEN DO:
        DISPLAY "Grand Total" @ inv-head.cust-name
                ld-total-c
                ld-total-p    @ inv-head.t-inv-rev
            WITH FRAME ord-c.
        DOWN WITH FRAME ord-c.
      END.

      ELSE DO:
        DISPLAY "Grand Total" @ inv-head.cust-name
                ld-total-p    @ inv-head.t-inv-rev
            WITH FRAME ord.
        DOWN WITH FRAME ord.
      END.
    END.
  end.
