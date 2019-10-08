
FOR EACH tt-report NO-LOCK,
    FIRST fg-bin NO-LOCK WHERE RECID(fg-bin) = tt-report.rec-id,
    FIRST itemfg NO-LOCK WHERE itemfg.company = cocode
                   AND itemfg.i-no = fg-bin.i-no
    BREAK BY itemfg.cust-no {1} BY tt-report.rct-date BY itemfg.i-no:

      STATUS DEFAULT "Printing Customer#/RctDate/FG Item#: " +
                     TRIM(itemfg.cust-no) + "/" +
                     TRIM(STRING(tt-report.rct-date)) + "/" +
                     TRIM(itemfg.i-no).    

    v-qty[1] = tt-report.qty.
        
      if first-of(itemfg.cust-no) then
        assign
         v-frst  = yes
         v-print = no.

    find last oe-ordl
            where oe-ordl.company   eq cocode
              and oe-ordl.i-no      eq fg-bin.i-no
              and (oe-ordl.ord-no   eq fg-bin.ord-no or
                   (oe-ordl.job-no  eq fg-bin.job-no and
                    oe-ordl.job-no2 eq fg-bin.job-no2))
            use-index item no-lock no-error.
    if avail oe-ordl THEN
      ASSIGN v-po-no   = oe-ordl.po-no
             v-price   = oe-ordl.price
             v-uom     = oe-ordl.pr-uom
             v-cas-cnt = oe-ordl.cas-cnt.
    ELSE ASSIGN v-po-no   = itemfg.cust-po-no
                v-price   = itemfg.sell-price
                v-uom     = itemfg.sell-uom
                v-cas-cnt = itemfg.case-count.

    if v-uom eq "L" and avail oe-ordl then
            v-ext[1] = v-price / oe-ordl.qty * v-qty[1].
    ELSE if v-uom eq "CS"  AND v-cas-cnt ne 0 then
            v-ext[1] = (v-qty[1] * v-price) / v-cas-cnt.
    else do:
            v-ext[1] = v-qty[1] * v-price.
            find first uom
                where uom.uom  eq v-uom
                  and uom.mult ne 0
                no-lock no-error.
            if avail uom then v-ext[1] = v-ext[1] / uom.mult.
    end.

    v-pallets = 0.

    if v-qty[1] ne 0 or vzer then do:
          v-pallets = v-qty[1] /*fg-bin.qty*/ /
               ((if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)).

          {sys/inc/roundup.i v-pallets}
          v-date = tt-report.tt-date.
          v-rct-date = tt-report.rct-date.
          v-ship-date = tt-report.ship-date.
          if vpcp then do:
            display itemfg.cust-no when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.part-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                    v-ship-date WHEN v-ship-date NE ?
                with frame itemx1.
            down with frame itemx1.
          end.
          else do:
            display itemfg.cust-no when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                    v-ship-date WHEN v-ship-date NE ?
                with frame itemx2.
            down with frame itemx2.
          end.
        
          v-frst = NO.

      IF tb_excel THEN  
            PUT STREAM excel UNFORMATTED
               '"' itemfg.cust-no '",' 
               '"' v-po-no '",'
               '"' itemfg.i-no '",' 
               '"' itemfg.part-no '",'
               '"' itemfg.i-name '",'
               '"' fg-bin.job-no '",'
               '"' fg-bin.job-no2 '",' 
               '"' v-qty[1] '",'
               '"' v-pallets '",'
               '"' v-price '",'
               '"' v-ext[1] '",'
               '"' v-date '",'
               '"' v-rct-date '",'
               '"' IF v-ship-date EQ ? THEN "" ELSE STRING(v-ship-date) '",'
          SKIP.
    END.

    ASSIGN v-qty[2] = v-qty[2] + v-qty[1]
                 v-ext[2] = v-ext[2] + v-ext[1]
                 v-qty[1] = 0
                 v-ext[1] = 0
                 v-print  = yes.
    ACCUMULATE v-pallets (TOTAL BY itemfg.cust-no).
    ACCUMULATE v-pallets (TOTAL).
    if vzer and not v-bin then do:
       if vpcp then do:
          display itemfg.cust-no        when v-frst
                  ""                    @ v-po-no
                  itemfg.i-no
                  itemfg.part-no
                  itemfg.i-name
                  0                     @ v-qty[1]
                  0                     @ v-pallets
                  itemfg.sell-price     @ v-price
                  0                     @ v-ext[1]
              with frame itemx1.
          down with frame itemx1.
       end.
       else do:
          display itemfg.cust-no        when v-frst
                  ""                    @ v-po-no
                  itemfg.i-no
                  itemfg.i-name
                  0                     @ v-qty[1]
                  0                     @ v-pallets
                  itemfg.sell-price     @ v-price
                  0                     @ v-ext[1]
              with frame itemx2.
          down with frame itemx2.
       end.
        
       v-frst = NO.
    end.

    if last-of(itemfg.cust-no) then do:
       if v-print AND (v-qty[2] ne 0 or vzer) then
         IF vpcp THEN
          put "-------" TO 108 "--------------" to 135 skip
              "Customer Total" at 76
              (ACCUM TOTAL BY itemfg.cust-no v-pallets) TO 108 FORM "->>>>>>9" 
              v-ext[2] to 135 format "->>,>>>,>>9.99"
              skip(1).
         ELSE
          put "-------" TO 92 "--------------" to 119 skip
              "Customer Total" at 60
              (ACCUM TOTAL BY itemfg.cust-no v-pallets) TO 92 FORM "->>>>>>9" 
              v-ext[2] to 119 format "->>,>>>,>>9.99"
              skip(1).

        assign
         v-qty[3] = v-qty[3] + v-qty[2]
         v-ext[3] = v-ext[3] + v-ext[2]
         v-qty[2] = 0
         v-ext[2] = 0.
    end.

    if last(itemfg.cust-no) AND (v-qty[3] ne 0 or vzer) THEN
      IF vpcp THEN
       put "-------" TO 108 "--------------" to 135 skip
            "   Grand Total" at 76
            (ACCUM TOTAL v-pallets) TO 108 FORM "->>>>>>9"
            v-ext[3] to 135 format "->>,>>>,>>9.99"
            skip(1).
      ELSE
       put "-------" TO 92 "--------------" to 119 skip
            "   Grand Total" at 60
            (ACCUM TOTAL v-pallets) TO 92 FORM "->>>>>>9"
            v-ext[3] to 119 format "->>,>>>,>>9.99"
            skip(1).
END.
