/*** fg/rep/fg-aging2.i - Calculates v-qohj[] and v-date ****/
/*** Notes:                                             ***/
       
       v-qty = v-qohj[1] + v-qohj[2] + v-qohj[3] +
              v-qohj[4] + v-qohj[5] + v-qohj[6].                  

      IF fg-rcpth.rita-code = "S" THEN DO:
         /* Per Joe, the shipments should be matched against receipts 
            by tag-no, job-no, etc. lv-avail-match will = yes if 
            a match can be made between shipment and a receipt and
            the quantity on the shipment will then be put into the 
            correct column by date range */
         lv-num-matches = 0. v-match-date = ?.
         lv-tag = IF fg-rcpth.rita-code EQ "T" THEN fg-rdtlh.tag2
                                                ELSE fg-rdtlh.tag.
          IF TRIM(lv-tag) EQ "" THEN DO:
            FOR EACH b-f-rc
              where b-f-rc.company    eq cocode
                and b-f-rc.i-no       eq fg-rcpth.i-no
                and b-f-rc.job-no     eq fg-rcpth.job-no
                and b-f-rc.job-no2    eq fg-rcpth.job-no2
                and b-f-rc.rita-code  eq "R"
                AND b-f-rc.job-no     GT ""
                and b-f-rc.trans-date lt v-date
                AND CAN-FIND(FIRST b-f-rd
                             WHERE b-f-rd.r-no      EQ b-f-rc.r-no
                               AND b-f-rd.rita-code EQ b-f-rc.rita-code
                               AND b-f-rd.loc       EQ (IF fg-rcpth.rita-code EQ "T"
                                                        THEN fg-rdtlh.loc2
                                                        ELSE fg-rdtlh.loc)
                               AND b-f-rd.loc-bin   EQ (IF fg-rcpth.rita-code EQ "T"
                                                        THEN fg-rdtlh.loc-bin2
                                                        ELSE fg-rdtlh.loc-bin)
                               AND b-f-rd.tag       EQ lv-tag
                               AND b-f-rd.cust-no   EQ fg-rdtlh.cust-no
                               AND b-f-rd.qty       GT 0
                             USE-INDEX rm-rdtl)
                             NO-LOCK.
                 lv-num-matches = lv-num-matches + 1.
                 IF v-match-date = ? THEN
                 v-match-date = b-f-rc.trans-date.
            END.

          END. /* if tag is blank */
          ELSE DO:
              FOR EACH b-f-rd
                  WHERE b-f-rd.company    EQ cocode
                    AND b-f-rd.tag        EQ lv-tag
                    AND b-f-rd.loc        EQ (IF fg-rcpth.rita-code EQ "T"
                                              THEN fg-rdtlh.loc2
                                              ELSE fg-rdtlh.loc)
                    AND b-f-rd.loc-bin    EQ (IF fg-rcpth.rita-code EQ "T"
                                              THEN fg-rdtlh.loc-bin2
                                              ELSE fg-rdtlh.loc-bin)
                    AND b-f-rd.cust-no    EQ fg-rdtlh.cust-no
                    AND b-f-rd.rita-code  EQ "R"
                    AND b-f-rd.qty        GT 0
                  USE-INDEX tag NO-LOCK,
    
                  FIRST b-f-rc
                  WHERE b-f-rc.r-no       EQ b-f-rd.r-no
                    AND b-f-rd.rita-code  EQ b-f-rc.rita-code
                    AND b-f-rc.i-no       EQ fg-rcpth.i-no
                    AND b-f-rc.job-no     EQ fg-rcpth.job-no
                    AND b-f-rc.job-no2    EQ fg-rcpth.job-no2
                    AND b-f-rc.trans-date LT v-date
                  USE-INDEX r-no NO-LOCK:
                 lv-num-matches = lv-num-matches + 1.
                 IF v-match-date = ? THEN
                 v-match-date = b-f-rc.trans-date.
              END. /* for first b-f-rd */
          END. /* else do */
          IF NOT AVAIL b-f-rc AND lv-num-matches = 0 THEN DO:
            FOR EACH b-f-rc
              where b-f-rc.company    eq cocode
                and b-f-rc.i-no       eq fg-rcpth.i-no
                and b-f-rc.job-no     eq fg-rcpth.job-no
                and b-f-rc.job-no2    eq fg-rcpth.job-no2
                and b-f-rc.rita-code  eq "R"
                AND fg-rcpth.job-no   GT ""
                and b-f-rc.trans-date lt v-date
                AND CAN-FIND(FIRST b-f-rd WHERE b-f-rd.r-no      EQ b-f-rc.r-no
                                            AND b-f-rd.rita-code EQ b-f-rc.rita-code
                                            AND b-f-rd.qty       GT 0)
              NO-LOCK.
              lv-num-matches = lv-num-matches + 1.
              IF v-match-date = ? THEN
              v-match-date = b-f-rc.trans-date.
            END.
          end. /* not avail b-f-rc */
      END. /* rita-code = 's', End block calculating number of matches */

      if fg-rcpth.rita-code eq "R"                                         or
         (index("RAT",fg-rcpth.rita-code) gt 0 AND fg-rdtlh.qty gt 0)      or
         (index("CE",fg-rcpth.rita-code) gt 0 and  v-qty le 0)             or
         (fg-rcpth.rita-code eq "S" AND (fg-rdtlh.qty lt 0 or v-qty eq 0)) or
         (fg-rcpth.rita-code eq "S" AND lv-num-matches = 1)        
          then do:
        /* Note:  lv-num-matches = indicates that a shipment could be 
           matched to a specific receipt.  If that is the case, saves
           v-match-date to use for aging  */
        v-date = fg-rcpth.trans-date.
          
        if index("ATCE",fg-rcpth.rita-code) gt 0             or
           (fg-rcpth.rita-code eq "R" and fg-rdtlh.qty LE 0) then do:

          lv-tag = IF fg-rcpth.rita-code EQ "T" THEN fg-rdtlh.tag2
                                                ELSE fg-rdtlh.tag.

          RELEASE b-f-rc.

          IF TRIM(lv-tag) EQ "" THEN
          for each b-f-rc
              where b-f-rc.company    eq cocode
                and b-f-rc.i-no       eq fg-rcpth.i-no
                and b-f-rc.job-no     eq fg-rcpth.job-no
                and b-f-rc.job-no2    eq fg-rcpth.job-no2
                and b-f-rc.rita-code  eq "R"
                and b-f-rc.trans-date lt v-date
                AND CAN-FIND(FIRST b-f-rd
                             WHERE b-f-rd.r-no      EQ b-f-rc.r-no
                               AND b-f-rd.rita-code EQ b-f-rc.rita-code
                               AND b-f-rd.loc       EQ (IF fg-rcpth.rita-code EQ "T"
                                                        THEN fg-rdtlh.loc2
                                                        ELSE fg-rdtlh.loc)
                               AND b-f-rd.loc-bin   EQ (IF fg-rcpth.rita-code EQ "T"
                                                        THEN fg-rdtlh.loc-bin2
                                                        ELSE fg-rdtlh.loc-bin)
                               AND b-f-rd.tag       EQ lv-tag
                               AND b-f-rd.cust-no   EQ fg-rdtlh.cust-no
                               AND b-f-rd.qty       GT 0
                             USE-INDEX rm-rdtl)
              no-lock

              by b-f-rc.trans-date desc
              by b-f-rc.r-no       desc:

            v-date = b-f-rc.trans-date.
            leave.
          end.

          ELSE
          FOR EACH b-f-rd
              WHERE b-f-rd.company    EQ cocode
                AND b-f-rd.tag        EQ lv-tag
                AND b-f-rd.loc        EQ (IF fg-rcpth.rita-code EQ "T"
                                          THEN fg-rdtlh.loc2
                                          ELSE fg-rdtlh.loc)
                AND b-f-rd.loc-bin    EQ (IF fg-rcpth.rita-code EQ "T"
                                          THEN fg-rdtlh.loc-bin2
                                          ELSE fg-rdtlh.loc-bin)
                AND b-f-rd.cust-no    EQ fg-rdtlh.cust-no
                AND b-f-rd.rita-code  EQ "R"
                AND b-f-rd.qty        GT 0
              USE-INDEX tag NO-LOCK,

              FIRST b-f-rc
              WHERE b-f-rc.r-no       EQ b-f-rd.r-no
                AND b-f-rd.rita-code  EQ b-f-rc.rita-code
                AND b-f-rc.i-no       EQ fg-rcpth.i-no
                AND b-f-rc.job-no     EQ fg-rcpth.job-no
                AND b-f-rc.job-no2    EQ fg-rcpth.job-no2
                AND b-f-rc.trans-date LT v-date
              USE-INDEX r-no NO-LOCK

              BY b-f-rc.trans-date DESC
              BY b-f-rc.r-no       DESC:

            v-date = b-f-rc.trans-date.
            LEAVE.
          END.
              
          IF NOT AVAIL b-f-rc THEN
          for each b-f-rc
              where b-f-rc.company    eq cocode
                and b-f-rc.i-no       eq fg-rcpth.i-no
                and b-f-rc.job-no     eq fg-rcpth.job-no
                and b-f-rc.job-no2    eq fg-rcpth.job-no2
                and b-f-rc.rita-code  eq "R"
                and b-f-rc.trans-date lt v-date
                AND CAN-FIND(FIRST b-f-rd WHERE b-f-rd.r-no      EQ b-f-rc.r-no
                                            AND b-f-rd.rita-code EQ b-f-rc.rita-code
                                            AND b-f-rd.qty       GT 0)
              no-lock
              by b-f-rc.trans-date desc
              by b-f-rc.r-no       desc:
            v-date = b-f-rc.trans-date.
            leave.
          end.
        end.

        /* If type 'S', need to match with the earliest date */
        IF fg-rcpth.rita-code eq "S" AND v-match-date NE ? THEN
          v-date = v-match-date.

        IF v-date GT ld-last THEN do:
            ld-last = v-date.
        END.
        
        IF fg-rcpth.rita-code EQ "C" THEN 
            ASSIGN v-qohj = 0.
        
        RUN which-bucket ((vdat - v-date), OUTPUT v).
        IF v GT 0 AND (v-date GT v-dates[v] OR v-dates[v] = ?) THEN
            v-dates[v] = v-date.

        IF rd_price = "Sell" THEN
        DO:
           release oe-ordl.
             
           FIND FIRST job-hdr WHERE
                job-hdr.company EQ tt-fg-bin.company AND
                job-hdr.job-no  EQ tt-fg-bin.job-no AND
                job-hdr.job-no2 EQ tt-fg-bin.job-no2 AND
                job-hdr.i-no    EQ tt-fg-bin.i-no AND
                job-hdr.ord-no  NE 0
                NO-LOCK NO-ERROR.
          
           IF AVAIL job-hdr THEN
           DO:
              FIND FIRST oe-ordl WHERE
                   oe-ordl.company EQ job-hdr.company AND
                   oe-ordl.ord-no  EQ job-hdr.ord-no AND
                   oe-ordl.i-no    EQ job-hdr.i-no
                   NO-LOCK NO-ERROR.
              RELEASE job-hdr.
           END.
          
           IF NOT AVAIL oe-ordl THEN
              FIND LAST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                    AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                    AND oe-ordl.i-no    EQ fg-rcpth.i-no
                  USE-INDEX item NO-ERROR.
          
           if avail oe-ordl then
              assign
                 v-price  = oe-ordl.t-price / oe-ordl.qty.
           else do:
              lv-case-count = itemfg.case-count.
              if itemfg.sell-uom eq "EA" THEN
                 v-price = itemfg.sell-price.
              ELSE
              IF itemfg.sell-uom = "CS" AND lv-case-count <> 0 THEN
                v-price = itemfg.sell-price / lv-case-count.
              else
                 run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                        itemfg.sell-price, output v-price).
           end.
        END.

        assign
         v         = if v gt 5 then 5 else v
         v-qohj[v] = (fg-rdtlh.qty *
                      if fg-rcpth.rita-code eq "S" then -1 else 1) +
                      v-qohj[v].

        IF rd_price = "Sell" THEN DO:
            /* task - 02171201 - v-qohj is 0 so sell price should be reduced wfk */
           if fg-rcpth.rita-code eq "C" THEN 
               v-sell-price[v] = (v-price * (v-qty *
                            if v-qty LT 0 then -1 else 1))
                           + v-sell-price[v].

           v-sell-price[v] = (v-price * (fg-rdtlh.qty *
                        if fg-rcpth.rita-code eq "S" then -1 else 1))
                       + v-sell-price[v].
        END.

      end.
      
      else
      if index("CE",fg-rcpth.rita-code) gt 0 then do:

        v-qty1 = fg-rdtlh.qty +
                 (if fg-rcpth.rita-code eq "C" then 0 else v-qty).
                 
        IF rd_price = "Sell" THEN
        DO:
           release oe-ordl.
             
           FIND FIRST job-hdr WHERE
                job-hdr.company EQ tt-fg-bin.company AND
                job-hdr.job-no  EQ tt-fg-bin.job-no AND
                job-hdr.job-no2 EQ tt-fg-bin.job-no2 AND
                job-hdr.i-no    EQ tt-fg-bin.i-no AND
                job-hdr.ord-no  NE 0
                NO-LOCK NO-ERROR.
          
           IF AVAIL job-hdr THEN
           DO:
              FIND FIRST oe-ordl WHERE
                   oe-ordl.company EQ job-hdr.company AND
                   oe-ordl.ord-no  EQ job-hdr.ord-no AND
                   oe-ordl.i-no    EQ job-hdr.i-no
                   NO-LOCK NO-ERROR.
              RELEASE job-hdr.
           END.
          
           IF NOT AVAIL oe-ordl THEN
              FIND LAST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                    AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                    AND oe-ordl.i-no    EQ fg-rcpth.i-no
                  USE-INDEX item NO-ERROR.
          
           if avail oe-ordl then
              assign
                 v-price  = oe-ordl.t-price / oe-ordl.qty.
           else do:
            
              if itemfg.sell-uom eq "EA" then
                 v-price = itemfg.sell-price.
              else
                 run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                        itemfg.sell-price, output v-price).
           end.
        END.

        if v-qty1 eq 0 then v-qohj = 0.
        
        if v-qty ne 0 then
        do v = 1 to 6:
          if v-qohj[v] ne 0 then
          DO:
             IF rd_price = "Sell" THEN
             DO:

                IF v-qohj[v] - (round(v-qty1 * (v-qohj[v] / v-qty),0)) GT 0 THEN
                   v-sell-price[v] = v-sell-price[v] - ((v-price * (v-qohj[v] - (round(v-qty1 * (v-qohj[v] / v-qty),0))))).
                ELSE
                   v-sell-price[v] = (v-price * (round(v-qty1 * (v-qohj[v] / v-qty),0) - v-qohj[v]))
                                   + v-sell-price[v].
             END.

             v-qohj[v] = round(v-qty1 * (v-qohj[v] / v-qty),0).

          END.
        end.
        
        v-qty = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                v-qohj[4] + v-qohj[5] + v-qohj[6].
                
        if v-qty ne v-qty1 then loop1:
        do v = 1 to 6:
          if v-qohj[v] ne 0 then do:

            v-qohj[v] = v-qohj[v] + (v-qty1 - v-qty).

            IF rd_price = "Sell" THEN
               v-sell-price[v] = (v-price * (v-qty1 - v-qty))
                           + v-sell-price[v].
            leave loop1.
          end.
        end.
      end.

      else
      if index("SAT",fg-rcpth.rita-code) gt 0 then do:
         
          v-qty = fg-rdtlh.qty * (IF fg-rcpth.rita-code EQ "S" THEN 1 ELSE -1).
         
         IF rd_price = "Sell" THEN
         DO:
            release oe-ordl.
              
            FIND FIRST job-hdr WHERE
                 job-hdr.company EQ tt-fg-bin.company AND
                 job-hdr.job-no  EQ tt-fg-bin.job-no AND
                 job-hdr.job-no2 EQ tt-fg-bin.job-no2 AND
                 job-hdr.i-no    EQ tt-fg-bin.i-no AND
                 job-hdr.ord-no  NE 0
                 NO-LOCK NO-ERROR.
           
            IF AVAIL job-hdr THEN
            DO:
               FIND FIRST oe-ordl WHERE
                    oe-ordl.company EQ job-hdr.company AND
                    oe-ordl.ord-no  EQ job-hdr.ord-no AND
                    oe-ordl.i-no    EQ job-hdr.i-no
                    NO-LOCK NO-ERROR.
               RELEASE job-hdr.
            END.
           
            IF NOT AVAIL oe-ordl THEN
               FIND LAST oe-ordl NO-LOCK
                   WHERE oe-ordl.company EQ cocode
                     AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                     AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                     AND oe-ordl.i-no    EQ fg-rcpth.i-no
                   USE-INDEX item NO-ERROR.
           
            if avail oe-ordl then
               assign
                  v-price  = oe-ordl.t-price / oe-ordl.qty.
            else do:
             
               if itemfg.sell-uom eq "EA" then
                  v-price = itemfg.sell-price.
               else
                  run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                         itemfg.sell-price, output v-price).
            end.
         END.
         
         do v = 5 to 1 by -1:
           if v-qohj[v] gt 0 then
           DO:
               assign
                 v-red     = min(v-qty,v-qohj[v])
                 v-qohj[v] = v-qohj[v] - v-red
                 v-qty     = v-qty     - v-red.

              IF rd_price = "Sell" THEN
                 v-sell-price[v] = v-sell-price[v] - (v-price * v-red).
           END.
              
           if v-qty le 0 then leave.
         end.

         if v-qty gt 0 then v-qohj[6] = v-qohj[6] - v-qty.
      end.

