
      v-qty = v-qohj[1] + v-qohj[2] + v-qohj[3] +
              v-qohj[4] + v-qohj[5] + v-qohj[6].                  

      if fg-rcpth.rita-code eq "R"                  or
         (index("RAT",fg-rcpth.rita-code) gt 0 and
          fg-rdtlh.qty gt 0)                        or
         (index("CE",fg-rcpth.rita-code) gt 0 and
          v-qty le 0)                               or
         (fg-rcpth.rita-code eq "S" and
          (fg-rdtlh.qty lt 0 or v-qty eq 0))        then do:
          
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

        IF v-date GT ld-last THEN ld-last = v-date.

        IF fg-rcpth.rita-code EQ "C" THEN v-qohj = 0.

        IF "{1}" EQ "9999999999" THEN
          RUN which-bucket ((vdat - v-date), OUTPUT v).

        ELSE DO:
          v = TRUNC((vdat - v-date) / {1},0) + 1.

          IF v EQ ? THEN v = 5.
        END.

        assign
         v         = if v gt 5 then 5 else v
         v-qohj[v] = (fg-rdtlh.qty *
                      if fg-rcpth.rita-code eq "S" then -1 else 1) +
                      v-qohj[v].
      end.
      
      else
      if index("CE",fg-rcpth.rita-code) gt 0 then do:
        v-qty1 = fg-rdtlh.qty +
                 (if fg-rcpth.rita-code eq "C" then 0 else v-qty).
                 
        if v-qty1 eq 0 then v-qohj = 0.
        
        if v-qty ne 0 then
        do v = 1 to 6:
          if v-qohj[v] ne 0 then
            v-qohj[v] = round(v-qty1 * (v-qohj[v] / v-qty),0).
        end.
        
        v-qty = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                v-qohj[4] + v-qohj[5] + v-qohj[6].
                
        if v-qty ne v-qty1 then loop1:
        do v = 1 to 6.
          if v-qohj[v] ne 0 then do:
            v-qohj[v] = v-qohj[v] + (v-qty1 - v-qty).
            leave loop1.
          end.
        end.
      end.

      else
      if index("SAT",fg-rcpth.rita-code) gt 0 then do:
        v-qty = fg-rdtlh.qty * (IF fg-rcpth.rita-code EQ "S" THEN 1 ELSE -1).
        
        do v = 5 to 1 by -1:
          if v-qohj[v] gt 0 then
            assign
             v-red     = min(v-qty,v-qohj[v])
             v-qohj[v] = v-qohj[v] - v-red
             v-qty     = v-qty     - v-red.
             
          if v-qty le 0 then leave.
        end.
        
        if v-qty gt 0 then v-qohj[6] = v-qohj[6] - v-qty.
      end.
