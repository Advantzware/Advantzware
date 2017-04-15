  
    v-printlines = 1.

    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock

        break by report.key-01
              by report.key-02
              by report.key-03:

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-boll.ord-no
            and oe-ordl.i-no    eq oe-boll.i-no
            and oe-ordl.line    eq oe-boll.line
          no-lock no-error.

      if v-printlines gt 25 then do:
        page.
        v-printlines = 1.
      end.

      if avail oe-ordl then
      find first oe-ord
          where oe-ord.company eq cocode
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock no-error.

      v-line-tot = v-line-tot + oe-boll.qty.
      
      find first w-cas where w-qty-case eq oe-boll.qty-case no-error.
      if not avail w-cas then create w-cas.
      assign
       w-qty-case = oe-boll.qty-case
       w-cases    = w-cases + oe-boll.cases.
      
      find first w-cas where w-qty-case eq oe-boll.partial no-error.
      if not avail w-cas then create w-cas.
      assign
       w-qty-case = oe-boll.partial
       w-cases    = w-cases + 1.

      if last-of(report.key-02) then do:
        assign
         i        = 0
         j        = 0
         v-job-no = if oe-ordl.job-no eq "" then ""
                    else (trim(oe-ordl.job-no) + "-" +
                          string(oe-ordl.job-no2,"99")).
                          
        for each w-cas break by w-cases * w-qty-case desc:
          j = j + 1.
          if j gt 2 and w-qty-case eq 0 then do:
            delete w-cas.
            j = j - 1.
          end.
        end.
                         
        for each w-cas break by w-cases * w-qty-case desc:
          i = i + 1.          
        
          display {1}
                  oe-boll.i-no          when i eq 1
                    fill(" ",12 -
                         length(trim(string(oe-boll.ord-no,">>>>>>>>")))) +
                    trim(string(oe-boll.ord-no,">>>>>>>>"))
                                        when i eq 2
                                        @ oe-boll.i-no
                  report.key-01         when i eq 1
                                        @ oe-rel.po-no
                    v-job-no            when i eq 2
                                        @ oe-rel.po-no
                  itemfg.i-name         when i eq 1
                    itemfg.part-dscr1   when i eq 2
                                        @ itemfg.i-name
                  w-cases               when w-qty-case ne 0
                                        @ oe-boll.cases
                  w-qty-case            when w-qty-case ne 0
                                        @ oe-boll.qty-case format "->>>>>>"
                  v-line-tot            when i eq j  format "->>>>>>"
                
              with frame ln-s.
          down with frame ln-s.
        
          delete w-cas.

          v-printlines = v-printlines + 1.
        end.
          
        put {1} skip(1).
          
        v-printlines = v-printlines + 1.
        
        v-line-tot = 0.
      end.
      
      v-po-tot = v-po-tot + oe-boll.qty.
      
      if last-of(report.key-01) then do:
        if report.key-01 ne "" then do:
          display {1}
                  "         P.O.#:" @ oe-boll.i-no
                  report.key-01     @ oe-rel.po-no
                  "Total"           @ itemfg.i-name
                  v-po-tot          @ v-line-tot format "->>>>>>"
              with frame ln-s.
          down with frame ln-s.
          
          put {1} skip(1).
          
          v-printlines = v-printlines + 2.
        end.  
        
        v-po-tot = 0.
      end.  

      IF "{1}" EQ "" THEN DELETE report.
    end. /* for each oe-boll */

    v-shiplines = 0.
    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-shiplines = v-shiplines + 1.
    end.

    if (v-shiplines + v-printlines) gt 36 then do:
      page {1}.
      v-printlines = 0.
    end.

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
    end.
    v-printlines = v-printlines + v-shiplines.

    put {1}
        skip(27 - v-printlines)
        "TOTAL CARTONS" at 6  v-tot-pkgs                 to 25
        "TOTAL PALLETS" at 32 v-tot-pal                  to 51
        "NET WEIGHT"    at 58 v-tot-wt  FORMAT "->>>>>>9" to 75 skip.
