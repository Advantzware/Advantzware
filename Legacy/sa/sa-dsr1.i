
        break by tt-report.key-01
              by tt-report.key-02

        with frame itemx

        transaction:

      find first w-data no-error.

      if first-of(tt-report.key-02) then do:
        create w-data.
        w-procat = tt-report.key-02.
      end.

      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

        RELEASE itemfg.

        IF ar-invl.i-no NE "" THEN
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq ar-invl.i-no
            no-lock no-error.

        assign
         v-amt  = ar-invl.amt
         v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                  else
                  if avail itemfg then
                    (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

        if v-sqft eq ? then v-sqft = 0.

        assign
         w-ytd-sqft = w-ytd-sqft + v-sqft
         w-ytd-amt  = w-ytd-amt  + v-amt.

        if ar-inv.inv-date ge v-ptd-first and
           ar-inv.inv-date le v-ptd-last  then
          assign
           w-ptd-sqft = w-ptd-sqft + v-sqft
           w-ptd-amt  = w-ptd-amt  + v-amt.

        if tdate eq ar-inv.inv-date then
          assign
           w-sqft = w-sqft + v-sqft
           w-amt  = w-amt  + v-amt.
      end.

      else do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

        if avail ar-cashl then do:
          find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

          assign
           v-amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc)
           w-ytd-amt = w-ytd-amt + v-amt.

          if ar-cash.check-date ge v-ptd-first and
             ar-cash.check-date le v-ptd-last  then
            w-ptd-amt = w-ptd-amt + v-amt.

          if tdate eq ar-cash.check-date then
            w-amt = w-amt + v-amt.

          RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

          IF AVAIL oe-retl THEN DO:

            RELEASE itemfg.

            IF oe-retl.i-no NE "" THEN
            find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no    eq oe-retl.i-no
                no-lock no-error.

            v-sqft = if avail itemfg then
                       (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                     else 0.

            if v-sqft eq ? then v-sqft = 0.

            w-ytd-sqft = w-ytd-sqft - v-sqft.

            if ar-cash.check-date ge v-ptd-first and
               ar-cash.check-date le v-ptd-last  then
              w-ptd-sqft = w-ptd-sqft - v-sqft.

            if tdate eq ar-cash.check-date then
              w-sqft = w-sqft - v-sqft.
          end.
        end.
      end.

      if last-of(tt-report.key-02) then do:
        /* 9507 CAH: following code added to detect and prevent division by a
        number very near 0, which will cause overflow in $/MSF */

        abs-sqft = if w-sqft lt 0 then -1 * w-sqft else w-sqft.
        if abs-sqft lt eps then w-sqft = 0.

        abs-sqft = if w-ptd-sqft lt 0 then -1 * w-ptd-sqft else w-ptd-sqft.
        if abs-sqft lt eps then w-ptd-sqft = 0.

        abs-sqft = if w-ytd-sqft lt 0 then -1 * w-ytd-sqft else w-ytd-sqft.
        if abs-sqft lt eps then w-ytd-sqft = 0.

        assign
         w-msf     = (if w-sqft eq 0 then 0 else w-amt / w-sqft)
         w-ptd-msf = (if w-ptd-sqft eq 0 then 0 else w-ptd-amt / w-ptd-sqft)
         w-ytd-msf = (if w-ytd-sqft eq 0 then 0 else w-ytd-amt / w-ytd-sqft).

        put skip(1).
        
        if v-misc and tt-report.key-01 eq "MISC" then do:         
          
          find first account
              where account.company eq cocode
                and account.actnum  eq tt-report.key-02
              no-lock no-error.
              
          if avail account then w-procat  = w-procat + "  " + account.dscr.
          put w-procat format "x(50)" space(2).
          
          put skip.
         
        end.
        
        display w-procat    when not (v-misc and tt-report.key-01 eq "MISC")
                w-sqft
                w-amt
                w-msf
                w-ptd-sqft
                w-ptd-amt
                w-ptd-msf
                w-ytd-sqft  when v-ytd
                w-ytd-amt   when v-ytd
                w-ytd-msf   when v-ytd
            with frame itemx down.
        down with frame itemx.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' IF not (v-misc and tt-report.key-01 eq "MISC") THEN w-procat
                   ELSE w-procat '",'
               '"' STRING(w-sqft,"->>>,>>9.999")                    '",'
               '"' STRING(w-amt,"->,>>>,>>9.99") '",'
               '"' STRING(w-msf,"->,>>>,>>9.99")                        '",'
               '"' STRING(w-ptd-sqft,"->>>,>>9.999")                    '",'
               '"' STRING(w-ptd-amt,"->,>>>,>>9.99") '",'
               '"' STRING(w-ptd-msf,"->,>>>,>>9.99")                        '",'
               '"' IF v-ytd THEN STRING(w-ytd-sqft,"->>>,>>9.999") 
                   ELSE "" '",'
               '"' IF v-ytd THEN STRING(w-ytd-amt,"->,>>>,>>9.99")
                   ELSE "" '",'
               '"' IF v-ytd THEN STRING(w-ytd-msf,"->,>>>,>>9.99")
                   ELSE "" '",'
               SKIP.

        assign
         v-{1}tot-sqft     = v-{1}tot-sqft     + w-sqft
         v-{1}tot-amt      = v-{1}tot-amt      + w-amt
         v-{1}tot-ptd-sqft = v-{1}tot-ptd-sqft + w-ptd-sqft
         v-{1}tot-ptd-amt  = v-{1}tot-ptd-amt  + w-ptd-amt
         v-{1}tot-ytd-sqft = v-{1}tot-ytd-sqft + w-ytd-sqft
         v-{1}tot-ytd-amt  = v-{1}tot-ytd-amt  + w-ytd-amt.

        delete w-data.
      end.

      if avail tt-report then delete tt-report.
    end.
