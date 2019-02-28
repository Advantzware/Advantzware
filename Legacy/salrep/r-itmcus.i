  
  For each ar-inv
      where ar-inv.company  eq cocode
        and ar-inv.posted   eq yes
        AND ar-inv.cust-no  GE fcust
        AND ar-inv.cust-no  LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
        AND ttCustList.log-fld no-lock) else true)
        and ar-inv.inv-date ge fdate
        and ar-inv.inv-date le tdate
        and (ar-inv.type    ne "FC" or v-inc-fc)
      no-lock:
       {custom/statusMsg.i " 'Processing Customer#  '  + ar-inv.cust-no "}
    create tt-report.
    assign
     tt-report.key-09  = ar-inv.cust-no
     tt-report.key-10  = "ar-inv"
     tt-report.rec-id  = recid(ar-inv).
  end.

  for each cust
      where cust.company eq cocode
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
        AND ttCustList.log-fld no-lock) else true)
     no-lock,

      each ar-cash
      where ar-cash.company    eq cocode
        and ar-cash.cust-no    eq cust.cust-no
        and ar-cash.check-date ge fdate
        and ar-cash.check-date le tdate
        and ar-cash.posted     eq yes
      no-lock,

      EACH ar-cashl
      WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
                     WHERE account.company EQ ar-cashl.company
                       AND account.actnum  EQ ar-cashl.actnum
                       AND account.type    EQ "R")
      NO-LOCK:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    create tt-report.
    assign
     tt-report.key-09  = ar-cash.cust-no
     tt-report.key-10  = "ar-cashl"
     tt-report.rec-id  = recid(ar-cashl).
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
      no-lock

      transaction:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    if tt-report.key-10 eq "ar-inv" then do:
      find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.
      
      v-ship = if ar-inv.ship-id ne "" then ar-inv.ship-id else
               if ar-inv.sold-id ne "" then ar-inv.sold-id else
                                            ar-inv.cust-no.
            
      if v-ship ge fship and
         v-ship le tship then
      for each ar-invl
          where ar-invl.x-no    eq ar-inv.x-no
            and ar-invl.i-no    ge fitem
            and ar-invl.i-no    le titem
            AND ar-invl.po-no   GE f-po
            AND ar-invl.po-no   LE t-po
            and (ar-invl.billable or not ar-invl.misc)
          no-lock:
          
        do i = 1 to 3:
          v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                      else ar-invl.sman[i].

          if v-sman-no  lt fsman                          or
             v-sman-no  gt tsman                          or
             (i ne 1 and
              (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

          create xtt-report.

          assign
           xtt-report.rec-id  = recid(ar-invl)
           xtt-report.key-01  = tt-report.key-09
           xtt-report.key-02  = if ar-invl.misc then ar-invl.i-name else
                                if ar-invl.i-no ne "" then ar-invl.i-no else
                                "AR SALE"
           xtt-report.key-03  = STRING((YEAR(ar-inv.inv-date) * 10000) +
                                       (MONTH(ar-inv.inv-date) * 100) +
                                       DAY(ar-inv.inv-date),"99999999")
           xtt-report.key-04  = ar-invl.po-no
           xtt-report.key-05  = v-ship
           xtt-report.key-06  = string(ar-invl.inv-no,"999999")
           xtt-report.key-07  = v-sman-no
           xtt-report.key-09  = tt-report.key-09.
         LEAVE.
        end.
      end.

      delete tt-report.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      assign
       v-exc            = yes
       tt-report.key-01 = tt-report.key-09
       tt-report.key-02 = "MEMO"
       tt-report.key-03 = STRING((YEAR(ar-cash.check-date) * 10000) +
                                 (MONTH(ar-cash.check-date) * 100) +
                                 DAY(ar-cash.check-date),"99999999")
       tt-report.key-05 = tt-report.key-09
       tt-report.key-06 = string(ar-cashl.inv-no,"999999")
       tt-report.key-07 = cust.sman.
      
      release ar-inv.
      
      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

      ASSIGN
       lv-r-no = 0
       lv-type = "".
          
      IF AVAIL reftable THEN
        ASSIGN
         lv-r-no = reftable.val[1]
         lv-type = reftable.dscr.
      ELSE
      IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
        ASSIGN
         lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
         lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).
     
      IF lv-r-no NE 0 THEN DO:
        find first oe-reth
            where oe-reth.company eq cocode
              and oe-reth.r-no    eq lv-r-no
            no-lock no-error.
        if avail oe-reth then
        find first ar-inv
             where ar-inv.company eq cocode
               and ar-inv.cust-no eq oe-reth.cust-no
               and ar-inv.inv-no  eq oe-reth.inv-no
             no-lock no-error.
       
        v-ship = IF AVAIL ar-inv then
                   if ar-inv.ship-id ne "" then ar-inv.ship-id else
                   if ar-inv.sold-id ne "" then ar-inv.sold-id else
                                                ar-inv.cust-no
                 ELSE ar-cash.cust-no.
        if v-ship ge fship and
           v-ship le tship then
        if lv-type eq "items" then do:
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
                and (ar-invl.billable or not ar-invl.misc)
                AND ar-invl.po-no   GE f-po
                AND ar-invl.po-no   LE t-po
              no-lock no-error.
          if avail ar-invl then do:
             do i = 1 to 3:
                v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                            else ar-invl.sman[i].
               
                if v-sman-no  lt fsman                          or
                   v-sman-no  gt tsman                          or
                   (i ne 1 and
                    (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
               
                create xtt-report.
               
                assign
                 v-exc              = no
                 xtt-report.rec-id  = recid(ar-cashl)
                 xtt-report.key-01  = tt-report.key-09
                 xtt-report.key-02  = if ar-invl.misc then ar-invl.i-name else
                                      if ar-invl.i-no ne "" then ar-invl.i-no else
                                      "AR SALE"
                 xtt-report.key-03  = STRING((YEAR(ar-inv.inv-date) * 10000) +
                                             (MONTH(ar-inv.inv-date) * 100) +
                                             DAY(ar-inv.inv-date),"99999999")
                 xtt-report.key-04  = ar-invl.po-no
                 xtt-report.key-05  = v-ship       
                 xtt-report.key-06  = tt-report.key-04
                 xtt-report.key-07  = v-sman-no
                 xtt-report.key-09  = tt-report.key-09.
               LEAVE.
             end.
            
             IF AVAIL tt-report THEN delete tt-report.
          end.
        end.

        else
        if lv-type   eq "freight"                  and
           "freight" ge fitem                      and
           "freight" le titem                      and
           cust.sman ge fsman                      and
           cust.sman le tsman                      then
          assign
           v-exc         = no
           tt-report.key-01 = "FREIGHT"
           tt-report.key-05 = v-ship.

        else
        if lv-type   eq "tax"                  and
           "tax"     ge fitem                  and
           "tax"     le titem                  and
           cust.sman ge fsman                  and
           cust.sman le tsman                  then
          assign
           v-exc         = no
           tt-report.key-01 = "TAX"
           tt-report.key-05 = v-ship.

        else
        if ""        ge fitem and
           ""        le titem and
           cust.sman ge fsman and
           cust.sman le tsman then v-exc = no.
      end.

      else
      if ""               ge fitem and
         ""               le titem and
         cust.sman        ge fsman and
         cust.sman        le tsman and
         ar-cashl.cust-no ge fship and
         ar-cashl.cust-no le tship then v-exc = no.
         
      if v-exc AND AVAIL tt-report then delete tt-report.
    end.
  end.

  for each tt-report,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05
            by tt-report.key-06
            by tt-report.key-07

      with frame detail down:
     
    create w-data.
    assign
     w-data.i-no    = tt-report.key-02
     w-data.inv-no  = int(tt-report.key-06)
     w-data.rec-id  = tt-report.rec-id.

    IF FIRST-OF(tt-report.key-02) THEN DO:
      FIND FIRST itemfg
          WHERE itemfg.company eq cocode
            AND itemfg.i-no    eq w-data.i-no
          NO-LOCK NO-ERROR.
      ASSIGN
       v-i-no = w-data.i-no
       v-inam = IF AVAIL itemfg THEN itemfg.i-name ELSE w-data.i-no.
    END.

    IF FIRST-OF(tt-report.key-01) THEN DO:
      HIDE FRAME itemx.
      HIDE FRAME r-top.
      ASSIGN
       v-cust = cust.cust-no
       v-cnam = cust.name
       v-cont = cust.contact
       v-phn# = cust.area-code + cust.phone
       v-fax# = cust.fax.
      IF FIRST(tt-report.key-01) THEN
         DISPLAY SKIP WITH FRAME r-top.
      ELSE DO:
        VIEW FRAME r-top.
        PAGE.
      END.

      IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' "Cust #: " + v-cust                       '",'
             '"' REPLACE(v-cnam,",","")                    '",'
             '"' REPLACE("Contact Name: " + v-cont,",","") '",'
             '"' "Phone: " + v-phn#                        '",'
             '"' "Fax: " + v-fax#                          '",'
             SKIP(1).

         PUT STREAM excel UNFORMATTED
             '"' "FG Item #: " + v-i-no   '",'
             '"' REPLACE(v-inam,",","")   '",'
             SKIP(1).
      END.
    END.

    ELSE
    IF FIRST-OF(tt-report.key-02) AND LINE-COUNTER NE 7 THEN
    DO:
       DISPLAY v-i-no v-inam WITH FRAME itemx.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              SKIP(1)
              '"' "FG Item #: " + v-i-no   '",'
              '"' REPLACE(v-inam,",","")   '",'
              SKIP(1).
    END.

    find first ar-invl
        where recid(ar-invl) eq w-data.rec-id
        no-lock no-error.

    v-ord = 0.
     
    if avail ar-invl then do:
      find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
      assign
       v-date   = ar-inv.inv-date
       v-ord    = ar-invl.ord-no
       v-pric   = ar-invl.unit-pr
       v-uom    = ar-invl.pr-uom
       v-qty[1] = ar-invl.ship-qty
       v-amt[1] = ar-invl.amt
       v-cst[1] = ar-invl.t-cost
       v-pct    = 1.

      do i = 1 to 3:
        if ar-invl.sman[i] eq tt-report.key-06 then
          assign
           v-pct = ar-invl.s-pct[i] / 100
           i     = 3.
      end.

      if v-pct eq 0 then
      do i = 1 to 3:
        if i eq 1 then j = 0.
        if ar-invl.sman[i] ne "" then j = j + 1.
        if i eq 3 then v-pct = 1 / j.
      end.

      if v-pct le 0 or v-pct eq ? then v-pct = 1.

      assign
       v-amt[1] = v-amt[1] * v-pct
       v-cst[1] = v-cst[1] * v-pct.
    end.

    else do:
      find first ar-cashl
          where recid(ar-cashl) eq w-data.rec-id
          no-lock no-error.

      if avail ar-cashl then do:
        find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

        assign
         v-date   = ar-cash.check-date
         v-ord    = 0
         v-pric   = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom    = ""
         v-qty[1] = 0
         v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-cst[1] = 0.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        if avail oe-retl then do:
          assign
           v-ord    = oe-retl.ord-no
           v-pric   = oe-retl.unit-pr
           v-uom    = oe-retl.uom
           v-qty[1] = - oe-retl.tot-qty-return.

          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
              no-lock no-error.

          if avail ar-invl then do:
            v-cst[1] = oe-retl.cost.
          
            /* Added for decimal problem */
            v-pric = ar-invl.unit-pr.
              
            do i = 1 to 3:
              if ar-invl.sman[i] eq tt-report.key-06 then
                assign
                 v-pct = ar-invl.s-pct[i] / 100
                 i     = 3.
            end.

            if v-pct eq 0 then
            do i = 1 to 3:
              if i eq 1 then j = 0.
              if ar-invl.sman[i] ne "" then j = j + 1.
              if i eq 3 then v-pct = 1 / j.
            end.

            if v-pct le 0 or v-pct eq ? then v-pct = 1.

            assign
             v-amt[1] = v-amt[1] * v-pct
             v-cst[1] = v-cst[1] * v-pct.
          end.
        end.
      end.
    end.

    v-qty[6] = 0.

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ v-ord
          AND oe-ordl.i-no    EQ w-data.i-no
          AND v-ord           NE 0
        NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
      v-qty[6] = oe-ordl.qty.
      IF v-cost1 EQ "O" THEN v-cst[1] = oe-ordl.cost * v-qty[1] / 1000.
    END.

    IF v-cst[1] EQ ? THEN v-cst[1] = 0.

    v-prof = (v-amt[1] - v-cst[1]) / v-amt[1] * 100.
    IF v-prof EQ ? THEN v-prof = 0.

    display tt-report.key-04
            tt-report.key-05
            w-data.inv-no
            v-date
            v-ord
            v-uom
            v-qty[6]
            v-qty[1]
            v-pric
            v-prof WHEN ll-secure.
    DOWN.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' tt-report.key-04                  '",'
           '"' tt-report.key-05                  '",'
           '"' STRING(v-date)                    '",' /* btr 01281103 */
           '"' STRING(w-data.inv-no,">>>>>>>>")  '",'
           '"' STRING(v-ord,">>>>>>>>")          '",'
           '"' v-uom                             '",'
           '"' STRING(v-qty[6],"->,>>>,>>>,>>>") '",'
           '"' STRING(v-qty[1],"->,>>>,>>>,>>>") '",'
           '"' STRING(v-pric,"->>>,>>9.99<<")    '",'
           '"' IF ll-secure THEN
                  STRING(v-prof,"->>,>>9.99%")
               ELSE ""                           '",'
           SKIP.

    IF AVAIL oe-ordl                                                      AND
       NOT CAN-FIND(FIRST tt-ordl WHERE tt-ordl.row-id EQ ROWID(oe-ordl)) THEN DO:
      v-qty[7] = v-qty[7] + v-qty[6].
      CREATE tt-ordl.
      tt-ordl.row-id = ROWID(oe-ordl).
    END.

    assign
     v-qty[2] = v-qty[2] + v-qty[1]
     v-amt[2] = v-amt[2] + v-amt[1]
     v-cst[2] = v-cst[2] + v-cst[1].

    if last-of(tt-report.key-05) then do:  
      
      assign
       v-qty[8] = v-qty[8] + v-qty[7]
       v-qty[3] = v-qty[3] + v-qty[2]
       v-amt[3] = v-amt[3] + v-amt[2]
       v-cst[3] = v-cst[3] + v-cst[2]

       v-qty[7] = 0
       v-qty[2] = 0
       v-amt[2] = 0
       v-cst[2] = 0.
    end.

    if last-of(tt-report.key-02) then do:
      
      put skip(2).
      
      assign
       v-qty[9] = v-qty[9] + v-qty[8]
       v-qty[4] = v-qty[4] + v-qty[3]
       v-amt[4] = v-amt[4] + v-amt[3]
       v-cst[4] = v-cst[4] + v-cst[3]

       v-qty[8] = 0
       v-qty[3] = 0
       v-amt[3] = 0
       v-cst[3] = 0.
    end.

    if last-of(tt-report.key-01) then do:
      
      assign
       v-qty[10] = v-qty[10] + v-qty[9]
       v-qty[5]  = v-qty[5]  + v-qty[4]
       v-amt[5]  = v-amt[5]  + v-amt[4]
       v-cst[5]  = v-cst[5]  + v-cst[4]

       v-qty[9] = 0
       v-qty[4] = 0
       v-amt[4] = 0
       v-cst[4] = 0.
    end.

    if last(tt-report.key-01) then do:
      put skip(1).

      underline tt-report.key-04 v-qty[6] v-qty[1] v-prof.

      v-prof = (v-amt[5] - v-cst[5]) / v-amt[5] * 100.
      IF v-prof EQ ? THEN v-prof = 0.

      display "   GRAND TOTALS" @ tt-report.key-04
              v-qty[10] @ v-qty[6]
              v-qty[5]  @ v-qty[1]
              v-prof WHEN ll-secure.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' "GRAND TOTALS"        '",'
             '"' ""                    '",'
             '"' ""                    '",'
             '"' ""                    '",'
             '"' ""                    '",'
             '"' ""                    '",'
             '"' STRING(v-qty[10],"->,>>>,>>>,>>>") '",'
             '"' STRING(v-qty[5],"->,>>>,>>>,>>>")  '",'
             '"' ""                                 '",'
             '"' IF ll-secure THEN
                    STRING(v-prof,"->>,>>9.99%")
                 ELSE ""                            '",'
             SKIP.
    end.

    delete w-data.
  end.
