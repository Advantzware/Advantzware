     
     for each report
      where report.term-id eq v-term
        and report.key-01  eq ""
        and report.key-02  eq ""
        and report.key-03  eq ""
        and report.key-04  eq ""
        and report.key-05  eq ""
        and report.key-05  eq ""
        and report.key-07  eq ""
        and report.key-08  eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq report.key-09
      no-lock

      transaction:

    if report.key-10 eq "ar-inv" then do:
      find ar-inv where recid(ar-inv) eq report.rec-id no-lock.
      
      run ship-info.

      if v-ship ge fship and
         v-ship le tship and
         v-shpz ge fshpz and
         v-shpz le tshpz then
      for each ar-invl where ar-invl.x-no eq ar-inv.x-no no-lock:
        run report-from-inv. 
      end.

      delete report.
    end.

    else
    if report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      assign
       v-exc         = yes
       report.key-04 = string(ar-cashl.inv-no,"9999999999")
       report.key-05 = cust.sman
       report.key-06 = "0000000000"
       report.key-07 = "MEMO".

      if ar-cashl.dscr matches "*oe return*" then do:
        release ar-inv.
        find first oe-reth
            where oe-reth.company eq cocode
              and oe-reth.r-no    eq int(substr(ar-cashl.dscr,51,12))
            no-lock no-error.
        if avail oe-reth then
        find first ar-inv
             where ar-inv.company eq cocode
               and ar-inv.cust-no eq oe-reth.cust-no
               and ar-inv.inv-no  eq oe-reth.inv-no
             no-lock no-error.
             
        run ship-info.

        if v-ship ge fship and
           v-ship le tship and
           v-shpz ge fshpz and
           v-shpz le tshpz then
        if substr(ar-cashl.dscr,38,5) eq "items" then do:
          release ar-invl.
          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq oe-reth.r-no
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.
          if avail oe-retl then
          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
              no-lock no-error.
          if avail ar-invl then do:
            run report-from-inv.

            delete report.
          end.
        end.

        else
        if substr(ar-cashl.dscr,38,7) eq "freight" and
           cust.sman ge fsman                      and
           cust.sman le tsman                      then
          assign
           v-exc         = no
           report.key-07 = "FREIGHT"
           report.key-08 = v-ship
           report.key-10 = v-shpz.

        else
        if substr(ar-cashl.dscr,38,3) eq "tax" and
           cust.sman ge fsman                  and
           cust.sman le tsman                  then
          assign
           v-exc         = no
           report.key-07 = "TAX"
           report.key-08 = v-ship
           report.key-10 = v-shpz.

        else
        if cust.sman ge fsman and
           cust.sman le tsman then
          assign
           v-exc         = no
           report.key-08 = v-ship
           report.key-10 = v-shpz.
      end.

      else
      if cust.sman    ge fsman and
         cust.sman    le tsman and
         cust.cust-no ge fship and
         cust.cust-no le tship and
         cust.zip     ge fshpz and
         cust.zip     le tshpz then
        assign
         v-exc         = no
         report.key-08 = cust.cust-no
         report.key-10 = cust.zip.
         
      if avail report then do:
        if v-exc then delete report.
      
        else do:
          run get-sort-fields2 (substr(v-sort,1,1), output report.key-01).
          run get-sort-fields2 (substr(v-sort,2,1), output report.key-02).
          run get-sort-fields2 (substr(v-sort,3,1), output report.key-03).
        end.
      end.
    end.
  end.

  for each report where report.term-id eq v-term,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq report.key-09
      no-lock

      break by report.key-01
            by report.key-02
            by report.key-03
            by report.key-04
            by report.key-05
            by report.key-06

      transaction:

    create w-data.
    assign
     w-data.i-no    = report.key-07
     w-data.inv-no  = int(report.key-04)
     w-data.rec-id  = report.rec-id.
     
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
       v-date   = ar-inv.inv-date
       v-ord    = ar-invl.ord-no
       v-pric   = ar-invl.unit-pr
       v-uom    = ar-invl.pr-uom
       v-qty[1] = ar-invl.ship-qty
       v-amt[1] = ar-invl.amt
       v-pct    = 1.

      do i = 1 to 3:
        if ar-invl.sman[i] eq report.key-05 then
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

      v-amt[1] = v-amt[1] * v-pct.
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
         v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc.

        if ar-cashl.dscr matches "*oe return*" then do:
          release oe-retl.
          release ar-invl.

          if substr(ar-cashl.dscr,38,5) eq "items" then
          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq int(substr(ar-cashl.dscr,51,12))
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.
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
            
              /* Added for decimal problem */
              assign v-pric = ar-invl.unit-pr.
              
              do i = 1 to 3:
                if ar-invl.sman[i] eq report.key-05 then
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

              v-amt[1] = v-amt[1] * v-pct.
            end.
          end.
        end.
      end.
    end.

    if v-det then do with frame detail:
      display w-data.i-no
              itemfg.i-name when avail itemfg
                w-data.i-no when not avail itemfg @ itemfg.i-name
              ar-invl.part-dscr1 when avail ar-invl
              ar-invl.part-no when avail ar-invl
              v-pric
              v-uom
              v-qty[1]
              v-amt[1].

      down.
    end.

    assign
     v-qty[2] = v-qty[2] + v-qty[1]
     v-amt[2] = v-amt[2] + v-amt[1].
     
    if last-of(report.key-06) then do with frame summary:
      display cust.cust-no
              cust.name
              report.key-05
              report.key-08
              report.key-10
              w-data.inv-no
              v-date
              v-ord
              v-qty[2]
              v-amt[2].

      down.
      
      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-amt[3] = v-amt[3] + v-amt[2]

       v-qty[2] = 0
       v-amt[2] = 0.
    end.
    
    if last-of(report.key-03) then do:
      if index("CSM",substr(v-sort,3,1)) ne 0 then do with frame summary:
        underline cust.name v-qty[2] v-amt[2].
        
        v-total = entry(index(v-sort-list,substr(v-sort,3,1)),v-sort-desc).

        display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                " TOTALS"   @ cust.name
                v-qty[3]    @ v-qty[2]
                v-amt[3]    @ v-amt[2].

        down.
        put skip(1).
      end.
      
      assign
       v-qty[4] = v-qty[4] + v-qty[3]
       v-amt[4] = v-amt[4] + v-amt[3]

       v-qty[3] = 0
       v-amt[3] = 0.
    end.

    if last-of(report.key-02) then do:
      if index("CSM",substr(v-sort,2,1)) ne 0 then do with frame summary:
        underline cust.name v-qty[2] v-amt[2].
        
        v-total = entry(index(v-sort-list,substr(v-sort,2,1)),v-sort-desc).

        display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                " TOTALS"   @ cust.name
                v-qty[4]    @ v-qty[2]
                v-amt[4]    @ v-amt[2].

        down.
        put skip(1).
      end.

      assign
       v-qty[5] = v-qty[5] + v-qty[4]
       v-amt[5] = v-amt[5] + v-amt[4]

       v-qty[4] = 0
       v-amt[4] = 0.
    end.
    
    if last-of(report.key-01) then do:
      if index("CSM",substr(v-sort,1,1)) ne 0 then do with frame summary:
        underline cust.name v-qty[2] v-amt[2].
        
        v-total = entry(index(v-sort-list,substr(v-sort,1,1)),v-sort-desc).

        display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                " TOTALS"   @ cust.name
                v-qty[5]    @ v-qty[2]
                v-amt[5]    @ v-amt[2].

        down.
        put skip(1).
      end.

      assign
       v-qty[6] = v-qty[6] + v-qty[5]
       v-amt[6] = v-amt[6] + v-amt[5]

       v-qty[5] = 0
       v-amt[5] = 0.
    end.

    if last(report.key-01) then do with frame summary:
      put skip(1).

      underline cust.name v-qty[2] v-amt[2].

      display "                  GRAND TOTALS" @ cust.name
              v-qty[6]                         @ v-qty[2]
              v-amt[6]                         @ v-amt[2].
    end.

    delete w-data.
    delete report.
  end.

/* end ---------------------------------- copr. 2002  advanced software, inc. */
