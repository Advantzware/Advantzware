
  find first ce-ctrl {sys/look/ce-ctrl.w} no-lock.

  find first period
      where period.company eq cocode
        and period.pst     le tdate
        and period.pend    ge tdate
      no-lock no-error.

  lo_trandate = if avail period then minimum(fdate,period.pst) else fdate.

  for each oe-ord
      where oe-ord.company  eq cocode
        and oe-ord.ord-date ge lo_trandate
        and oe-ord.ord-date le tdate
        and oe-ord.type     ne "T"
      no-lock
      by oe-ord.company by oe-ord.ord-date by oe-ord.ord-no:

    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
          AND oe-ordl.is-a-component EQ NO
        no-lock,
          
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-ordl.i-no
        no-lock

        break by oe-ordl.line:
        
      v-exclude = yes.
      do i = 1 to 3:
        if v-exclude               and
           oe-ordl.s-man[i] ge fsman and
           oe-ordl.s-man[i] le tsman then v-exclude = no.
      end. /* do i.. */

      if v-exclude then next.

      /* At this point we have either 1, 2 or 3 valid salesman, in any  */
      /* combination of the array. */
      
      v-misc = false.
      do i = 1 to 3:
        if v-misc then leave.

        if oe-ordl.s-man[i] lt fsman or
           oe-ordl.s-man[i] gt tsman then next.

        /* if no salesman number then assign to misc, ie, blank no */
        if i eq 1               and
           oe-ordl.s-man[1] eq "" and
           oe-ordl.s-man[2] eq "" and
           oe-ordl.s-man[3] eq "" then v-sman = "MISC".

        else   /* if blank salesman # then ignore */
        if oe-ordl.s-man[i] eq "" then next.

        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
        else v-sman = oe-ordl.s-man[i].

        if first(oe-ordl.line) and p-m-chg then do:
          for each oe-ordm
              where oe-ordm.company eq cocode
                and oe-ordm.ord-no  eq oe-ord.ord-no
              no-lock:
                
            if oe-ord.ord-date ge fdate and
               oe-ord.ord-date le tdate then do:
               
              create report.
              assign
               report.term-id = v-term
               report.key-01  = v-sman
               report.key-02  = STRING(oe-ord.ord-no,">>>>>9")
               report.rec-id  = recid(oe-ordm).
            end.
              
            find first wkrecap where wkrecap.procat eq "P/M" no-error.
            if not avail wkrecap then do:
              create wkrecap.
              assign
               wkrecap.procat     = "P/M"
               wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
            end.
            
            else wkrecap.num-of-ord = wkrecap.num-of-ord + 1.

            j = if oe-ord.ord-date ge fdate and
                   oe-ord.ord-date le tdate then 1 else 2.

            k = if oe-ord.ord-date ge period.pst  and
                   oe-ord.ord-date le period.pend then 2 else 1.

            /* We cannot disturb loop variable i from within loop,
               so use ii: */
            if j le k then
            do ii = j to k:
              wkrecap.revenue[ii] = wkrecap.revenue[ii] + oe-ordm.amt.
            end.
          end.
        end.  

        if oe-ord.ord-date ge fdate and
           oe-ord.ord-date le tdate then do:
             
          create report.
          assign
           report.term-id = v-term
           report.key-01  = v-sman
           report.key-02  = STRING(oe-ord.ord-no,">>>>>9")
           report.key-03  = string(i,"9")
           report.rec-id  = recid(oe-ordl).
        end.    /* date in selected period */

        assign
         v-pct  = oe-ordl.s-pct[i] / 100
         v-qty  = oe-ordl.qty * v-pct
         v-sqft = itemfg.t-sqft * v-qty / 1000
         v-amt  = oe-ordl.t-price * v-pct.

        find first wkrecap
            where wkrecap.procat eq if avail itemfg then itemfg.procat else ?
            no-error.
        if not avail wkrecap then do:
          create wkrecap.
          assign
           wkrecap.procat     = if avail itemfg then itemfg.procat else ?
           wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
        end.
        
        else wkrecap.num-of-ord = wkrecap.num-of-ord + 1.

        j = if oe-ord.ord-date ge fdate and
               oe-ord.ord-date le tdate then 1 else 2.

        k = if oe-ord.ord-date ge period.pst  and
               oe-ord.ord-date le period.pend then 2 else 1.

        if j le k then
        do ii = j to k:
          assign
           wkrecap.t-sqft[ii]  = wkrecap.t-sqft[ii] + v-sqft
           wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
        end.
      end. /* do i = 1 to 3... */

      if oe-ord.ord-date ne mdate then do:
        mdate = oe-ord.ord-date.
         
        if oe-ord.ord-date ge fdate and
           oe-ord.ord-date le tdate then
          v-per-days[1] = v-per-days[1] + 1.
          
        if oe-ord.ord-date ge period.pst  and
           oe-ord.ord-date le period.pend then
          v-per-days[2] = v-per-days[2] + 1.
      end.
    end.
  end.  /* for each oe-ord */

  for each report where report.term-id eq v-term
      break by report.key-01 BY report.key-02:
      
    {oe/rep/oe-sman3.i}
    
    find first oe-ordl where recid(oe-ordl) eq report.rec-id no-lock no-error.

    if first-of(report.key-01) then do:
      find first sman
          where sman.company eq cocode
            and sman.sman    eq w-data.sman
          no-lock no-error.
            
      v-sname = if avail sman then sman.sname
                else "* NOT IN SALES REP FILE *".
            
      if first(report.key-01) then do:
        if v-break then display "" with frame r-top-s.
        display "" with frame f-det.
        down 0.
      end.
        
      else
      if v-break then page.
        
      if not v-break then
        put skip
            "Sales Rep: "
            w-data.sman
            " - "
            v-sname
            skip.
    end.

    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq w-data.ord-no
        no-lock no-error.
    find cust of oe-ord no-lock no-error.

    assign
     v-revenue     = w-data.revenue
     v-price-per-m = v-revenue / w-data.t-sqft
     v-profit      = (v-revenue - w-data.cost) / v-revenue * 100.

    if v-price-per-m eq ? then v-price-per-m = 0.
    if v-profit      eq ? then v-profit      = 0.
    
    accumulate
     w-data.t-sqft (total by report.key-01)
     v-revenue     (total by report.key-01)
     w-data.cost   (total by report.key-01).

    if item-dscr then do with no-box frame det1000 down stream-io width 132:
      display oe-ord.due-date when avail oe-ord column-label " !Due!Date"
                                                format "99/99/99"
              w-data.ord-no
              cust.name when avail cust format "x(20)"
              w-data.comm
              w-data.item-n
              w-data.qty
              w-data.t-sqft
              v-price-per-m
              v-revenue
              v-profit when prt-profit.
          
      down.
      
      if last-of(report.key-01) then do:
        assign
         v-price-per-m = (accum total by report.key-01 v-revenue) /
                         (accum total by report.key-01 w-data.t-sqft)
         v-profit      = ((accum total by report.key-01 v-revenue) -
                          (accum total by report.key-01 w-data.cost)) /
                         (accum total by report.key-01 v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-profit      eq ? then v-profit      = 0.

        underline w-data.t-sqft
                  v-revenue
                  v-price-per-m
                  v-profit when prt-profit.

        display "* REP TOTALS *" @ cust.name
                (accum total by report.key-01 w-data.t-sqft) @ w-data.t-sqft
                (accum total by report.key-01 v-revenue) @ v-revenue
                v-price-per-m
                v-profit when prt-profit.
        down 2.
      end.

      if last(report.key-01) then do:
        assign
         v-price-per-m = (accum total v-revenue) /
                         (accum total w-data.t-sqft)
         v-profit      = ((accum total v-revenue) -
                          (accum total w-data.cost)) /
                         (accum total v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-profit      eq ? then v-profit      = 0.

        underline w-data.t-sqft
                  v-revenue
                  v-price-per-m
                  v-profit when prt-profit.

        display "** COMPANY TOTALS **" @ cust.name
                string(v-n-lines,">>>>>>>>>>>>>>>>>>>>>>>>>>9") @ w-data.item-n
                "Line Items" @ w-data.qty
                (accum total w-data.t-sqft) @ w-data.t-sqft
                (accum total v-revenue) @ v-revenue
                v-price-per-m
                v-profit when prt-profit.
        down.
      end.
    end.
      
    else
      if prt-sqft then do with no-box frame det2000 down stream-io width 132:
        display oe-ord.due-date when avail oe-ord column-label " !Due!Date"
                                                  format "99/99/99"
                w-data.ord-no
                cust.name when avail cust format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-profit when prt-profit.
            
        down.
        
        if last-of(report.key-01) then do:
          assign
           v-price-per-m = (accum total by report.key-01 v-revenue) /
                           (accum total by report.key-01 w-data.t-sqft)
           v-profit      = ((accum total by report.key-01 v-revenue) -
                            (accum total by report.key-01 w-data.cost)) /
                           (accum total by report.key-01 v-revenue) * 100.

          if v-price-per-m eq ? then v-price-per-m = 0.
          if v-profit      eq ? then v-profit      = 0.
      
          underline w-data.t-sqft
                    v-price-per-m
                    v-revenue
                    v-profit when prt-profit.

          display "* REP TOTALS *" @ cust.name
                  (accum total by report.key-01 w-data.t-sqft) @ w-data.t-sqft
                  v-price-per-m
                  (accum total by report.key-01 v-revenue) @ v-revenue
                  v-profit when prt-profit.
          down 2.
        end.    /* last-of sman */

        if last(report.key-01) then do:
          assign
           v-price-per-m = (accum total v-revenue) /
                           (accum total w-data.t-sqft)
           v-profit      = ((accum total v-revenue) -
                            (accum total w-data.cost)) /
                           (accum total v-revenue) * 100.

          if v-price-per-m eq ? then v-price-per-m = 0.
          if v-profit      eq ? then v-profit      = 0.
     
          underline w-data.t-sqft
                    v-price-per-m
                    v-revenue
                    v-profit when prt-profit.
                 
          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>9") @ w-data.procat
                  "Line Items" @ w-data.qty
                  (accum total w-data.t-sqft) @ w-data.t-sqft
                  v-price-per-m
                  (accum total v-revenue) @ v-revenue
                  v-profit when prt-profit.
          down.
        end.
      end.
      
      else do with no-box frame det3000 down stream-io width 132.
        display oe-ord.due-date when avail oe-ord column-label " !Due!Date"
                                                  format "99/99/99"
                w-data.ord-no
                cust.name when avail cust format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                oe-ordl.part-no when avail oe-ordl
                                        column-label " !Customer!Part Number"
                v-price-per-m
                w-data.price
                v-revenue
                v-profit when prt-profit.
                        
        down.
        
        if last-of(report.key-01) then do:
          assign
           v-price-per-m = (accum total by report.key-01 v-revenue) /
                           (accum total by report.key-01 w-data.t-sqft)
           v-profit      = ((accum total by report.key-01 v-revenue) -
                            (accum total by report.key-01 w-data.cost)) /
                           (accum total by report.key-01 v-revenue) * 100.

          if v-price-per-m eq ? then v-price-per-m = 0.
          if v-profit      eq ? then v-profit      = 0.
      
          underline v-price-per-m
                    v-revenue
                    v-profit when prt-profit.

          display "* REP TOTALS *" @ cust.name
                  v-price-per-m
                  (accum total by report.key-01 v-revenue) @ v-revenue
                  v-profit when prt-profit.
          down 2.
        end.    /* last-of sman */

        if last(report.key-01) then do:
          assign
           v-price-per-m = (accum total v-revenue) /
                           (accum total w-data.t-sqft)
           v-profit      = ((accum total v-revenue) -
                            (accum total w-data.cost)) /
                           (accum total v-revenue) * 100.

          if v-price-per-m eq ? then v-price-per-m = 0.
          if v-profit      eq ? then v-profit      = 0.
     
          underline v-price-per-m
                    v-revenue
                    v-profit when prt-profit.
                 
          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>9") @ w-data.procat
                  "Line Items" @ w-data.qty
                  (accum total v-revenue) @ v-revenue
                  v-profit when prt-profit.
          down.
        end.
      end.
      
    delete w-data.
    delete report.
  end.

  if v-break then hide frame r-top-s no-pause.

  page.

  for each wkrecap break by wkrecap.procat with frame f-recap:

    do i = 1 to 2:
      wkrecap.price-per-m[i] = wkrecap.revenue[i] / wkrecap.t-sqft[i].
      if wkrecap.price-per-m[i] eq ? then wkrecap.price-per-m[i] = 0.
    end.

    find first fgcat
        where fgcat.company eq cocode
          and fgcat.procat  eq wkrecap.procat
        no-lock no-error.
          
    display wkrecap.procat
            fgcat.dscr when avail fgcat
              "Prep/Misc" when wkrecap.procat eq "P/M" @ procat.dscr
            wkrecap.num-of-ord
            wkrecap.revenue
            wkrecap.t-sqft
            wkrecap.price-per-m
         with frame f-recap.
    down.

    accumulate
     wkrecap.revenue[1] (total)
     wkrecap.t-sqft[1]  (total)
     wkrecap.revenue[2] (total)
     wkrecap.t-sqft[2]  (total).

    if last(wkrecap.procat) then do:
      underline wkrecap.revenue
                wkrecap.t-sqft
                wkrecap.price-per-m.
      down.

      v-msf[1] = (accum total wkrecap.revenue[1]) /
                 (accum total wkrecap.t-sqft[1]).
      v-msf[2] = (accum total wkrecap.revenue[2]) /
                 (accum total wkrecap.t-sqft[2]).

      if v-msf[1] eq ? then v-msf[1] = 0.
      if v-msf[2] eq ? then v-msf[2] = 0.

      display (accum total wkrecap.revenue[1]) @ wkrecap.revenue[1]
              (accum total wkrecap.t-sqft[1])  @ wkrecap.t-sqft[1]
              v-msf[1] @ wkrecap.price-per-m[1]
              (accum total wkrecap.revenue[2]) @ wkrecap.revenue[2]
              (accum total wkrecap.t-sqft[2])  @ wkrecap.t-sqft[2]
              v-msf[2] @ wkrecap.price-per-m[2].
      down.

      display "Number of Days" @ procat.dscr
              v-per-days[1] format ">>>9" @ wkrecap.revenue[1]
              v-per-days[2] format ">>>9" @ wkrecap.revenue[2].
      down.

      underline wkrecap.revenue[1]
                wkrecap.revenue[2].
      down.
      
      display "Average" @ procat.dscr
              ((accum total wkrecap.revenue[1]) / v-per-days[1])
                                            @ wkrecap.revenue[1]
              ((accum total wkrecap.revenue[2]) / v-per-days[2])
                                            @ wkrecap.revenue[2].
      down.
    end.
  end.
