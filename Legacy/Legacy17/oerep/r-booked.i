         
         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-profit
                w-data.t-tons
                v-price-per-t

             with no-box frame det1000 down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-margin
                w-data.t-tons
                v-price-per-t

             with no-box frame det1000m down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-profit
             with no-box frame det1500 down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-margin
             with no-box frame det1500m down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
                w-data.t-tons
                v-price-per-m
             with no-box frame det2000 down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
                w-data.t-tons
                v-price-per-m
             with no-box frame det2000m down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
             with no-box frame det2500 down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
             with no-box frame det2500m down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                oe-ordl.part-no column-label " !Customer!Part Number"
                w-data.item-n
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
                w-data.t-tons
                v-price-per-m
             with no-box frame det3000 down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                oe-ordl.part-no column-label " !Customer!Part Number"
                w-data.item-n
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
                w-data.t-tons
                v-price-per-m
             with no-box frame det3000m down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.item-n
                w-data.qty
                oe-ordl.part-no column-label " !Customer!Part Number"
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
             with no-box frame det3500 down stream-io width 180.

         FORMAT oe-ord.due-date column-label " !Due!Date"
                                format "99/99/99"
                w-data.ord-no
                cust.name       format "x(20)"
                w-data.comm
                w-data.procat
                w-data.item-n
                w-data.qty
                oe-ordl.part-no column-label " !Customer!Part Number"
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
             with no-box frame det3500m down stream-io width 180.

format wkrecap.procat
       fgcat.dscr column-label "Category Description" 
       wkrecap.num-of-ord
       wkrecap.revenue[1]
       wkrecap.t-sqft[1]
       wkrecap.price-per-m[1]
       wkrecap.revenue[2]
       wkrecap.t-sqft[2]
       wkrecap.price-per-m[2]
    header "                                      ----------- Dates Selected" +
           " --------- ---------- Period to Date ----------"
    with frame f-recap down no-box stream-io width 180.

format wkrecap.procat
       fgcat.dscr column-label "Category Description" 
       wkrecap.num-of-ord
       wkrecap.revenue[1]
       wkrecap.t-sqft[1]
       wkrecap.price-per-m[1]
       wkrecap.t-tons[1]
       wkrecap.price-per-t[1]
       wkrecap.revenue[2]
       wkrecap.t-sqft[2]
       wkrecap.price-per-m[2]
       wkrecap.t-tons[2]
       wkrecap.price-per-t[2]
    header "                                      --------------------- Dates Selected" +
           " ------------------- -------------------- Period to Date --------------------"
    with frame f-recap-t down no-box stream-io width 180.


  find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock.

  find first period
      where period.company eq cocode
        and period.pst     le tdate
        and period.pend    ge tdate
      no-lock no-error.

  lo_trandate = if avail period then minimum(fdate,period.pst) else fdate.

  for each oe-ord
      where oe-ord.company  eq cocode
        AND oe-ord.cust-no  GE begin_cust-no
        AND oe-ord.cust-no  LE end_cust-no
        and oe-ord.ord-date ge lo_trandate
        and oe-ord.ord-date le tdate
        and oe-ord.type     ne "T"
        AND oe-ord.stat     NE "D"
      no-lock
      by oe-ord.company by oe-ord.ord-date by oe-ord.ord-no:

     IF tb_exclude-transfer THEN
      DO:

       IF oe-ord.TYPE EQ "T" THEN
          NEXT.

       v-code = "".
        
       FOR EACH oe-rel FIELDS(r-no) WHERE
           oe-rel.company = oe-ord.company AND 
           oe-rel.ord-no  = oe-ord.ord-no
           NO-LOCK,
           FIRST reftable WHERE
                 reftable.reftable EQ "oe-rel.s-code" AND 
                 reftable.company  EQ STRING(oe-rel.r-no,"9999999999") AND
                 reftable.CODE EQ "T"
                 NO-LOCK:
      
                 v-code = "T".
                 LEAVE.
        END.
       
       IF v-code = "T" THEN
          NEXT.
    END.      
    {custom/statusMsg.i "'Processing Order # ' + string(oe-ord.ord-no)"}
    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
          AND (oe-ordl.is-a-component EQ NO OR tb_exclude-set-comps = NO)
        no-lock,
          
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-ordl.i-no
          and itemfg.procat  ge begin_fg-cat
          and itemfg.procat  le end_fg-cat
        no-lock

        break by oe-ordl.line:
        
      v-exclude = yes.
      do i = 1 to 3:
        if v-exclude                 and
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

        if oe-ord.ord-date ge fdate and
           oe-ord.ord-date le tdate then do:
             
          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = v-sman
           tt-report.key-02  = IF tb_sortby THEN
                                 STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
           tt-report.key-03  = string(i,"9")
           tt-report.rec-id  = recid(oe-ordl).           
        end.    /* date in selected period */

        assign
         v-pct  = oe-ordl.s-pct[i] / 100
         v-qty  = oe-ordl.qty * v-pct
         v-sqft = itemfg.t-sqft * v-qty / 1000
         v-tons = itemfg.weight-100 * v-qty / 100 / 2000
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

        k = if AVAIL period AND oe-ord.ord-date ge period.pst  and
               oe-ord.ord-date le period.pend then 2 else 1.

        if j le k then
        do ii = j to k:
          assign
           wkrecap.t-sqft[ii]  = wkrecap.t-sqft[ii] + v-sqft
           wkrecap.t-tons[ii]  = wkrecap.t-tons[ii] + v-tons
           wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
        end.
      end. /* do i = 1 to 3... */

      if oe-ord.ord-date ne mdate then do:
        mdate = oe-ord.ord-date.
         
        if oe-ord.ord-date ge fdate and
           oe-ord.ord-date le tdate then
          v-per-days[1] = v-per-days[1] + 1.
          
        if AVAIL period AND oe-ord.ord-date ge period.pst  and
           oe-ord.ord-date le period.pend then
          v-per-days[2] = v-per-days[2] + 1.
      end.
    end.

    if p-m-chg then 
    for each oe-ordm
        where oe-ordm.company eq cocode
          and oe-ordm.ord-no  eq oe-ord.ord-no
        no-lock:

      v-exclude = yes.
      do i = 1 to 3:
        if v-exclude                 and
           oe-ordm.s-man[i] ge fsman and
           oe-ordm.s-man[i] le tsman then v-exclude = no.
      end. /* do i.. */

      if v-exclude then next.

      /* At this point we have either 1, 2 or 3 valid salesman, in any  */
      /* combination of the array. */
      
      v-misc = false.
      do i = 1 to 3:
        if v-misc then leave.

        if oe-ordm.s-man[i] lt fsman or
           oe-ordm.s-man[i] gt tsman then next.

        /* if no salesman number then assign to misc, ie, blank no */
        if i eq 1                 and
           oe-ordm.s-man[1] eq "" and
           oe-ordm.s-man[2] eq "" and
           oe-ordm.s-man[3] eq "" then v-sman = "MISC".

        else   /* if blank salesman # then ignore */
        if oe-ordm.s-man[i] eq "" then next.

        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
        else v-sman = oe-ordm.s-man[i].

        if oe-ord.ord-date ge fdate and
           oe-ord.ord-date le tdate then do:
               
          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = v-sman
           tt-report.key-02  = IF tb_sortby THEN
                                 STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
           tt-report.key-03  = string(i,"9")
           tt-report.rec-id  = recid(oe-ordm).
        end.

        assign
         v-pct = oe-ordm.s-pct[i] / 100
         v-amt = oe-ordm.amt * v-pct.

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

        k = if AVAIL period AND oe-ord.ord-date ge period.pst  and
               oe-ord.ord-date le period.pend then 2 else 1.

        /* We cannot disturb loop variable i from within loop,
           so use ii: */
        if j le k then
        do ii = j to k:
          wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
        end.
      end.
    end.
  end.  /* for each oe-ord */

  for each tt-report where tt-report.term-id eq ""
      break by tt-report.key-01 by tt-report.key-02:
      
    {oe/rep/oe-sman2.i}
    
    find first oe-ordl where recid(oe-ordl) eq tt-report.rec-id no-lock no-error.

    IF AVAIL oe-ordl THEN
        {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"}

    IF FIRST-OF(tt-report.key-01) THEN DO:
      FIND FIRST sman
          WHERE sman.company eq cocode
            AND sman.sman    eq w-data.sman
          NO-LOCK NO-ERROR.
            
      v-sname = IF AVAIL sman THEN sman.sname
                ELSE "* NOT IN SALES REP FILE *".
            
      IF FIRST(tt-report.key-01) THEN DO:
        VIEW FRAME r-top.
        IF v-break THEN VIEW FRAME r-top1.
        ELSE PAGE.
      END.

      IF v-break THEN PAGE.
        
      ELSE
        PUT SKIP
            "Sales Rep: "
            w-data.sman
            " - "
            v-sname
            SKIP.
    END.

    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq w-data.ord-no
        no-lock no-error.
    find cust of oe-ord no-lock no-error.

    assign
     v-revenue     = w-data.revenue
     v-price-per-m = v-revenue / w-data.t-sqft
     v-price-per-t = v-revenue / w-data.t-tons
     v-profit      = (v-revenue - w-data.cost) / v-revenue * 100
     v-margin      = w-data.margin.

    if v-price-per-m eq ? then v-price-per-m = 0.
    if v-price-per-t eq ? then v-price-per-t = 0.
    if v-profit      eq ? then v-profit      = 0.
    IF v-margin      EQ ? THEN v-margin      = 0.
    
    accumulate
     w-data.t-sqft (total by tt-report.key-01)
     w-data.t-tons (total by tt-report.key-01)
     v-revenue     (total by tt-report.key-01)
     w-data.cost   (total by tt-report.key-01).

/*    if item-dscr then do:
      if tb_ton then do:

        IF tb_margin THEN
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.item-n
                   w-data.qty
                   w-data.t-sqft
                   v-price-per-m
                   v-revenue
                   v-margin
                   w-data.t-tons
                   v-price-per-t
               
               with frame det1000m.
            
           down with frame det1000m.
        END.
        ELSE
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.item-n
                   w-data.qty
                   w-data.t-sqft
                   v-price-per-m
                   v-revenue
                   v-profit when prt-profit
                   w-data.t-tons
                   v-price-per-t
               
               with frame det1000.
            
           down with frame det1000.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-data.sman                                       '",'
              '"' v-sname                                           '",'
              '"' (IF avail oe-ord AND oe-ord.due-date NE ? THEN
                   STRING(oe-ord.due-date) ELSE "")                 '",'
              '"' w-data.ord-no                                     '",'
              '"' (IF avail cust THEN cust.name ELSE "")            '",'
              '"' (IF tb_comm THEN w-data.comm ELSE 0)          '",'
              '"' w-data.item-n                                     '",'
              '"' STRING(w-data.qty,">,>>>,>>>")                    '",'
              '"' STRING(w-data.t-sqft,">>,>>>.999")                '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
              '"' STRING(v-revenue,"->,>>>,>>9.99")                 '",'
              '"' (IF prt-profit THEN STRING(v-profit,"->>,>>9.9")
                   ELSE IF tb_margin THEN STRING(v-margin,"->>,>>9.9") ELSE "") '",'
              '"' STRING(w-data.t-tons,"->,>>>.9")                  '",'
              '"' STRING(v-price-per-t,"->,>>>.99")                 '",'
             SKIP.
      end.

      else do:

        IF tb_margin THEN
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.item-n
                   w-data.qty
                   w-data.t-sqft
                   v-price-per-m
                   v-revenue
                   v-margin
               with frame det1500m.
            
           down with frame det1500m.
        END.
        ELSE
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.item-n
                   w-data.qty
                   w-data.t-sqft
                   v-price-per-m
                   v-revenue
                   v-profit when prt-profit
               with frame det1500.
            
           down with frame det1500.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-data.sman                                       '",'
              '"' v-sname                                           '",'
              '"' (IF avail oe-ord AND oe-ord.due-date NE ? THEN
                   STRING(oe-ord.due-date) ELSE "")                 '",'
              '"' w-data.ord-no                                     '",'
              '"' (IF avail cust THEN cust.name ELSE "")            '",'
              '"' ""                                                '",'
              '"' w-data.item-n                                     '",'
              '"' STRING(w-data.qty,">,>>>,>>>")                    '",'
              '"' STRING(w-data.t-sqft,">>,>>>.999")                '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
              '"' STRING(v-revenue,"->,>>>,>>9.99")                 '",'
              '"' (IF prt-profit THEN STRING(v-profit,"->>,>>9.9")
                   ELSE IF tb_margin THEN STRING(v-margin,"->>,>>9.9") ELSE "") '",'
             SKIP.
      end.
      
      if last-of(tt-report.key-01) then do:
        assign
         v-price-per-m = (accum total by tt-report.key-01 v-revenue) /
                         (accum total by tt-report.key-01 w-data.t-sqft)
         v-price-per-t = (accum total by tt-report.key-01 v-revenue) /
                         (accum total by tt-report.key-01 w-data.t-tons)
         v-profit      = ((accum total by tt-report.key-01 v-revenue) -
                          (accum total by tt-report.key-01 w-data.cost)) /
                         (accum total by tt-report.key-01 v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-price-per-t eq ? then v-price-per-t = 0.
        if v-profit      eq ? then v-profit      = 0.

        if tb_ton then do:
          underline w-data.t-sqft
                    v-revenue
                    v-price-per-m
                    v-profit when prt-profit
                    w-data.t-tons
                    v-price-per-t
              
              with no-box frame det1000.

          display "* REP TOTALS *" @ cust.name
                  (accum total by tt-report.key-01 w-data.t-sqft) @ w-data.t-sqft
                  (accum total by tt-report.key-01 v-revenue) @ v-revenue
                  v-price-per-m
                  v-profit when prt-profit
                  (accum total by tt-report.key-01 w-data.t-tons) @ w-data.t-tons
                  v-price-per-t
              
              with no-box frame det1000.

          down 2 with no-box frame det1000.
        end.

        else do:
          underline w-data.t-sqft
                    v-revenue
                    v-price-per-m
                    v-profit when prt-profit
              
              with no-box frame det1500.

          display "* REP TOTALS *" @ cust.name
                  (accum total by tt-report.key-01 w-data.t-sqft) @ w-data.t-sqft
                  (accum total by tt-report.key-01 v-revenue) @ v-revenue
                  v-price-per-m
              
              with no-box frame det1500.

          down 2 with no-box frame det1500.
        end.
      end.

      if last(tt-report.key-01) then do:
        assign
         v-price-per-m = (accum total v-revenue) /
                         (accum total w-data.t-sqft)
         v-price-per-t = (accum total v-revenue) /
                         (accum total w-data.t-tons)
         v-profit      = ((accum total v-revenue) -
                          (accum total w-data.cost)) /
                         (accum total v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-price-per-t eq ? then v-price-per-t = 0.
        if v-profit      eq ? then v-profit      = 0.

        if tb_ton then do:
          underline w-data.t-sqft
                    v-revenue
                    v-price-per-m
                    v-profit when prt-profit
                    w-data.t-tons
                    v-price-per-t
              
              with no-box frame det1000.

          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>>>>>>>>>>>>>>>>>>>>>>>9") @ w-data.item-n
                  "Line Items" @ w-data.qty
                  (accum total w-data.t-sqft) @ w-data.t-sqft
                  (accum total v-revenue) @ v-revenue
                  v-price-per-m
                  v-profit when prt-profit
                  (accum total w-data.t-tons) @ w-data.t-tons
                  v-price-per-t
              
              with no-box frame det1000.

          down with no-box frame det1000.
        end.

        else do:
          underline w-data.t-sqft
                    v-revenue
                    v-price-per-m
                    v-profit when prt-profit
              
              with no-box frame det1500.

          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>>>>>>>>>>>>>>>>>>>>>>>9") @ w-data.item-n
                  "Line Items" @ w-data.qty
                  (accum total w-data.t-sqft) @ w-data.t-sqft
                  (accum total v-revenue) @ v-revenue
                  v-price-per-m
              
              with no-box frame det1500.

          down with no-box frame det1500.
        end.
      end.
    end.
      
    else */
    if prt-sqft then do:
      if tb_ton then do:

        IF tb_margin THEN
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.qty
                   w-data.sqft
                   w-data.t-sqft
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-margin
                   w-data.t-tons
                   v-price-per-m
               
               with frame det2000m.
               
           down with frame det2000m.
        END.
        ELSE
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.qty
                   w-data.sqft
                   w-data.t-sqft
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-profit when prt-profit
                   w-data.t-tons
                   v-price-per-m
               
               with frame det2000.
               
           down with frame det2000.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-data.sman                                       '",'
              '"' v-sname                                           '",'
              '"' (IF avail oe-ord AND oe-ord.due-date NE ? THEN
                   STRING(oe-ord.due-date) ELSE "")                 '",'
              '"' w-data.ord-no                                     '",'
              '"' (IF avail cust THEN cust.name ELSE "")            '",'
              '"' (IF tb_comm THEN w-data.comm ELSE 0)          '",'
              '"' w-data.procat                                     '",'
              '"' w-data.item-n                                     '",'
              '"' STRING(w-data.qty,">,>>>,>>>")                    '",'
              '"' STRING(w-data.sqft,">>,>>>.999")                  '",'
              '"' STRING(w-data.t-sqft,">>,>>>.999")                '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
              '"' STRING(w-data.price,">>>,>>9.99<<<<")             '",'
              '"' STRING(v-revenue,"->,>>>,>>9.99")                 '",'
              '"' (IF prt-profit THEN STRING(v-profit,"->>,>>9.9")
                   ELSE IF tb_margin THEN STRING(v-margin,"->>,>>9.9") ELSE "") '",'
              '"' STRING(w-data.t-tons,"->,>>>.9")                  '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
             SKIP.
      end.

      else do:

        IF tb_margin THEN
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.qty
                   w-data.sqft
                   w-data.t-sqft
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-margin
               with frame det2500m.
               
           down with frame det2500m.
        END.
        ELSE
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.qty
                   w-data.sqft
                   w-data.t-sqft
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-profit when prt-profit
               with frame det2500.
               
           down with frame det2500.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-data.sman                                       '",'
              '"' v-sname                                           '",'
              '"' (IF avail oe-ord AND oe-ord.due-date NE ? THEN
                   STRING(oe-ord.due-date) ELSE "")                 '",'
              '"' w-data.ord-no                                     '",'
              '"' (IF avail cust THEN cust.name ELSE "")            '",'
              '"' (IF tb_comm THEN w-data.comm ELSE 0)          '",'
              '"' w-data.procat                                     '",'
              '"' w-data.item-n                                     '",'
              '"' STRING(w-data.qty,">,>>>,>>>")                    '",'
              '"' STRING(w-data.sqft,">>,>>>.999")                  '",'
              '"' STRING(w-data.t-sqft,">>,>>>.999")                '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
              '"' STRING(w-data.price,">>>,>>9.99<<<<")             '",'
              '"' STRING(v-revenue,"->,>>>,>>9.99")                 '",'
              '"' (IF prt-profit THEN STRING(v-profit,"->>,>>9.9")
                   ELSE IF tb_margin THEN STRING(v-margin,"->>,>>9.9") ELSE "") '",'
             SKIP.
      end.
        
      if last-of(tt-report.key-01) then do:
        assign
         v-price-per-m = (accum total by tt-report.key-01 v-revenue) /
                         (accum total by tt-report.key-01 w-data.t-sqft)
         v-price-per-t = (accum total by tt-report.key-01 v-revenue) /
                         (accum total by tt-report.key-01 w-data.t-tons)
         v-profit      = ((accum total by tt-report.key-01 v-revenue) -
                          (accum total by tt-report.key-01 w-data.cost)) /
                         (accum total by tt-report.key-01 v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-price-per-t eq ? then v-price-per-t = 0.
        if v-profit      eq ? then v-profit      = 0.
    
        if tb_ton then do:
          underline w-data.t-sqft
                    v-price-per-m
                    v-revenue
                    v-profit when prt-profit
                    w-data.t-tons
                    v-price-per-t
              
              with frame det2000.

          display "* REP TOTALS *" @ cust.name
                  (accum total by tt-report.key-01 w-data.t-sqft) @ w-data.t-sqft
                  v-price-per-m
                  (accum total by tt-report.key-01 v-revenue) @ v-revenue
                  v-profit when prt-profit
                  (accum total by tt-report.key-01 w-data.t-tons) @ w-data.t-tons
                  v-price-per-t
              
              with frame det2000.

          down 2 with frame det2000.
        end.

        else do:
          underline w-data.t-sqft
                    v-price-per-m
                    v-revenue
                    v-profit when prt-profit
              
              with frame det2500.

          display "* REP TOTALS *" @ cust.name
                  (accum total by tt-report.key-01 w-data.t-sqft) @ w-data.t-sqft
                  v-price-per-m
                  (accum total by tt-report.key-01 v-revenue) @ v-revenue
                  v-profit when prt-profit
              
              with frame det2500.

          down 2 with frame det2500.
        end.
      end.    /* last-of sman */

      if last(tt-report.key-01) then do:
        assign
         v-price-per-m = (accum total v-revenue) /
                         (accum total w-data.t-sqft)
         v-price-per-t = (accum total v-revenue) /
                         (accum total w-data.t-tons)
         v-profit      = ((accum total v-revenue) -
                          (accum total w-data.cost)) /
                         (accum total v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-price-per-t eq ? then v-price-per-t = 0.
        if v-profit      eq ? then v-profit      = 0.
     
        if tb_ton then do:
          underline w-data.t-sqft
                    v-price-per-m
                    v-revenue
                    v-profit when prt-profit
                    w-data.t-tons
                    v-price-per-t
              
              with frame det2000.
                
          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>9") @ w-data.procat
                  "Line Items" @ w-data.qty
                  (accum total w-data.t-sqft) @ w-data.t-sqft
                  v-price-per-m
                  (accum total v-revenue) @ v-revenue
                  v-profit when prt-profit
                  (accum total w-data.t-tons) @ w-data.t-tons
                  v-price-per-t
              
              with frame det2000.

          down with frame det2000.
        end.

        else do:
          underline w-data.t-sqft
                    v-price-per-m
                    v-revenue
                    v-profit when prt-profit
              
              with frame det2500.
               
          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>9") @ w-data.procat
                  "Line Items" @ w-data.qty
                  (accum total w-data.t-sqft) @ w-data.t-sqft
                  v-price-per-m
                  (accum total v-revenue) @ v-revenue
                  v-profit when prt-profit
              
              with frame det2500.

          down with frame det2500.
        end.
      end.
    end.
      
    else do: 
      if tb_ton then do:

        IF tb_margin THEN
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust format "x(20)"
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.item-n
                   w-data.qty
                   oe-ordl.part-no when avail oe-ordl
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-margin
                   w-data.t-tons
                   v-price-per-m
               
               with frame det3000m.
                         
           down with frame det3000m.
        END.
        ELSE
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust format "x(20)"
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.item-n
                   w-data.qty
                   oe-ordl.part-no when avail oe-ordl
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-profit when prt-profit
                   w-data.t-tons
                   v-price-per-m
               
               with frame det3000.
                         
           down with frame det3000.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-data.sman                                       '",'
              '"' v-sname                                           '",'
              '"' (IF avail oe-ord AND oe-ord.due-date NE ? THEN
                   STRING(oe-ord.due-date) ELSE "")                 '",'
              '"' w-data.ord-no                                     '",'
              '"' (IF avail cust THEN cust.name ELSE "")            '",'
              '"' (IF tb_comm THEN w-data.comm ELSE 0)          '",'
              '"' w-data.procat                                     '",'
              '"' w-data.item-n                                     '",'
              '"' STRING(w-data.qty,">,>>>,>>>")                    '",'
              '"' (IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE "")   '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
              '"' STRING(w-data.price,">>>,>>9.99<<<<")             '",'
              '"' STRING(v-revenue,"->,>>>,>>9.99")                 '",'
              '"' (IF prt-profit THEN STRING(v-profit,"->>,>>9.9")
                   ELSE IF tb_margin THEN STRING(v-margin,"->>,>>9.9") ELSE "") '",'
              '"' STRING(w-data.t-tons,"->,>>>.9")                  '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
             SKIP.
      end.

      else do:

        IF tb_margin THEN
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.item-n
                   w-data.qty
                   oe-ordl.part-no when avail oe-ordl
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-margin
               with frame det3500m.
                         
           down with frame det3500m.
        END.
        ELSE
        DO:
           display oe-ord.due-date when avail oe-ord
                   w-data.ord-no
                   cust.name when avail cust
                   w-data.comm WHEN tb_comm
                   w-data.procat
                   w-data.item-n
                   w-data.qty
                   oe-ordl.part-no when avail oe-ordl
                   v-price-per-m
                   w-data.price
                   v-revenue
                   v-profit when prt-profit
               with frame det3500.
                         
           down with frame det3500.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-data.sman                                       '",'
              '"' v-sname                                           '",'
              '"' (IF avail oe-ord AND oe-ord.due-date NE ? THEN
                   STRING(oe-ord.due-date) ELSE "")                 '",'
              '"' w-data.ord-no                                     '",'
              '"' (IF avail cust THEN cust.name ELSE "")            '",'
              '"' (IF tb_comm THEN w-data.comm ELSE 0)          '",'
              '"' w-data.procat                                     '",'
              '"' w-data.item-n                                     '",'
              '"' STRING(w-data.qty,">,>>>,>>>")                    '",'
              '"' (IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE "")   '",'
              '"' STRING(v-price-per-m,"->,>>>.99")                 '",'
              '"' STRING(w-data.price,">>>,>>9.99<<<<")             '",'
              '"' STRING(v-revenue,"->,>>>,>>9.99")                 '",'
              '"' (IF prt-profit THEN STRING(v-profit,"->>,>>9.9")
                   ELSE IF tb_margin THEN STRING(v-margin,"->>,>>9.9") ELSE "") '",'
             SKIP.
      end.

      if last-of(tt-report.key-01) then do:
        assign
         v-price-per-m = (accum total by tt-report.key-01 v-revenue) /
                         (accum total by tt-report.key-01 w-data.t-sqft)
         v-price-per-t = (accum total by tt-report.key-01 v-revenue) /
                         (accum total by tt-report.key-01 w-data.t-tons)
         v-profit      = ((accum total by tt-report.key-01 v-revenue) -
                          (accum total by tt-report.key-01 w-data.cost)) /
                         (accum total by tt-report.key-01 v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-price-per-t eq ? then v-price-per-t = 0.
        if v-profit      eq ? then v-profit      = 0.
      
        if tb_ton then do:
          underline v-price-per-m
                    v-revenue
                    v-profit when prt-profit
                    w-data.t-tons
                    v-price-per-t
              
              with frame det3000.

          display "* REP TOTALS *" @ cust.name
                  v-price-per-m
                  (accum total by tt-report.key-01 v-revenue) @ v-revenue
                  v-profit when prt-profit
                  (accum total by tt-report.key-01 w-data.t-tons) @ w-data.t-tons
                  v-price-per-t
              
              with frame det3000.

          down 2 with frame det3000.
        end.

        else do:
          underline v-price-per-m
                    v-revenue
                    v-profit when prt-profit
              
              with frame det3500.

          display "* REP TOTALS *" @ cust.name
                  v-price-per-m
                  (accum total by tt-report.key-01 v-revenue) @ v-revenue
                  v-profit when prt-profit
              
              with frame det3500.

          down 2 with frame det3500.
        end.
      end.    /* last-of sman */

      if last(tt-report.key-01) then do:
        assign
         v-price-per-m = (accum total v-revenue) /
                         (accum total w-data.t-sqft)
         v-price-per-t = (accum total v-revenue) /
                         (accum total w-data.t-tons)
         v-profit      = ((accum total v-revenue) -
                          (accum total w-data.cost)) /
                         (accum total v-revenue) * 100.

        if v-price-per-m eq ? then v-price-per-m = 0.
        if v-price-per-t eq ? then v-price-per-t = 0.
        if v-profit      eq ? then v-profit      = 0.
     
        if tb_ton then do:
          underline v-price-per-m
                    v-revenue
                    v-profit when prt-profit
                    w-data.t-tons
                    v-price-per-t
              
              with frame det3000.
                 
          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>9") @ w-data.procat
                  "Line Items" @ w-data.qty
                  (accum total v-revenue) @ v-revenue
                  v-profit when prt-profit
                  (accum total w-data.t-tons) @ w-data.t-tons
                  v-price-per-t
              
              with frame det3000.

          down with frame det3000.
        end.

        else do:
          underline v-price-per-m
                    v-revenue
                    v-profit when prt-profit
              
              with frame det3500.
                 
          display "** COMPANY TOTALS **" @ cust.name
                  string(v-n-lines,">>>>9") @ w-data.procat
                  "Line Items" @ w-data.qty
                  (accum total v-revenue) @ v-revenue
                  v-profit when prt-profit
              
              with frame det3500.

          down with frame det3500.
        end.
      end.
    end.
      
    delete w-data.
    delete tt-report.
  end.

  if v-break then hide frame r-top1 no-pause.

  page.

  for each wkrecap break by wkrecap.procat:
    do i = 1 to 2:
      assign
       wkrecap.price-per-m[i] = wkrecap.revenue[i] / wkrecap.t-sqft[i]
       wkrecap.price-per-t[i] = wkrecap.revenue[i] / wkrecap.t-tons[i].

      if wkrecap.price-per-m[i] eq ? then wkrecap.price-per-m[i] = 0.
      if wkrecap.price-per-t[i] eq ? then wkrecap.price-per-t[i] = 0.
    end.

    find first fgcat
        where fgcat.company eq cocode
          and fgcat.procat  eq wkrecap.procat
        no-lock no-error.
          
    IF tb_ton THEN DO WITH FRAME f-recap-t:
      display wkrecap.procat
              fgcat.dscr when avail fgcat
                "Prep/Misc" when wkrecap.procat eq "P/M" @ procat.dscr
              wkrecap.num-of-ord
              wkrecap.revenue
              wkrecap.t-sqft
              wkrecap.price-per-m
              wkrecap.t-tons
              wkrecap.price-per-t.
      down.
    END.

    ELSE
    DO WITH FRAME f-recap:
      display wkrecap.procat
              fgcat.dscr when avail fgcat
                "Prep/Misc" when wkrecap.procat eq "P/M" @ procat.dscr
              wkrecap.num-of-ord
              wkrecap.revenue
              wkrecap.t-sqft
              wkrecap.price-per-m.
      down.
    END.

    accumulate
     wkrecap.revenue[1] (total)
     wkrecap.t-sqft[1]  (total)
     wkrecap.t-tons[1]  (total)
     wkrecap.revenue[2] (total)
     wkrecap.t-sqft[2]  (total)
     wkrecap.t-tons[2]  (total).

    if last(wkrecap.procat) then do:
      assign
       v-msf[1] = (accum total wkrecap.revenue[1]) /
                  (accum total wkrecap.t-sqft[1])
       v-msf[2] = (accum total wkrecap.revenue[2]) /
                  (accum total wkrecap.t-sqft[2])
       v-ton[1] = (accum total wkrecap.revenue[1]) /
                  (accum total wkrecap.t-tons[1])
       v-ton[2] = (accum total wkrecap.revenue[2]) /
                  (accum total wkrecap.t-tons[2]).

      if v-msf[1] eq ? then v-msf[1] = 0.
      if v-msf[2] eq ? then v-msf[2] = 0.
      if v-ton[1] eq ? then v-ton[1] = 0.
      if v-ton[2] eq ? then v-ton[2] = 0.

      IF tb_ton THEN DO WITH FRAME f-recap-t:
        underline wkrecap.revenue
                  wkrecap.t-sqft
                  wkrecap.price-per-m
                  wkrecap.t-tons
                  wkrecap.price-per-t.
        down.

        display (accum total wkrecap.revenue[1]) @ wkrecap.revenue[1]
                (accum total wkrecap.t-sqft[1])  @ wkrecap.t-sqft[1]
                v-msf[1] @ wkrecap.price-per-m[1]
                (accum total wkrecap.t-tons[1])  @ wkrecap.t-tons[1]
                v-ton[1] @ wkrecap.price-per-t[1]
                (accum total wkrecap.revenue[2]) @ wkrecap.revenue[2]
                (accum total wkrecap.t-sqft[2])  @ wkrecap.t-sqft[2]
                v-msf[2] @ wkrecap.price-per-m[2]
                (accum total wkrecap.t-tons[2])  @ wkrecap.t-tons[2]
                v-ton[2] @ wkrecap.price-per-t[2].
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
      END.

      ELSE DO WITH FRAME f-recap:
        underline wkrecap.revenue
                  wkrecap.t-sqft
                  wkrecap.price-per-m.
        down.

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
      END.
    end.
  end.
