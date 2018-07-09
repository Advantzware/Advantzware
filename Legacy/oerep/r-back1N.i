
  /*do i = 1 to 3:
    assign
     fstat = tstat
     tstat = if i eq 1 then "C" else
             if i eq 2 then "D" else "Z".*/
    
    for each xoe-ord
        where xoe-ord.company  eq cocode
          and xoe-ord.opened   eq yes
          and xoe-ord.ord-no   ge v-ford-no[1]
          and xoe-ord.ord-no   le v-ford-no[2]
          and xoe-ord.cust-no  ge v-fcust[1]
          and xoe-ord.cust-no  le v-fcust[2]
          and xoe-ord.sman[1]  ge v-fslsm[1]
          and xoe-ord.sman[1]  le v-fslsm[2]
        use-index opened no-lock,

        each oe-ordl
        where oe-ordl.company    eq xoe-ord.company
          and oe-ordl.ord-no     eq xoe-ord.ord-no
          AND oe-ordl.stat       NE "C"
          and oe-ordl.i-no       ge v-fitem[1]
          and oe-ordl.i-no       le v-fitem[2]
          and oe-ordl.req-date   ge v-fdate[1]
          and oe-ordl.req-date   le v-fdate[2] 
        no-lock:
      
      IF NOT(NOT tb_comp OR NOT oe-ordl.is-a-component) THEN NEXT.

      IF v-exclude-transfers THEN
      DO:
         IF xoe-ord.TYPE EQ "T" THEN
            NEXT.

         v-code = "".
        
         FOR EACH oe-rel FIELDS(r-no) WHERE
             oe-rel.company = xoe-ord.company AND 
             oe-rel.ord-no  = xoe-ord.ord-no AND 
             oe-rel.s-code  = "T"
                   NO-LOCK:
        
                   v-code = "T".
                   LEAVE.
         END.
        
         IF v-code = "T" THEN
            NEXT.
      END.

      IF oe-ordl.ship-qty EQ oe-ordl.t-ship-qty THEN
        li-qty[2] = oe-ordl.ship-qty.
      ELSE
        RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT li-qty[1], OUTPUT li-qty[2]).

      IF li-qty[2] LT oe-ordl.qty THEN DO:  
        create tt-report.
        assign
         tt-report.key-01  = (IF v-sort EQ "Item" THEN "        "
                              ELSE if v-sort EQ "Cust" then string(xoe-ord.cust-no,"x(8)")
                              ELSE STRING(xoe-ord.sman[1],"x(8)")) +
                             string(oe-ordl.part-no,"x(20)") +
                             string(oe-ordl.i-no,   "x(20)")
         tt-report.key-02  = trim(oe-ordl.job-no) + "-" +
                             string(oe-ordl.job-no2,"99")
         tt-report.key-03  = oe-ordl.i-no
         tt-report.key-04  = string(xoe-ord.ord-no,"9999999999")
         tt-report.rec-id  = recid(oe-ordl).
      END.
    end.
  /*end.*/
    
  if v-jobs then
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.i-no    ge v-fitem[1]
        and job-hdr.i-no    le v-fitem[2]
        and job-hdr.cust-no ge v-fcust[1]
        and job-hdr.cust-no le v-fcust[2]
        and job-hdr.ord-no  eq 0
      no-lock,
        
      first job
      where job.company    eq cocode
        and job.job        eq job-hdr.job
        and job.job-no     eq job-hdr.job-no
        and job.job-no2    eq job-hdr.job-no2
        and job.start-date ge v-fdate[1]
        and job.start-date le v-fdate[2]
        and job.opened     EQ YES
      no-lock,  
        
      first cust
      where cust.company eq cocode
        and cust.cust-no eq job-hdr.cust-no
        and cust.sman    ge v-fslsm[1]
        and cust.sman    le v-fslsm[2]
      no-lock,
        
      first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock  
        
      break by job-hdr.job
            by job-hdr.job-no
            by job-hdr.job-no2
            by job-hdr.i-no
            
      transaction:

    v-qty[1] = v-qty[1] + job-hdr.qty.
      
    if last-of(job-hdr.i-no) then do:
      for each fg-act
          where fg-act.company eq cocode
            and fg-act.job     eq job-hdr.job
            and fg-act.job-no  eq job-hdr.job-no
            and fg-act.job-no2 eq job-hdr.job-no2
            and fg-act.i-no    eq job-hdr.i-no
          use-index job-idx no-lock:
            
        v-qty[2] = v-qty[2] + fg-act.qty.  
      end.
        
      if v-qty[2] lt v-qty[1] then do:
        create tt-report.
        assign
         tt-report.key-01  = (IF v-sort EQ "Item" THEN "        "
                              ELSE if v-sort EQ "Cust" then string(job-hdr.cust-no,"x(8)")
                              ELSE STRING(cust.sman,"X(8)")) +
                             string(itemfg.part-no,"x(20)") +
                             string(job-hdr.i-no,  "x(20)")
         tt-report.key-02  = (trim(job.job-no) + "-" + string(job.job-no2,"99"))
         tt-report.key-03  = job-hdr.i-no
         tt-report.key-04  = string(v-qty[1] - v-qty[2],"9999999999")
         tt-report.key-05  = job-hdr.cust-no
         tt-report.rec-id  = recid(job).
      end.
        
      v-qty = 0.
    end.      
  end.

  
  for each tt-report
      break by substr(tt-report.key-01,1,8)
            by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            
      transaction:

       {custom/statusMsg.i "'Processing Order # ' + string(oe-ord.ord-no)"} 

    if first-of(substr(tt-report.key-01,1,8)) and v-sort NE "Item" then do:

      IF v-sort EQ "Cust" THEN
      DO: 
         find first cust
              where cust.company eq cocode
              and cust.cust-no eq substr(tt-report.key-01,1,8)
              no-lock no-error.
         v-name = if avail cust then cust.name else "Not on File".
      END.
      ELSE
      DO: 
         FIND FIRST sman WHERE
              sman.company EQ cocode AND
              sman.sman EQ substr(tt-report.key-01,1,3)
              NO-LOCK NO-ERROR.
         v-name-rep = IF AVAIL sman THEN sman.sname ELSE "Not on File".
      END. 
           
      if first(substr(tt-report.key-01,1,8)) then do:
       /* display with frame r-top-1.
        display with frame r-top-2.*/
      end. 
      
      else page.
    end.

    v-get-qty = (v-fg-qty     and first-of(tt-report.key-01)) or
                (not v-fg-qty and first-of(tt-report.key-02)).

    {oe/rep/backlog.i}
    
    IF AVAIL w-ord THEN
    DO:

        {custom/statusMsg.i "'Processing Order # ' + string(w-ord.ord-no)"} 

       v-uom = if w-ord.uom ne "" then caps(w-ord.uom) else "EA".
       
       /* if v-ponum then do with frame ordhead-po:
          DISPLAY w-ord.due-date
                  w-ord.ord-date
                  w-ord.ord-no
                  w-ord.po-num
                  w-ord.part-no
                  w-ord.pallets  when v-get-qty
                  w-ord.qty-onh  when v-get-qty
                  w-ord.qty-due
                  w-ord.price    when v-priceflag
                  v-uom          when v-priceflag
                  w-ord.t-price  when v-priceflag.
          down.
        end.
       
        else do with frame ordhead:
          display w-ord.due-date
                  w-ord.ord-date
                  w-ord.ord-no
                  w-ord.part-no
                  w-ord.pallets when v-get-qty
                  w-ord.qty-onh when v-get-qty
                  w-ord.qty-due
                  w-ord.price   when v-priceflag
                  v-uom         when v-priceflag
                  w-ord.t-price when v-priceflag.
          down.
        end.
       
        IF tb_excel THEN DO:
        
          IF v-ponum THEN
            po-num-excel = '"' + w-ord.po-num + '",'.
       
          PUT STREAM excel UNFORMATTED
              '"' (IF w-ord.due-date NE ? THEN STRING(w-ord.due-date)
                   ELSE "")                                                 '",'
              '"' (IF w-ord.ord-date NE ? THEN STRING(w-ord.ord-date)
                   ELSE "")                                                 '",'
              '"' w-ord.ord-no                                              '",'
                  po-num-excel 
              '"' w-ord.part-no                                             '",'
              '"' (IF v-get-qty THEN STRING(w-ord.pallets)
                   ELSE "")                                                 '",'
              '"' (IF v-get-qty THEN STRING(w-ord.qty-onh,"->,>>>,>>9.9<<<")
                   ELSE "")                                                 '",'
              '"' STRING(w-ord.qty-due,"->,>>>,>>9")                        '",'
              '"' (IF v-priceflag THEN STRING(w-ord.price,">>>,>>9.99<<<")
                      + v-uom ELSE "")                                      '",'
              '"' (IF v-priceflag THEN STRING(w-ord.t-price,">>,>>>,>>9.99")
                   ELSE "")                                                 '",'
              SKIP.
        END. */
          

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
      
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "due-dt"   THEN cVarValue = IF w-ord.due-date NE ? THEN STRING(w-ord.due-date) ELSE "" . 
                     WHEN "ord-dt"   THEN cVarValue = IF w-ord.ord-date NE ? THEN STRING(w-ord.ord-date) ELSE "". 
                     WHEN "ord"      THEN cVarValue = STRING(w-ord.ord-no) .
                     WHEN "po-no"    THEN cVarValue = STRING(w-ord.po-num,"x(15)") .
                     WHEN "cust-prt" THEN cVarValue = string(w-ord.part-no,"x(15)") .
                     WHEN "palt"     THEN cVarValue = IF v-get-qty THEN STRING(w-ord.pallets) ELSE ""  .
                     WHEN "qty-oh"   THEN cVarValue = IF v-get-qty THEN STRING(w-ord.qty-onh,"->,>>>,>>9.9<<<") ELSE "" .
                     WHEN "qty-due"  THEN cVarValue = STRING(w-ord.qty-due,"->,>>>,>>9") .
                     WHEN "price"    THEN cVarValue = STRING(w-ord.price,">>>,>>9.99<<<")  .
                     WHEN "sals"     THEN cVarValue = IF v-priceflag AND w-ord.t-price NE ? THEN STRING(w-ord.t-price,">>,>>>,>>9.99") ELSE "" .
                     WHEN "uom"      THEN cVarValue = STRING(v-uom) .
                     WHEN "cus-name" THEN cVarValue = STRING(v-name)  .
                     WHEN "rep-name" THEN cVarValue = STRING(v-name-rep) .
                     WHEN "rel-type" THEN cVarValue = STRING(w-ord.rel-type,"x(12)") .
                         
                     
                END CASE.
                  
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
      
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                   cExcelDisplay SKIP.
         END.    
       
        assign
         v-tot-qty[1]   = v-tot-qty[1] + w-ord.qty-due
         v-tot-cost[1]  = v-tot-cost[1] + w-ord.cost .
         IF w-ord.t-price NE ? THEN
             ASSIGN v-tot-sales[1] = v-tot-sales[1] + w-ord.t-price.
    END.  /*avail w-ord*/
    
    if last-of(tt-report.key-01) then do:
      if v-sub-item then do:
        if not first-of(tt-report.key-01) then do:
          if line-counter + 2 gt page-size then page.
        
          /*if v-ponum then do with frame ordhead-po:
            underline w-ord.qty-due
                      w-ord.t-price   when v-priceflag.
            down.
        
            display "   PART TOTALS:" @ w-ord.part-no
                    v-tot-qty[1]      @ w-ord.qty-due
                    v-tot-sales[1]    when v-priceflag
                                      @ w-ord.t-price.
            down.
          end.

          else do with frame ordhead:
            underline w-ord.qty-due
                      w-ord.t-price   when v-priceflag.
            down.
                      
            display "   PART TOTALS:" @ w-ord.part-no
                    v-tot-qty[1]      @ w-ord.qty-due
                    v-tot-sales[1]    when v-priceflag
                                      @ w-ord.t-price.
            down.
          end.*/
          PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                 cTmpField = ""
                 cVarValue = ""
                 cExcelDisplay = ""
                 cExcelVarValue = "".
      
          DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  CASE cTmpField:             
                       WHEN "due-dt"   THEN cVarValue = "" . 
                       WHEN "ord-dt"   THEN cVarValue = "". 
                       WHEN "ord"      THEN cVarValue = "" .
                       WHEN "po-no"    THEN cVarValue = "".
                       WHEN "cust-prt" THEN cVarValue = "" .
                       WHEN "palt"     THEN cVarValue = ""  .
                       WHEN "qty-oh"   THEN cVarValue = "" .
                       WHEN "qty-due"  THEN cVarValue = STRING(v-tot-qty[1],"->,>>>,>>9") .
                       WHEN "price"    THEN cVarValue = ""  .
                       WHEN "sals"     THEN cVarValue = IF v-priceflag THEN STRING(v-tot-sales[1],">>,>>>,>>9.99") ELSE "" .
                       WHEN "uom"      THEN cVarValue = "" .
                       WHEN "cus-name" THEN cVarValue = ""  .
                       WHEN "rep-name" THEN cVarValue = "" .
                       WHEN "rel-type" THEN cVarValue = "" .
                  END CASE.
                    
                  cExcelVarValue = cVarValue.
                  cDisplay = cDisplay + cVarValue +
                             FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                  cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          END.
         
          PUT UNFORMATTED "   PART TOTALS:" substring(cDisplay,16,300) SKIP.

        end.
        
        put skip(1).
      end.
      
      assign
       v-tot-qty[2]   = v-tot-qty[2] + v-tot-qty[1]
       v-tot-cost[2]  = v-tot-cost[2] + v-tot-cost[1]
       v-tot-sales[2] = v-tot-sales[2] + v-tot-sales[1]
       
       v-tot-qty[1]   = 0
       v-tot-cost[1]  = 0
       v-tot-sales[1] = 0.
    end.
    
    if last-of(substr(tt-report.key-01,1,8)) then do:
      if v-sort NE "Item" then do:
        if not first-of(substr(tt-report.key-01,1,8)) then do:
          if line-counter + 2 gt page-size then page.
        
          /*if v-ponum then do with frame ordhead-po:
            underline w-ord.qty-due
                      w-ord.t-price   when v-priceflag.
            down.
        
            display lv-total-label @ w-ord.part-no
                    v-tot-qty[2]      @ w-ord.qty-due
                    v-tot-sales[2]    when v-priceflag
                                      @ w-ord.t-price.
            down.
          end.

          else do with frame ordhead:
            underline w-ord.qty-due
                      w-ord.t-price   when v-priceflag.
            down.
                      
            DISPLAY lv-total-label @ w-ord.part-no
                    v-tot-qty[2]      @ w-ord.qty-due
                    v-tot-sales[2]    when v-priceflag
                                      @ w-ord.t-price.
            down.
          end.*/
          PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                 cTmpField = ""
                 cVarValue = ""
                 cExcelDisplay = ""
                 cExcelVarValue = "".
      
          DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  CASE cTmpField:             
                       WHEN "due-dt"   THEN cVarValue = "" . 
                       WHEN "ord-dt"   THEN cVarValue = "". 
                       WHEN "ord"      THEN cVarValue = "" .
                       WHEN "po-no"    THEN cVarValue = "".
                       WHEN "cust-prt" THEN cVarValue = "" .
                       WHEN "palt"     THEN cVarValue = ""  .
                       WHEN "qty-oh"   THEN cVarValue = "" .
                       WHEN "qty-due"  THEN cVarValue = STRING(v-tot-qty[2],"->,>>>,>>9") .
                       WHEN "price"    THEN cVarValue = ""  .
                       WHEN "sals"     THEN cVarValue = IF v-priceflag THEN STRING(v-tot-sales[2],">>,>>>,>>9.99") ELSE "" .
                       WHEN "uom"      THEN cVarValue = "" .
                       WHEN "cus-name" THEN cVarValue = ""  .
                       WHEN "rep-name" THEN cVarValue = "" .
                       WHEN "rel-type" THEN cVarValue = "".
                       
                  END CASE.
                    
                  cExcelVarValue = cVarValue.
                  cDisplay = cDisplay + cVarValue +
                             FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                  cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          END.
         
          PUT UNFORMATTED lv-total-label substring(cDisplay,16,300) SKIP.

        end.
        
        put skip(1).
      end.
      
      assign
       v-tot-qty[3]   = v-tot-qty[3] + v-tot-qty[2]
       v-tot-cost[3]  = v-tot-cost[3] + v-tot-cost[2]
       v-tot-sales[3] = v-tot-sales[3] + v-tot-sales[2]
       
       v-tot-qty[2]   = 0
       v-tot-cost[2]  = 0
       v-tot-sales[2] = 0.
    end. 

    if last(tt-report.key-01) then do:
      if line-counter + 2 gt page-size then page.

      /*if v-ponum then do with frame ordhead-po:
        underline w-ord.qty-due
                  w-ord.t-price   when v-priceflag.
        down.
        
        display "  GRAND TOTALS:" @ w-ord.part-no
                v-tot-qty[3]      @ w-ord.qty-due
                v-tot-sales[3]    when v-priceflag
                                  @ w-ord.t-price.
        down.
      end.

      else do with frame ordhead:
        underline w-ord.qty-due
                  w-ord.t-price   when v-priceflag.
        down.
                  
        display "  GRAND TOTALS:" @ w-ord.part-no
                v-tot-qty[3]      @ w-ord.qty-due
                v-tot-sales[3]    when v-priceflag
                                  @ w-ord.t-price.
        down.
      end.*/
      PUT SKIP str-line SKIP .
      ASSIGN cDisplay = ""
                 cTmpField = ""
                 cVarValue = ""
                 cExcelDisplay = ""
                 cExcelVarValue = "".
      
          DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  CASE cTmpField:             
                       WHEN "due-dt"   THEN cVarValue = "" . 
                       WHEN "ord-dt"   THEN cVarValue = "". 
                       WHEN "ord"      THEN cVarValue = "" .
                       WHEN "po-no"    THEN cVarValue = "".
                       WHEN "cust-prt" THEN cVarValue = "" .
                       WHEN "palt"     THEN cVarValue = ""  .
                       WHEN "qty-oh"   THEN cVarValue = "" .
                       WHEN "qty-due"  THEN cVarValue = STRING(v-tot-qty[3],"->,>>>,>>9") .
                       WHEN "price"    THEN cVarValue = ""  .
                       WHEN "sals"     THEN cVarValue = IF v-priceflag THEN STRING(v-tot-sales[3],">>,>>>,>>9.99") ELSE "" .
                       WHEN "uom"      THEN cVarValue = "" .  
                       WHEN "cus-name" THEN cVarValue = ""  .
                       WHEN "rep-name" THEN cVarValue = "" .
                       WHEN "rel-type" THEN cVarValue = "" .
                       
                  END CASE.
                    
                  cExcelVarValue = cVarValue.
                  cDisplay = cDisplay + cVarValue +
                             FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                  cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
          END.
         
          PUT UNFORMATTED "  GRAND TOTALS:" substring(cDisplay,16,300) SKIP.
    end.

    IF AVAIL w-ord THEN
       delete w-ord.
  end. /* for each */
