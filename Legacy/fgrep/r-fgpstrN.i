/*fgrep/r-fgpstrN.i*/


ASSIGN 
          v-shipto = ""
          v-shipto-name =  ""
          iBol-no = 0 .

for each tt-report where tt-report.term-id eq "" no-lock,
      first fg-rdtlh where recid(fg-rdtlh) eq tt-report.rec-id no-lock,
      /* find fg-rcpth */
      first fg-rcpth
      where fg-rdtlh.r-no      eq fg-rcpth.r-no
        and fg-rdtlh.rita-code eq fg-rcpth.rita-code
      no-lock
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            BY tt-report.DATE:

       {custom/statusMsg.i " 'Processing FG Item#  '  + fg-rcpth.i-no "}

      BUFFER bfg-rcpth:FIND-BY-ROWID(ROWID(fg-rcpth), NO-LOCK) .
      BUFFER b-fgrdtlh:FIND-BY-ROWID(ROWID(fg-rdtlh), NO-LOCK) .
      IF fg-rcpth.job-no <> v-current-job THEN
      ASSIGN v-new-job = YES
             v-current-job = fg-rcpth.job-no
             v-current-job2 = fg-rcpth.job-no2.
      ELSE
          ASSIGN v-new-job = NO.

    if first-of(tt-report.key-01) then do:             
      v-whse = fg-rdtlh.loc.
      
      if first(tt-report.key-01) then do:
        hide frame r-top.
        VIEW frame r-top.
        page.
      end.
      else put skip(3) "WHSE: " v-whse skip(1).
    end.
    
    v-stnd-cost = 0.

    if fg-rcpth.rita-code eq "S" then do:
      find first fg-bin
          where fg-bin.company eq fg-rcpth.company
            and fg-bin.job-no  eq fg-rcpth.job-no
            and fg-bin.job-no2 eq fg-rcpth.job-no2
            and fg-bin.i-no    eq fg-rcpth.i-no
            and fg-bin.loc     eq fg-rdtlh.loc
            and fg-bin.loc-bin eq fg-rdtlh.loc-bin
            and fg-bin.tag     eq fg-rdtlh.tag
          use-index job no-lock no-error.

      ASSIGN lv-cost-uom = if avail fg-bin then fg-bin.pur-uom else "M".
    end.
    ELSE 
        ASSIGN lv-cost-uom = fg-rcpth.pur-uom.
        
    /* SAB: Moved this up to get the UOM ahead of the "v-fg-cost" calculation below. */
  /*  ASSIGN
     lv-sell-price = 0
     lv-sell-uom   = "EA". */

        FIND FIRST oe-bolh
          WHERE oe-bolh.company EQ cocode
            AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-LOCK NO-ERROR.

        IF AVAIL oe-bolh  THEN 
            FIND first shipto where shipto.company eq cocode
                and shipto.cust-no eq oe-bolh.cust-no
                AND shipto.ship-id EQ oe-bolh.ship-id 
                NO-LOCK NO-ERROR .
          
            IF AVAIL shipto THEN
                ASSIGN 
                v-shipto = shipto.ship-id
                v-shipto-name =  shipto.ship-name.
            ELSE
                ASSIGN 
                    v-shipto = ""
                    v-shipto-name = "" .
          IF fg-rcpth.rita-code EQ "S" THEN DO:
              FIND FIRST oe-boll NO-LOCK
               WHERE oe-boll.company EQ oe-bolh.company
               AND oe-boll.b-no    EQ oe-bolh.b-no NO-ERROR.
            IF AVAIL oe-boll THEN
                  ASSIGN iBol-no = IF AVAIL oe-boll THEN oe-boll.bol-no ELSE 0.
          END.
          ELSE DO:
            ASSIGN iBol-no = 0.
          END.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq fg-rcpth.i-no
        use-index i-no no-lock no-error.
    if avail itemfg then do:

      ASSIGN
      /* lv-sell-price = itemfg.sell-price
       lv-sell-uom   = itemfg.sell-uom */   
       lv-cost-uom   = itemfg.prod-uom.

        /* calculate the cost based on fg-rcpth.pur-uom. */
        assign
         v-fg-qty   = fg-rdtlh.qty
         v-fg-cost  = fg-rdtlh.cost * (v-fg-qty / IF lv-cost-uom EQ "M" THEN 1000 ELSE 1)
         v-fg-value = 0
         v-msf[1]   = 0
         v-msf[2]   = 0.
        
        release job-mat.
        if fg-rcpth.rita-code eq "R" then
            RUN calc-msf-for-r (INPUT ROWID(fg-rcpth),
                                INPUT ROWID(fg-rdtlh),
                                INPUT last-of(tt-report.key-02),
                                INPUT v-corr,
                                OUTPUT v-on,
                                OUTPUT v-qty-pallet,
                                OUTPUT v-msf[1],
                                OUTPUT v-msf[2]).
    
          if fg-rcpth.rita-code eq "R" then do:
            if v-msf[1] gt fg-rdtlh.qty * itemfg.t-sqft then
              v-msf[2] = v-msf[2] +
                         (v-msf[1] - (fg-rdtlh.qty * itemfg.t-sqft)).
              v-msf[1] = fg-rdtlh.qty * itemfg.t-sqft.
          END.
        FIND eb WHERE eb.company = itemfg.company AND
                      eb.est-no = itemfg.est-no AND
                      eb.stock-no = itemfg.i-no NO-LOCK NO-ERROR.
        FIND ef OF eb NO-LOCK NO-ERROR.
        v-caliper = IF AVAIL ef THEN string(ef.cal) ELSE "".
        IF AVAIL eb THEN ASSIGN v-numColors = (eb.i-col)
                                v-numup = (eb.num-up)
                                v-sheetsize = IF AVAIL ef THEN string(ef.gsh-wid) + "x" + string(ef.gsh-len) ELSE "".
    END.
    job-start = ? .
    FIND FIRST job WHERE job.company EQ fg-rcpth.company 
            AND job.job-no EQ fg-rcpth.job-no
            AND job.job-no2 EQ fg-rcpth.job-no2
            NO-LOCK NO-ERROR.
        IF AVAIL job  THEN
            ASSIGN
            job-start = job.start-date .
    FIND job-hdr WHERE job-hdr.company = fg-rcpth.company
                   AND job-hdr.job-no = fg-rcpth.job-no
                   AND job-hdr.job-no2 = fg-rcpth.job-no2
                   AND job-hdr.i-no = fg-rcpth.i-no NO-LOCK NO-ERROR.
    IF AVAIL job-hdr AND job-hdr.est-no <> "" THEN DO:
       FIND eb WHERE eb.company = job-hdr.company
                 AND eb.est-no = job-hdr.est-no
                 AND eb.stock-no = job-hdr.i-no NO-LOCK NO-ERROR.
       IF AVAIL eb THEN ASSIGN v-numColors = (eb.i-col)
                               v-numup = (eb.num-up)
                               v-sheetsize = IF AVAIL ef THEN string(ef.gsh-wid) + "x" + string(ef.gsh-len) ELSE "".
        FIND ef OF eb NO-LOCK NO-ERROR.
        v-caliper = IF AVAIL ef THEN string(ef.cal) ELSE "".
    END.
    ASSIGN 
        prom-date = ? 
        due-date = ?
        order-no = 0 .
        
    IF AVAIL job-hdr AND job-hdr.ord-no <> 0 THEN DO:
        FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                    AND oe-ordl.ord-no = int(job-hdr.ord-no )
                    AND oe-ordl.i-no   = itemfg.i-no NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN
        ASSIGN prom-date = oe-ordl.prom-date 
               due-date = oe-ordl.req-date 
               order-no = oe-ordl.ord-no   .
    END.
    ELSE IF AVAIL oe-bolh THEN DO:
       FIND FIRST oe-boll WHERE oe-boll.company = oe-bolh.company 
            AND oe-boll.b-no = oe-bolh.b-no
            AND oe-boll.i-no = itemfg.i-no NO-LOCK NO-ERROR.
       IF AVAIL oe-boll THEN 
          FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                    AND oe-ordl.ord-no = int(oe-boll.ord-no )
                    AND oe-ordl.i-no   = itemfg.i-no NO-LOCK NO-ERROR.

       IF AVAIL oe-ordl THEN
          ASSIGN prom-date = oe-ordl.prom-date 
              due-date = oe-ordl.req-date 
              order-no = oe-ordl.ord-no   .
    END.
       

    RUN calc-sell-price (INPUT ROWID(fg-rcpth), 
                         OUTPUT lv-sell-price, 
                         OUTPUT lv-sell-uom).

    RUN calc-fg-value (INPUT lv-sell-price,
                       INPUT lv-sell-uom,
                       INPUT ROWID(fg-rdtlh),
                       OUTPUT v-fg-value).
    assign
     v-msf[1] = v-msf[1] / 1000
     v-msf[2] = v-msf[2] / 1000.

    if index("RTASEC", fg-rcpth.rita-code) ne 0 then
      v-tran-type = entry(index("RTASEC", fg-rcpth.rita-code),v-entrytype).
    else v-tran-type = "".

    if line-counter eq 56 then page.

    if fg-rcpth.po-no ne " " then
    find first po-ord
        where po-ord.company eq cocode
          and po-ord.po-no   eq int(fg-rcpth.po-no)
        no-lock no-error.   
    IF AVAIL po-ord THEN
        BUFFER bpo-ord:FIND-BY-ROWID(ROWID(po-ord), NO-LOCK) .
    RUN calc-case-and-tag (INPUT ROWID(fg-rcpth),
                           INPUT ROWID(fg-rdtlh),
                           INPUT v-fg-qty,
                           OUTPUT v-cases,
                           OUTPUT v-qty-case,
                           OUTPUT v-tag).

    find first loadtag where loadtag.item-type = NO
                         AND loadtag.company eq cocode
                         and loadtag.i-no    eq fg-rcpth.i-no
                         AND loadtag.tag-no  EQ fg-rdtlh.tag
                       NO-LOCK NO-ERROR.
    IF AVAIL loadtag THEN
      FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
   
    find first fg-bin
          where fg-bin.company eq fg-rcpth.company
            and fg-bin.job-no  eq fg-rcpth.job-no
            and fg-bin.job-no2 eq fg-rcpth.job-no2
            and fg-bin.i-no    eq fg-rcpth.i-no
            and fg-bin.loc     eq fg-rdtlh.loc
            and fg-bin.loc-bin eq fg-rdtlh.loc-bin
            and fg-bin.tag     eq fg-rdtlh.tag
            and fg-bin.po-no   eq fg-rcpth.po-no
            AND fg-bin.bol-no  EQ fg-rdtlh.bol-no
          use-index job no-lock no-error.
    IF NOT AVAIL fg-bin THEN 
        find first fg-bin
          where fg-bin.company eq fg-rcpth.company
            and fg-bin.job-no  eq fg-rcpth.job-no
            and fg-bin.job-no2 eq fg-rcpth.job-no2
            and fg-bin.i-no    eq fg-rcpth.i-no
            and fg-bin.loc     eq fg-rdtlh.loc
            and fg-bin.loc-bin eq fg-rdtlh.loc-bin
            and fg-bin.tag     eq fg-rdtlh.tag
          use-index job no-lock no-error.

    IF NOT AVAIL fg-bin THEN 
         find first fg-bin
          where fg-bin.company eq fg-rcpth.company
            and fg-bin.i-no    eq fg-rcpth.i-no
            and fg-bin.po-no   eq fg-rcpth.po-no
            AND fg-bin.loc     eq fg-rdtlh.loc
            AND fg-bin.loc-bin eq fg-rdtlh.loc-bin
            AND fg-bin.tag     eq fg-rdtlh.tag
          use-index job no-lock no-error.

    IF NOT AVAIL fg-bin THEN 
         find first fg-bin
          where fg-bin.company eq fg-rcpth.company
            and fg-bin.i-no    eq fg-rcpth.i-no
            AND fg-bin.loc     eq fg-rdtlh.loc
            AND fg-bin.loc-bin eq fg-rdtlh.loc-bin
            AND fg-bin.tag     eq fg-rdtlh.tag
          use-index job no-lock no-error.

        IF AVAIL fg-bin THEN
            ASSIGN v-stnd-cost = fg-bin.std-tot-cost.
        ELSE IF AVAIL itemfg THEN
            ASSIGN v-stnd-cost = itemfg.total-std-cost.
        iBinQtyb = 0.
        IF fg-rcpth.rita-code = "C" THEN
            FOR EACH bf-fg-rcpth NO-LOCK
              WHERE bf-fg-rcpth.company EQ cocode 
                AND bf-fg-rcpth.i-no EQ fg-rcpth.i-no
                AND bf-fg-rcpth.job-no EQ fg-rcpth.job-no
                AND bf-fg-rcpth.job-no2 EQ fg-rcpth.job-no2
                AND bf-fg-rcpth.po-no EQ fg-rcpth.po-no
                AND bf-fg-rcpth.rita-code ne "C" ,
               EACH bf-fg-rdtlh NO-LOCK WHERE
                    bf-fg-rdtlh.r-no EQ bf-fg-rcpth.r-no
                AND bf-fg-rdtlh.loc EQ fg-rdtlh.loc
                AND bf-fg-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
                AND bf-fg-rdtlh.tag EQ fg-rdtlh.tag
                AND bf-fg-rdtlh.cust-no EQ fg-rdtlh.cust-no 
                AND bf-fg-rdtlh.bol-no EQ fg-rdtlh.bol-no
                AND bf-fg-rdtlh.inv-no EQ fg-rdtlh.inv-no BY fg-rcpth.trans-date DESC :
            iBinQtyb = iBinQtyb +  bf-fg-rdtlh.qty  .
	LEAVE.
        END.

   
    BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
    ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "po-ord" THEN hField = IF AVAIL bpo-ord THEN BUFFER bpo-ord:BUFFER-FIELD(cTmpField) ELSE ?.
                 ELSE IF cFieldName BEGINS "itemfg" THEN hField = BUFFER bitemfg:BUFFER-FIELD(cTmpField).
                 ELSE IF cFieldName BEGINS "fg-rdtlh" THEN hField = BUFFER b-fgrdtlh:BUFFER-FIELD(cTmpField).
                 ELSE hField = BUFFER bfg-rcpth:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-rcpth.job-no2,"99") ELSE "".                  
                     
                     IF cFieldName <> "fg-rdtld.loc" THEN
                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + (IF fg-rcpth.job-no  <> "" THEN  "-" + string(fg-rcpth.job-no2,"99") ELSE  "") + ",".
                     ELSE cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
       END.
       ELSE DO: 
            /*{sys/inc/rptDisp3.i cTmpField}*/
            
             

            CASE cTmpField:               
                /* v-qty-case lv-cost-uom v-fg-qty v-fg-cost v-fg-value  */
                 WHEN "v-tran-type" THEN cVarValue = substring(v-tran-type,1,1).
                 WHEN "v-tag" THEN cVarValue = string(v-tag).
                 WHEN "v-rfid#" THEN cVarValue = IF AVAIL rfidtag THEN SUBSTRING(rfidtag.rfidtag,13) ELSE "".
                 WHEN "v-cases" THEN cVarValue = string(v-cases).
                 WHEN "v-qty-case" THEN cVarValue = string(v-qty-case).
                 WHEN "lv-cost-uom" THEN cVarValue = string(lv-cost-uom).
                 WHEN "v-fg-qty" THEN cVarValue = string(v-fg-qty).
                 WHEN "v-fg-cost" THEN cVarValue = string(v-fg-cost,"->>>>>9.99").
                 WHEN "v-fg-value" THEN cVarValue = string(v-fg-value,"->>>>>,>>9.99").
                 WHEN "v-numUp" THEN cVarValue = string(v-numUP).
                 WHEN "v-numColors" THEN cVarValue = string(v-numColors).
                 WHEN "v-SheetSize" THEN cVarValue = string(v-SheetSize,"X(15)").
                 WHEN "v-Caliper" THEN cVarValue = string(v-Caliper).
                 WHEN "wt-h" THEN cVarValue = string(fg-rdtlh.tot-wt,">>,>>9.99").
                 WHEN "rec-time" THEN cVarValue = STRING(fg-rdtlh.trans-time,'HH:MM').
                 WHEN "unt-sel" THEN cVarValue = string(lv-sell-price,"->>,>>>,>>9.99<<<<").
                 WHEN "suom" THEN cVarValue = string(lv-sell-uom).
                 WHEN "unt-cst" THEN cVarValue = IF v-stnd-cost NE 0 THEN STRING(v-stnd-cost,"->>>,>>9.99<<") ELSE "".

                 WHEN "prom-date" THEN cVarValue = IF prom-date <> ? THEN string(prom-date,"99/99/9999") ELSE "".
                 WHEN "due-date" THEN cVarValue = IF due-date <> ? THEN string(due-date,"99/99/9999") ELSE "".
                 WHEN "job-start" THEN cVarValue = IF job-start <> ? THEN string(job-start,"99/99/9999") ELSE "".
                 WHEN "shipto" THEN cVarValue = v-shipto .
                 WHEN "shipname" THEN cVarValue = v-shipto-name  .
                 WHEN "order-no" THEN cVarValue = string(order-no,">>>>>>>")  .
                 WHEN "bef-qty" THEN cVarValue = string(iBinQtyb,"->>>>>>>>9")    .
                 WHEN "bin-qty" THEN do:
                      cVarValue =  STRING(v-fg-qty - iBinQtyb,"->>>>>>>>9")  .
                 END.
                  WHEN "bol-no" THEN cVarValue = string(iBol-no,">>>>>>>")  .
            END CASE.
              
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
        /*PUT STREAM excel UNFORMATTED
          '"' STRING(fg-rcpth.trans-date)                              '",'
          '"' REPLACE(STRING(fg-rcpth.i-no),'"','')                    '",'
          '"' fg-rcpth.i-name                                          '",'
          '"' fg-rcpth.po-no                                           '",'.
        */
        PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.

        /*
        IF  rsShowVendor = "Vendor" THEN
            PUT STREAM excel UNFORMATTED
               '"' (IF avail po-ord AND fg-rcpth.po-no <> "" THEN po-ord.vend-no
               ELSE "")                                                '",'.
        ELSE            
            PUT STREAM excel UNFORMATTED
               '"' (IF fg-rcpth.job-no <> "" THEN fg-rcpth.job-no + "-" + string(fg-rcpth.job-no2,"99")
               ELSE "")                                                '",'.
        
        PUT STREAM excel UNFORMATTED
          '"' STRING(v-tran-type,"X(1)")                               '",'
          '"' v-tag                                                    '",'
          '"' STRING(v-cases,"->>,>>9")                                '",'
          '"' STRING(v-qty-case,"->>>,>>9")                            '",'
/*           '"' ""                                                       '",' */
          '"' fg-rdtlh.loc-bin                                         '",'
          '"' lv-cost-uom                                              '",'
          '"' STRING(v-fg-qty,"->>>,>>9")                              '",'
          '"' STRING(v-fg-cost,"->>>,>>9.99<<")                        '",'
          '"' STRING(v-fg-value,"->>,>>>,>>9.99")                      '",'
          SKIP.
          */
    END.

    if fg-rdtlh.rita-code eq "T" then
      put "To: " to 94 fg-rdtlh.loc2 fg-rdtlh.loc-bin2 skip.

    if v-pr-tots then do:    
      IF rsShowVendor = "Vendor" AND rsShowTag = "TAG#" THEN
      ASSIGN v-tot-pos1 = 116
             v-tot-pos2 = 128
             v-tot-pos3 = 143.
      ELSE IF rsShowVendor = "Vendor" AND rsShowTag = "RFID#"
              OR rsShowVendor = "Job#" AND rsShowTag = "Tag#" THEN
      ASSIGN v-tot-pos1 = 119
             v-tot-pos2 = 131
             v-tot-pos3 = 146.
      ELSE IF rsShowVendor = "Job#" AND rsShowTag = "RFID#" THEN
      ASSIGN v-tot-pos1 = 123
             v-tot-pos2 = 135
             v-tot-pos3 = 150.
      
      assign
       v-tot-qty = v-tot-qty + v-fg-qty
       v-tot-cost = v-tot-cost + v-fg-cost
       v-grd-tot-cost = v-grd-tot-cost + v-fg-cost   
          /* Do this when sell uom = "L" and first-of job */
/*        v-tot-value = v-tot-value + v-fg-value  */
/*        v-grd-tot-value = v-grd-tot-value + v-fg-value */
       v-msf[3] = v-msf[3] + v-msf[1]
       v-msf[4] = v-msf[4] + v-msf[2].

      /* Do not accumulate total for sell-uom = "L" */
      IF (v-new-job = YES AND lv-sell-uom = "L") OR (lv-sell-uom <> "L") THEN
          ASSIGN v-tot-value = v-tot-value + v-fg-value
                 v-grd-tot-value = v-grd-tot-value + v-fg-value.

      if fg-rdtlh.rita-code eq "R" or
         fg-rdtlh.rita-code eq "A" or
         fg-rdtlh.rita-code eq "E" then
        v-cum-tot  = v-cum-tot + v-fg-cost.
      else
      if fg-rdtlh.rita-code eq "S" then
        v-cum-tot  = v-cum-tot - v-fg-cost.
    end.  /*   if v-pr-tots   */ 
    
    if v-pr-tots then do:                                                              if last-of(tt-report.key-02) then do:
        put "-----------" to v-tot-pos1
            "----------" to v-tot-pos2
            "--------------" to v-tot-pos3
            skip.                

        if fg-rcpth.rita-code eq "R" then
          put "MSF->  FG: " + trim(STRING(v-msf[3],"->>,>>9.9<<")) +
              "  Wst: " + trim(STRING(v-msf[4],"->>,>>9.9<<"))    +
              "  Tot: " + trim(STRING(v-msf[3] + v-msf[4],"->>,>>9.9<<"))
                             format "x(63)" at 15.

        put v-tot-qty to v-tot-pos1
            v-tot-cost to v-tot-pos2
            v-tot-value to v-tot-pos3 skip(1).

        assign
         v-msf[5]    = v-msf[5] + v-msf[3]
         v-msf[6]    = v-msf[6] + v-msf[4]
         v-tot-qty   = 0
         v-tot-cost  = 0
         v-tot-value = 0
         v-msf[3]    = 0
         v-msf[4]    = 0.
      end.  /* if last-of(fg-rcpth.i-no) */        
    end. /* if v-pr-tots */
  end.
