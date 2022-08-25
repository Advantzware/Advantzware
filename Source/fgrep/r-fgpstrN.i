/*fgrep/r-fgpstrN.i*/
DEFINE VARIABLE cReason AS CHARACTER NO-UNDO.

ASSIGN 
    v-shipto      = ""
    v-shipto-name = ""
    iBol-no       = 0 .

FOR EACH tt-report WHERE tt-report.term-id EQ "" NO-LOCK,
    FIRST fg-rdtlh WHERE RECID(fg-rdtlh) EQ tt-report.rec-id NO-LOCK,
    /* find fg-rcpth */
    FIRST fg-rcpth
    WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
    AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
    NO-LOCK
    BREAK BY tt-report.key-01
    BY tt-report.key-02
    BY tt-report.key-03
    BY tt-report.key-04
    BY tt-report.DATE:

    {custom/statusMsg.i " 'Processing FG Item#  '  + fg-rcpth.i-no "}

    BUFFER bfg-rcpth:FIND-BY-ROWID(ROWID(fg-rcpth), NO-LOCK) .
    BUFFER b-fgrdtlh:FIND-BY-ROWID(ROWID(fg-rdtlh), NO-LOCK) .
    IF fg-rcpth.job-no <> v-current-job THEN
        ASSIGN v-new-job      = YES
            v-current-job  = fg-rcpth.job-no
            v-current-job2 = fg-rcpth.job-no2.
    ELSE
        ASSIGN v-new-job = NO.

    IF FIRST-OF(tt-report.key-01) THEN 
    DO:             
        v-whse = fg-rdtlh.loc.
      
        IF FIRST(tt-report.key-01) THEN 
        DO:
            HIDE FRAME r-top.
            VIEW FRAME r-top.
            PAGE.
            PUT SKIP 
                "WHSE: " v-whse SKIP(1).
        END.
        ELSE PUT SKIP(1) "WHSE: " v-whse SKIP(1).
    END.
    
    ASSIGN
        v-stnd-cost = 0
        dBinMatCost = 0
        dBinDLCost  = 0
        dBinVOHCost = 0
        dBinFOHCost = 0.

    IF fg-rcpth.rita-code EQ "S" THEN 
    DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ fg-rcpth.company
            AND fg-bin.job-no  EQ fg-rcpth.job-no
            AND fg-bin.job-no2 EQ fg-rcpth.job-no2
            AND fg-bin.i-no    EQ fg-rcpth.i-no
            AND fg-bin.loc     EQ fg-rdtlh.loc
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND fg-bin.tag     EQ fg-rdtlh.tag
            USE-INDEX job NO-LOCK NO-ERROR.

        ASSIGN 
            lv-cost-uom = IF AVAILABLE fg-bin THEN fg-bin.pur-uom ELSE "M".
    END.
    ELSE 
        ASSIGN lv-cost-uom = fg-rcpth.pur-uom.
        
    /* SAB: Moved this up to get the UOM ahead of the "v-fg-cost" calculation below. */
    /*  ASSIGN
       lv-sell-price = 0
       lv-sell-uom   = "EA". */

    FIND FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-LOCK NO-ERROR.

    IF AVAILABLE oe-bolh  THEN 
        FIND FIRST shipto WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ oe-bolh.cust-no
            AND shipto.ship-id EQ oe-bolh.ship-id 
            NO-LOCK NO-ERROR .
          
    IF AVAILABLE shipto THEN
        ASSIGN 
            v-shipto      = shipto.ship-id
            v-shipto-name = shipto.ship-name.
    ELSE
        ASSIGN 
            v-shipto      = ""
            v-shipto-name = "" .
    IF fg-rcpth.rita-code EQ "S" THEN 
    DO:
        FIND FIRST oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no NO-ERROR.
        IF AVAILABLE oe-boll THEN
            ASSIGN iBol-no = IF AVAILABLE oe-boll THEN oe-boll.bol-no ELSE 0.
    END.
    ELSE 
    DO:
        ASSIGN 
            iBol-no = 0.
    END.

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rcpth.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:

        ASSIGN
            /* lv-sell-price = itemfg.sell-price
             lv-sell-uom   = itemfg.sell-uom */   
            lv-cost-uom = itemfg.prod-uom.

        /* calculate the cost based on fg-rcpth.pur-uom. */
        ASSIGN
            v-fg-qty   = fg-rdtlh.qty
            v-fg-cost  = fg-rdtlh.cost * (v-fg-qty / IF lv-cost-uom EQ "M" THEN 1000 ELSE 1)
            v-fg-value = 0
            v-msf[1]   = 0
            v-msf[2]   = 0.
        
        RELEASE job-mat.
        IF fg-rcpth.rita-code EQ "R" THEN
            RUN calc-msf-for-r (INPUT ROWID(fg-rcpth),
                INPUT ROWID(fg-rdtlh),
                INPUT LAST-OF(tt-report.key-02),
                INPUT v-corr,
                OUTPUT v-on,
                OUTPUT v-qty-pallet,
                OUTPUT v-msf[1],
                OUTPUT v-msf[2]).
    
        IF fg-rcpth.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT fg-rdtlh.qty * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] +
                    (v-msf[1] - (fg-rdtlh.qty * itemfg.t-sqft)).
            v-msf[1] = fg-rdtlh.qty * itemfg.t-sqft.
        END.
        FIND eb WHERE eb.company = itemfg.company AND
            eb.est-no = itemfg.est-no AND
            eb.stock-no = itemfg.i-no NO-LOCK NO-ERROR.
        FIND ef OF eb NO-LOCK NO-ERROR.
        v-caliper = IF AVAILABLE ef THEN STRING(ef.cal) ELSE "".
        IF AVAILABLE eb THEN ASSIGN v-numColors = (eb.i-col)
                v-numup     = (eb.num-up)
                v-sheetsize = IF AVAILABLE ef THEN STRING(ef.gsh-wid) + "x" + string(ef.gsh-len) ELSE "".
    END.
    job-start = ? .
    FIND FIRST job WHERE job.company EQ fg-rcpth.company 
        AND job.job-no EQ fg-rcpth.job-no
        AND job.job-no2 EQ fg-rcpth.job-no2
        NO-LOCK NO-ERROR.
    IF AVAILABLE job  THEN
        ASSIGN
            job-start = job.start-date .
    FIND job-hdr WHERE job-hdr.company = fg-rcpth.company
        AND job-hdr.job-no = fg-rcpth.job-no
        AND job-hdr.job-no2 = fg-rcpth.job-no2
        AND job-hdr.i-no = fg-rcpth.i-no NO-LOCK NO-ERROR.
    IF AVAILABLE job-hdr AND job-hdr.est-no <> "" THEN 
    DO:
        FIND eb WHERE eb.company = job-hdr.company
            AND eb.est-no = job-hdr.est-no
            AND eb.stock-no = job-hdr.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE eb THEN ASSIGN v-numColors = (eb.i-col)
                v-numup     = (eb.num-up)
                v-sheetsize = IF AVAILABLE ef THEN STRING(ef.gsh-wid) + "x" + string(ef.gsh-len) ELSE "".
        FIND ef OF eb NO-LOCK NO-ERROR.
        v-caliper = IF AVAILABLE ef THEN STRING(ef.cal) ELSE "".
    END.
    ASSIGN 
        prom-date = ? 
        due-date  = ?
        order-no  = 0 .
        
    IF AVAILABLE job-hdr AND job-hdr.ord-no <> 0 THEN 
    DO:
        FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
            AND oe-ordl.ord-no = int(job-hdr.ord-no )
            AND oe-ordl.i-no   = itemfg.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ordl THEN
            ASSIGN prom-date = oe-ordl.prom-date 
                due-date  = oe-ordl.req-date 
                order-no  = oe-ordl.ord-no   .
    END.
    ELSE IF AVAILABLE oe-bolh THEN 
        DO:
            FIND FIRST oe-boll WHERE oe-boll.company = oe-bolh.company 
                AND oe-boll.b-no = oe-bolh.b-no
                AND oe-boll.i-no = itemfg.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE oe-boll THEN 
                FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                    AND oe-ordl.ord-no = int(oe-boll.ord-no )
                    AND oe-ordl.i-no   = itemfg.i-no NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ordl THEN
                ASSIGN prom-date = oe-ordl.prom-date 
                    due-date  = oe-ordl.req-date 
                    order-no  = oe-ordl.ord-no   .
        END.
       

    RUN calc-sell-price (INPUT ROWID(fg-rcpth), 
        OUTPUT lv-sell-price, 
        OUTPUT lv-sell-uom).

    RUN calc-fg-value (INPUT lv-sell-price,
        INPUT lv-sell-uom,
        INPUT ROWID(fg-rdtlh),
        OUTPUT v-fg-value).
    ASSIGN
        v-msf[1] = v-msf[1] / 1000
        v-msf[2] = v-msf[2] / 1000.

    IF INDEX("RTASEC", fg-rcpth.rita-code) NE 0 THEN
        v-tran-type = ENTRY(INDEX("RTASEC", fg-rcpth.rita-code),v-entrytype).
    ELSE v-tran-type = "".

    IF LINE-COUNTER EQ 56 THEN PAGE.

    IF fg-rcpth.po-no NE " " THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ int(fg-rcpth.po-no)
            NO-LOCK NO-ERROR.   
    IF AVAILABLE po-ord THEN
        BUFFER bpo-ord:FIND-BY-ROWID(ROWID(po-ord), NO-LOCK) .
    RUN calc-case-and-tag (INPUT ROWID(fg-rcpth),
        INPUT ROWID(fg-rdtlh),
        INPUT v-fg-qty,
        OUTPUT v-cases,
        OUTPUT v-qty-case,
        OUTPUT v-tag).

    FIND FIRST loadtag WHERE loadtag.item-type = NO
        AND loadtag.company EQ cocode
        AND loadtag.i-no    EQ fg-rcpth.i-no
        AND loadtag.tag-no  EQ fg-rdtlh.tag
        NO-LOCK NO-ERROR.
    IF AVAILABLE loadtag THEN
        FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
   
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ fg-rcpth.company
        AND fg-bin.job-no  EQ fg-rcpth.job-no
        AND fg-bin.job-no2 EQ fg-rcpth.job-no2
        AND fg-bin.i-no    EQ fg-rcpth.i-no
        AND fg-bin.loc     EQ fg-rdtlh.loc
        AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
        AND fg-bin.tag     EQ fg-rdtlh.tag
        AND fg-bin.po-no   EQ fg-rcpth.po-no
        AND fg-bin.bol-no  EQ fg-rdtlh.bol-no
        USE-INDEX job NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fg-bin THEN 
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ fg-rcpth.company
            AND fg-bin.job-no  EQ fg-rcpth.job-no
            AND fg-bin.job-no2 EQ fg-rcpth.job-no2
            AND fg-bin.i-no    EQ fg-rcpth.i-no
            AND fg-bin.loc     EQ fg-rdtlh.loc
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND fg-bin.tag     EQ fg-rdtlh.tag
            USE-INDEX job NO-LOCK NO-ERROR.

    IF NOT AVAILABLE fg-bin THEN 
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ fg-rcpth.company
            AND fg-bin.i-no    EQ fg-rcpth.i-no
            AND fg-bin.po-no   EQ fg-rcpth.po-no
            AND fg-bin.loc     EQ fg-rdtlh.loc
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND fg-bin.tag     EQ fg-rdtlh.tag
            USE-INDEX po-no NO-LOCK NO-ERROR.

    IF NOT AVAILABLE fg-bin THEN 
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ fg-rcpth.company
            AND fg-bin.i-no    EQ fg-rcpth.i-no
            AND fg-bin.loc     EQ fg-rdtlh.loc
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND fg-bin.tag     EQ fg-rdtlh.tag
            USE-INDEX co-ino NO-LOCK NO-ERROR.

    IF AVAILABLE fg-bin THEN 
    DO:
        ASSIGN 
            v-stnd-cost = fg-bin.std-tot-cost
            dBinMatCost = fg-bin.std-mat-cost
            dBinDLCost  = fg-bin.std-lab-cost
            dBinVOHCost = fg-bin.std-var-cost
            dBinFOHCost = fg-bin.std-fix-cost
            .
    END.
    ELSE IF AVAILABLE itemfg THEN
            ASSIGN v-stnd-cost = itemfg.total-std-cost.
    iBinQtyb = 0.
    IF fg-rcpth.rita-code = "C" THEN
        FOR EACH bf-fg-rcpth NO-LOCK
            WHERE bf-fg-rcpth.company EQ cocode 
            AND bf-fg-rcpth.i-no EQ fg-rcpth.i-no
            AND bf-fg-rcpth.job-no EQ fg-rcpth.job-no
            AND bf-fg-rcpth.job-no2 EQ fg-rcpth.job-no2
            AND bf-fg-rcpth.po-no EQ fg-rcpth.po-no
            AND bf-fg-rcpth.rita-code NE "C" ,
            EACH bf-fg-rdtlh NO-LOCK WHERE
            bf-fg-rdtlh.r-no EQ bf-fg-rcpth.r-no
            AND bf-fg-rdtlh.loc EQ fg-rdtlh.loc
            AND bf-fg-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
            AND bf-fg-rdtlh.tag EQ fg-rdtlh.tag
            AND bf-fg-rdtlh.cust-no EQ fg-rdtlh.cust-no 
            AND bf-fg-rdtlh.bol-no EQ fg-rdtlh.bol-no
            AND bf-fg-rdtlh.inv-no EQ fg-rdtlh.inv-no BY fg-rcpth.trans-date DESCENDING :
            iBinQtyb = iBinQtyb +  bf-fg-rdtlh.qty  .
            LEAVE.
        END.

    ASSIGN 
        cReason = "".
    FIND FIRST rejct-cd NO-LOCK WHERE rejct-cd.CODE EQ fg-rdtlh.reject-code[1] NO-ERROR.
    IF fg-rdtlh.reject-code[1] NE "" THEN
        ASSIGN cReason = fg-rdtlh.reject-code[1] + IF AVAILABLE rejct-cd AND rejct-cd.dscr NE "" THEN ( " - " + rejct-cd.dscr) ELSE "".
    ELSE cReason = "".

   
    BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        IF INDEX(cTmpField,".") > 0 THEN 
        DO:
            cFieldName = cTmpField.
            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
            IF cFieldName BEGINS "po-ord" THEN hField = IF AVAILABLE bpo-ord THEN BUFFER bpo-ord:BUFFER-FIELD(cTmpField) ELSE ?.
            ELSE IF cFieldName BEGINS "itemfg" THEN hField = BUFFER bitemfg:BUFFER-FIELD(cTmpField).
                ELSE IF cFieldName BEGINS "fg-rdtlh" THEN hField = BUFFER b-fgrdtlh:BUFFER-FIELD(cTmpField).
                    ELSE hField = BUFFER bfg-rcpth:BUFFER-FIELD(cTmpField).
            IF hField <> ? THEN 
            DO:                 
                cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                IF ENTRY(i,cSelectedList) = "Job#" THEN
                    cTmpField = IF cTmpField <> "" THEN TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cTmpField, fg-rcpth.job-no2))) ELSE "".                  
                     
                IF cFieldName <> "fg-rdtld.loc" THEN
                    cDisplay = cDisplay + cTmpField + 
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                        .
                IF ENTRY(i,cSelectedList) = "Job#" THEN
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, GetFieldValue(hField))) + 
                                    (IF fg-rcpth.job-no  <> "" THEN 
                                    TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', fg-rcpth.job-no, fg-rcpth.job-no2))) ELSE  "") + ",".
                ELSE cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, GetFieldValue(hField))) + ",".
            END.
            ELSE 
            DO:
                cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
            END.
        END.
        ELSE 
        DO: 
            /*{sys/inc/rptDisp3.i cTmpField}*/
            
             

            CASE cTmpField:               
                WHEN "loc" THEN 
                    cVarValue = STRING(fg-rdtlh.loc,"x(5)"). 
                WHEN "v-tran-type" THEN 
                    cVarValue = SUBSTRING(v-tran-type,1,1).
                WHEN "v-tag" THEN 
                    cVarValue = STRING(v-tag).
                WHEN "v-rfid#" THEN 
                    cVarValue = IF AVAILABLE rfidtag THEN SUBSTRING(rfidtag.rfidtag,13) ELSE "".
                WHEN "v-cases" THEN 
                    cVarValue = STRING(v-cases).
                WHEN "v-qty-case" THEN 
                    cVarValue = STRING(v-qty-case).
                WHEN "lv-cost-uom" THEN 
                    cVarValue = STRING(lv-cost-uom).
                WHEN "v-fg-qty" THEN 
                    cVarValue = STRING(v-fg-qty).
                WHEN "v-fg-cost" THEN 
                    cVarValue = STRING(v-fg-cost,"->>>>>9.99").
                WHEN "v-fg-value" THEN 
                    cVarValue = STRING(v-fg-value,"->>>>>,>>9.99").
                WHEN "v-numUp" THEN 
                    cVarValue = STRING(v-numUP).
                WHEN "v-numColors" THEN 
                    cVarValue = STRING(v-numColors).
                WHEN "v-SheetSize" THEN 
                    cVarValue = STRING(v-SheetSize,"X(15)").
                WHEN "v-Caliper" THEN 
                    cVarValue = STRING(v-Caliper).
                WHEN "wt-h" THEN 
                    cVarValue = STRING(fg-rdtlh.tot-wt,">>,>>9.99").
                WHEN "rec-time" THEN 
                    cVarValue = STRING(fg-rdtlh.trans-time,'HH:MM').
                WHEN "unt-sel" THEN 
                    cVarValue = STRING(lv-sell-price,"->>,>>>,>>9.99<<<<").
                WHEN "suom" THEN 
                    cVarValue = STRING(lv-sell-uom).
                WHEN "unt-cst" THEN 
                    cVarValue = IF v-stnd-cost NE 0 THEN STRING(v-stnd-cost,"->>>,>>9.99<<") ELSE "".

                WHEN "prom-date" THEN 
                    cVarValue = IF prom-date <> ? THEN STRING(prom-date,"99/99/9999") ELSE "".
                WHEN "due-date" THEN 
                    cVarValue = IF due-date <> ? THEN STRING(due-date,"99/99/9999") ELSE "".
                WHEN "job-start" THEN 
                    cVarValue = IF job-start <> ? THEN STRING(job-start,"99/99/9999") ELSE "".
                WHEN "shipto" THEN 
                    cVarValue = v-shipto .
                WHEN "shipname" THEN 
                    cVarValue = v-shipto-name  .
                WHEN "order-no" THEN 
                    cVarValue = STRING(order-no,">>>>>>>")  .
                WHEN "bef-qty" THEN 
                    cVarValue = STRING(iBinQtyb,"->>>>>>>>9")    .
                WHEN "bin-qty" THEN 
                    DO:
                        cVarValue =  STRING(v-fg-qty - iBinQtyb,"->>>>>>>>9")  .
                    END.
                WHEN "bol-no" THEN 
                    cVarValue = STRING(iBol-no,">>>>>>>")  .
                WHEN "Reason" THEN 
                    cVarValue =  STRING(cReason,"x(30)")      .
                WHEN "Reason-cd" THEN 
                    cVarValue = IF AVAILABLE fg-rdtlh AND fg-rdtlh.reject-code[1] NE "" THEN STRING(fg-rdtlh.reject-code[1],"x(2)") ELSE ""    .
                WHEN "Reason-dscr" THEN 
                    cVarValue = IF AVAILABLE rejct-cd AND rejct-cd.dscr NE "" THEN STRING(rejct-cd.dscr,"x(25)") ELSE ""   .
                WHEN "bin-mat-cost" THEN 
                    cVarValue = IF dBinMatCost NE 0 THEN STRING(dBinMatCost,"->>>,>>9.99<<") ELSE "".
                WHEN "bin-dl-cost"  THEN 
                    cVarValue = IF dBinDLCost  NE 0 THEN STRING(dBinDLCost,"->>>,>>9.99<<") ELSE "".
                WHEN "bin-voh-cost" THEN 
                    cVarValue = IF dBinVOHCost NE 0 THEN STRING(dBinVOHCost,"->>>,>>9.99<<") ELSE "".
                WHEN "bin-foh-cost" THEN 
                    cVarValue = IF dBinFOHCost NE 0 THEN STRING(dBinFOHCost,"->>>,>>9.99<<") ELSE "".
            END CASE.
              
            cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
            IF cTmpField <> "loc" THEN 
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF rd-dest = 3 THEN 
    DO:
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
           '"' (IF fg-rcpth.job-no <> "" THEN TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', fg-rcpth.job-no, fg-rcpth.job-no2)))
           ELSE "") FORM "X(13)"                                   '",'.
        
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

    IF fg-rdtlh.rita-code EQ "T" THEN
        PUT "To: " TO 94 fg-rdtlh.loc2 fg-rdtlh.loc-bin2 SKIP.

    IF v-pr-tots THEN 
    DO:    
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
      
        ASSIGN
            v-tot-qty      = v-tot-qty + v-fg-qty
            v-tot-cost     = v-tot-cost + v-fg-cost
            v-grd-tot-cost = v-grd-tot-cost + v-fg-cost   
            /* Do this when sell uom = "L" and first-of job */
            /*        v-tot-value = v-tot-value + v-fg-value  */
            /*        v-grd-tot-value = v-grd-tot-value + v-fg-value */
            v-msf[3]       = v-msf[3] + v-msf[1]
            v-msf[4]       = v-msf[4] + v-msf[2].

        /* Do not accumulate total for sell-uom = "L" */
        IF (v-new-job = YES AND lv-sell-uom = "L") OR (lv-sell-uom <> "L") THEN
            ASSIGN v-tot-value     = v-tot-value + v-fg-value
                v-grd-tot-value = v-grd-tot-value + v-fg-value.

        IF fg-rdtlh.rita-code EQ "R" OR
            fg-rdtlh.rita-code EQ "A" OR
            fg-rdtlh.rita-code EQ "E" THEN
            v-cum-tot  = v-cum-tot + v-fg-cost.
        ELSE
            IF fg-rdtlh.rita-code EQ "S" THEN
                v-cum-tot  = v-cum-tot - v-fg-cost.
    END.  /*   if v-pr-tots   */ 
    
    IF v-pr-tots THEN 
    DO:                                                              
        IF LAST-OF(tt-report.key-02) THEN 
        DO:
            PUT "-----------" TO v-tot-pos1
                "----------" TO v-tot-pos2
                "--------------" TO v-tot-pos3
                SKIP.                

            IF fg-rcpth.rita-code EQ "R" THEN
                PUT "MSF->  FG: " + trim(STRING(v-msf[3],"->>,>>9.9<<")) +
                    "  Wst: " + trim(STRING(v-msf[4],"->>,>>9.9<<"))    +
                    "  Tot: " + trim(STRING(v-msf[3] + v-msf[4],"->>,>>9.9<<"))
                    FORMAT "x(63)" AT 15.

            PUT v-tot-qty TO v-tot-pos1
                v-tot-cost TO v-tot-pos2
                v-tot-value TO v-tot-pos3 SKIP(1).

            ASSIGN
                v-msf[5]    = v-msf[5] + v-msf[3]
                v-msf[6]    = v-msf[6] + v-msf[4]
                v-tot-qty   = 0
                v-tot-cost  = 0
                v-tot-value = 0
                v-msf[3]    = 0
                v-msf[4]    = 0.
        END.  /* if last-of(fg-rcpth.i-no) */        
    END. /* if v-pr-tots */
END.
