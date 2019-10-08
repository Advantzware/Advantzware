
FOR EACH tt-report NO-LOCK,
    FIRST fg-bin NO-LOCK WHERE RECID(fg-bin) = tt-report.rec-id,
    FIRST itemfg NO-LOCK WHERE itemfg.company = cocode
                   AND itemfg.i-no = fg-bin.i-no
    BREAK BY itemfg.cust-no {1} BY tt-report.rct-date BY itemfg.i-no:
    
      STATUS DEFAULT "Printing Customer#/RctDate/FG Item#: " +
                     TRIM(itemfg.cust-no) + "/" +
                     TRIM(STRING(tt-report.rct-date)) + "/" +
                     TRIM(itemfg.i-no).    

    v-qty[1] = tt-report.qty.
    FIND FIRST tt-itemfg WHERE tt-itemfg.itemfg-row = rowid(itemfg) NO-LOCK NO-ERROR.
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ itemfg.cust-no NO-LOCK NO-ERROR.

          v-sales-rep = "" .
          IF AVAIL tt-itemfg THEN 
            v-sales-rep = tt-itemfg.slsrep.
        
      IF FIRST-OF(itemfg.cust-no) THEN
        ASSIGN
         v-frst  = YES
         v-print = NO.

    FIND LAST oe-ordl
            WHERE oe-ordl.company   EQ cocode
              AND oe-ordl.i-no      EQ fg-bin.i-no
              AND (oe-ordl.ord-no   EQ fg-bin.ord-no OR
                   (oe-ordl.job-no  EQ fg-bin.job-no AND
                    oe-ordl.job-no2 EQ fg-bin.job-no2))
            USE-INDEX item NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN
      ASSIGN v-po-no   = oe-ordl.po-no
             v-price   = oe-ordl.price
             v-uom     = oe-ordl.pr-uom
             v-cas-cnt = oe-ordl.cas-cnt.
    ELSE ASSIGN v-po-no   = itemfg.cust-po-no
                v-price   = itemfg.sell-price
                v-uom     = itemfg.sell-uom
                v-cas-cnt = itemfg.case-count.

    IF v-uom EQ "L" AND AVAIL oe-ordl THEN
            v-ext[1] = v-price / oe-ordl.qty * v-qty[1].
    ELSE IF v-uom EQ "CS"  AND v-cas-cnt NE 0 THEN
            v-ext[1] = (v-qty[1] * v-price) / v-cas-cnt.
    ELSE DO:
            v-ext[1] = v-qty[1] * v-price.
            FIND FIRST uom
                WHERE uom.uom  EQ v-uom
                  AND uom.mult NE 0
                NO-LOCK NO-ERROR.
            IF AVAIL uom THEN v-ext[1] = v-ext[1] / uom.mult.
    END.

    v-pallets = 0.

    IF v-qty[1] NE 0 OR vzer THEN DO:
          v-pallets = v-qty[1] /*fg-bin.qty*/ /
               ((IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count)   *
                (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet)).

          {sys/inc/roundup.i v-pallets}
          v-date = tt-report.tt-date.
          v-rct-date = tt-report.rct-date.
          v-ship-date = tt-report.ship-date.
          ASSIGN
              v-date2 = DATE(tt-report.key-06)
              v-date3 = DATE(tt-report.key-07) .

          vjob-no = fg-bin.job-no + "-" + string(fg-bin.job-no2) .

           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = STRING(itemfg.cust-no,"x(8)") .
                         WHEN "po"   THEN cVarValue = STRING(v-po-no,"x(15)").
                         WHEN "item"   THEN cVarValue = STRING(itemfg.i-no,"x(15)").
                         WHEN "part"  THEN cVarValue = STRING(itemfg.part-no,"x(15)") .
                         WHEN "desc"   THEN cVarValue = STRING(itemfg.i-name,"x(30)") .
                         WHEN "job"  THEN cVarValue = IF TRIM(vjob-no) NE "-0" THEN STRING(vjob-no,"x(10)") ELSE "" .
                         WHEN "qty-hand"   THEN cVarValue = STRING(v-qty[1],"->>,>>>,>>>") .
                         WHEN "pall"  THEN cVarValue = STRING(v-pallets,"->>>>>>") .
                         WHEN "sell-pr"    THEN cVarValue = STRING(v-price,"->>,>>9.99") .
                         WHEN "tot-val"   THEN cVarValue = STRING(v-ext[1],"->>,>>>,>>9.99").
                         WHEN "q-date"   THEN cVarValue = IF v-date2 NE ? THEN STRING(v-date2,"99/99/99") ELSE "".
                         WHEN "ord-date"  THEN cVarValue = IF v-date3 NE ? THEN STRING(v-date3,"99/99/99") ELSE "" .
                         WHEN "rec-date"   THEN cVarValue = IF v-rct-date NE ? THEN STRING(v-rct-date,"99/99/99") ELSE "" .
                         WHEN "ship-date"  THEN cVarValue = IF v-ship-date NE ? THEN STRING(v-ship-date,"99/99/99") ELSE "" .
                         WHEN "rep" THEN cVarValue = STRING(v-sales-rep,"x(3)").
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

          /*if vpcp then do:
            display itemfg.cust-no when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.part-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                    v-ship-date WHEN v-ship-date NE ?
                with frame itemx1.
            down with frame itemx1.
          end.
          else do:
            display itemfg.cust-no when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                    v-ship-date WHEN v-ship-date NE ?
                with frame itemx2.
            down with frame itemx2.
          end.*/
        
          v-frst = NO.

     /* IF tb_excel THEN  
            PUT STREAM excel UNFORMATTED
               '"' itemfg.cust-no '",' 
               '"' v-po-no '",'
               '"' itemfg.i-no '",' 
               '"' itemfg.part-no '",'
               '"' itemfg.i-name '",'
               '"' fg-bin.job-no '",'
               '"' fg-bin.job-no2 '",' 
               '"' v-qty[1] '",'
               '"' v-pallets '",'
               '"' v-price '",'
               '"' v-ext[1] '",'
               '"' v-date '",'
               '"' v-rct-date '",'
               '"' IF v-ship-date EQ ? THEN "" ELSE STRING(v-ship-date) '",'
          SKIP.*/
    END.

    ASSIGN v-qty[2] = v-qty[2] + v-qty[1]
                 v-ext[2] = v-ext[2] + v-ext[1]
                 v-qty[1] = 0
                 v-ext[1] = 0
                 v-print  = YES.
    ACCUMULATE v-pallets (TOTAL BY itemfg.cust-no).
    ACCUMULATE v-pallets (TOTAL).
    IF vzer AND NOT v-bin THEN DO:

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = STRING(itemfg.cust-no,"x(8)") .
                         WHEN "po"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = STRING(itemfg.i-no,"x(15)").
                         WHEN "part"  THEN cVarValue = STRING(itemfg.part-no,"x(15)") .
                         WHEN "desc"   THEN cVarValue = STRING(itemfg.i-name,"x(30)") .
                         WHEN "job"  THEN cVarValue = "" .
                         WHEN "qty-hand"   THEN cVarValue = "" .
                         WHEN "pall"  THEN cVarValue = "" .
                         WHEN "sell-pr"    THEN cVarValue = STRING(v-price,"->>>,>>9.99") .
                         WHEN "tot-val"   THEN cVarValue = "".
                         WHEN "q-date"   THEN cVarValue = "".
                         WHEN "ord-date"  THEN cVarValue = "" .
                         WHEN "rec-date"   THEN cVarValue = "" .
                         WHEN "ship-date"  THEN cVarValue = "" .
                         WHEN "rep" THEN cVarValue = STRING(v-sales-rep,"x(3)").
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
       /*if vpcp then do:
          display itemfg.cust-no        when v-frst
                  ""                    @ v-po-no
                  itemfg.i-no
                  itemfg.part-no
                  itemfg.i-name
                  0                     @ v-qty[1]
                  0                     @ v-pallets
                  itemfg.sell-price     @ v-price
                  0                     @ v-ext[1]
              with frame itemx1.
          down with frame itemx1.
       end.
       else do:
          display itemfg.cust-no        when v-frst
                  ""                    @ v-po-no
                  itemfg.i-no
                  itemfg.i-name
                  0                     @ v-qty[1]
                  0                     @ v-pallets
                  itemfg.sell-price     @ v-price
                  0                     @ v-ext[1]
              with frame itemx2.
          down with frame itemx2.
       end.*/
        
       v-frst = NO.
    END.

    IF LAST-OF(itemfg.cust-no) THEN DO:
       IF v-print AND (v-qty[2] NE 0 OR vzer) THEN DO:
         /*IF vpcp THEN
          put "-------" TO 108 "--------------" to 135 skip
              "Customer Total" at 76
              (ACCUM TOTAL BY itemfg.cust-no v-pallets) TO 108 FORM "->>>>>>9" 
              v-ext[2] to 135 format "->>,>>>,>>9.99"
              skip(1).
         ELSE
          put "-------" TO 92 "--------------" to 119 skip
              "Customer Total" at 60
              (ACCUM TOTAL BY itemfg.cust-no v-pallets) TO 92 FORM "->>>>>>9" 
              v-ext[2] to 119 format "->>,>>>,>>9.99"
              skip(1).*/
        PUT str-line SKIP.
       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "po"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "part"  THEN cVarValue = "" .
                         WHEN "desc"   THEN cVarValue = "" .
                         WHEN "job"  THEN cVarValue = "" .
                         WHEN "qty-hand"   THEN cVarValue = "" .
                         WHEN "pall"  THEN cVarValue = STRING((ACCUM TOTAL BY itemfg.cust-no v-pallets),"->>>>>>") .
                         WHEN "sell-pr"    THEN cVarValue = "" .
                         WHEN "tot-val"   THEN cVarValue = STRING(v-ext[2],"->>,>>>,>>9.99").
                         WHEN "q-date"   THEN cVarValue =  "".
                         WHEN "ord-date"  THEN cVarValue =  "" .
                         WHEN "rec-date"   THEN cVarValue = "" .
                         WHEN "ship-date"  THEN cVarValue =  "" .
                         WHEN "rep" THEN cVarValue = "".
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Customer Total" SUBSTRING(cDisplay,25,300) SKIP(1).
            /*IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.*/

    END.

        ASSIGN
         v-qty[3] = v-qty[3] + v-qty[2]
         v-ext[3] = v-ext[3] + v-ext[2]
         v-qty[2] = 0
         v-ext[2] = 0.
    END.

    IF LAST(itemfg.cust-no) AND (v-qty[3] NE 0 OR vzer) THEN DO:
      /*IF vpcp THEN
       put "-------" TO 108 "--------------" to 135 skip
            "   Grand Total" at 76
            (ACCUM TOTAL v-pallets) TO 108 FORM "->>>>>>9"
            v-ext[3] to 135 format "->>,>>>,>>9.99"
            skip(1).
      ELSE
       put "-------" TO 92 "--------------" to 119 skip
            "   Grand Total" at 60
            (ACCUM TOTAL v-pallets) TO 92 FORM "->>>>>>9"
            v-ext[3] to 119 format "->>,>>>,>>9.99"
            skip(1).*/
        PUT str-line SKIP.

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "po"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "part"  THEN cVarValue = "" .
                         WHEN "desc"   THEN cVarValue = "" .
                         WHEN "job"  THEN cVarValue = "" .
                         WHEN "qty-hand"   THEN cVarValue = "" .
                         WHEN "pall"  THEN cVarValue = STRING((ACCUM TOTAL v-pallets),"->>>>>>") .
                         WHEN "sell-pr"    THEN cVarValue = "" .
                         WHEN "tot-val"   THEN cVarValue = STRING(v-ext[3],"->>,>>>,>>9.99").
                         WHEN "q-date"   THEN cVarValue =  "".
                         WHEN "ord-date"  THEN cVarValue =  "" .
                         WHEN "rec-date"   THEN cVarValue = "" .
                         WHEN "ship-date"  THEN cVarValue =  "" .
                         WHEN "rep" THEN cVarValue = "".
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Grand Total" SUBSTRING(cDisplay,22,300) SKIP(1).
    END.
END.
