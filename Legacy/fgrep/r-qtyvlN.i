DEFINE VARIABLE iCommited AS INTEGER.
DEFINE VARIABLE cLotNum AS CHARACTER NO-UNDO.
EMPTY TEMP-TABLE tt-cust.

FOR EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no GE fcst
    AND cust.cust-no LE tcst
    AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
    AND ttCustList.log-fld no-lock) else true),
    EACH itemfg WHERE itemfg.company = cocode
                       AND itemfg.cust-no = cust.cust-no
                       AND itemfg.cust-po-no >= fpo#
                       AND itemfg.cust-po-no <= tpo#
           NO-LOCK:
    RUN fg/fgSlsRep.p (INPUT itemfg.company,
        INPUT itemfg.cust-no,
        INPUT itemfg.part-no,
        INPUT itemfg.i-no,
        OUTPUT cSlsRep).
    IF      cSlsRep    GE fslm
        AND cSlsRep    LE tslm THEN 
    DO:
        FIND FIRST tt-cust WHERE tt-cust.cust-no EQ cust.cust-no
          NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-cust THEN DO:
        CREATE tt-cust.
        ASSIGN 
            tt-cust.cust-no  = cust.cust-no  
            tt-cust.cust-row = ROWID(cust)
            tt-cust.slsRep = cSlsRep.
        END.
        
    END.  
END.

FOR EACH tt-cust,
    FIRST cust
        WHERE ROWID(cust) EQ tt-cust.cust-row
        NO-LOCK
        BREAK BY cust.cust-no:

        {custom/statusMsg.i "'Processing Customer # ' + cust.cust-no"} 

      IF FIRST-OF(cust.cust-no) THEN
        ASSIGN
         v-frst     = YES
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0
         v-print    = NO.

      v-sell-price = 0 .

        FOR EACH itemfg WHERE itemfg.company = cocode
                           AND itemfg.cust-no = cust.cust-no
                           AND itemfg.cust-po-no >= fpo#
                           AND itemfg.cust-po-no <= tpo#
               NO-LOCK 
               BY itemfg.cust-no
               BY itemfg.i-no:


        RUN fg/fgSlsRep.p (INPUT itemfg.company,
            INPUT itemfg.cust-no,
            INPUT itemfg.part-no,
            INPUT itemfg.i-no,
            OUTPUT cSlsRep).
            
        IF NOT (cSlsRep    GE fslm
            AND cSlsRep    LE tslm) THEN 
            NEXT.
            
        /* This replaces the use of first-of(i-no) since using a next to skip items */
        IF itemfg.i-no NE cPrevIno OR itemfg.cust-no NE cPrevCust  THEN
          ASSIGN v-frst-i-no = YES.
        ASSIGN
          cPrevIno = itemfg.i-no
          cPrevCust = itemfg.cust-no.
          
          IF ((type NE "A") AND (type NE itemfg.i-code)) OR (type = "A") THEN
          DO:
            FOR EACH fg-bin NO-LOCK WHERE fg-bin.company = cocode AND
                                        fg-bin.i-no = itemfg.i-no
                                        USE-INDEX co-ino
                              BREAK BY fg-bin.loc
                                    BY fg-bin.job-no
                                    BY fg-bin.job-no2:

              IF (fg-bin.loc EQ "CUST" OR trim(fg-bin.cust-no) GT "") AND NOT v-custown THEN
                NEXT.
              ELSE
                v-qty-onh = v-qty-onh + fg-bin.qty.

              IF LAST-OF(fg-bin.loc) THEN
              DO:
                IF (v-qty-onh NE 0 ) OR (v-qty-onh EQ 0 AND zbal) THEN
                DO:
                  IF itemfg.sell-uom = "CS" AND itemfg.case-count NE 0 THEN
                    v-ext = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                  ELSE
                    FIND FIRST uom WHERE uom.uom = itemfg.sell-uom AND
                                        uom.mult NE 0 NO-LOCK NO-ERROR.
                  IF AVAILABLE uom THEN
                    v-ext = (v-qty-onh * itemfg.sell-price / uom.mult).
                  ELSE
                    v-ext = (v-qty-onh * itemfg.sell-price) / 1000.

                  IF itemfg.sell-uom = "L" THEN
                    v-ext = itemfg.sell-price.

                  FOR EACH xbin WHERE xbin.company = cocode AND
                                    xbin.i-no    = itemfg.i-no AND
                                    xbin.loc     = fg-bin.loc
                                    NO-LOCK BREAK BY xbin.job-no
                                                  BY xbin.job-no2:
                   IF FIRST-OF(xbin.job-no) OR first-of(xbin.job-no2) THEN
                      ASSIGN
                        v-qty-job = 0
                        v-ext-job = 0.

                   v-qty-job = v-qty-job + xbin.qty.

                   IF LAST-OF(xbin.job-no) OR last-of(xbin.job-no2) THEN
                   DO:
                      FIND FIRST xbin2 WHERE xbin2.company = cocode AND
                                             xbin2.i-no = itemfg.i-no AND
                                             xbin2.loc = fg-bin.loc AND
                                            (xbin2.job-no  <> xbin.job-no OR
                                             xbin2.job-no2 <> xbin.job-no2) AND
                                             xbin2.qty <> 0
                                             NO-LOCK NO-ERROR.
                      IF AVAILABLE xbin2 AND v-qty-job = 0 THEN
                         NEXT.
                      IF itemfg.sell-uom = "CS" AND
                         itemfg.case-count NE 0 THEN
                         v-ext-job = (v-qty-job * itemfg.sell-price) /
                                      itemfg.case-count.
                      ELSE
                         FIND FIRST uom WHERE uom.uom = itemfg.sell-uom AND
                                              uom.mult NE 0 NO-LOCK NO-ERROR.
                      IF AVAILABLE uom THEN
                         v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                      ELSE
                         v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                      IF itemfg.sell-uom = "L" THEN
                         v-ext-job = itemfg.sell-price.
                      
                      trans-date = ?.
/*                    for each fg-rcpth where fg-rcpth.company = cocode and      */
/*                                            fg-rcpth.i-no    = itemfg.i-no and */
/*                                            fg-rcpth.rita-code  = "R" and      */
/*                                            fg-rcpth.job-no  = xbin.job-no and */
/*                                            fg-rcpth.job-no2 = xbin.job-no2    */
/*                                            no-lock                            */
/*                                            break by fg-rcpth.trans-date:      */
/* /*                        if last-of(fg-rcpth.trans-date) then */             */
/*                       if FIRST-OF(fg-rcpth.trans-date) then                   */
/*                           trans-date = fg-rcpth.trans-date.                   */
/*                     end.  
                                                    */
                      cLotNum   = "".
                      IF AVAIL xbin THEN
                         IF TRIM(xbin.tag) NE "" THEN
                            FOR EACH b-rdtlh NO-LOCK WHERE b-rdtlh.company   EQ xbin.company
                                                       AND b-rdtlh.tag       EQ xbin.tag
                                                       AND b-rdtlh.rita-code EQ "R"
                                                       USE-INDEX tag,
                               EACH b-rcpth NO-LOCK WHERE b-rcpth.r-no      EQ b-rdtlh.r-no
                                                      AND b-rcpth.i-no      EQ xbin.i-no
                                                      AND b-rcpth.rita-code EQ b-rdtlh.rita-code 
                                                USE-INDEX r-no
                                                       BY b-rcpth.trans-date
                                                       BY b-rcpth.r-no:
                                  LEAVE.
                            END.
                            ELSE
                               IF TRIM(xbin.job-no) NE "" THEN
                                  FOR EACH b-rcpth NO-LOCK WHERE b-rcpth.company   EQ xbin.company
                                                             AND b-rcpth.job-no    EQ xbin.job-no
                                                             AND b-rcpth.job-no2   EQ xbin.job-no2
                                                             AND b-rcpth.i-no      EQ xbin.i-no
                                                             AND b-rcpth.rita-code EQ "R"
                                                       USE-INDEX job,
                                     EACH b-rdtlh NO-LOCK WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
                                                            AND b-rdtlh.rita-code EQ b-rcpth.rita-code
                                                             BY b-rcpth.trans-date
                                                             BY b-rcpth.r-no:
                                        LEAVE.
                                  END.

                         IF AVAIL b-rcpth THEN trans-date = b-rcpth.trans-date.
                         IF AVAIL b-rdtlh AND b-rdtlh.stack-code NE "" AND cLotNum EQ "" THEN
                           cLotNum  =  string(b-rdtlh.stack-code,"x(20)") .

                    IF LINE-COUNTER >= 56 THEN PAGE.
                    IF xbin.job-no = "" AND xbin.job-no2 = 0 THEN
                          v-job = "".
                       ELSE
                          v-job = xbin.job-no + "-" + string(xbin.job-no2).

                     FIND FIRST oe-ordl
                         WHERE oe-ordl.company EQ cocode
                           AND oe-ordl.job-no  EQ xbin.job-no
                           AND oe-ordl.job-no2 EQ xbin.job-no2
                           AND oe-ordl.i-no    EQ xbin.i-no
                         USE-INDEX job NO-LOCK NO-ERROR.

                     IF AVAIL oe-ordl THEN
                       v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
       

                     v-sales-rep = "" .

                     RUN fg/fgSlsRep.p (INPUT itemfg.company,
                         INPUT itemfg.cust-no,
                         INPUT itemfg.part-no,
                         INPUT itemfg.i-no,
                         OUTPUT v-sales-rep).
  
    
                     IF AVAIL oe-ordl THEN 
                         ASSIGN v-sell-price = (oe-ordl.t-price / oe-ordl.qty * 1000) .
                     ELSE 
                         ASSIGN v-sell-price = itemfg.sell-price .

                     ASSIGN iCommited = 0
                           /* cLotNum   = "" */.
                     FOR EACH oe-rel NO-LOCK WHERE oe-rel.company EQ oe-ordl.company
                         AND oe-rel.ord-no EQ oe-ordl.ord-no
                         AND oe-rel.i-no EQ oe-ordl.i-no 
                         AND oe-rel.LINE EQ oe-ordl.LINE 
                         AND oe-rel.link-no EQ 0 :
                         iCommited = iCommited + oe-rel.tot-qty .
                       /*  IF oe-rel.lot-no NE "" AND cLotNum EQ "" THEN
                           cLotNum  =  string(oe-rel.lot-no,"x(15)") . */
                     END.
                       

             ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".
            
             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "cust"       THEN cVarValue = STRING(cust.cust-no)  .                                                  
                          WHEN "po"         THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.po-no,"x(15)") ELSE STRING(itemfg.cust-po-no,"x(15)") .                           
                          WHEN "sman"       THEN cVarValue = STRING(v-sales-rep) .                                            
                          WHEN "itm"        THEN cVarValue = STRING(itemfg.i-no,"x(15)") .                                                                         
                          WHEN "cust-prt"   THEN cVarValue = STRING(itemfg.part-no,"x(15)") .                                                           
                          WHEN "dscr"       THEN cVarValue = STRING(itemfg.i-name,"x(15)") .                                                                                   
                          WHEN "job"        THEN cVarValue = STRING(v-job) .                                                                                         
                          WHEN "qty-oh"     THEN cVarValue = STRING(v-qty-job,"->>>,>>>,>>9") .                                                                            
                          WHEN "rcpt-dt"    THEN cVarValue = IF NOT(zbal AND v-qty-onh = 0) AND NOT(v-qty-job = 0) AND trans-date NE ? THEN STRING(trans-date) ELSE "" . 
                          WHEN "sel-prc"    THEN cVarValue = STRING(v-sell-price,">>,>>>,>>9.99") .   
                          WHEN "ttl-val"    THEN cVarValue = STRING(v-ext-job,"->>>,>>>,>>9.99") .
                          WHEN "commtd"     THEN cVarValue = STRING(iCommited,"->>,>>>,>>9") .  
                          WHEN "qty-case"   THEN cVarValue = STRING(itemfg.case-count,"->>,>>9") . 
                          WHEN "cust-lot"   THEN cVarValue = cLotNum . 

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

                     ASSIGN
                      v-tot-ext = v-tot-ext + v-ext-job
                      v-grand-tot-ext = v-grand-tot-ext + v-ext-job.
                   END. /* if last-of(... */
                END. /* for each xbin */

                IF v-frst-i-no THEN
                ASSIGN v-tot-ord = v-tot-ord + v-qty-ord
                       v-grand-tot-ord = v-grand-tot-ord + v-qty-ord
                       v-tot-ship = v-tot-ship + v-qty-ship
                       v-grand-tot-ship = v-grand-tot-ship + v-qty-ship
                       v-frst-i-no = NO.

                ASSIGN v-tot-onh = v-tot-onh + v-qty-onh
                       v-frst = NO
                       v-grand-tot-onh = v-grand-tot-onh + v-qty-onh
                       v-qty-onh = 0
                       v-qty-ship = 0
                       v-print = YES.


              END. /* qty onh */
              END. /* last of bin */
            END. /* for each bin */
          END. /* item type */
        END.  /* for each item */
     /* end.  /* rec date */*/

      IF v-print                      AND
         fcst NE tcst                 AND
         (v-tot-onh NE 0 OR zbal)     THEN DO:

          PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".
            
             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "cust"       THEN cVarValue = ""  .                                                  
                          WHEN "po"         THEN cVarValue = "" .                           
                          WHEN "sman"       THEN cVarValue = "" .                                            
                          WHEN "itm"        THEN cVarValue = "" .                                                                         
                          WHEN "cust-prt"   THEN cVarValue = "" .                                                           
                          WHEN "dscr"       THEN cVarValue = "" .                                                                                   
                          WHEN "job"        THEN cVarValue = "" .                                                                                         
                          WHEN "qty-oh"     THEN cVarValue = STRING(v-tot-onh,"->>>,>>>,>>9") .                                                                            
                          WHEN "rcpt-dt"    THEN cVarValue = "" . 
                          WHEN "sel-prc"    THEN cVarValue = "" .   
                          WHEN "ttl-val"    THEN cVarValue = STRING(v-tot-ext,"->>>,>>>,>>9.99") .
                          WHEN "commtd"     THEN cVarValue = "" .  
                          WHEN "qty-case"   THEN cVarValue = "" .
                          WHEN "cust-lot"   THEN cVarValue = "" . 
                     END CASE.
                       
                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
             END.
            
             PUT UNFORMATTED "   CUSTOMER TOTALS:" SUBSTRING(cDisplay,20,300) SKIP(1).
      END.

    END.  /* for each cust */

   
    PUT SKIP str-line SKIP .
     ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".
            
             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "cust"       THEN cVarValue = ""  .                                                  
                          WHEN "po"         THEN cVarValue = "" .                           
                          WHEN "sman"       THEN cVarValue = "" .                                            
                          WHEN "itm"        THEN cVarValue = "" .                                                                         
                          WHEN "cust-prt"   THEN cVarValue = "" .                                                           
                          WHEN "dscr"       THEN cVarValue = "" .                                                                                   
                          WHEN "job"        THEN cVarValue = "" .                                                                                         
                          WHEN "qty-oh"     THEN cVarValue = STRING(v-grand-tot-onh,"->>>,>>>,>>9") .                                                                            
                          WHEN "rcpt-dt"    THEN cVarValue = "" . 
                          WHEN "sel-prc"    THEN cVarValue = "" .   
                          WHEN "ttl-val"    THEN cVarValue = STRING(v-grand-tot-ext,"->>>,>>>,>>9.99") . 
                          WHEN "commtd"     THEN cVarValue = "" .  
                          WHEN "qty-case"   THEN cVarValue = "" .
                          WHEN "cust-lot"   THEN cVarValue = "" . 
                     END CASE.
                       
                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
             END.
            
             PUT UNFORMATTED "   GRAND TOTALS:" SUBSTRING(cDisplay,17,300) SKIP(1).
