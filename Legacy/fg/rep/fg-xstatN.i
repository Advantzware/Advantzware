/*******************************************************************
  Program: fg-xstatN.i
 
   Author: Advanced Software
  Written:
  Updated: 01/18/07
  
Called by: r-stajobN.w

 *******************************************************************/
DEFINE VARIABLE chrCust   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrPoNo   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrINo    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrPart   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrName   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrJob    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrOrdQty AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrShpQty AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrQtyOH  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrPrice  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrTotV   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrSman   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrTransDte AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-prodqty AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-prodqty2 AS DECIMAL NO-UNDO .
DEFINE VARIABLE cSlsRep AS CHARACTER NO-UNDO .
DEFINE VARIABLE decPrice    AS DECIMAL    NO-UNDO.

DEFINE VARIABLE v-shipto AS CHARACTER NO-UNDO .
DEFINE VARIABLE v-shipto-name AS CHARACTER NO-UNDO .
DEFINE VARIABLE v-fg-lot AS CHARACTER NO-UNDO .

ASSIGN 
          v-shipto = ""
          v-shipto-name =  "" .
          

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
 EACH cust
   WHERE cust.company EQ cocode
     AND cust.cust-no EQ ttCustList.cust-no /*fcst*/
    /* and cust.cust-no le tcst*/
     /*and cust.sman    ge fslm
     and cust.sman    le tslm*/  /* task  06111510 */
   NO-LOCK
   BY cust.cust-no:

    
      FIND FIRST shipto WHERE shipto.company EQ cocode
          AND shipto.cust-no EQ cust.cust-no
          NO-LOCK NO-ERROR .

      IF AVAILABLE shipto THEN
          ASSIGN 
          v-shipto = shipto.ship-id
          v-shipto-name =  shipto.ship-name .

      ASSIGN
         v-frst     = YES
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0
         v-tot-allo = 0
         v-print    = NO
         v-fg-lot = "" .

     FOR EACH oe-ordl
            NO-LOCK
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.cust-no EQ cust.cust-no
              AND oe-ordl.po-no   GE fpo#
              AND oe-ordl.po-no   LE tpo#
              AND oe-ordl.job-no GE begin_job-no
              AND oe-ordl.job-no LE END_job-no,

           FIRST oe-ord 
                WHERE oe-ord.company EQ oe-ordl.company 
                  AND oe-ord.ord-no EQ oe-ordl.ord-no
                  AND ((oe-ord.opened EQ YES AND v-ocb NE "C") OR
                       (oe-ord.opened EQ NO  AND v-ocb NE "O"))
                NO-LOCK,
                   
           FIRST itemfg WHERE itemfg.company EQ cocode
                          AND itemfg.i-no    EQ oe-ordl.i-no
/*                           and itemfg.cust-no eq cust.cust-no */
                          AND (itemfg.i-code EQ typex OR typex EQ "A")
                        NO-LOCK

          /* for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
                           and itemfg.cust-po-no >= fpo#
                           and itemfg.cust-po-no <= tpo#
               no-lock*/ 
              BREAK BY IF v-sortby THEN oe-ordl.part-no ELSE oe-ordl.job-no 
                     BY IF NOT v-sortby THEN oe-ordl.job-no2 ELSE 0
                  BY itemfg.i-no
                  BY oe-ordl.i-no
                 /* by oe-ordl.job-no*/
                 /* by oe-ordl.job-no2*/ :

                  {custom/statusMsg.i " 'Processing Order#  '  + string(oe-ordl.ord-no) "}

                 RUN fg/fgSlsRep.p (INPUT itemfg.company,
                            INPUT itemfg.cust-no,
                            INPUT itemfg.part-no,
                            INPUT itemfg.i-no,
                            OUTPUT cSlsRep).
        
               FOR EACH tt-oe-rel NO-LOCK:
                   DELETE tt-oe-rel.
               END.
              
               v-sales-rep = "" .
               IF AVAILABLE cust AND cust.ACTIVE NE "X" AND cSlsRep EQ "" THEN DO:
                 FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                     AND cust-part.i-no = itemfg.i-no
                     AND cust-part.cust-no EQ cust.cust-no
                      NO-LOCK, 
                     FIRST reftable WHERE reftable.reftable = "cp-lab-p" 
                     AND reftable.company = cust-part.company  
                     AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK:
         
                     IF cust-part.spare-char-1 NE "" THEN DO:
                         FIND FIRST sman WHERE sman.company = itemfg.company
                             AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                         IF AVAILABLE sman THEN v-sales-rep = sman.sman.
                         LEAVE .
                     END.
                  END. /* end of cust-part */
         
                  IF AVAILABLE cust AND v-sales-rep EQ "" THEN DO:
                      FIND FIRST sman WHERE sman.company = cust.company
                          AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                      IF cSlsRep EQ "" AND AVAILABLE sman THEN v-sales-rep = sman.sman.
                  END.
               END.
               ELSE DO:
                   FIND FIRST sman WHERE sman.company = cust.company
                       AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                   IF cSlsRep EQ "" AND AVAILABLE sman THEN v-sales-rep = sman.sman.
               END.

               IF v-sales-rep EQ "" AND cSlsRep GT "" THEN 
               v-sales-rep = cSlsRep.

               IF  NOT(v-sales-rep    GE fslm
                     AND v-sales-rep    LE tslm) THEN NEXT .
                      
           v-int-rel = "".
           v-prodqty = 0 .

             ASSIGN
                 vmat-cost  = 0 
                 vmach-cost = 0
                 vtot-costm = 0 
                 vtot-job-cost = 0.
            
             IF oe-ordl.job-no <> ""  THEN DO:
                 FIND FIRST job-hdr WHERE job-hdr.company = cocode
                     AND job-hdr.job-no = oe-ordl.job-no
                     AND job-hdr.i-no   = oe-ordl.i-no NO-LOCK NO-ERROR.

                 IF AVAILABLE job-hdr THEN
                     ASSIGN
                     vtot-costm = job-hdr.std-mat-cost + job-hdr.std-lab-cost + job-hdr.std-fix-cost 
                                    + job-hdr.std-var-cost 
                     vtot-job-cost = vtot-costm * job-hdr.qty / 1000 .
             END.

           FOR EACH oe-rel
               WHERE oe-rel.company EQ oe-ordl.company
               AND oe-rel.ord-no  EQ oe-ordl.ord-no
               AND oe-rel.i-no    EQ oe-ordl.i-no
               AND oe-rel.line    EQ oe-ordl.line
               NO-LOCK:

/*               FIND FIRST reftable                                          */
/*                   WHERE reftable.reftable EQ "oe-rel.s-code"               */
/*                   AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999")*/
/*                   NO-LOCK NO-ERROR.                                        */
               
               FIND FIRST oe-rell NO-LOCK
                   WHERE oe-rell.company  EQ cocode
                   AND oe-rell.ord-no   EQ oe-rel.ord-no
                   AND oe-rell.rel-no   EQ oe-rel.rel-no
                   AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                   AND oe-rell.i-no     EQ oe-rel.i-no
                   AND oe-rell.line     EQ oe-rel.LINE NO-ERROR .
                  FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
                  
                  CREATE tt-oe-rel.
                  ASSIGN
                      tt-oe-rel.rel-date = STRING(oe-rel.rel-date)
                      tt-oe-rel.tot-qty  = IF oe-rel.qty GT 0 THEN oe-rel.qty ELSE oe-rel.tot-qty 
                      tt-oe-rel.link-no  = oe-rel.link-no
                      tt-oe-rel.po-no    = oe-rel.po-no.

                   ASSIGN tt-oe-rel.lot-no = oe-rel.lot-no.

                 v-prodqty2 = 0 .
                 
                 IF AVAILABLE tt-oe-rel AND tt-oe-rel.lot-no NE "" THEN DO:
                     FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                        WHERE fg-rcpth.company EQ oe-ordl.company
                       /* AND fg-rcpth.job-no EQ oe-ordl.job-no
                        AND fg-rcpth.job-no2 EQ oe-ordl.job-no2 */
                        AND fg-rcpth.i-no EQ oe-ordl.i-no
                        AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                        EACH fg-rdtlh FIELDS(qty) NO-LOCK
                        WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                        AND fg-rdtlh.stack-code EQ tt-oe-rel.lot-no:
                        v-prodqty = v-prodqty + fg-rdtlh.qty.
                        v-prodqty2 = v-prodqty2 + fg-rdtlh.qty.
                     END.

                     tt-oe-rel.qty = v-prodqty2 .
                 END. 
                 
                  IF AVAILABLE oe-relh  THEN 
                      ASSIGN  tt-oe-rel.rel-no   = oe-relh.release#.


                      IF LOOKUP(oe-rel.s-code, "S,I,L") GT 0 THEN
                          tt-oe-rel.link-no = tt-oe-rel.link-no.
                      ELSE DO:
                          IF LOOKUP(oe-rel.s-code, "B,A,P,C,Z") GT 0 THEN
                          tt-oe-rel.link-no = tt-oe-rel.rel-no.
                      END.
                  
           END.
           v-qty-allo = 0.
           IF oereordr-log OR oereordr-log EQ ? THEN
           RUN oe/oereordr.p (BUFFER itemfg, INPUT oereordr-log, OUTPUT v-qty-allo).
           ELSE v-qty-allo = itemfg.q-alloc. 

           RUN oe/ordlsqty.p (ROWID(oe-ordl), 
                              OUTPUT li-inv-qty, 
                              OUTPUT li-ship-qty).
           v-frst-ord = YES.
           v-qty-onh = 0.
           v-ext-job = 0.

           IF FIRST-OF(itemfg.i-no) THEN
             ASSIGN v-frst-i-no = YES.

          /* if ((typex ne "A") and (typex ne itemfg.i-code)) or (typex = "A") then
           do:*/
            IF rd_smry-dtl = "D" THEN DO:
               FOR EACH fg-bin NO-LOCK WHERE fg-bin.company = cocode 
                                         AND fg-bin.i-no = itemfg.i-no
                                         AND fg-bin.job-no GE /*begin_job-no*/ oe-ordl.job-no
                                         AND fg-bin.job-no LE /*end_job-no*/   oe-ordl.job-no
                                         AND fg-bin.job-no2 GE int(begin_job-no2)
                                         AND fg-bin.job-no2 LE int(end_job-no2)
                                        USE-INDEX co-ino
                                          BREAK  BY fg-bin.loc
                                                 BY fg-bin.job-no
                                                 BY fg-bin.job-no2 :
                            
                   IF (fg-bin.loc EQ "CUST" OR trim(fg-bin.cust-no) GT "") AND
                       NOT v-custown THEN
                     NEXT.
                   ELSE
                     v-qty-onh = v-qty-onh + fg-bin.qty.

                   IF LAST-OF(fg-bin.job-no2) THEN
                   DO:
                       IF (v-qty-onh NE 0 ) OR (v-qty-onh EQ 0 AND zbal) THEN
                       DO:
                          
                           IF itemfg.sell-uom = "CS" AND itemfg.case-count NE 0 THEN
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                           ELSE
                             FIND FIRST uom WHERE uom.uom = itemfg.sell-uom 
                                              AND uom.mult NE 0 
                                   NO-LOCK NO-ERROR.

                           IF AVAILABLE uom THEN
                             v-ext-job = (v-qty-onh * itemfg.sell-price / uom.mult).
                           ELSE
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / 1000.

                           IF itemfg.sell-uom = "L" THEN
                             v-ext-job = itemfg.sell-price.

                           FOR EACH xbin WHERE xbin.company = cocode 
                                           AND xbin.i-no    = itemfg.i-no 
                                           AND xbin.loc     = fg-bin.loc
                                           AND xbin.job-no GE  fg-bin.job-no
                                           AND xbin.job-no LE   fg-bin.job-no
                                           AND xbin.job-no2 GE  fg-bin.job-no2
                                           AND xbin.job-no2 LE  fg-bin.job-no2
                                          NO-LOCK BREAK BY xbin.job-no
                                                        BY xbin.job-no2:
                               IF FIRST-OF(xbin.job-no) OR 
                                  first-of(xbin.job-no2) THEN
                                 ASSIGN
                                   v-qty-job = 0
                                   v-ext-job = 0.

                               v-qty-job = v-qty-job + xbin.qty.

                               IF LAST-OF(xbin.job-no) OR 
                                  last-of(xbin.job-no2) THEN
                               DO:
                                   FIND FIRST xbin2 WHERE xbin2.company = cocode 
                                                      AND xbin2.i-no = itemfg.i-no 
                                                      AND xbin2.loc = fg-bin.loc 
                                                      AND (xbin2.job-no  <> xbin.job-no 
                                                           OR 
                                                           xbin2.job-no2 <> xbin.job-no2) 
                                                      AND xbin2.qty <> 0 
                                                    NO-LOCK NO-ERROR.
                                   IF AVAILABLE xbin2 AND v-qty-job = 0 THEN
                                     NEXT.

                                   IF itemfg.sell-uom = "CS" AND
                                      itemfg.case-count NE 0 THEN
                                     v-ext-job = 
                            (v-qty-job * itemfg.sell-price) / itemfg.case-count.
                                   ELSE
                                     FIND FIRST uom WHERE uom.uom = itemfg.sell-uom 
                                                      AND uom.mult NE 0 
                                           NO-LOCK NO-ERROR.

                                   IF AVAILABLE uom THEN
                                     v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                                   ELSE
                                     v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                                   IF itemfg.sell-uom = "L" THEN
                                     v-ext-job = itemfg.sell-price.

                                   
                                   ASSIGN
                                       trans-date = ?
                                       ship-date  = ? 
                                       qty-prod   = 0 .
                                   FOR EACH fg-rcpth 
                                       WHERE fg-rcpth.company = cocode 
                                       AND fg-rcpth.i-no    = itemfg.i-no 
                                       AND fg-rcpth.rita-code  = "R" 
                                       AND fg-rcpth.job-no  = oe-ordl.job-no 
                                       AND fg-rcpth.job-no2 = oe-ordl.job-no2
                                       NO-LOCK
                                       BREAK BY fg-rcpth.trans-date DESCENDING:
                                       trans-date = fg-rcpth.trans-date.
                                       LEAVE.
                                  END.

                                  IF oe-ordl.job-no NE '' THEN
                                      FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                                      WHERE fg-rcpth.company EQ oe-ordl.company
                                      AND fg-rcpth.job-no EQ oe-ordl.job-no
                                      AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                                      AND fg-rcpth.i-no EQ oe-ordl.i-no
                                      AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                                      EACH fg-rdtlh FIELDS(qty) NO-LOCK
                                      WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                                      qty-prod = qty-prod + fg-rdtlh.qty.
                                 END.

                                 ASSIGN v-fg-lot = "" .
                                 IF oe-ordl.job-no NE '' THEN
                                 FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                                      WHERE fg-rcpth.company EQ oe-ordl.company
                                      AND fg-rcpth.job-no EQ oe-ordl.job-no
                                      AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                                      AND fg-rcpth.i-no EQ oe-ordl.i-no
                                      AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                                      EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                                      WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                      AND fg-rdtlh.stack-code NE ""
                                      BREAK BY fg-rcpth.trans-date:
                                       v-fg-lot = fg-rdtlh.stack-code.
                                       LEAVE.
                                  END.

                                 FOR EACH oe-boll NO-LOCK
                                     WHERE oe-boll.company  EQ oe-ordl.company
                                     AND oe-boll.ord-no   EQ oe-ordl.ord-no
                                     AND oe-boll.i-no     EQ oe-ordl.i-no
                                     AND oe-boll.line     EQ oe-ordl.line
                                     USE-INDEX ord-no,
                                     FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK:
                                     ASSIGN ship-date = oe-bolh.bol-date .
                                END.
                                

                                /* v-job-no = "".
                                if oe-ordl.job-no ne "" then
                                v-job-no = trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99").
                                v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.*/
                                IF xbin.job-no = "" AND xbin.job-no2 = 0 THEN
                                    v-job-no = "".
                                ELSE
                                    v-job-no = STRING(xbin.job-no,"x(6)") + "-" + string(xbin.job-no2,"99").
                                    v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
                                    /* Task 11221301 */
                                    ASSIGN v-rfq = "" .                                    
                                    IF oe-ordl.est-no NE "" THEN
                                        FOR EACH quotehd WHERE quotehd.company = itemfg.company AND
                                        quotehd.loc = locode AND
                                        quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESCENDING:
                                        v-rfq = IF AVAILABLE quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                                       LEAVE.
                                    END.  /* Task 11221301 */
                                    
                                 ASSIGN cDisplay = ""
                                        cTmpField = ""
                                        cVarValue = ""
                                        cExcelDisplay = ""
                                        cExcelVarValue = "".
                                    v-summ-temp = 0 .
                                    STRING(v-row-id) = "".
                                    FOR EACH tt-oe-rel NO-LOCK:
                                        v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
                                   END.
                                  

                                   BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
                                   BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
                                   FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
                                   IF AVAILABLE tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
                                   DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                                       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                                       IF INDEX(cTmpField,".") > 0 THEN DO:
                                           cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                                           IF cTmpField = "Cust-no" OR
                                              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
                                           ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
                                                cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
                                                cDisplay = cDisplay + cTmpField + 
                                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                                                     .

                                                 cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
                                       END.
                                       ELSE DO:            
                                           CASE cTmpField:               
                                                 WHEN "v-job-no" THEN cVarValue = v-job-no.
                                                 WHEN "v-rel#" THEN cvarValue = IF AVAILABLE tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                                                 WHEN "v-relDate" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.rel-date) ELSE "".
                                                 WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                                                    STRING(v-qty-allo,"->>>>>>,>>9") .
                                                 WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                                                 WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                                                 WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                                                 WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                                                 WHEN "v-price" THEN cVarValue = STRING(itemfg.sell-price,"->>>,>>9.99").
                                                 WHEN "v-ext" THEN cVarValue = STRING(v-ext-job,"->>>,>>>,>>9.99").
                                                 WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                                                 WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                                                 WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                                                 WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                                                 WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                                                 WHEN "relqty" THEN cVarValue = (IF rd_smry-dtl = "D" AND AVAILABLE tt-oe-rel  THEN STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE STRING(v-summ-temp,"->>>>>>,>>9") ) .
                                                 WHEN "loc" THEN cVarValue = STRING(fg-bin.loc,"x(5)").
                                                 WHEN "relpo" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.po-no) ELSE "".
                                                 WHEN "rellot" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.lot-no) ELSE "".
                                                 WHEN "prodqty" THEN cVarValue = (IF rd_smry-dtl = "D" AND AVAILABLE tt-oe-rel  THEN STRING(tt-oe-rel.qty,"->>>,>>>,>>>,>>9") ELSE STRING(v-prodqty,"->>>,>>>,>>>,>>9")) .
                                                 WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                                                 WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                                                 WHEN "shipto" THEN cVarValue = v-shipto .
                                                 WHEN "shipname" THEN cVarValue = v-shipto-name  .
                                                 WHEN "fac-costm" THEN cVarValue = STRING(vtot-costm,"->>,>>>,>>9.99")  .
                                                 WHEN "tot-fac-cost" THEN cVarValue = IF v-job-no <> "" THEN STRING( v-qty-onh * vtot-costm ,"->>>>,>>>,>>9.99")  ELSE "" .
                                                 WHEN "ord-price" THEN cVarValue = STRING( oe-ordl.price,"->>>,>>9.99") .
                                                 WHEN "on-hand-cost" THEN DO: 
                                                     cVarValue = IF v-job-no <> "" THEN STRING( v-qty-onh / 1000 * vtot-costm ,"->,>>>,>>9.99") ELSE "" .
                                                 END.
                                                 
                                           END CASE.

                                           cExcelVarValue = cVarValue.  
                                           cDisplay = cDisplay + cVarValue +
                                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                                                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                                       END.
                                 END.

                                 PUT UNFORMATTED cDisplay SKIP.
                                 IF tb_excel THEN DO:
                                     PUT STREAM excel UNFORMATTED  
                                         cExcelDisplay SKIP.
                                 END.
                                 IF v-frst-i-no THEN DO:
                                     IF v-qty-onh NE ? THEN
                                         ASSIGN
                                         v-tot-onh        = v-tot-onh       + v-qty-onh
                                         v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh.
                                     IF v-ext-job NE ? THEN
                                         ASSIGN
                                         v-tot-ext        = v-tot-ext       + v-ext-job
                                         v-grand-tot-ext  = v-grand-tot-ext + v-ext-job.

                                     IF v-qty-allo NE ? THEN
                                         ASSIGN
                                         v-tot-allo = v-tot-allo + v-qty-allo
                                         v-grand-tot-allo = v-grand-tot-allo + v-qty-allo.
                                              /*if v-frst-ord then*/
                                 END.

                                 ASSIGN
                                    v-tot-ord        = v-tot-ord        + oe-ordl.qty
                                    v-tot-ship       = v-tot-ship       + li-ship-qty
                                    v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                                    v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
                                 ASSIGN v-qty-onh = 0
                                     li-ship-qty = 0 .
                                           

    IF rd_smry-dtl = "D" THEN DO:
       FOR EACH tt-oe-rel WHERE ROWID(tt-oe-rel) NE v-row-id NO-LOCK:               /*Task# 12021301*/
                               /* PUT SPACE(98)
                                    tt-oe-rel.rel-no SPACE(1)  
                                    tt-oe-rel.rel-date SPACE(5)
                                    tt-oe-rel.tot-qty SKIP.*/
           ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:   
                WHEN "cust.cust-no" THEN cVarValue = "".
                WHEN "oe-ordl.po-no" THEN cVarValue = "".
                WHEN "sman" THEN cVarValue = "" .
                WHEN "oe-ordl.i-no" THEN cVarValue = "".
                WHEN "oe-ordl.part-no" THEN cVarValue = "".
                WHEN "oe-ordl.i-name" THEN cVarValue = "".
                WHEN "v-job-no" THEN cVarValue = "".
                WHEN "v-rel#" THEN cvarValue = IF AVAILABLE tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .
                WHEN "v-relDate" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(itemfg.sell-price,"->>>,>>9.99").
                WHEN "ord-price" THEN cVarValue = STRING( oe-ordl.price,"->>>,>>9.99") .
                WHEN "v-ext" THEN cVarValue = STRING(v-ext-job,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE "".
                WHEN "loc" THEN cVarValue = "" .
                WHEN "relpo" THEN cVarValue =  IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue =  IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.qty,"->>>,>>>,>>>,>>9") ELSE "". 
                WHEN "fg-lot" THEN cVarValue = "" .
                WHEN "shipto" THEN cVarValue = "".
                WHEN "shipname" THEN cVarValue = "" . 
                WHEN "fac-costm" THEN cVarValue = ""  .
                WHEN "tot-fac-cost" THEN cVarValue = ""  . 
                WHEN "on-hand-cost" THEN cVarValue = "".
                                   
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
        
          ASSIGN v-frst  = NO
           v-print = YES.

            END.
          END. 
        END.
      END.
    END.
   END.
  END. /* for each fg-bin */
    IF NOT CAN-FIND(FIRST fg-bin WHERE fg-bin.company        EQ cocode
                                       AND fg-bin.i-no           EQ itemfg.i-no
                                       AND fg-bin.job-no         EQ oe-ordl.job-no
                                       AND fg-bin.job-no2        EQ oe-ordl.job-no2
                                       AND (v-custown OR 
                                            (fg-bin.loc NE "CUST" AND trim(fg-bin.cust-no) EQ "")))
      THEN DO:

        IF itemfg.sell-uom   EQ "CS" AND
                   itemfg.case-count NE 0    THEN
                  v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                ELSE IF itemfg.sell-uom EQ "L" THEN 
                  v-ext = oe-ordl.price.
                ELSE
                DO:
                    FIND FIRST uom WHERE uom.uom  EQ itemfg.sell-uom
                                     AND uom.mult NE 0
                                   NO-LOCK NO-ERROR.

                    v-ext = v-qty-onh * oe-ordl.price / (IF AVAILABLE uom THEN uom.mult ELSE 1000).
                END.              
           /* END.*/
            /*if first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) then*/
                                      ASSIGN
                                        v-tot-onh        = v-tot-onh       + v-qty-onh
                                        v-tot-ext        = v-tot-ext       + v-ext
                                        v-tot-allo = v-tot-allo + v-qty-allo
                                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                                        v-grand-tot-ext  = v-grand-tot-ext + v-ext
                                        v-grand-tot-allo = v-grand-tot-allo + v-qty-allo.
                                    /*if v-frst-ord then*/
                                      ASSIGN
                                        v-tot-ord        = v-tot-ord        + oe-ordl.qty
                                        v-tot-ship       = v-tot-ship       + li-ship-qty
                                        v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                                        v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
            ASSIGN
                trans-date = ?
                ship-date  = ? 
                qty-prod   = 0 .
            FOR EACH fg-rcpth 
                         WHERE fg-rcpth.company = cocode 
                           AND fg-rcpth.i-no    = itemfg.i-no 
                           AND fg-rcpth.rita-code  = "R" 
                           AND fg-rcpth.job-no  = oe-ordl.job-no 
                           AND fg-rcpth.job-no2 = oe-ordl.job-no2
                             NO-LOCK
                             BREAK BY fg-rcpth.trans-date DESCENDING:
                 trans-date = fg-rcpth.trans-date.
                 LEAVE.
            END.
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company EQ oe-ordl.company
                AND fg-rcpth.job-no EQ oe-ordl.job-no
                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                AND fg-rcpth.i-no EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                qty-prod = qty-prod + fg-rdtlh.qty.
            END.

            ASSIGN v-fg-lot = "" .
            IF oe-ordl.job-no NE '' THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company EQ oe-ordl.company
                 AND fg-rcpth.job-no EQ oe-ordl.job-no
                 AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                 AND fg-rcpth.i-no EQ oe-ordl.i-no
                 AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                 EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                 WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 AND fg-rdtlh.stack-code NE ""
                 BREAK BY fg-rcpth.trans-date:
                  v-fg-lot = fg-rdtlh.stack-code.
                  LEAVE.
             END.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company  EQ oe-ordl.company
                AND oe-boll.ord-no   EQ oe-ordl.ord-no
                AND oe-boll.i-no     EQ oe-ordl.i-no
                AND oe-boll.line     EQ oe-ordl.line
                USE-INDEX ord-no,
                FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK:
                ASSIGN ship-date = oe-bolh.bol-date .
            END.
           
            v-job-no = "".
            IF oe-ordl.job-no NE "" THEN
                  v-job-no = STRING(oe-ordl.job-no,"x(6)") + "-" + string(oe-ordl.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
            /* Task 11221301 */
            ASSIGN v-rfq = "" .                                    
            IF oe-ordl.est-no NE "" THEN
                FOR EACH quotehd WHERE quotehd.company = itemfg.company AND
                                   quotehd.loc = locode AND
                                   quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESCENDING:
                v-rfq = IF AVAILABLE quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                LEAVE.
                END.  /* Task 11221301 */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
           v-summ-temp = 0 .
           STRING(v-row-id) = "".
       FOR EACH tt-oe-rel NO-LOCK:
           v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
       END.
       

        BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
        BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
        FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
        IF AVAILABLE tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" OR
              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
          cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "v-job-no" THEN cVarValue = v-job-no.
                WHEN "v-rel#" THEN cvarValue = IF AVAILABLE tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                WHEN "v-relDate" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
                WHEN "ord-price" THEN cVarValue = STRING( oe-ordl.price,"->>>,>>9.99") .
                WHEN "v-ext" THEN cVarValue = STRING(v-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = (IF rd_smry-dtl = "D" AND AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE STRING(v-summ-temp,"->>>>>>,>>9") ) .
                WHEN "loc" THEN cVarValue = STRING(itemfg.loc,"x(5)").   
                WHEN "relpo" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = STRING(v-prodqty,"->>>,>>>,>>>,>>9") .
                WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                WHEN "shipto" THEN cVarValue = v-shipto .
                WHEN "shipname" THEN cVarValue = v-shipto-name .
                WHEN "fac-costm" THEN cVarValue = IF vtot-costm <> 0 THEN STRING(vtot-costm,"->>,>>>,>>9.99") ELSE "" .
                WHEN "tot-fac-cost" THEN cVarValue = IF v-job-no <> "" THEN STRING(v-qty-onh * vtot-costm,"->>>>,>>>,>>9.99")  ELSE "" .
                WHEN "on-hand-cost" THEN DO: 
                    cVarValue = IF v-job-no <> "" THEN STRING( v-qty-onh / 1000 * vtot-costm,"->,>>>,>>9.99") ELSE "" .
                END.

            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
    IF rd_smry-dtl = "D" THEN DO:
       FOR EACH tt-oe-rel WHERE ROWID(tt-oe-rel) NE v-row-id NO-LOCK:               /*Task# 12021301*/
                               /* PUT SPACE(98)
                                    tt-oe-rel.rel-no SPACE(1)  
                                    tt-oe-rel.rel-date SPACE(5)
                                    tt-oe-rel.tot-qty SKIP.*/
           ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:   
                WHEN "cust.cust-no" THEN cVarValue = "".
                WHEN "oe-ordl.po-no" THEN cVarValue = "".
                WHEN "sman" THEN cVarValue = "" .
                WHEN "oe-ordl.i-no" THEN cVarValue = "".
                WHEN "oe-ordl.part-no" THEN cVarValue = "".
                WHEN "oe-ordl.i-name" THEN cVarValue = "".
                WHEN "v-job-no" THEN cVarValue = "".
                WHEN "v-rel#" THEN cvarValue = IF AVAILABLE tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .
                WHEN "v-relDate" THEN cVarValue = STRING(tt-oe-rel.rel-date).
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
                WHEN "ord-price" THEN cVarValue = STRING( oe-ordl.price,"->>>,>>9.99") .
                WHEN "v-ext" THEN cVarValue = STRING(v-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9").
                WHEN "loc" THEN cVarValue = "" .
                WHEN "relpo" THEN cVarValue =  "".
                WHEN "rellot" THEN cVarValue = "".
                WHEN "prodqty" THEN cVarValue = "" .
                WHEN "fg-lot" THEN cVarValue = "".
                WHEN "shipto" THEN cVarValue =  "".
                WHEN "shipname" THEN cVarValue = "" .
                WHEN "fac-costm" THEN cVarValue = ""  .
                WHEN "tot-fac-cost" THEN cVarValue = ""  .
                WHEN "on-hand-cost" THEN cVarValue = "".
                                   
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
      END.
    END. 

      END.  /* not find fg-bin */  
 END. /* if datail*/
   ELSE DO:
                     FOR EACH fg-bin NO-LOCK WHERE fg-bin.company = cocode 
                                         AND fg-bin.i-no = itemfg.i-no
                                         AND fg-bin.job-no GE /*begin_job-no*/ oe-ordl.job-no 
                                         AND fg-bin.job-no LE /*end_job-no  */ oe-ordl.job-no 
                                         AND fg-bin.job-no2 GE int(begin_job-no2)
                                         AND fg-bin.job-no2 LE int(end_job-no2)
                                          USE-INDEX co-ino
                                          BREAK /*by fg-bin.loc*/
                                             BY fg-bin.job-no
                                             BY fg-bin.job-no2 :
                            
                   IF (fg-bin.loc EQ "CUST" OR trim(fg-bin.cust-no) GT "") AND
                       NOT v-custown THEN
                     NEXT.
                   ELSE
                     v-qty-onh = v-qty-onh + fg-bin.qty.

                   IF LAST-OF(fg-bin.job-no2) THEN
                   DO:
                       IF (v-qty-onh NE 0 ) OR (v-qty-onh EQ 0 AND zbal) THEN
                       DO:
                          
                           IF itemfg.sell-uom = "CS" AND itemfg.case-count NE 0 THEN
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                           ELSE
                             FIND FIRST uom WHERE uom.uom = itemfg.sell-uom 
                                              AND uom.mult NE 0 
                                   NO-LOCK NO-ERROR.

                           IF AVAILABLE uom THEN
                             v-ext-job = (v-qty-onh * itemfg.sell-price / uom.mult).
                           ELSE
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / 1000.

                           IF itemfg.sell-uom = "L" THEN
                             v-ext-job = itemfg.sell-price.

                           FOR EACH xbin WHERE xbin.company = cocode 
                                          AND xbin.i-no    =  itemfg.i-no 
                                          AND xbin.job-no  GE fg-bin.job-no
                                          AND xbin.job-no  LE fg-bin.job-no
                                          AND xbin.job-no2 GE fg-bin.job-no2
                                          AND xbin.job-no2 LE fg-bin.job-no2
                                          NO-LOCK BREAK BY xbin.job-no
                                                        BY xbin.job-no2:
                               IF FIRST-OF(xbin.job-no) OR 
                                  first-of(xbin.job-no2) THEN
                                 ASSIGN
                                   v-qty-job = 0
                                   v-ext-job = 0.

                               v-qty-job = v-qty-job + xbin.qty.

                               IF LAST-OF(xbin.job-no) OR 
                                  last-of(xbin.job-no2) THEN
                               DO:
                                   FIND FIRST xbin2 WHERE xbin2.company = cocode 
                                                      AND xbin2.i-no = itemfg.i-no 
                                                      AND xbin2.loc = fg-bin.loc 
                                                      AND (xbin2.job-no  <> xbin.job-no 
                                                           OR 
                                                           xbin2.job-no2 <> xbin.job-no2) 
                                                      AND xbin2.qty <> 0 
                                                    NO-LOCK NO-ERROR.
                                   IF AVAILABLE xbin2 AND v-qty-job = 0 THEN
                                     NEXT.

                                   IF itemfg.sell-uom = "CS" AND
                                      itemfg.case-count NE 0 THEN
                                     v-ext-job = 
                            (v-qty-job * itemfg.sell-price) / itemfg.case-count.
                                   ELSE
                                     FIND FIRST uom WHERE uom.uom = itemfg.sell-uom 
                                                      AND uom.mult NE 0 
                                           NO-LOCK NO-ERROR.

                                   IF AVAILABLE uom THEN
                                     v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                                   ELSE
                                     v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                                   IF itemfg.sell-uom = "L" THEN
                                     v-ext-job = itemfg.sell-price.
                                   
                                     
            ASSIGN
                trans-date = ?
                ship-date  = ? 
                qty-prod   = 0 .
            FOR EACH fg-rcpth 
                         WHERE fg-rcpth.company = cocode 
                           AND fg-rcpth.i-no    = itemfg.i-no 
                           AND fg-rcpth.rita-code  = "R" 
                           AND fg-rcpth.job-no  = oe-ordl.job-no 
                           AND fg-rcpth.job-no2 = oe-ordl.job-no2
                             NO-LOCK
                             BREAK BY fg-rcpth.trans-date DESC:
                 trans-date = fg-rcpth.trans-date.
                 LEAVE.
            END.
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company EQ oe-ordl.company
                AND fg-rcpth.job-no EQ oe-ordl.job-no
                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                AND fg-rcpth.i-no EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                qty-prod = qty-prod + fg-rdtlh.qty.
            END.

            ASSIGN v-fg-lot = "" .
            IF oe-ordl.job-no NE '' THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company EQ oe-ordl.company
                 AND fg-rcpth.job-no EQ oe-ordl.job-no
                 AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                 AND fg-rcpth.i-no EQ oe-ordl.i-no
                 AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                 EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                 WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 AND fg-rdtlh.stack-code NE ""
                 BREAK BY fg-rcpth.trans-date:
                  v-fg-lot = fg-rdtlh.stack-code.
                  LEAVE.
             END.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company  EQ oe-ordl.company
                AND oe-boll.ord-no   EQ oe-ordl.ord-no
                AND oe-boll.i-no     EQ oe-ordl.i-no
                AND oe-boll.line     EQ oe-ordl.line
                USE-INDEX ord-no,
                FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK:
                ASSIGN ship-date = oe-bolh.bol-date .
            END.
          
           /* v-job-no = "".
            if oe-ordl.job-no ne "" then
                  v-job-no = trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.*/
            IF xbin.job-no = "" AND xbin.job-no2 = 0 THEN
                                     v-job-no = "".
                                   ELSE
                                     v-job-no = STRING(xbin.job-no,"x(6)") + "-" + string(xbin.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
            /* Task 11221301 */
            ASSIGN v-rfq = "" .                                    
            IF oe-ordl.est-no NE "" THEN
                FOR EACH quotehd WHERE quotehd.company = itemfg.company AND
                                   quotehd.loc = locode AND
                                   quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESCENDING:
                v-rfq = IF AVAILABLE quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                LEAVE.
                END.  /* Task 11221301 */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
           v-summ-temp = 0 .
           STRING(v-row-id) = "".
       FOR EACH tt-oe-rel NO-LOCK:
           v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
       END.
       

        BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
        BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
        FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
        IF AVAILABLE tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" OR
              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
          cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "v-job-no" THEN cVarValue = v-job-no.
                WHEN "v-rel#" THEN cvarValue = IF AVAILABLE tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                WHEN "v-relDate" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(itemfg.sell-price,"->>>,>>9.99").
                WHEN "ord-price" THEN cVarValue = STRING( oe-ordl.price,"->>>,>>9.99") .
                WHEN "v-ext" THEN cVarValue = STRING(v-ext-job,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = STRING(v-summ-temp,"->>>>>>,>>9") .
                WHEN "loc" THEN cVarValue = STRING(fg-bin.loc,"x(5)").
                WHEN "relpo" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = STRING(v-prodqty,"->>>,>>>,>>>,>>9") .
                WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                WHEN "shipto" THEN cVarValue = v-shipto.
                WHEN "shipname" THEN cVarValue = v-shipto-name .
                WHEN "fac-costm" THEN cVarValue = IF vtot-costm <> 0 THEN STRING(vtot-costm,"->>,>>>,>>9.99") ELSE "" .
                WHEN "tot-fac-cost" THEN cVarValue = IF vtot-job-cost <> 0 THEN STRING(vtot-job-cost,"->>>>,>>>,>>9.99") ELSE ""  .
                WHEN "on-hand-cost" THEN DO: 
                    cVarValue = IF v-job-no <> "" THEN STRING( ROUND( v-qty-onh / 1000 * vtot-costm,2),"->,>>>,>>9.99") ELSE "" .
                END.
                                   
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
    IF v-frst-i-no THEN DO:
         IF v-qty-onh NE ? THEN
             ASSIGN
             v-tot-onh        = v-tot-onh       + v-qty-onh
             v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh .
         IF v-ext-job NE ?  THEN
             ASSIGN
             v-tot-ext        = v-tot-ext       + v-ext-job
             v-grand-tot-ext  = v-grand-tot-ext + v-ext-job.
         IF v-qty-allo NE ? THEN
             ASSIGN
             v-tot-allo = v-tot-allo + v-qty-allo
             v-grand-tot-allo = v-grand-tot-allo + v-qty-allo.
     /*if v-frst-ord then*/
    END.

     ASSIGN
         v-tot-ord        = v-tot-ord        + oe-ordl.qty
         v-tot-ship       = v-tot-ship       + li-ship-qty
         v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
         v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
      
     ASSIGN v-qty-onh = 0
            li-ship-qty = 0 .


         END.
        END.
      END.
    END.
   END. /* end of fg-bin */
    IF NOT CAN-FIND(FIRST fg-bin WHERE fg-bin.company        EQ cocode
                                       AND fg-bin.i-no           EQ itemfg.i-no
                                       AND fg-bin.job-no         EQ oe-ordl.job-no
                                       AND fg-bin.job-no2        EQ oe-ordl.job-no2
                                       AND (v-custown OR 
                                            (fg-bin.loc NE "CUST" AND trim(fg-bin.cust-no) EQ "")))
      THEN DO:

        IF itemfg.sell-uom   EQ "CS" AND
                   itemfg.case-count NE 0    THEN
                  v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                ELSE IF itemfg.sell-uom EQ "L" THEN 
                  v-ext = oe-ordl.price.
                ELSE
                DO:
                    FIND FIRST uom WHERE uom.uom  EQ itemfg.sell-uom
                                     AND uom.mult NE 0
                                   NO-LOCK NO-ERROR.

                    v-ext = v-qty-onh * oe-ordl.price / (IF AVAILABLE uom THEN uom.mult ELSE 1000).
                END.              
           /* END.*/
            /*if first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) then*/
                                      ASSIGN
                                        v-tot-onh        = v-tot-onh       + v-qty-onh
                                        v-tot-ext        = v-tot-ext       + v-ext
                                        v-tot-allo = v-tot-allo + v-qty-allo
                                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                                        v-grand-tot-ext  = v-grand-tot-ext + v-ext
                                        v-grand-tot-allo = v-grand-tot-allo + v-qty-allo.
                                    /*if v-frst-ord then*/
                                      ASSIGN
                                        v-tot-ord        = v-tot-ord        + oe-ordl.qty
                                        v-tot-ship       = v-tot-ship       + li-ship-qty
                                        v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                                        v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
            ASSIGN
                trans-date = ?
                ship-date  = ? 
                qty-prod   = 0 .
            FOR EACH fg-rcpth 
                         WHERE fg-rcpth.company = cocode 
                           AND fg-rcpth.i-no    = itemfg.i-no 
                           AND fg-rcpth.rita-code  = "R" 
                           AND fg-rcpth.job-no  = oe-ordl.job-no 
                           AND fg-rcpth.job-no2 = oe-ordl.job-no2
                             NO-LOCK
                             BREAK BY fg-rcpth.trans-date DESC:
                 trans-date = fg-rcpth.trans-date.
                 LEAVE.
            END.
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company EQ oe-ordl.company
                AND fg-rcpth.job-no EQ oe-ordl.job-no
                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                AND fg-rcpth.i-no EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                qty-prod = qty-prod + fg-rdtlh.qty.
            END.

            ASSIGN v-fg-lot = "" .
            IF oe-ordl.job-no NE '' THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company EQ oe-ordl.company
                 AND fg-rcpth.job-no EQ oe-ordl.job-no
                 AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                 AND fg-rcpth.i-no EQ oe-ordl.i-no
                 AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                 EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                 WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 AND fg-rdtlh.stack-code NE ""
                 BREAK BY fg-rcpth.trans-date:
                  v-fg-lot = fg-rdtlh.stack-code.
                  LEAVE.
             END.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company  EQ oe-ordl.company
                AND oe-boll.ord-no   EQ oe-ordl.ord-no
                AND oe-boll.i-no     EQ oe-ordl.i-no
                AND oe-boll.line     EQ oe-ordl.line
                USE-INDEX ord-no,
                FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK:
                ASSIGN ship-date = oe-bolh.bol-date .
            END.
            
            v-job-no = "".
            IF oe-ordl.job-no NE "" THEN
                  v-job-no = STRING(oe-ordl.job-no,"x(6)") + "-" + string(oe-ordl.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
            /* Task 11221301 */
            ASSIGN v-rfq = "" .                                    
            IF oe-ordl.est-no NE "" THEN
                FOR EACH quotehd WHERE quotehd.company = itemfg.company AND
                                   quotehd.loc = locode AND
                                   quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESCENDING:
                v-rfq = IF AVAILABLE quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                LEAVE.
                END.  /* Task 11221301 */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
           v-summ-temp = 0 .
           STRING(v-row-id) = "".
       FOR EACH tt-oe-rel NO-LOCK:
           v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
       END.
       

        BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
        BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
        FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
        IF AVAILABLE tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" OR
              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
          cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "v-job-no" THEN cVarValue = v-job-no.
                WHEN "v-rel#" THEN cvarValue = IF AVAILABLE tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                WHEN "v-relDate" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
                WHEN "ord-price" THEN cVarValue = STRING( oe-ordl.price,"->>>,>>9.99") .
                WHEN "v-ext" THEN cVarValue = STRING(v-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = STRING(v-summ-temp,"->>>>>>,>>9") .
                WHEN "loc" THEN cVarValue = STRING(itemfg.loc,"x(5)"). 
                WHEN "relpo" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue = IF AVAILABLE tt-oe-rel THEN STRING(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = STRING(v-prodqty,"->>>,>>>,>>>,>>9") .  
                WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                WHEN "shipto" THEN cVarValue = v-shipto .
                WHEN "shipname" THEN cVarValue = v-shipto-name .
                WHEN "fac-costm" THEN cVarValue = IF vtot-costm <> 0 THEN STRING(vtot-costm,"->>,>>>,>>9.99") ELSE "" .
                WHEN "tot-fac-cost" THEN cVarValue = IF vtot-job-cost <> 0 THEN STRING(vtot-job-cost,"->>>>,>>>,>>9.99") ELSE ""  .
                WHEN "on-hand-cost" THEN DO: 
                    cVarValue = IF v-job-no <> "" THEN STRING( v-qty-onh / 1000 * vtot-costm,"->,>>>,>>9.99") ELSE ""  .
                END.
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
  END.  /* not find fg-bin */  
 END.  /*ENd of summery */
             
 
          
   END.  /* for each item */  
   

    IF v-print                      AND
       fcst NE tcst                 AND
       (v-tot-onh NE 0 OR zbal)     THEN DO:
         /* put "-----------"         to 132
              "----------"          to 145
              "------------"        to 158
              "------------"        to 170
              "--------------"      to 210 skip
              "CUSTOMER TOTALS:"    to 62
              v-tot-ord             to 132
              v-tot-allo  FORM "->>>,>>>,>>9"   to 145
              v-tot-ship            to 158
              v-tot-onh             to 170
              v-tot-ext             to 210
              skip(1).*/
          
        PUT    SKIP  str-line SKIP .

          ASSIGN cDisplay = ""                                  /*Task# 12021301*/
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:   
                WHEN "cust.cust-no" THEN cVarValue = "".
                WHEN "oe-ordl.po-no" THEN cVarValue = "".
                WHEN "sman" THEN cVarValue = "".
                WHEN "oe-ordl.i-no" THEN cVarValue = "".
                WHEN "oe-ordl.part-no" THEN cVarValue = "".
                WHEN "oe-ordl.i-name" THEN cVarValue = "".
                WHEN "v-job-no" THEN cVarValue = "".
                WHEN "v-rel#" THEN cvarValue = "".
                WHEN "v-relDate" THEN cVarValue = "".
                WHEN "v-relQty" THEN cVarValue = STRING(v-tot-allo,"->>>>>>,>>9").
                WHEN "v-rctDate" THEN cVarValue = "".
                WHEN "v-qty-onh" THEN cVarValue = STRING(v-tot-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(v-tot-ship,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(v-tot-ord,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = "".
                WHEN "ord-price" THEN cVarValue = "" .
                WHEN "v-ext" THEN cVarValue = STRING(v-tot-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = "".
                WHEN "qty-pro" THEN cVarValue = "".
                WHEN "qty-bal" THEN cVarValue = "".
                WHEN "ord-date" THEN cVarValue = "".
                WHEN "ship-date" THEN cVarValue = "".
                WHEN "relqty" THEN cVarValue = "".
                WHEN "relpo" THEN cVarValue = "".
                WHEN "rellot" THEN cVarValue = "".
                WHEN "prodqty" THEN cVarValue = ""  .
                WHEN "fg-lot" THEN cVarValue = "" .
                WHEN "shipto" THEN cVarValue = "".
                WHEN "shipname" THEN cVarValue = "" .  
                WHEN "fac-costm" THEN cVarValue = "" .
                WHEN "tot-fac-cost" THEN cVarValue = ""  .
                WHEN "on-hand-cost" THEN cVarValue = "" .
                                   
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
        END.
            PUT UNFORMATTED "        CUSTOMER TOTALS:" SUBSTRING(cDisplay,25,300) SKIP(1).

    END. /*(v-tot-onh ne 0 or zbal)*/

END.  /* for each cust */
     
       /*   put "-----------"        to 132
              "-----------"        to 145
              "-----------"        to 158
              "-----------"        to 170
              "---------------"      to 210 skip
              "GRAND TOTALS:"       to 62
              v-grand-tot-ord       to 132
              v-grand-tot-allo   FORM "->>>,>>>,>>9"   to 145
              v-grand-tot-ship      to 158
              v-grand-tot-onh       to 170
              v-grand-tot-ext       to 210
              skip(1).*/
PUT    SKIP  str-line SKIP .

ASSIGN cDisplay = ""                                                    /*Task# 12021301*/
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:   
                WHEN "cust.cust-no" THEN cVarValue = "".
                WHEN "oe-ordl.po-no" THEN cVarValue = "".
                WHEN "sman" THEN cVarValue = "".
                WHEN "oe-ordl.i-no" THEN cVarValue = "".
                WHEN "oe-ordl.part-no" THEN cVarValue = "".
                WHEN "oe-ordl.i-name" THEN cVarValue = "".
                WHEN "v-job-no" THEN cVarValue = "".
                WHEN "v-rel#" THEN cvarValue = "".
                WHEN "v-relDate" THEN cVarValue = "".
                WHEN "v-relQty" THEN cVarValue = STRING(v-grand-tot-allo,"->>>>>>,>>9").
                WHEN "v-rctDate" THEN cVarValue = "".
                WHEN "v-qty-onh" THEN cVarValue = STRING(v-grand-tot-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(v-grand-tot-ship,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(v-grand-tot-ord,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = "".
                WHEN "ord-price" THEN cVarValue = "" .
                WHEN "v-ext" THEN cVarValue = STRING(v-grand-tot-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = "".
                WHEN "qty-pro" THEN cVarValue = "".
                WHEN "qty-bal" THEN cVarValue = "".
                WHEN "ord-date" THEN cVarValue = "".
                WHEN "ship-date" THEN cVarValue = "".
                WHEN "relqty" THEN cVarValue = "".
                WHEN "relpo" THEN cVarValue = "".
                WHEN "rellot" THEN cVarValue = "".
                WHEN "prodqty" THEN cVarValue = "" .
                WHEN "fg-lot" THEN cVarValue = "".
                WHEN "shipto" THEN cVarValue = "".
                WHEN "shipname" THEN cVarValue = "" .
                WHEN "fac-costm" THEN cVarValue = ""  .
                WHEN "tot-fac-cost" THEN cVarValue = ""  . 
                WHEN "on-hand-cost" THEN cVarValue = ""  .
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
        END.
            PUT UNFORMATTED "        GRAND TOTALS:" SUBSTRING(cDisplay,22,300) SKIP(1).
