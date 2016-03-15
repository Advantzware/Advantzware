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
DEFINE VARIABLE v-prodqty AS DEC NO-UNDO.
DEFINE VARIABLE v-prodqty2 AS DEC NO-UNDO .
DEF VARIABLE cSlsRep AS CHAR NO-UNDO .
DEFINE VARIABLE decPrice    AS DECIMAL    NO-UNDO.

DEF VAR v-shipto AS CHAR NO-UNDO .
DEF VAR v-shipto-name AS CHAR NO-UNDO .
DEF VAR v-fg-lot AS CHAR NO-UNDO .

ASSIGN 
          v-shipto = ""
          v-shipto-name =  "" .
          

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
 each cust
   where cust.company eq cocode
     and cust.cust-no EQ ttCustList.cust-no /*fcst*/
    /* and cust.cust-no le tcst*/
     /*and cust.sman    ge fslm
     and cust.sman    le tslm*/  /* task  06111510 */
   no-lock
   by cust.cust-no:

    
      FIND first shipto where shipto.company eq cocode
          and shipto.cust-no eq cust.cust-no
          NO-LOCK NO-ERROR .

      IF AVAIL shipto THEN
          ASSIGN 
          v-shipto = shipto.ship-id
          v-shipto-name =  shipto.ship-name .

      assign
         v-frst     = yes
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0
         v-tot-allo = 0
         v-print    = NO
         v-fg-lot = "" .

     for each oe-ordl
            no-lock
            where oe-ordl.company eq cocode
              and oe-ordl.cust-no eq cust.cust-no
              and oe-ordl.po-no   ge fpo#
              and oe-ordl.po-no   le tpo#
              AND oe-ordl.job-no GE begin_job-no
              and oe-ordl.job-no LE END_job-no,

           first oe-ord 
                where oe-ord.company eq oe-ordl.company 
                  and oe-ord.ord-no eq oe-ordl.ord-no
                  and ((oe-ord.opened EQ YES AND v-ocb NE "C") OR
                       (oe-ord.opened EQ NO  AND v-ocb NE "O"))
                no-lock,
                   
           first itemfg where itemfg.company eq cocode
                          and itemfg.i-no    eq oe-ordl.i-no
/*                           and itemfg.cust-no eq cust.cust-no */
                          and (itemfg.i-code eq typex or typex eq "A")
                        no-lock

          /* for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
                           and itemfg.cust-po-no >= fpo#
                           and itemfg.cust-po-no <= tpo#
               no-lock*/ 
              break by if v-sortby then oe-ordl.part-no else oe-ordl.job-no 
                     by IF NOT v-sortby then oe-ordl.job-no2 else ""
                  by itemfg.i-no
                  by oe-ordl.i-no
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
               IF AVAIL cust AND cust.ACTIVE NE "X" AND cSlsRep EQ "" THEN do:
                 FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                     AND cust-part.i-no = itemfg.i-no
                     AND cust-part.cust-no EQ cust.cust-no
                      NO-LOCK, 
                     FIRST reftable WHERE reftable.reftable = "cp-lab-p" 
                     AND reftable.company = cust-part.company  
                     AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK:
         
                     IF cust-part.spare-char-1 NE "" THEN do:
                         FIND FIRST sman WHERE sman.company = itemfg.company
                             AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                         IF AVAIL sman THEN v-sales-rep = sman.sman.
                         LEAVE .
                     END.
                  END. /* end of cust-part */
         
                  IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
                      FIND FIRST sman WHERE sman.company = cust.company
                          AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                      IF cSlsRep EQ "" AND AVAIL sman THEN v-sales-rep = sman.sman.
                  END.
               END.
               ELSE DO:
                   FIND FIRST sman WHERE sman.company = cust.company
                       AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                   IF cSlsRep EQ "" AND AVAIL sman THEN v-sales-rep = sman.sman.
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

                 IF AVAIL job-hdr THEN
                     ASSIGN
                     vtot-costm = job-hdr.std-mat-cost + job-hdr.std-lab-cost + job-hdr.std-fix-cost 
                                    + job-hdr.std-var-cost 
                     vtot-job-cost = vtot-costm * job-hdr.qty / 1000 .
             END.

           FOR EACH oe-rel
               where oe-rel.company EQ oe-ordl.company
               AND oe-rel.ord-no  EQ oe-ordl.ord-no
               AND oe-rel.i-no    EQ oe-ordl.i-no
               AND oe-rel.line    EQ oe-ordl.line
               NO-LOCK:

               FIND FIRST reftable
                   WHERE reftable.reftable EQ "oe-rel.s-code"
                   AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
                   NO-LOCK NO-ERROR.
               
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
                      tt-oe-rel.rel-date = string(oe-rel.rel-date)
                      tt-oe-rel.tot-qty  = IF oe-rel.qty GT 0 THEN oe-rel.qty ELSE oe-rel.tot-qty 
                      tt-oe-rel.link-no  = oe-rel.link-no
                      tt-oe-rel.po-no    = oe-rel.po-no.

                  FIND FIRST ref-lot-no WHERE
                            ref-lot-no.reftable EQ "oe-rel.lot-no" AND
                            ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
                       NO-LOCK NO-ERROR.
              
                 IF AVAIL ref-lot-no THEN
                   ASSIGN tt-oe-rel.lot-no = ref-lot-no.CODE.

                 v-prodqty2 = 0 .
                 
                 IF AVAIL tt-oe-rel AND tt-oe-rel.lot-no NE "" THEN do:
                     FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
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

                    /* for each fg-rcpth
                         where fg-rcpth.company = cocode
                         AND fg-rcpth.i-no    = itemfg.i-no 
                         AND fg-rcpth.rita-code  = "R" 
                         AND fg-rcpth.job-no  = oe-ordl.job-no 
                         AND fg-rcpth.job-no2 = oe-ordl.job-no2
                         AND fg-rdtlh.stack-code = tt-oe-rel.lot-no
                         no-lock:
                         v-prodqty = fg-rcpth.qty .
                         LEAVE.
                     END.*/
                 END. 
                 
                  IF AVAIL oe-relh  THEN 
                      ASSIGN  tt-oe-rel.rel-no   = oe-relh.release#.

                  IF AVAIL reftable THEN do:                                /*Task# 12021301*/
                      IF LOOKUP(reftable.code, "S,I,L") GT 0 THEN
                          tt-oe-rel.link-no = tt-oe-rel.link-no.
                      ELSE do:
                          IF LOOKUP(reftable.code, "B,A,P,C,Z") GT 0 THEN
                          tt-oe-rel.link-no = tt-oe-rel.rel-no.
                      END.
                  END.
           END.
           v-qty-allo = 0.
           IF oereordr-log OR oereordr-log EQ ? THEN
           RUN oe/oereordr.p (BUFFER itemfg, INPUT oereordr-log, OUTPUT v-qty-allo).
           ELSE v-qty-allo = itemfg.q-alloc. 

           RUN oe/ordlsqty.p (ROWID(oe-ordl), 
                              OUTPUT li-inv-qty, 
                              OUTPUT li-ship-qty).
           v-frst-ord = yes.
           v-qty-onh = 0.
           v-ext-job = 0.

           if first-of(itemfg.i-no) then
             assign v-frst-i-no = yes.

          /* if ((typex ne "A") and (typex ne itemfg.i-code)) or (typex = "A") then
           do:*/
            IF rd_smry-dtl = "D" THEN do:
               for each fg-bin no-lock where fg-bin.company = cocode 
                                         AND fg-bin.i-no = itemfg.i-no
                                         AND fg-bin.job-no GE /*begin_job-no*/ oe-ordl.job-no
                                         AND fg-bin.job-no LE /*end_job-no*/   oe-ordl.job-no
                                         AND fg-bin.job-no2 GE int(begin_job-no2)
                                         AND fg-bin.job-no2 LE int(end_job-no2)
                                        use-index co-ino
                                          BREAK  by fg-bin.loc
                                                 by fg-bin.job-no
                                                 by fg-bin.job-no2 :
                            
                   if (fg-bin.loc eq "CUST" or trim(fg-bin.cust-no) gt "") and
                       not v-custown then
                     next.
                   else
                     v-qty-onh = v-qty-onh + fg-bin.qty.

                   if last-of(fg-bin.job-no2) then
                   do:
                       if (v-qty-onh ne 0 ) or (v-qty-onh eq 0 and zbal) then
                       do:
                          
                           if itemfg.sell-uom = "CS" and itemfg.case-count ne 0 then
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                           else
                             find first uom where uom.uom = itemfg.sell-uom 
                                              AND uom.mult ne 0 
                                   no-lock no-error.

                           if available uom then
                             v-ext-job = (v-qty-onh * itemfg.sell-price / uom.mult).
                           else
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / 1000.

                           if itemfg.sell-uom = "L" then
                             v-ext-job = itemfg.sell-price.

                           for each xbin where xbin.company = cocode 
                                           AND xbin.i-no    = itemfg.i-no 
                                           AND xbin.loc     = fg-bin.loc
                                           AND xbin.job-no GE  fg-bin.job-no
                                           AND xbin.job-no LE   fg-bin.job-no
                                           AND xbin.job-no2 GE  fg-bin.job-no2
                                           AND xbin.job-no2 LE  fg-bin.job-no2
                                          no-lock break by xbin.job-no
                                                        by xbin.job-no2:
                               if first-of(xbin.job-no) or 
                                  first-of(xbin.job-no2) then
                                 assign
                                   v-qty-job = 0
                                   v-ext-job = 0.

                               v-qty-job = v-qty-job + xbin.qty.

                               if last-of(xbin.job-no) or 
                                  last-of(xbin.job-no2) then
                               do:
                                   find first xbin2 where xbin2.company = cocode 
                                                      and xbin2.i-no = itemfg.i-no 
                                                      AND xbin2.loc = fg-bin.loc 
                                                      and (xbin2.job-no  <> xbin.job-no 
                                                           OR 
                                                           xbin2.job-no2 <> xbin.job-no2) 
                                                      and xbin2.qty <> 0 
                                                    no-lock no-error.
                                   if available xbin2 and v-qty-job = 0 then
                                     next.

                                   if itemfg.sell-uom = "CS" and
                                      itemfg.case-count ne 0 then
                                     v-ext-job = 
                            (v-qty-job * itemfg.sell-price) / itemfg.case-count.
                                   else
                                     find first uom where uom.uom = itemfg.sell-uom 
                                                      AND uom.mult ne 0 
                                           no-lock no-error.

                                   if available uom then
                                     v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                                   else
                                     v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                                   if itemfg.sell-uom = "L" then
                                     v-ext-job = itemfg.sell-price.

                                   
                                   ASSIGN
                                       trans-date = ?
                                       ship-date  = ? 
                                       qty-prod   = 0 .
                                   for each fg-rcpth 
                                       where fg-rcpth.company = cocode 
                                       AND fg-rcpth.i-no    = itemfg.i-no 
                                       AND fg-rcpth.rita-code  = "R" 
                                       AND fg-rcpth.job-no  = oe-ordl.job-no 
                                       AND fg-rcpth.job-no2 = oe-ordl.job-no2
                                       no-lock
                                       break by fg-rcpth.trans-date DESC:
                                       trans-date = fg-rcpth.trans-date.
                                       LEAVE.
                                  end.

                                  IF oe-ordl.job-no NE '' THEN
                                      FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
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
                                 FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
                                      WHERE fg-rcpth.company EQ oe-ordl.company
                                      AND fg-rcpth.job-no EQ oe-ordl.job-no
                                      AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                                      AND fg-rcpth.i-no EQ oe-ordl.i-no
                                      AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                                      EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                                      WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                      AND fg-rdtlh.stack-code NE ""
                                      break by fg-rcpth.trans-date:
                                       v-fg-lot = fg-rdtlh.stack-code.
                                       LEAVE.
                                  end.

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
                                if xbin.job-no = "" and xbin.job-no2 = 0 then
                                    v-job-no = "".
                                else
                                    v-job-no = string(xbin.job-no,"x(6)") + "-" + string(xbin.job-no2,"99").
                                    v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
                                    /* Task 11221301 */
                                    ASSIGN v-rfq = "" .                                    
                                    IF oe-ordl.est-no NE "" THEN
                                        FOR EACH quotehd where quotehd.company = itemfg.company and
                                        quotehd.loc = locode AND
                                        quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESC:
                                        v-rfq = IF AVAIL quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                                       LEAVE.
                                    END.  /* Task 11221301 */
                                    
                                 ASSIGN cDisplay = ""
                                        cTmpField = ""
                                        cVarValue = ""
                                        cExcelDisplay = ""
                                        cExcelVarValue = "".
                                    v-summ-temp = 0 .
                                    string(v-row-id) = "".
                                    FOR EACH tt-oe-rel NO-LOCK:
                                        v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
                                   END.

                                   BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
                                   BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
                                   FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
                                   IF AVAIL tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
                                   DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                                       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                                       IF INDEX(cTmpField,".") > 0 THEN DO:
                                           cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                                           IF cTmpField = "Cust-no" or
                                              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
                                           ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
                                                cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
                                                cDisplay = cDisplay + cTmpField + 
                                                FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                                                     .

                                                 cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
                                       END.
                                       ELSE DO:            
                                           CASE cTmpField:               
                                                 WHEN "v-job-no" THEN cVarValue = v-job-no.
                                                 WHEN "v-rel#" THEN cvarValue = IF AVAIL tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                                                 WHEN "v-relDate" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.rel-date) ELSE "".
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
                                                 WHEN "relqty" THEN cVarValue = (IF rd_smry-dtl = "D" AND AVAIL tt-oe-rel  THEN STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE STRING(v-summ-temp,"->>>>>>,>>9") ) .
                                                 WHEN "loc" THEN cVarValue = STRING(fg-bin.loc,"x(5)").
                                                 WHEN "relpo" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.po-no) ELSE "".
                                                 WHEN "rellot" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.lot-no) ELSE "".
                                                 WHEN "prodqty" THEN cVarValue = (IF rd_smry-dtl = "D" AND AVAIL tt-oe-rel  THEN string(tt-oe-rel.qty,"->>>,>>>,>>>,>>9") ELSE string(v-prodqty,"->>>,>>>,>>>,>>9")) .
                                                 WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                                                 WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                                                 WHEN "shipto" THEN cVarValue = v-shipto .
                                                 WHEN "shipname" THEN cVarValue = v-shipto-name  .
                                                 WHEN "fac-costm" THEN cVarValue = STRING(vtot-costm,"->>,>>>,>>9.99")  .
                                                 WHEN "tot-fac-cost" THEN cVarValue = STRING(vtot-job-cost,"->>>>,>>>,>>9.99")  .
                                                 
                                           END CASE.

                                           cExcelVarValue = cVarValue.  
                                           cDisplay = cDisplay + cVarValue +
                                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                                                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                                       END.
                                 END.

                                 PUT UNFORMATTED cDisplay SKIP.
                                 IF tb_excel THEN DO:
                                     PUT STREAM excel UNFORMATTED  
                                         cExcelDisplay SKIP.
                                 END.
                                 if v-frst-i-no then do:
                                     IF v-qty-onh NE ? THEN
                                         assign
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

                                 assign
                                    v-tot-ord        = v-tot-ord        + oe-ordl.qty
                                    v-tot-ship       = v-tot-ship       + li-ship-qty
                                    v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                                    v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
                                 ASSIGN v-qty-onh = 0
                                     li-ship-qty = 0 .
                                           

    IF rd_smry-dtl = "D" THEN DO:
       FOR EACH tt-oe-rel WHERE rowid(tt-oe-rel) NE v-row-id NO-LOCK:               /*Task# 12021301*/
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
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:   
                WHEN "cust.cust-no" THEN cVarValue = "".
                WHEN "oe-ordl.po-no" THEN cVarValue = "".
                WHEN "sman" THEN cVarValue = "" .
                WHEN "oe-ordl.i-no" THEN cVarValue = "".
                WHEN "oe-ordl.part-no" THEN cVarValue = "".
                WHEN "oe-ordl.i-name" THEN cVarValue = "".
                WHEN "v-job-no" THEN cVarValue = "".
                WHEN "v-rel#" THEN cvarValue = IF AVAIL tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .
                WHEN "v-relDate" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.rel-date) ELSE "".
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
                WHEN "relqty" THEN cVarValue = IF AVAIL tt-oe-rel THEN STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE "".
                WHEN "loc" THEN cVarValue = "" .
                WHEN "relpo" THEN cVarValue =  IF AVAIL tt-oe-rel THEN STRING(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue =  IF AVAIL tt-oe-rel THEN STRING(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = IF AVAIL tt-oe-rel THEN STRING(tt-oe-rel.qty,"->>>,>>>,>>>,>>9") ELSE "". 
                WHEN "fg-lot" THEN cVarValue = "" .
                WHEN "shipto" THEN cVarValue = "".
                WHEN "shipname" THEN cVarValue = "" . 
                WHEN "fac-costm" THEN cVarValue = ""  .
                WHEN "tot-fac-cost" THEN cVarValue = ""  . 
                                   
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
        
          ASSIGN v-frst  = NO
           v-print = YES.

            END.
          END. 
        END.
      END.
    END.
   END.
  END. /* for each fg-bin */
    IF NOT CAN-FIND(first fg-bin where fg-bin.company        eq cocode
                                       and fg-bin.i-no           eq itemfg.i-no
                                       and fg-bin.job-no         eq oe-ordl.job-no
                                       and fg-bin.job-no2        eq oe-ordl.job-no2
                                       and (v-custown or 
                                            (fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "")))
      THEN DO:

        if itemfg.sell-uom   eq "CS" and
                   itemfg.case-count ne 0    then
                  v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                else if itemfg.sell-uom eq "L" then 
                  v-ext = oe-ordl.price.
                else
                do:
                    find first uom where uom.uom  eq itemfg.sell-uom
                                     and uom.mult ne 0
                                   no-lock no-error.

                    v-ext = v-qty-onh * oe-ordl.price / (if avail uom then uom.mult else 1000).
                end.              
           /* END.*/
            /*if first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) then*/
                                      assign
                                        v-tot-onh        = v-tot-onh       + v-qty-onh
                                        v-tot-ext        = v-tot-ext       + v-ext
                                        v-tot-allo = v-tot-allo + v-qty-allo
                                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                                        v-grand-tot-ext  = v-grand-tot-ext + v-ext
                                        v-grand-tot-allo = v-grand-tot-allo + v-qty-allo.
                                    /*if v-frst-ord then*/
                                      assign
                                        v-tot-ord        = v-tot-ord        + oe-ordl.qty
                                        v-tot-ship       = v-tot-ship       + li-ship-qty
                                        v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                                        v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
            ASSIGN
                trans-date = ?
                ship-date  = ? 
                qty-prod   = 0 .
            for each fg-rcpth 
                         where fg-rcpth.company = cocode 
                           AND fg-rcpth.i-no    = itemfg.i-no 
                           AND fg-rcpth.rita-code  = "R" 
                           AND fg-rcpth.job-no  = oe-ordl.job-no 
                           AND fg-rcpth.job-no2 = oe-ordl.job-no2
                             no-lock
                             break by fg-rcpth.trans-date DESC:
                 trans-date = fg-rcpth.trans-date.
                 LEAVE.
            end.
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
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
            FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company EQ oe-ordl.company
                 AND fg-rcpth.job-no EQ oe-ordl.job-no
                 AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                 AND fg-rcpth.i-no EQ oe-ordl.i-no
                 AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                 EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                 WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 AND fg-rdtlh.stack-code NE ""
                 break by fg-rcpth.trans-date:
                  v-fg-lot = fg-rdtlh.stack-code.
                  LEAVE.
             end.

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
            if oe-ordl.job-no ne "" then
                  v-job-no = STRING(oe-ordl.job-no,"x(6)") + "-" + string(oe-ordl.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
            /* Task 11221301 */
            ASSIGN v-rfq = "" .                                    
            IF oe-ordl.est-no NE "" THEN
                FOR EACH quotehd where quotehd.company = itemfg.company and
                                   quotehd.loc = locode AND
                                   quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESC:
                v-rfq = IF AVAIL quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                LEAVE.
                END.  /* Task 11221301 */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
           v-summ-temp = 0 .
           string(v-row-id) = "".
       FOR EACH tt-oe-rel NO-LOCK:
           v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
       END.
       

        BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
        BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
        FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
        IF AVAIL tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" or
              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "v-job-no" THEN cVarValue = v-job-no.
                WHEN "v-rel#" THEN cvarValue = IF AVAIL tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                WHEN "v-relDate" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
                WHEN "v-ext" THEN cVarValue = STRING(v-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = (IF rd_smry-dtl = "D" AND AVAIL tt-oe-rel THEN STRING(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE STRING(v-summ-temp,"->>>>>>,>>9") ) .
                WHEN "loc" THEN cVarValue = STRING(itemfg.loc,"x(5)").   
                WHEN "relpo" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = string(v-prodqty,"->>>,>>>,>>>,>>9") .
                WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                WHEN "shipto" THEN cVarValue = v-shipto .
                WHEN "shipname" THEN cVarValue = v-shipto-name .
                WHEN "fac-costm" THEN cVarValue = IF vtot-costm <> 0 THEN STRING(vtot-costm,"->>,>>>,>>9.99") ELSE "" .
                WHEN "tot-fac-cost" THEN cVarValue = IF vtot-job-cost <> 0 THEN STRING(vtot-job-cost,"->>>>,>>>,>>9.99") ELSE ""  .
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
    IF rd_smry-dtl = "D" THEN DO:
       FOR EACH tt-oe-rel WHERE rowid(tt-oe-rel) NE v-row-id NO-LOCK:               /*Task# 12021301*/
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
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:   
                WHEN "cust.cust-no" THEN cVarValue = "".
                WHEN "oe-ordl.po-no" THEN cVarValue = "".
                WHEN "sman" THEN cVarValue = "" .
                WHEN "oe-ordl.i-no" THEN cVarValue = "".
                WHEN "oe-ordl.part-no" THEN cVarValue = "".
                WHEN "oe-ordl.i-name" THEN cVarValue = "".
                WHEN "v-job-no" THEN cVarValue = "".
                WHEN "v-rel#" THEN cvarValue = IF AVAIL tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .
                WHEN "v-relDate" THEN cVarValue = string(tt-oe-rel.rel-date).
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
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
      END.
    END. 

      END.  /* not find fg-bin */  
 END. /* if datail*/
   ELSE DO:
                     for each fg-bin no-lock where fg-bin.company = cocode 
                                         AND fg-bin.i-no = itemfg.i-no
                                         AND fg-bin.job-no GE /*begin_job-no*/ oe-ordl.job-no 
                                         AND fg-bin.job-no LE /*end_job-no  */ oe-ordl.job-no 
                                         AND fg-bin.job-no2 GE int(begin_job-no2)
                                         AND fg-bin.job-no2 LE int(end_job-no2)
                                          use-index co-ino
                                          BREAK /*by fg-bin.loc*/
                                             by fg-bin.job-no
                                             by fg-bin.job-no2 :
                            
                   if (fg-bin.loc eq "CUST" or trim(fg-bin.cust-no) gt "") and
                       not v-custown then
                     next.
                   else
                     v-qty-onh = v-qty-onh + fg-bin.qty.

                   if last-of(fg-bin.job-no2) then
                   do:
                       if (v-qty-onh ne 0 ) or (v-qty-onh eq 0 and zbal) then
                       do:
                          
                           if itemfg.sell-uom = "CS" and itemfg.case-count ne 0 then
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                           else
                             find first uom where uom.uom = itemfg.sell-uom 
                                              AND uom.mult ne 0 
                                   no-lock no-error.

                           if available uom then
                             v-ext-job = (v-qty-onh * itemfg.sell-price / uom.mult).
                           else
                             v-ext-job = (v-qty-onh * itemfg.sell-price) / 1000.

                           if itemfg.sell-uom = "L" then
                             v-ext-job = itemfg.sell-price.

                           for each xbin where xbin.company = cocode 
                                          AND xbin.i-no    =  itemfg.i-no 
                                          AND xbin.loc     =  fg-bin.loc
                                          AND xbin.job-no  GE fg-bin.job-no
                                          AND xbin.job-no  LE fg-bin.job-no
                                          AND xbin.job-no2 GE fg-bin.job-no2
                                          AND xbin.job-no2 LE fg-bin.job-no2
                                          no-lock break by xbin.job-no
                                                        by xbin.job-no2:
                               if first-of(xbin.job-no) or 
                                  first-of(xbin.job-no2) then
                                 assign
                                   v-qty-job = 0
                                   v-ext-job = 0.

                               v-qty-job = v-qty-job + xbin.qty.

                               if last-of(xbin.job-no) or 
                                  last-of(xbin.job-no2) then
                               do:
                                   find first xbin2 where xbin2.company = cocode 
                                                      and xbin2.i-no = itemfg.i-no 
                                                      AND xbin2.loc = fg-bin.loc 
                                                      and (xbin2.job-no  <> xbin.job-no 
                                                           OR 
                                                           xbin2.job-no2 <> xbin.job-no2) 
                                                      and xbin2.qty <> 0 
                                                    no-lock no-error.
                                   if available xbin2 and v-qty-job = 0 then
                                     next.

                                   if itemfg.sell-uom = "CS" and
                                      itemfg.case-count ne 0 then
                                     v-ext-job = 
                            (v-qty-job * itemfg.sell-price) / itemfg.case-count.
                                   else
                                     find first uom where uom.uom = itemfg.sell-uom 
                                                      AND uom.mult ne 0 
                                           no-lock no-error.

                                   if available uom then
                                     v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                                   else
                                     v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                                   if itemfg.sell-uom = "L" then
                                     v-ext-job = itemfg.sell-price.
                                   
                                     
            ASSIGN
                trans-date = ?
                ship-date  = ? 
                qty-prod   = 0 .
            for each fg-rcpth 
                         where fg-rcpth.company = cocode 
                           AND fg-rcpth.i-no    = itemfg.i-no 
                           AND fg-rcpth.rita-code  = "R" 
                           AND fg-rcpth.job-no  = oe-ordl.job-no 
                           AND fg-rcpth.job-no2 = oe-ordl.job-no2
                             no-lock
                             break by fg-rcpth.trans-date:
                 trans-date = fg-rcpth.trans-date.
                 LEAVE.
            end.
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
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
            FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company EQ oe-ordl.company
                 AND fg-rcpth.job-no EQ oe-ordl.job-no
                 AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                 AND fg-rcpth.i-no EQ oe-ordl.i-no
                 AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                 EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                 WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 AND fg-rdtlh.stack-code NE ""
                 break by fg-rcpth.trans-date:
                  v-fg-lot = fg-rdtlh.stack-code.
                  LEAVE.
             end.

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
            if xbin.job-no = "" and xbin.job-no2 = 0 then
                                     v-job-no = "".
                                   else
                                     v-job-no = string(xbin.job-no,"x(6)") + "-" + string(xbin.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
            /* Task 11221301 */
            ASSIGN v-rfq = "" .                                    
            IF oe-ordl.est-no NE "" THEN
                FOR EACH quotehd where quotehd.company = itemfg.company and
                                   quotehd.loc = locode AND
                                   quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESC:
                v-rfq = IF AVAIL quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                LEAVE.
                END.  /* Task 11221301 */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
           v-summ-temp = 0 .
           string(v-row-id) = "".
       FOR EACH tt-oe-rel NO-LOCK:
           v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
       END.
       

        BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
        BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
        FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
        IF AVAIL tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" or
              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "v-job-no" THEN cVarValue = v-job-no.
                WHEN "v-rel#" THEN cvarValue = IF AVAIL tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                WHEN "v-relDate" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.rel-date) ELSE "".
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
                WHEN "relqty" THEN cVarValue = STRING(v-summ-temp,"->>>>>>,>>9") .
                WHEN "loc" THEN cVarValue = STRING(fg-bin.loc,"x(5)").
                WHEN "relpo" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = string(v-prodqty,"->>>,>>>,>>>,>>9") .
                WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                WHEN "shipto" THEN cVarValue = v-shipto.
                WHEN "shipname" THEN cVarValue = v-shipto-name .
                WHEN "fac-costm" THEN cVarValue = IF vtot-costm <> 0 THEN STRING(vtot-costm,"->>,>>>,>>9.99") ELSE "" .
                WHEN "tot-fac-cost" THEN cVarValue = IF vtot-job-cost <> 0 THEN STRING(vtot-job-cost,"->>>>,>>>,>>9.99") ELSE ""  .
                                   
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
    if v-frst-i-no then do:
         IF v-qty-onh NE ? THEN
             assign
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

     assign
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
    IF NOT CAN-FIND(first fg-bin where fg-bin.company        eq cocode
                                       and fg-bin.i-no           eq itemfg.i-no
                                       and fg-bin.job-no         eq oe-ordl.job-no
                                       and fg-bin.job-no2        eq oe-ordl.job-no2
                                       and (v-custown or 
                                            (fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "")))
      THEN DO:

        if itemfg.sell-uom   eq "CS" and
                   itemfg.case-count ne 0    then
                  v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                else if itemfg.sell-uom eq "L" then 
                  v-ext = oe-ordl.price.
                else
                do:
                    find first uom where uom.uom  eq itemfg.sell-uom
                                     and uom.mult ne 0
                                   no-lock no-error.

                    v-ext = v-qty-onh * oe-ordl.price / (if avail uom then uom.mult else 1000).
                end.              
           /* END.*/
            /*if first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) then*/
                                      assign
                                        v-tot-onh        = v-tot-onh       + v-qty-onh
                                        v-tot-ext        = v-tot-ext       + v-ext
                                        v-tot-allo = v-tot-allo + v-qty-allo
                                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                                        v-grand-tot-ext  = v-grand-tot-ext + v-ext
                                        v-grand-tot-allo = v-grand-tot-allo + v-qty-allo.
                                    /*if v-frst-ord then*/
                                      assign
                                        v-tot-ord        = v-tot-ord        + oe-ordl.qty
                                        v-tot-ship       = v-tot-ship       + li-ship-qty
                                        v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                                        v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
            ASSIGN
                trans-date = ?
                ship-date  = ? 
                qty-prod   = 0 .
            for each fg-rcpth 
                         where fg-rcpth.company = cocode 
                           AND fg-rcpth.i-no    = itemfg.i-no 
                           AND fg-rcpth.rita-code  = "R" 
                           AND fg-rcpth.job-no  = oe-ordl.job-no 
                           AND fg-rcpth.job-no2 = oe-ordl.job-no2
                             no-lock
                             break by fg-rcpth.trans-date:
                 trans-date = fg-rcpth.trans-date.
                 LEAVE.
            end.
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
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
            FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company EQ oe-ordl.company
                 AND fg-rcpth.job-no EQ oe-ordl.job-no
                 AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                 AND fg-rcpth.i-no EQ oe-ordl.i-no
                 AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                 EACH fg-rdtlh FIELDS(stack-code) NO-LOCK
                 WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 AND fg-rdtlh.stack-code NE ""
                 break by fg-rcpth.trans-date:
                  v-fg-lot = fg-rdtlh.stack-code.
                  LEAVE.
             end.

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
            if oe-ordl.job-no ne "" then
                  v-job-no = STRING(oe-ordl.job-no,"x(6)") + "-" + string(oe-ordl.job-no2,"99").
            v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.
            /* Task 11221301 */
            ASSIGN v-rfq = "" .                                    
            IF oe-ordl.est-no NE "" THEN
                FOR EACH quotehd where quotehd.company = itemfg.company and
                                   quotehd.loc = locode AND
                                   quotehd.est-no = oe-ordl.est-no NO-LOCK  BY quo-date DESC:
                v-rfq = IF AVAIL quotehd AND quotehd.rfq <> "0" THEN quotehd.rfq ELSE "".
                LEAVE.
                END.  /* Task 11221301 */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
           v-summ-temp = 0 .
           string(v-row-id) = "".
       FOR EACH tt-oe-rel NO-LOCK:
           v-summ-temp = v-summ-temp + tt-oe-rel.tot-qty .
       END.
       

        BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
        BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
        FIND FIRST tt-oe-rel NO-LOCK NO-ERROR.
        IF AVAIL tt-oe-rel THEN ASSIGN v-row-id = ROWID(tt-oe-rel) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" or
              cTmpField = "Sman" THEN hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField).
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).          
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "v-job-no" THEN cVarValue = v-job-no.
                WHEN "v-rel#" THEN cvarValue = IF AVAIL tt-oe-rel AND tt-oe-rel.link-no <> 0 THEN STRING(tt-oe-rel.link-no) ELSE "" .                   /*Task# 12021301*/
                WHEN "v-relDate" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/99") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
                WHEN "v-ext" THEN cVarValue = STRING(v-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").
                WHEN "qty-pro" THEN cVarValue = STRING(qty-prod,"->>>>>,>>9").
                WHEN "qty-bal" THEN cVarValue = STRING(oe-ordl.qty - li-ship-qty ,"->>>>>,>>9").
                WHEN "ord-date" THEN cVarValue = IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/99") ELSE "" .
                WHEN "ship-date" THEN cVarValue = IF ship-date <> ? THEN STRING(ship-date,"99/99/99") ELSE "" .
                WHEN "relqty" THEN cVarValue = STRING(v-summ-temp,"->>>>>>,>>9") .
                WHEN "loc" THEN cVarValue = STRING(itemfg.loc,"x(5)"). 
                WHEN "relpo" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.po-no) ELSE "".
                WHEN "rellot" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.lot-no) ELSE "".
                WHEN "prodqty" THEN cVarValue = string(v-prodqty,"->>>,>>>,>>>,>>9") .  
                WHEN "sman" THEN cVarValue = STRING(v-sales-rep).
                WHEN "fg-lot" THEN cVarValue = IF v-fg-lot NE "" THEN STRING(v-fg-lot) ELSE "".
                WHEN "shipto" THEN cVarValue = v-shipto .
                WHEN "shipname" THEN cVarValue = v-shipto-name .
                WHEN "fac-costm" THEN cVarValue = IF vtot-costm <> 0 THEN STRING(vtot-costm,"->>,>>>,>>9.99") ELSE "" .
                WHEN "tot-fac-cost" THEN cVarValue = IF vtot-job-cost <> 0 THEN STRING(vtot-job-cost,"->>>>,>>>,>>9.99") ELSE ""  .
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
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
             
 
          
   end.  /* for each item */  
   

    if v-print                      and
       fcst ne tcst                 and
       (v-tot-onh ne 0 or zbal)     THEN do:
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
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                WHEN "v-relQty" THEN cVarValue = string(v-tot-allo,"->>>>>>,>>9").
                WHEN "v-rctDate" THEN cVarValue = "".
                WHEN "v-qty-onh" THEN cVarValue = string(v-tot-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = string(v-tot-ship,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = string(v-tot-ord,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = "".
                WHEN "v-ext" THEN cVarValue = string(v-tot-ext,"->>>,>>>,>>9.99").
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
                                   
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
        END.
            PUT UNFORMATTED "        CUSTOMER TOTALS:" substring(cDisplay,25,300) SKIP(1).

    END. /*(v-tot-onh ne 0 or zbal)*/

end.  /* for each cust */
     
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
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                WHEN "v-relQty" THEN cVarValue = string(v-grand-tot-allo,"->>>>>>,>>9").
                WHEN "v-rctDate" THEN cVarValue = "".
                WHEN "v-qty-onh" THEN cVarValue = string(v-grand-tot-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = string(v-grand-tot-ship,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = string(v-grand-tot-ord,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = "".
                WHEN "v-ext" THEN cVarValue = string(v-grand-tot-ext,"->>>,>>>,>>9.99").
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
                                   
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
        END.
            PUT UNFORMATTED "        GRAND TOTALS:" substring(cDisplay,22,300) SKIP(1).

