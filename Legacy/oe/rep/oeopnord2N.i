/* ---------------------------------- oe/rep/oeopnord2.i from oehots2.i ------- */
/* Open Order Report                                                          */
/* Author: Gilbert Marquez                                                    */
/* December 24, 2008                                                          */
/* -------------------------------------------------------------------------- */

IF v-first THEN 
   /*IF tb_exp-po THEN
      PUT UNFORMATTED
         "<FCourier New><B><C5><P26>Hots <P15>(0-Z-7)<P8>" skip(1)
         string(TODAY) AT 3 FORM "x(5)"  "Begin Date:" AT 30 string(begin_date) FORM "x(5)" "  End date:" string(end_date) FORM "x(5)" 
         "Page: " AT 145 PAGE-NUM FORM ">>9"SKIP
         "Rel Date Ord Date  Order# Cust PO#       Item Name                 Routing         Ven PO#     Brd Rcpt Qty Order  Qty Comp Qty Onhnd Ship City     Status"
       SKIP v-line SKIP.                                                                                                                        
   ELSE*/
      PUT UNFORMATTED
         "<FCourier New><B><C5><P26>Hots <P15>(0-Z-7)<P8>" skip(1)
         STRING(TODAY) AT 3 FORM "x(5)"  "Begin Date:" AT 30 string(begin_date) FORM "x(5)" "  End date:" string(end_date) FORM "x(5)" 
         "Page: " AT 145 PAGE-NUM FORM ">>9"SKIP
         /*"Rel Date Ord Date  Order# Cust PO#       Item Name                 Routing         Ven PO#     Brd Rcpt Qty Order  Qty Comp Qty Onhnd Ship City     Status"         */
          str-tit4
          SKIP str-tit5 SKIP.
         

v-first = NO.
IF AVAIL itemfg THEN DO:
   /* ROUTING,PO and Vend */
   ASSIGN 
      lv-board-po-no = 0
      lv-vend-no = ""
      ld-qty-rec = 0
      lv-routing = "".
   
   FOR EACH tt-fg-set:
      DELETE tt-fg-set.
   END.
   
   RELEASE job-hdr.
   RELEASE job.
   RELEASE reftable.
   
   IF TRIM(w-ord.job-no) EQ "" THEN
      FOR EACH job-hdr NO-LOCK WHERE 
               job-hdr.company EQ cocode
           AND job-hdr.ord-no  EQ w-ord.ord-no
           AND job-hdr.cust-no EQ w-ord.cust-no
           AND job-hdr.i-no    EQ w-ord.i-no
           AND job-hdr.opened  EQ YES
         BY ROWID(job-hdr) DESC:
         LEAVE.
      END.
   ELSE DO:
      FIND FIRST job-hdr WHERE 
                 job-hdr.company EQ cocode
             AND job-hdr.job-no  EQ w-ord.job-no
             AND job-hdr.job-no2 EQ w-ord.job-no2
             AND job-hdr.ord-no  EQ w-ord.ord-no
             AND job-hdr.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE 
                    job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ w-ord.job-no
                AND job-hdr.job-no2 EQ w-ord.job-no2
                AND job-hdr.ord-no  EQ w-ord.ord-no NO-LOCK NO-ERROR.
   END.
   
   IF AVAIL job-hdr THEN
      FIND FIRST job WHERE 
                 job.company EQ job-hdr.company
             AND job.job     EQ job-hdr.job
             AND job.job-no  EQ job-hdr.job-no
             AND job.job-no2 EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL job THEN DO: 
         IF (itemfg.isaset OR w-ord.is-a-component) AND
            CAN-FIND(FIRST reftable WHERE 
                           reftable.reftable EQ "jc/jc-calc.p"
                       AND reftable.company  EQ job.company
                       AND reftable.loc      EQ ""
                       AND reftable.code     EQ STRING(job.job,"999999999")) THEN
            FOR EACH reftable NO-LOCK WHERE 
                     reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company  EQ job-hdr.company
                 AND reftable.loc      EQ ""
                 AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                 AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
                     (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component)):
               CREATE tt-fg-set.
               ASSIGN 
                  tt-fg-set.part-no      = reftable.code2
                  tt-fg-set.QtyPerSet     = reftable.val[12]
                  tt-fg-set.part-qty-dec = reftable.val[13].
            END.
         ELSE DO:
            CREATE tt-fg-set.
            ASSIGN 
               tt-fg-set.part-no      = job-hdr.i-no
               tt-fg-set.QtyPerSet     = job-hdr.frm
               tt-fg-set.part-qty-dec = job-hdr.blank-no.
         END.
         
         FOR EACH tt-fg-set
            BREAK BY tt-fg-set.QtyPerSet
                  BY tt-fg-set.part-qty-dec:

            ll-po = NO.
            IF LAST-OF(tt-fg-set.QtyPerSet) THEN
               FOR EACH po-ordl WHERE 
                        po-ordl.company   EQ job.company
                    AND po-ordl.job-no    EQ job.job-no
                    AND po-ordl.job-no2   EQ job.job-no2
                    AND po-ordl.s-num     EQ INTEGER(tt-fg-set.QtyPerSet)
                    AND po-ordl.item-type EQ YES USE-INDEX job-no NO-LOCK,
                  FIRST po-ord WHERE 
                        po-ord.company EQ po-ordl.company
                    AND po-ord.po-no   EQ po-ordl.po-no NO-LOCK,
                  FIRST ITEM WHERE 
                        item.company EQ po-ordl.company
                    AND item.i-no    EQ po-ordl.i-no
                    AND INDEX("1234BPR",item.mat-type) GT 0 NO-LOCK
                  BREAK BY po-ordl.po-no
                        BY po-ordl.i-no
                        BY po-ordl.rec_key:

                     ll-po = YES.
                     ASSIGN 
                        lv-board-po-no = po-ordl.po-no
                        lv-vend-no = po-ord.vend-no.
                     IF po-ordl.cons-uom EQ "EA" THEN 
                        ld-qty-rec = po-ordl.t-rec-qty.
                     ELSE 
                        RUN sys/ref/convquom.p(po-ordl.cons-uom,"EA",item.basis-w,po-ordl.s-len,po-ordl.s-wid,item.s-dep,po-ordl.t-rec-qty, output ld-qty-rec).
                     {sys/inc/roundup.i ld-qty-rec}
                     LEAVE.
               END.
               lv-routing = "".
               IF FIRST(tt-fg-set.QtyPerSet) THEN
                  FOR EACH job-mch WHERE 
                           job-mch.company EQ job.company
                       AND job-mch.job     EQ job.job
                       AND job-mch.job-no  EQ job.job-no
                       AND job-mch.job-no2 EQ job.job-no2
                       AND job-mch.frm     EQ INTEGER(tt-fg-set.QtyPerSet) NO-LOCK
                     BREAK BY job-mch.line:

                     lv-routing = lv-routing + job-mch.m-code + ",".
                  END.
         END. /* each tt-fg-set*/
      END.  /* job*/
   
      IF lv-routing = "" AND itemfg.est-no <> "" THEN 
         FOR EACH est-op NO-LOCK WHERE 
                  est-op.company = itemfg.company
              AND est-op.est-no = itemfg.est-no
              AND est-op.line LT 500 :
              /*((ASI.est-op.qty eq est-qty.eqty and est.est-type ne 8) or
                 (ASI.est-op.qty eq lv-eqty and est.est-type ge 7)) NO-LOCK*/
            lv-routing = lv-routing + est-op.m-code + ",".
         END.

      /* spec notes */
      lv-text = "".
      IF tb_notes AND rd_lComments THEN 
         FOR EACH notes WHERE 
                  notes.rec_key   EQ itemfg.rec_key
              AND notes.note_type EQ "S"
              AND notes.note_code GE begin_spec
              AND notes.note_code LE end_spec NO-LOCK
            BREAK BY notes.note_code:
            
            IF FIRST(notes.note_code) THEN
               lv-text = TRIM(notes.note_code) + ":" + TRIM(notes.note_text).
            LEAVE.
         END.
      ELSE IF NOT rd_lComments THEN DO: 

         FOR EACH eb fields(test style) WHERE 
                  eb.company  EQ cocode
              AND eb.est-no   EQ oe-ordl.est-no
              AND eb.stock-no EQ oe-ordl.i-no NO-LOCK:
            ASSIGN 
               lv-text = eb.test.
            LEAVE.
         END.

         IF NOT AVAIL eb THEN
            FOR EACH eb fields(test style) WHERE 
                eb.company  EQ cocode AND
                eb.est-no   EQ itemfg.est-no AND
                eb.stock-no EQ itemfg.i-no
                NO-LOCK:
                ASSIGN 
                   lv-text = eb.test.
                LEAVE.
            END.

         ASSIGN 
            lv-text = (IF avail eb and itemfg.style eq "" THEN eb.style
                       ELSE itemfg.style)
                    + "  " + lv-text.  /* style + test */
      END.
  
   IF v-sort = "C" THEN DO:
      IF lv-prev-cust-name <> "" AND lv-prev-cust-name <> w-ord.cust-name THEN 
         PUT SKIP(1).
      IF lv-prev-cust-name <> w-ord.cust-name THEN 
         PUT "<B>" w-ord.cust-name FORM "x(30)" "</B>" SKIP.
   END.
  
   FIND FIRST shipto WHERE 
              shipto.company = cocode
          AND shipto.cust-no = w-ord.cust-no  
          AND shipto.ship-id = w-ord.ship-id NO-LOCK NO-ERROR.
  
   v-ship-city = IF AVAIL shipto AND shipto.ship-city <> "" THEN shipto.ship-city ELSE w-ord.ship-id.
   IF substring(lv-routing,15,1) = "," THEN 
      lv-routing = SUBSTRING(lv-routing,1,14).
  
   IF length(lv-routing) > 1 AND substring(lv-routing,LENGTH(lv-routing),1) = "," THEN  
      lv-routing = SUBSTRING(lv-routing,1,LENGTH(lv-routing) - 1).
   
   FIND FIRST tt-ord WHERE tt-ord.tt-recid = RECID(w-ord) NO-LOCK NO-ERROR.

   IF lv-vend-no = "" AND AVAIL tt-ord AND tt-ord.tt-po-no <> 0 THEN DO:
      FIND FIRST po-ord WHERE 
                 po-ord.company = cocode
             AND po-ord.po-no = tt-ord.tt-po-no NO-LOCK NO-ERROR.
      lv-vend-no = po-ord.vend-no.
   END.

   /*IF tb_exp-po THEN
      DISPLAY 
          w-ord.rel-date  
          w-ord.last-date FORMAT "99/99/99"    AT 10 
          w-ord.ord-no    FORMAT "999999"      AT 20  
          w-ord.po-num    FORMAT "x(15)"       AT 27 
          itemfg.i-name   FORMAT "x(25)"       AT 42
          lv-routing      FORMAT "x(15)"       AT 68           
          lv-vend-no      FORMAT "x(8)"        AT 85                                   
          ld-qty-rec      FORMAT "->>>>>>>>9"  AT 94 
          w-ord.ord-qty   FORMAT "->>>>>>>>9"  AT 104
          v-comp-qty      FORMAT "->>>>>>>>9"  AT 114
          w-ord.onh-qty   FORMAT "->>>>>>>>9"  AT 124
          v-ship-city     FORMAT "x(15)"       AT 135
          w-ord.xls-status                     AT 151
         WITH DOWN FRAME hots-po{1} NO-BOX STREAM-IO NO-LABEL WIDTH 250.
   ELSE*/
     /* DISPLAY 
          w-ord.rel-date  
          w-ord.last-date FORMAT "99/99/99"   AT 10  
          w-ord.ord-no    FORMAT "999999"     AT 20  
          w-ord.po-num    FORMAT "x(15)"      AT 27  
          itemfg.i-name   FORMAT "x(25)"      AT 42
          lv-routing      FORMAT "x(15)"      AT 68            
          lv-vend-no      FORMAT "x(8)"       AT 85  
          ld-qty-rec      FORMAT "->>>>>>>>9" AT 94 
          w-ord.ord-qty   FORMAT "->>>>>>>>9" AT 104
          v-comp-qty      FORMAT "->>>>>>>>9" AT 114
          w-ord.onh-qty   FORMAT "->>>>>>>>9" AT 124
          v-ship-city     FORMAT "x(15)"      AT 135
          w-ord.xls-status                    AT 151 
         WITH DOWN FRAME hots{1} NO-BOX STREAM-IO NO-LABEL WIDTH 250.*/

      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
      
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:       
                         WHEN "item"    THEN cVarValue = IF AVAIL itemfg THEN  string(itemfg.i-no,"x(15)") ELSE "" .
                         WHEN "rel-date"    THEN cVarValue = IF w-ord.rel-date NE ? THEN string(date(w-ord.rel-date),"99/99/99") ELSE "" .
                         WHEN "ord-date"   THEN cVarValue = IF w-ord.last-date NE ? THEN string(w-ord.last-date,"99/99/99")  ELSE "".
                         WHEN "ord-no"   THEN cVarValue = STRING(w-ord.ord-no,"99999999").
                         WHEN "cust-po"  THEN cVarValue = STRING(w-ord.po-num,"x(15)") .
                         WHEN "item-name"   THEN cVarValue = IF AVAIL itemfg THEN STRING(itemfg.i-name,"x(25)") ELSE "" .
                         WHEN "rout"  THEN cVarValue = STRING(lv-routing,"x(15)") .
                         WHEN "vend-po"   THEN cVarValue = STRING(lv-vend-no,"x(8)") .
                         WHEN "brd-rc"  THEN cVarValue = STRING(ld-qty-rec,"->>>>>>>>9") .

                         WHEN "qty-ord"  THEN cVarValue = STRING(w-ord.ord-qty,"->>>>>>>>9") .
                         WHEN "qty-comp"   THEN cVarValue = STRING(v-comp-qty,"->>>>>>>>9") .
                         WHEN "qty-hand"  THEN cVarValue = STRING(w-ord.onh-qty,"->>>>>>>>9") .
                         WHEN "ship-city"   THEN cVarValue = STRING(v-ship-city,"x(15)") .
                         WHEN "stat"  THEN cVarValue = STRING(w-ord.xls-status,"x(6)") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 /* last time we use lv-text here, put a ',' in pos 5 for excel */
                IF trim(lv-text) <> "" and NOT rd_lComments THEN 
                    ASSIGN lv-text = substring(lv-text,1,4) + '","' + substring(lv-text,6).
                PUT STREAM st-excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

   /*IF tb_excel THEN DO:      
      /* last time we use lv-text here, put a ',' in pos 5 for excel */
      IF trim(lv-text) <> "" and NOT rd_lComments THEN 
         ASSIGN lv-text = substring(lv-text,1,4) + '","' + substring(lv-text,6).
      PUT STREAM st-excel UNFORMATTED
         '"' w-ord.rel-date       '",'              
         '"' w-ord.last-date      '",'
         '"' w-ord.ord-no         '",'
         '"' w-ord.po-num         '",'
         '"' itemfg.i-name          '",'              
         '"' lv-routing           '",'
         '"' w-ord.rel-no         '",'
         '"' lv-vend-no           '",'
         '"' ld-qty-rec           '",'
         '"' w-ord.ord-qty        '",'
         '"' v-comp-qty           '",'
         '"' w-ord.onh-qty        '",'
         '"'  v-ship-city         '",'                                               
         '"' tt-report.key-06    '",' 
         SKIP.
   END.*/
   
 
   /*IF LAST-OF(w-ord.cust-name) THEN PUT SKIP(1).*/
   ASSIGN
      v-comp-qty = 0
      lv-prev-cust-name = w-ord.cust-name     
      lv-first-last-set = NO.
   
   FOR EACH tt-fg-set 
      BREAK BY tt-fg-set.QtyPerSet
            BY tt-fg-set.part-qty-dec:
      
      lv-first-last-set = FIRST(tt-fg-set.QtyPerSet) AND LAST(tt-fg-set.QtyPerSet).
      
      IF NOT lv-first-last-set THEN DO:
         PUT SPACE(5)
            "S/B: "
            TRIM(STRING(tt-fg-set.QtyPerSet,">>")) + "/" + TRIM(STRING(tt-fg-set.part-qty-dec,">>"))   FORMAT "x(5)"
            SPACE(1).
         ll-po = NO.
         IF LAST-OF(tt-fg-set.QtyPerSet) THEN
            FOR EACH po-ordl WHERE 
                     po-ordl.company   EQ job.company
                 AND po-ordl.job-no    EQ job.job-no
                 AND po-ordl.job-no2   EQ job.job-no2
                 AND po-ordl.s-num     EQ INTEGER(tt-fg-set.QtyPerSet)
                 AND po-ordl.item-type EQ YES
               USE-INDEX job-no NO-LOCK,
               FIRST po-ord WHERE 
                     po-ord.company EQ po-ordl.company
                 AND po-ord.po-no   EQ po-ordl.po-no NO-LOCK,
               FIRST item WHERE 
                     item.company EQ po-ordl.company
                 AND item.i-no    EQ po-ordl.i-no
                 AND INDEX("1234BPR",item.mat-type) GT 0 NO-LOCK
               BREAK BY po-ordl.po-no
                     BY po-ordl.i-no
                     BY po-ordl.rec_key:
               
               ll-po = YES.
               IF po-ordl.pr-qty-uom EQ "EA" THEN
                  ld-qty-ord = po-ordl.ord-qty.
               ELSE
                  RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",item.basis-w,po-ordl.s-len,po-ordl.s-wid,item.s-dep,po-ordl.ord-qty, output ld-qty-ord).
               {sys/inc/roundup.i ld-qty-ord}
    
               IF po-ordl.cons-uom EQ "EA" THEN
                  ld-qty-rec = po-ordl.t-rec-qty.
               ELSE
                  RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA",item.basis-w,po-ordl.s-len,po-ordl.s-wid,item.s-dep,po-ordl.t-rec-qty, output ld-qty-rec).
               {sys/inc/roundup.i ld-qty-rec}
    
               /*IF NOT LAST(po-ordl.po-no) THEN PUT SPACE(16).*/
    
               PUT 
                  "Brd PO#: " 
                  TRIM(STRING(po-ordl.po-no,">>>>>>>>")) FORMAT "x(8)"
                  SPACE(1)
                  "Vendor: "
                  po-ord.vend-no                         FORMAT "x(8)"
                  SPACE(1)
                  "Qty Rec'd: "
                  TRIM(STRING(ld-qty-rec,">>>,>>>,>>9")) FORMAT "x(11)"
                  SPACE(1).
    
               IF NOT LAST(po-ordl.po-no) THEN PUT SKIP.
            END.
       
         IF NOT ll-po THEN PUT SPACE(58).

         FOR EACH job-mch NO-LOCK WHERE 
                  job-mch.company EQ job.company
              AND job-mch.job     EQ job.job
              AND job-mch.job-no  EQ job.job-no
              AND job-mch.job-no2 EQ job.job-no2
              AND job-mch.frm     EQ tt-fg-set.QtyPerSet
            BREAK BY job-mch.line:
          
            IF FIRST(job-mch.line) THEN 
               PUT "Routing: ".
            PUT UNFORMATTED job-mch.m-code.
            
            IF NOT LAST(job-mch.line) THEN 
               PUT ", ".
         END.
         PUT SKIP.
       END. /* not first-last */
   END. /* for each tt-fg-set*/

   FOR EACH tt-formtext:
      DELETE tt-formtext.
   END.
END.
/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
