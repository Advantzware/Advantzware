/* ------------------------------------------- cec/quote/quoknight-xl.i  */
/* print quote items                                                           */
/* -------------------------------------------------------------------------- */

FIND FIRST est WHERE est.company EQ xquo.company
                 AND est.est-no  EQ xquo.est-no
               NO-LOCK NO-ERROR.
                       
FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:
    ASSIGN
       numfit       = 0
       ll-prt-dscr2 = s-print-2nd-dscr AND xqitm.part-dscr2 NE "" .
    
    /* Creating next TEMP-TABLE record for Jasper */
    create ttQuoKnightXL.
   
     FOR EACH xqqty OF xqitm NO-LOCK:
         numfit = numfit + 1.
     END.
   
     IF numfit LT 4 THEN numfit = 4.
   
     RELEASE eb.
     RELEASE ef.
   
     IF AVAIL est THEN
        FIND FIRST eb 
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               AND eb.part-no EQ xqitm.part-no
               AND eb.form-no NE 0
             NO-LOCK NO-ERROR.
   
     IF NOT AVAIL eb AND xqitm.est-no <> "" THEN
        FIND FIRST eb
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               and eb.form-no NE 0
             NO-LOCK NO-ERROR.
         
     IF AVAIL eb THEN
        FIND FIRST ef
             WHERE ef.company EQ est.company
               AND ef.est-no  EQ est.est-no
               AND ef.form-no EQ eb.form-no
             NO-LOCK NO-ERROR.
   
     FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
   
     lv-two-box = NO.
     IF est.est-type = 6 THEN DO:  /* two piece box */
        FIND FIRST bf-eb WHERE bf-eb.company EQ est.company
                           AND bf-eb.est-no  EQ est.est-no
                           and bf-eb.form-no = 0 NO-LOCK NO-ERROR.
        IF AVAIL bf-eb AND bf-eb.part-no = eb.part-no THEN lv-two-box = YES.
        FIND FIRST bf-eb WHERE bf-eb.company EQ est.company
                           AND bf-eb.est-no  EQ est.est-no
                           and bf-eb.form-no <> 0
                           AND RECID(bf-eb) <> RECID(eb)
                           NO-LOCK NO-ERROR.
        IF AVAIL bf-eb THEN lv-two-box = NO.
     END. /* END OF IF est.est-type = 6 */
     
     IF numfit LT 5 THEN numfit = 5 + INT(ll-prt-dscr2).
   
     DO i = 1 TO numfit:
        lv-yes-printed = NO.
        
        IF i EQ 1 THEN DO:
   
           lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".
   
           /* excel output */
           ASSIGN inrowcount = inrowcount + 1 .
           
           create ttQuoKnightXL.
           assign ttQuoKnightXL.est_num = TRIM(lv-est-no)
                  ttQuoKnightXL.description = xqitm.part-dscr1
                  ttQuoKnightXL.item = xqitm.part-no.
           
           ASSIGN LvLineCnt = inrowcount.
        END.
        ELSE
        IF i EQ 2 THEN DO:
           trim-size = "".
           IF AVAIL est AND est.est-type = 6 THEN DO: /* set header */
              IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
                 DO cc = 1 TO LENGTH(xqitm.size):
                    IF SUBSTR(xqitm.size,cc,1) NE "" THEN
                       trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
                 end.
              ELSE
                 trim-size = xqitm.size.

              IF AVAIL eb THEN
                 FIND FIRST bf-eb WHERE bf-eb.company = xquo.company
                        AND bf-eb.est-no = xquo.est-no
                        AND bf-eb.form-no > 0
                        AND recid(bf-eb) <>  RECID(eb)
                      NO-LOCK NO-ERROR.

              IF NOT AVAIL bf-eb THEN DO: /* 2 piece box RSC */
                 FIND FIRST style WHERE
                      style.company = xqitm.company AND
                      xqitm.style = style.style
                      NO-LOCK NO-ERROR.

                 style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.
              END.
              ELSE
                 style-dscr = "".
           END.
           ELSE DO:
              FIND FIRST style WHERE
                   style.company = xqitm.company AND
                   style.style = xqitm.style
                   NO-LOCK NO-ERROR.

              style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.
       
              IF AVAIL eb THEN
                 ASSIGN
                    ld-len = eb.len * ld-metric
                    ld-wid = eb.wid * ld-metric
                    ld-dep = eb.dep * ld-metric.
              ELSE
                 ASSIGN ld-len = 0
                        ld-wid = 0
                        ld-dep = 0.
       
              IF AVAIL est AND est.metric THEN DO:
                {sys/inc/roundup.i ld-len}
                {sys/inc/roundup.i ld-wid}
                {sys/inc/roundup.i ld-dep}
              END.
       
              IF AVAIL eb AND AVAIL style AND int(style.industry) > 1 AND NOT est.metric THEN
                 trim-size = TRIM(STRING({sys/inc/k16v.i ld-len})) + "x" +
                             TRIM(STRING({sys/inc/k16v.i ld-wid})) + "x" +
                             TRIM(STRING({sys/inc/k16v.i ld-dep})).
       
              ELSE
              IF AVAIL eb THEN
                trim-size = TRIM(STRING(ld-len,lv-format)) + "x" +
                            TRIM(STRING(ld-wid,lv-format)) + "x" +
                            TRIM(STRING(ld-dep,lv-format)).
       
              ELSE trim-size = "".
           END.

           ASSIGN
              lv-part-dscr2 = IF ll-prt-dscr2 THEN xqitm.part-dscr2 ELSE style-dscr
              LvLineCnt = LvLineCnt + 1 .
       
           create ttQuoKnightXL.
           assign ttQuoKnightXL.description = lv-part-dscr2
                  ttQuoKnightXL.item = trim-size.
           
        END.
        ELSE
        IF i EQ 3 THEN DO:
           lv-i-coldscr = IF ll-prt-dscr2 THEN style-dscr ELSE xqitm.i-coldscr.
           
           ASSIGN LvLineCnt = LvLineCnt + 1 .
          
           create ttQuoKnightXL.
           assign ttQuoKnightXL.description = lv-i-coldscr
                  ttQuoKnightXL.item = "DIE#: " + (IF AVAIL eb THEN eb.die-no ELSE "") .
       
        END.
        ELSE
        IF i EQ 4 THEN DO:
           
           ASSIGN LvLineCnt = LvLineCnt + 1 .
                  
           create ttQuoKnightXL.
           assign ttQuoKnightXL.description = xqitm.i-dscr
                  ttQuoKnightXL.item = "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") .
        END.
        ELSE
        IF i EQ 5 THEN DO:
           v-board = "".
           
           IF s-print-2nd-dscr THEN
             IF v-boardDescription EQ 'Est' THEN
                v-board = IF AVAIL ef THEN ef.brd-dscr ELSE "".
             ELSE
                v-board = xqitm.i-dscr.
           ELSE
           IF AVAIL ef THEN
              v-board = ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                        ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6].
           
           RELEASE bf-eb.
           IF AVAILABLE(est) AND est.est-type = 6 THEN
              FIND FIRST bf-eb WHERE bf-eb.company EQ xquo.company
                                 AND bf-eb.est-no  EQ xquo.est-no
                                 AND bf-eb.form-no EQ 0
                                 NO-LOCK NO-ERROR.
       
           ASSIGN
              lv-fg# = IF AVAILABLE(est) AND est.est-type EQ 6 
                                         AND AVAIL bf-eb THEN bf-eb.stock-no
                       ELSE IF AVAIL eb THEN eb.stock-no ELSE xqitm.part-no
              LvLineCnt = LvLineCnt + 1 .
           
           create ttQuoKnightXL.
           assign ttQuoKnightXL.description = xqitm.part-dscr2
                  ttQuoKnightXL.item = "FG#: " + lv-fg# .
           
           ASSIGN LvLineCnt = LvLineCnt + 1 .
           
           create ttQuoKnightXL.
           assign ttQuoKnightXL.description = v-board.
        END.
       
        ELSE
        IF numfit EQ 6 THEN DO:
           v-board = "".
           IF AVAIL ef THEN
              v-board = ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                        ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6].
             
              IF v-board NE "" THEN 
              DO:
                 ASSIGN LvLineCnt = LvLineCnt + 1.
                 create ttQuoKnightXL.
                 assign ttQuoKnightXL.description = v-board.
              END.
              ELSE
                 numfit = 5.
       
           IF v-board = "" THEN numfit = 5.
        END.
        
        ELSE DO:
           create ttQuoKnightXL.
        END.
     
        IF AVAIL xqqty THEN DO:
           ASSIGN
              xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                       IF xqqty.uom EQ "C" THEN
                         ((xqqty.qty / 100) * xqqty.price)     ELSE
                       IF xqqty.uom EQ "M" THEN
                         ((xqqty.qty / 1000) * xqqty.price)    ELSE
                         (xqqty.qty * xqqty.price)
       
              lv-uom = IF est.est-type = 6 AND NOT lv-two-box THEN "SET" ELSE xqqty.uom.
       
           assign ttQuoKnightXL.quantity = xqqty.qty.
       
           IF lv-uom EQ "EA" THEN
              assign ttQuoKnightXL.price = "$" + TRIM(STRING(xqqty.price,">>>,>>9.999")).
           ELSE
              assign ttQuoKnightXL.price = "$" + TRIM(STRING(xqqty.price,">>>,>>9.99")).
                   
           ASSIGN ttQuoKnightXL.uom = lv-uom
                  v-line-total = v-line-total + xqqty.price.
        END.
       
        ASSIGN inrowcount = inrowcount + 1 . 
        
        FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
     END. /* DO i = 1 TO numfit */
   
     IF s-print-comp THEN DO :      /* Print components of a set */
      
        FIND FIRST est
             WHERE est.company EQ xquo.company
               AND est.est-no  EQ xquo.est-no
             NO-LOCK NO-ERROR.
      
        IF AVAIL est then
           FIND FIRST ef WHERE ef.company EQ xquo.company
                           AND ef.est-no  EQ xquo.est-no NO-LOCK NO-ERROR. 
        
        IF AVAILABLE(est) AND 
           AVAILABLE(ef)  AND 
           est.est-type EQ 6 AND /* this field identifies if a set is to be print */
          (est.form-qty GT 1 OR NOT can-find(eb OF ef)) THEN
          FOR EACH ef WHERE ef.company EQ est.company
                         AND ef.est-no  EQ est.est-no NO-LOCK,      
               EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
               
             IF FIRST(ef.form-no) THEN DO:
               
               ASSIGN inrowcount = inrowcount + 1.
               create ttQuoKnightXL.
                      ttQuoKnightXL.description = "Components".
             END.
             ELSE IF LINE-COUNTER GT PAGE-SIZE - 2 THEN PAGE.
          
             FIND FIRST style WHERE style.company = eb.company
                                AND style.style = eb.style NO-LOCK NO-ERROR.
          
             ASSIGN
              ld-len = eb.len * ld-metric
              ld-wid = eb.wid * ld-metric
              ld-dep = eb.dep * ld-metric.
          
             IF est.metric THEN DO:
               {sys/inc/roundup.i ld-len}
               {sys/inc/roundup.i ld-wid}
               {sys/inc/roundup.i ld-dep}
             END.
          
             IF AVAIL style AND INT(style.industry) > 1 AND NOT est.metric THEN
                trim-size = TRIM(STRING({sys/inc/k16v.i ld-len})) + "x" +
                            TRIM(STRING({sys/inc/k16v.i ld-wid})) + "x" +
                            TRIM(STRING({sys/inc/k16v.i ld-dep})).
          
             ELSE
                trim-size = TRIM(STRING(ld-len,lv-format)) + "x" +
                            TRIM(STRING(ld-wid,lv-format)) + "x" +
                            TRIM(STRING(ld-dep,lv-format)).
                       
             IF LENGTH(TRIM(trim-size)) GT 24 THEN
                DO cc = 1 TO LENGTH(trim-size):
                   IF SUBSTR(trim-size,cc,1) NE "" THEN
                      temp-trim-size = temp-trim-size + SUBST(trim-size,cc,1).
                END.
             ELSE
                temp-trim-size = trim-size.
           
             ASSIGN inrowcount = inrowcount + 1.
             create ttQuoKnightXL.
                    ttQuoKnightXL.description = eb.part-no + " "  + temp-trim-size.
             
             FIND FIRST style
                  WHERE style.company EQ cocode
                    AND style.style   EQ eb.style
                  NO-LOCK NO-ERROR.
      
             ASSIGN
                style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style
                inrowcount = inrowcount + 1.
      
             create ttQuoKnightXL.
             ASSIGN ttQuoKnightXL.description = eb.part-dscr1 + " "  + style-dscr
                    v-board = ef.board    + " - " +
                              ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                              ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6].
                     
             IF SUBSTR(v-board,LENGTH(TRIM(v-board)),1) EQ "-" THEN
                SUBSTR(v-board,LENGTH(TRIM(v-board)),1) = "".
          
             IF v-board EQ "" THEN DO:
                FIND FIRST item
                     WHERE item.company EQ cocode
                       AND item.i-no    EQ ef.board
                     NO-LOCK NO-ERROR.
                IF AVAIL item THEN
                   v-board = IF item.i-name   GT "" THEN item.i-name   ELSE
                             IF item.est-dscr GT "" THEN item.est-dscr ELSE
                                item.i-dscr.
             END.
          
             ASSIGN inrowcount = inrowcount + 1.
             create ttQuoKnightXL.
             assign ttQuoKnightXL.description = eb.part-dscr2 + " "  + v-board.
             ASSIGN inrowcount = inrowcount + 1.
             create ttQuoKnightXL.
             assign ttQuoKnightXL.description = eb.i-coldscr.
          END. /*each ef*/
     END.    /* disp components */
   
     numfit = 0.
      
     FOR EACH xqqty OF xqitm NO-LOCK,
         EACH xqchg NO-LOCK
         where xqchg.company eq xqqty.company
             and xqchg.loc eq xqqty.loc
             and xqchg.q-no eq xqqty.q-no
             and xqchg.line eq xqqty.line
             and xqchg.qty eq xqqty.qty
         BREAK BY xqchg.qty
               BY xqchg.charge:
     
       IF FIRST-OF(xqchg.qty) THEN DO:
          IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
       END.
       numfit = numfit + 1.
     END.
     
     FOR EACH xqqty OF xqitm NO-LOCK,
         EACH xqchg WHERE xqchg.company = xqqty.company
                      AND xqchg.loc = xqqty.loc
                      AND xqchg.q-no = xqqty.q-no
                      AND xqchg.LINE = xqqty.LINE
                      AND xqchg.qty = xqqty.qty
                      AND NOT CAN-FIND(FIRST est-prep
                                       WHERE est-prep.company EQ xquo.company
                                         AND est-prep.est-no  EQ xquo.est-no
                                         AND est-prep.s-num   EQ xqchg.s-num
                                         AND est-prep.b-num   EQ xqchg.b-num
                                         AND est-prep.dscr    EQ xqchg.charge)
         NO-LOCK 
         BREAK BY xqchg.qty
               BY xqchg.charge:
     
         IF FIRST-OF(xqchg.qty) THEN DO:
            IF FIRST(xqchg.qty) THEN PUT SKIP(1).
           
            ASSIGN inrowcount = inrowcount + 1.
            create ttQuoKnightXL.
            assign ttQuoKnightXL.description = "For QTY: " + TRIM(STRING(xqchg.qty,">>>>>>>")).
         END.
                                                 
         lv-chg-amt = xqchg.amt.   
         
         IF xqchg.bill EQ "N" THEN
         do:
            IF not FIRST-OF(xqchg.qty) THEN DO:
               ASSIGN inrowcount = inrowcount + 1.
               create ttQuoKnightXL.
            END.
            
            IF FIRST-OF(xqchg.qty) THEN
                 assign ttQuoKnightXL.description = ttQuoKnightXL.description + "  " + STRING(xqchg.charge).
            ELSE
                 assign ttQuoKnightXL.description = xqchg.charge.
         end.
         
         ELSE IF INDEX("TML",xqchg.bill) GT 0 THEN
         do:
            IF not FIRST-OF(xqchg.qty) THEN DO:
               ASSIGN inrowcount = inrowcount + 1.
               create ttQuoKnightXL.
            END.
           
            IF FIRST-OF(xqchg.qty) THEN
               assign ttQuoKnightXL.description = ttQuoKnightXL.description + "  " + STRING(xqchg.charge).
            ELSE
               assign ttQuoKnightXL.description = xqchg.charge.
                  
            assign ttQuoKnightXL.price = STRING(lv-chg-amt,"$>>,>>9.99").
         
         end.
         
         ELSE IF xqchg.bill EQ "W" THEN
         do:
            IF not FIRST-OF(xqchg.qty) THEN DO:
               ASSIGN inrowcount = inrowcount + 1.
               create ttQuoKnightXL.
            END.
           
            IF FIRST-OF(xqchg.qty) THEN
               assign ttQuoKnightXL.description = ttQuoKnightXL.description + "  " + STRING(xqchg.charge).
            ELSE
               assign ttQuoKnightXL.description = xqchg.charge.
         end.
     END.
END.

ASSIGN INROWCOUNT = INROWCOUNT + 1
       numfit = 0.

FOR EACH xqchg OF xquo NO-LOCK
    WHERE (xqchg.qty EQ 0 AND xqchg.line EQ 0)
       OR CAN-FIND(FIRST est-prep
                   WHERE est-prep.company EQ xquo.company
                     AND est-prep.est-no  EQ xquo.est-no
                     AND est-prep.s-num   EQ xqchg.s-num
                     AND est-prep.b-num   EQ xqchg.b-num
                     AND est-prep.dscr    EQ xqchg.charge)
       OR NOT CAN-FIND(FIRST xqqty
                       WHERE xqchg.company EQ xqqty.company
                         AND xqchg.loc     EQ xqqty.loc
                         AND xqchg.q-no    EQ xqqty.q-no
                         AND xqchg.line    EQ xqqty.line
                         AND xqchg.qty     EQ xqqty.qty)
    BREAK BY xqchg.charge:

    IF FIRST(xqchg.charge) THEN
       numfit = numfit + 1.

    numfit = numfit + 1.
END.

FOR EACH xqchg OF xquo NO-LOCK
    WHERE (xqchg.qty EQ 0 AND xqchg.line EQ 0)
       OR CAN-FIND(FIRST est-prep
                   WHERE est-prep.company EQ xquo.company
                     AND est-prep.est-no  EQ xquo.est-no
                     AND est-prep.s-num   EQ xqchg.s-num
                     AND est-prep.b-num   EQ xqchg.b-num
                     AND est-prep.dscr    EQ xqchg.charge)
       OR NOT CAN-FIND(FIRST xqqty
                       WHERE xqchg.company EQ xqqty.company
                         AND xqchg.loc     EQ xqqty.loc
                         AND xqchg.q-no    EQ xqqty.q-no
                         AND xqchg.line    EQ xqqty.line
                         AND xqchg.qty     EQ xqqty.qty)
    BREAK BY xqchg.qty
          BY xqchg.charge
          BY xqchg.s-num
          BY xqchg.b-num:
 
    lv-chg-amt = xqchg.amt .

    IF xqchg.bill EQ "N" THEN
    do:
       ASSIGN inrowcount = inrowcount + 1.
       create ttQuoKnightXL.
       assign ttQuoKnightXL.description = xqchg.charge.
    end.
     
    ELSE IF INDEX("TML",xqchg.bill) GT 0 THEN
    do:
       ASSIGN inrowcount = inrowcount + 1.

       create ttQuoKnightXL.
       assign ttQuoKnightXL.description = xqchg.charge
              ttQuoKnightXL.price = STRING(lv-chg-amt,"$>>,>>9.99")
              ttQuoKnightXL.uom = "EA".
    end.
    
    ELSE IF xqchg.bill EQ "W" THEN
    do:
       ASSIGN inrowcount = inrowcount + 1.
       create ttQuoKnightXL.
       assign ttQuoKnightXL.description = xqchg.charge.
    end.

    IF LAST-OF(xqchg.qty) THEN
       LEAVE.
END.

if (ch-multi and v-last) or (not ch-multi) then do:
   if ch-inst then do:
     
      FIND FIRST est WHERE est.company = xquo.company
                       AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
      {custom/notespr2.i job v-inst2 EXTENT(v-inst2) "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept" }
      idx = 0.
      DO i = 1 TO EXTENT(v-dept-inst) /*6*/:
         v-dept-inst[i] = v-inst2[i].
         IF v-dept-inst[i] NE '' THEN idx = idx + 1.
      END.
      IF idx NE 0 THEN DO:
         
         ASSIGN inrowcount = inrowcount + 2.
  
         create ttQuoKnightXL.
         create ttQuoKnightXL.
         assign ttQuoKnightXL.description = "Department Notes: ".
         
         DO idx = 1 TO EXTENT(v-dept-inst):
            IF v-dept-inst[idx] NE "" THEN 
            do:
               ASSIGN inrowcount = inrowcount + 1.
               create ttQuoKnightXL.
               assign ttQuoKnightXL.description = v-dept-inst[idx].
            end.
         END. /* do idx */
      END. /* if idx */
     
      for each est-inst
          where est-inst.company = xquo.company
            AND est-inst.est-no eq xquo.est-no
            and est-inst.dept  ge fdept
            and est-inst.dept  le tdept
          no-lock break by est-inst.est-no by est-inst.dept:
        
          if first-of(est-inst.est-no) then do:
             
             ASSIGN inrowcount = inrowcount + 2.
             
            create ttQuoKnightXL.
            create ttQuoKnightXL.
            assign ttQuoKnightXL.est_num = "---Dept---"
                   ttQuoKnightXL.description = "--Department Manufacturing Instructions--".
             
          end.
          
          ASSIGN inrowcount = inrowcount + 1.
          
          create ttQuoKnightXL.
          assign ttQuoKnightXL.est_num = est-inst.dept
                 ttQuoKnightXL.description = est-inst.inst[1]
                 inrowcount = inrowcount + 1.
          
          create ttQuoKnightXL.
          assign ttQuoKnightXL.description = est-inst.inst[2]
                 inrowcount = inrowcount + 1.
          
          create ttQuoKnightXL.
          assign ttQuoKnightXL.description = est-inst.inst[3].
      end.
   end.
  
   do i = 1 to 5:
      if xquo.comment[i] ne "" then do:
         ASSIGN inrowcount = inrowcount + 1 .
         create ttQuoKnightXL.
         create ttQuoKnightXL.
         leave.
       end.
   end.
  
   do i = 1 to 5:
      if xquo.comment[i] ne "" then 
      do:
         ASSIGN inrowcount = inrowcount + 1.
         
         assign comment[i] = xquo.comment[i].
         
         /* Not clear how to handle this, need to modify jrxml file report.
            Merge cells means combine text for cell range into one cell. This is handling by
            storing the data into single TEMT-TABLE record that is exporting bu jasper.
            The HorizontalAlignment = 3 probably means left alignment in Excel however
            need to be comfirmed by tests */
/*         chExcelApplication:Range("B" + STRING(inrowcount) + ":F"
                                  + STRING(inrowcount)):MergeCells = TRUE NO-ERROR.
         
         chExcelApplication:range("B" + STRING(inrowcount) + ":F"
                                  + STRING(inrowcount)):HorizontalAlignment = 3 NO-ERROR.*/
      end.
   end.
   rptExcel:set-report-param("comment1", comment[1]).
   rptExcel:set-report-param("comment2", comment[2]).
   rptExcel:set-report-param("comment3", comment[3]).
   rptExcel:set-report-param("comment4", comment[4]).
   rptExcel:set-report-param("comment5", comment[5]).
end.

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
