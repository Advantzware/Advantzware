/* ------------------------------------------- cec/quote/quoxprnt.i 11/00 JLF */
/* print quote items in xPrint format                                         */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.              */
/* -------------------------------------------------------------------------- */

lDataPrint = FALSE .
logSetPrinting = FALSE.
FIND FIRST est WHERE est.company EQ xquo.company
                 AND est.est-no  EQ xquo.est-no
               NO-LOCK NO-ERROR.


FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:
  ASSIGN
   numfit       = 0
   ll-prt-dscr2 = s-print-2nd-dscr AND xqitm.part-dscr2 NE "".

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.

  RUN printHeader (12, OUTPUT v-printline).
         
  RELEASE eb.
  RELEASE ef.
  FIND FIRST est WHERE est.company = xquo.company
                   AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
  IF AVAILABLE est THEN
  FIND FIRST eb
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        AND eb.part-no EQ xqitm.part-no
        AND eb.form-no NE 0
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE eb AND xqitm.est-no <> "" THEN
  IF AVAILABLE est THEN FIND FIRST eb
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        AND eb.form-no NE 0
      NO-LOCK NO-ERROR.
        
  IF AVAILABLE eb THEN DO:
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ eb.form-no
        NO-LOCK NO-ERROR.

  IF AVAILABLE est AND est.metric THEN
    ASSIGN
     ld-metric = 25.4
     lv-format = "->>,>>>mm".
  ELSE
    ASSIGN
     ld-metric = 1
     lv-format = ">>>>>9.99<<<".
  END.

  
  IF numfit < 7  THEN numfit = 7.
  


  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  FIND FIRST itemfg WHERE itemfg.company = xqitm.company
                      AND itemfg.i-no = xqitm.part-no NO-LOCK NO-ERROR.
  ASSIGN lPrintSecDscr = NO .
  DO i = 1 TO numfit /*WITH FRAME item-10p */:
    lDataPrint = FALSE .
    IF i EQ 1 THEN DO:
      /*      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.  */
      lv-est-no = IF AVAILABLE eb THEN xquo.est-no ELSE "".
      lv-part-dscr1 = IF AVAILABLE est AND est.est-type EQ 6 
                                   AND AVAILABLE itemfg 
                      THEN itemfg.i-name
                      ELSE xqitm.part-dscr1.
     
      PUT "<C1>" TRIM(lv-est-no) FORM "x(8)" .
      IF length(xqitm.part-no) LE 15 THEN
      PUT "<C8.5>" xqitm.part-no FORMAT "x(15)"          
          "<C22>" TRIM(lv-part-dscr1) FORMAT "x(30)".
      ELSE do: 
         PUT
          "<C8.5>" xqitm.part-no FORMAT "x(30)".
          IF lv-part-dscr1 NE "" THEN
          PUT 
            SKIP           
            "<C22>" TRIM(lv-part-dscr1) FORMAT "x(30)".
      END. 
          lDataPrint = TRUE .
   
    END.

    ELSE IF i EQ 2 THEN DO:  
      IF ll-prt-dscr2 AND xqitm.part-dscr2 NE "" THEN DO:
           PUT xquo.q-no "<C22>" xqitm.part-dscr2  .
           lPrintSecDscr = YES .
           lDataPrint = TRUE .
      END.
       ELSE DO:
            numfit = numfit + 1 .
            NEXT .
       END.
    END.

    ELSE
    IF i EQ 3 THEN DO:  
      trim-size = "".
      IF AVAILABLE est AND est.est-type = 6 THEN DO: /* set header */
         IF AVAILABLE eb THEN FIND FIRST bf-eb WHERE bf-eb.company = xquo.company
                            AND bf-eb.est-no = xquo.est-no
                            AND bf-eb.form-no > 0
                            AND recid(bf-eb) <>  RECID(eb) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bf-eb THEN DO: /* 2 piece box RSC */
            FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
            style-dscr = IF AVAILABLE style THEN style.dscr ELSE xqitm.style.
         END.
         ELSE style-dscr = "".
      END.
      ELSE DO:
           FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
           style-dscr = IF AVAILABLE style THEN style.dscr ELSE xqitm.style.
      END.
      IF lPrintSecDscr THEN DO: 
          IF style-dscr NE "" THEN DO:
              PUT "<C22>" style-dscr FORM "x(30)" .
              lDataPrint = TRUE .
          END.
          ELSE DO:
            numfit = numfit + 1 .
            NEXT .
          END.
      END.
      ELSE DO: 
          PUT  xquo.q-no   "<C22>" style-dscr  FORM "x(30)" .
          lDataPrint = TRUE .
      END.
    END.

     ELSE
    IF i EQ 4 THEN DO:  
      trim-size = "".
      IF AVAILABLE est AND est.est-type = 6 THEN DO: /* set header */
         IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
         DO cc = 1 TO LENGTH(xqitm.size):
            IF SUBSTR(xqitm.size,cc,1) NE "" THEN
               trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
         END.

         ELSE trim-size = xqitm.size.
      END.
      ELSE DO:
          IF AVAILABLE eb THEN ASSIGN
            ld-len = eb.len * ld-metric
            ld-wid = eb.wid * ld-metric
            ld-dep = eb.dep * ld-metric.
           ELSE ASSIGN ld-len = 0
                       ld-wid = 0
                       ld-dep = 0.

           IF AVAILABLE est AND est.metric THEN DO:
             {sys/inc/roundup.i ld-len}
             {sys/inc/roundup.i ld-wid}
             {sys/inc/roundup.i ld-dep}
           END.

           IF AVAILABLE eb AND AVAILABLE style AND int(style.industry) > 1 AND NOT est.metric THEN
             trim-size = TRIM(STRING({sys/inc/k16v.i ld-len},lv-format)) + " x " +
                         TRIM(STRING({sys/inc/k16v.i ld-wid},lv-format)) + " x " +
                         TRIM(STRING({sys/inc/k16v.i ld-dep},lv-format)).

           ELSE
           IF AVAILABLE eb THEN
             trim-size = TRIM(STRING(ld-len,lv-format)) + " x " +
                         TRIM(STRING(ld-wid,lv-format)) + " x " +
                         (STRING(ld-dep,lv-format)).

           ELSE trim-size = "".   
      END.
      IF trim-size NE "" THEN DO:
          PUT  "<C22>" trim-size  FORM "x(30)" .
          lDataPrint = TRUE .
      END.
      ELSE DO:
            numfit = numfit + 1 .
            NEXT .
      END.
    END.

    ELSE
    IF i EQ 5 THEN DO:   
     IF  xqitm.i-coldscr NE "" THEN DO:
       PUT "<C22>" xqitm.i-coldscr   FORM "x(30)".
       lDataPrint = TRUE .
     END.
     ELSE DO:
            numfit = numfit + 1 .
            NEXT .
     END.
    END.

    ELSE
    IF i EQ 6 THEN DO:  
       IF xqitm.i-dscr NE "" THEN  DO:
         PUT "<C22>" xqitm.i-dscr  FORMAT "x(30)".
         lDataPrint = TRUE .
       END.
       ELSE DO:
            numfit = numfit + 1 .
            NEXT .
       END.
    END.
    
    ELSE
    IF i EQ 7  THEN DO:                
           IF AVAIL ef THEN
           DO j = 1 TO 6:
               IF ef.adder[j] NE "" THEN
                   cAddrDesc[j] = cAddrDesc[j] + ef.adder[j].
               IF cAddrDesc[j] NE "" THEN DO:
                   IF ef.adder[j + 6] NE "" THEN
                       cAddrDesc[j] = cAddrDesc[j] + " - " + ef.adder[j + 6].
               END.
           END.          
           DO j = 1 TO 6:
               IF cAddrDesc[j] NE "" THEN DO:                    
                   PUT "<C22>" cAddrDesc[j] FORMAT "x(35)" .
                     
                   IF AVAILABLE xqqty THEN DO:
                        xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                        IF xqqty.uom EQ "C" THEN
                        ((xqqty.qty / 100) * xqqty.price)     ELSE
                        IF xqqty.uom EQ "M" THEN
                        ((xqqty.qty / 1000) * xqqty.price)    ELSE
                        (xqqty.qty * xqqty.price).
                                             
                        PUT "<C49>" xqqty.qty FORMAT ">>>>>>>9"  SPACE(2)
                        xqqty.rels SPACE(4)
                        xqqty.price FORMAT "->>>>,>>9.99<<" SPACE(2)
                        xqqty.uom.
                        
                        v-line-total = v-line-total + xqqty.price.
                    END.
                    PUT  SKIP.
                    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.                    
               END.
           END.
    END.

    IF i GT 7 THEN PUT SPACE(58) .

   IF AVAILABLE xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).

       
       PUT "<C49>" xqqty.qty FORMAT ">>>>>>>9"  SPACE(2)
           xqqty.rels SPACE(4)
           xqqty.price FORMAT "->>>>,>>9.99<<" SPACE(2)
           xqqty.uom.
           
       v-line-total = v-line-total + xqqty.price.
       lDataPrint = TRUE .
    END.

/*  IF i NE 3 THEN DOWN. */
    IF lDataPrint THEN DO: 
        PUT  SKIP.
    END.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
   

    RUN printHeader (12, OUTPUT v-printline).
   
  END. /* do numfit */

  IF s-print-comp THEN DO :      /* Print components of a set */
  
    FIND FIRST est
        WHERE est.company EQ xquo.company
          AND est.est-no  EQ xquo.est-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE est THEN
        FIND FIRST ef WHERE ef.company EQ xquo.company
                        AND ef.est-no  EQ xquo.est-no NO-LOCK NO-ERROR. 

    IF AVAILABLE(est) AND 
       AVAILABLE(ef)  AND 
       est.est-type EQ 6 AND /* this field identifies if a set is to be print */
      (est.form-qty GT 1 OR NOT CAN-FIND(eb OF ef)) THEN
    FOR EACH ef WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est.est-no NO-LOCK,      
        EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
        
      RUN printHeader (12,OUTPUT idummy).
      IF FIRST(ef.form-no) THEN DO:
        IF LINE-COUNTER GT PAGE-SIZE - 2 THEN PAGE.
        PUT "Components" AT 5 SKIP.
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

      IF AVAILABLE style AND INT(style.industry) > 1 AND NOT est.metric THEN
        trim-size = TRIM(STRING({sys/inc/k16v.i ld-len},lv-format)) + "x" +
                    TRIM(STRING({sys/inc/k16v.i ld-wid},lv-format)) + "x" +
                    TRIM(STRING({sys/inc/k16v.i ld-dep},lv-format)).

      ELSE
        trim-size = TRIM(STRING(ld-len,lv-format)) + "x" +
                    TRIM(STRING(ld-wid,lv-format)) + "x" +
                    TRIM(STRING(ld-dep,lv-format)).
                
      IF LENGTH(TRIM(trim-size)) GT 24 THEN
      DO cc = 1 TO LENGTH(trim-size):
        IF SUBSTR(trim-size,cc,1) NE "" THEN
           temp-trim-size = temp-trim-size + SUBST(trim-size,cc,1).
      END.
      ELSE temp-trim-size = trim-size.
    
      PUT eb.part-no AT 10 FORM "x(21)" temp-trim-size   SKIP.
    
      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
      style-dscr = IF AVAILABLE style THEN style.dscr ELSE eb.style.      
      PUT eb.part-dscr1 AT 10 FORM "x(21)"
          style-dscr SKIP.

      DO i = 1 TO 6:
          IF AVAIL ef AND ef.adder[i] NE "" THEN
              cAdder = cAdder + ef.adder[i] + ",".
      END.
      cAdder = SUBSTRING(cAdder,1,LENGTH(cAdder) - 1).

    
      v-board = /*IF AVAIL ef THEN*/
                  ef.board    + " - " + cAdder.
                  
                /*ELSE "-"*/.
              
      IF SUBSTR(v-board,LENGTH(TRIM(v-board)),1) EQ "-" THEN
        SUBSTR(v-board,LENGTH(TRIM(v-board)),1) = "".

      IF v-board EQ "" THEN DO:
        FIND FIRST item
            WHERE item.company EQ cocode
              AND item.i-no    EQ ef.board
            NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN
          v-board = IF item.i-name   GT "" THEN item.i-name   ELSE
                    IF item.est-dscr GT "" THEN item.est-dscr ELSE
                       item.i-dscr.
      END.
      
      PUT eb.part-dscr2  AT 10  FORM "x(21)"
          v-board  FORM "x(80)"   SKIP .
    
      PUT eb.i-coldscr   AT 31 SKIP.
    END.
  END.    /* disp components */

  numfit = 0.
  
  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg NO-LOCK
      WHERE xqchg.company EQ xqqty.company
          AND xqchg.loc EQ xqqty.loc
          AND xqchg.q-no EQ xqqty.q-no
          AND xqchg.line EQ xqqty.line
          AND xqchg.qty EQ xqqty.qty
      BREAK BY xqchg.qty
            BY xqchg.charge:

    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
    END.
    numfit = numfit + 1.
  END.

  RUN printHeader (12,OUTPUT idummy).
  

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
      PUT "For QTY: " AT 5
          TRIM(STRING(xqchg.qty,">>>>>>>")).
    END.
                                            
    lv-chg-amt = xqchg.amt.   

    IF xqchg.bill EQ "N" THEN
       PUT xqchg.charge AT 22
          /*"N/C"        TO 80*/  SKIP.
      
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN
       PUT xqchg.charge       AT 22
           /*   "Time & Materials" AT 45  */
           lv-chg-amt /*xqchg.amt*/  TO 80 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
       PUT xqchg.charge    AT 22
           /*  "Will Advise "  TO 45 */  SKIP.

    RUN printHeader (12,OUTPUT v-printline).
  END.

  IF NOT LAST(xqitm.part-no) THEN DO:
      PUT SKIP(1).
      RUN printHeader (12,OUTPUT v-printline).
  END.
  RUN printHeader (12,OUTPUT v-printline).
  
END.

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

  IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.
END.

RUN printHeader (12,OUTPUT idummy).


logPrint = FALSE. /* rdb 02/02/07 01310703 */
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

 
  IF FIRST(xqchg.charge) THEN PUT SKIP(1).
 
    /*IF xqchg.qty EQ 0 OR FIRST-OF(xqchg.b-num) THEN DO:*/
     lv-chg-amt = xqchg.amt .

     IF xqchg.bill EQ "N" THEN
       PUT xqchg.charge AT 22
          /* "N/C"        TO 80 */ SKIP.
      
     ELSE
       IF INDEX("TML",xqchg.bill) GT 0 THEN
         PUT xqchg.charge       AT 22
         /* "Time & Materials" AT 45 */
         /* rdb 02/02/07 01310703 
             lv-chg-amt          TO 80 SKIP. 
          */
             lv-chg-amt          TO 83  
                   "EA"          AT 90 SKIP.
     
     ELSE
       IF xqchg.bill EQ "W" THEN
         PUT xqchg.charge    AT 22
          /*"Will Advise "  TO 45 */ SKIP.

 /*
  END.
  */

  RUN printHeader (12,OUTPUT v-printline).

  IF LAST-OF(xqchg.qty) THEN
    LEAVE.
END.  
/*if (ch-multi and v-last) or (not ch-multi) then */ DO:
  IF ch-inst THEN DO:
      RUN printHeader (12,OUTPUT idummy).
      FIND FIRST est WHERE est.company = xquo.company
                       AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
      {custom/notespr2.i job v-inst2 EXTENT(v-inst2) "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept" }
      idx = 0.
      DO i = 1 TO EXTENT(v-dept-inst) /*6*/:
             v-dept-inst[i] = v-inst2[i].
             IF v-dept-inst[i] NE '' THEN idx = idx + 1.
      END.
      IF idx NE 0 THEN DO:
         IF v-notesPageSpan THEN RUN printHeader (12,OUTPUT idummy).
         ELSE RUN printHeader (idx,OUTPUT idummy).
         
         PUT "Department Notes: " SKIP.
         DO idx = 1 TO EXTENT(v-dept-inst):
           IF v-dept-inst[idx] NE "" THEN PUT v-dept-inst[idx] AT 3.
           IF idx NE EXTENT(v-dept-inst) THEN PUT SKIP.
           IF v-notesPageSpan THEN
           RUN printHeader (12,OUTPUT idummy).
         END. /* do idx */
      END. /* if idx */
  END.
  PUT SKIP(1).
END.

/*put skip(1).*/


/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
