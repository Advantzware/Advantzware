/* ------------------------------------------- cec/quote/quoxprnt.i 11/00 JLF */
/* print quote items in xPrint format                                          */
/* -------------------------------------------------------------------------- */
/* Mode : 001, Task # - 01171305 */

logSetPrinting = FALSE.
FIND FIRST est WHERE est.company EQ xquo.company
                 AND est.est-no  EQ xquo.est-no
               NO-LOCK NO-ERROR.

IF AVAILABLE(est) AND est.est-type EQ 6 THEN
  logSetPrinting = TRUE.

FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:



  ASSIGN
   numfit       = 0
   ll-prt-dscr2 = s-print-2nd-dscr AND xqitm.part-dscr2 NE "".

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.

  RUN printHeader (6, OUTPUT v-printline).
         
  RELEASE eb.
  RELEASE ef.
  FIND FIRST est WHERE est.company = xquo.company
                   AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
  IF AVAIL est THEN
  FIND FIRST eb
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        AND eb.part-no EQ xqitm.part-no
        AND eb.form-no NE 0
      NO-LOCK NO-ERROR.
  IF NOT AVAIL eb AND xqitm.est-no <> "" THEN
  IF AVAIL est THEN FIND FIRST eb
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        and eb.form-no NE 0
      NO-LOCK NO-ERROR.
        
  IF AVAIL eb THEN DO:
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ eb.form-no
        NO-LOCK NO-ERROR.

  IF AVAIL est AND est.metric THEN
    ASSIGN
     ld-metric = 25.4
     lv-format = "->>,>>>mm".
  ELSE
    ASSIGN
     ld-metric = 1
     lv-format = ">>>>>9.9<<<<".
  END.

 
  IF numfit LT 5 AND s-print-2nd-dscr THEN numfit = 6.
  ELSE IF numfit LT 5 THEN numfit = 5.

  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  FIND FIRST itemfg WHERE itemfg.company = xqitm.company
                      AND itemfg.i-no = xqitm.part-no NO-LOCK NO-ERROR.

  /* numfit is the number fields to fit */
  DO i = 1 TO numfit :
    
    IF i EQ 1 THEN DO:
      /* Output item description or item name */
      lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".
      lv-part-dscr1 = IF AVAIL est AND est.est-type EQ 6 
                                   AND AVAIL itemfg 
                      THEN itemfg.i-name
                      ELSE xqitm.part-dscr1.


      PUT TRIM(lv-est-no) FORM "x(5)" AT 2
          xqitm.part-no AT 8 FORMAT "x(21)"          
           TRIM(lv-part-dscr1) AT 29 FORMAT "x(28)". 

    END.
    ELSE
    IF i EQ 2 THEN DO:
      /* Output set header Description or Desc2, quote#, trim size */
      trim-size = "".
      IF AVAIL est AND est.est-type = 6 THEN DO: /* set header */
         IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
         DO cc = 1 TO LENGTH(xqitm.size):
            IF SUBSTR(xqitm.size,cc,1) NE "" THEN
               trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
         end.

         ELSE trim-size = xqitm.size.
         IF AVAIL eb THEN FIND FIRST bf-eb WHERE bf-eb.company = xquo.company
                            AND bf-eb.est-no = xquo.est-no
                            AND bf-eb.form-no > 0
                            AND recid(bf-eb) <>  RECID(eb) NO-LOCK NO-ERROR.
         IF NOT AVAIL bf-eb THEN DO: /* 2 piece box RSC */
            FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
            style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.
         END.
         ELSE style-dscr = "".
      END.
      ELSE DO:
           /* Output Part Description2, quote number, trim size */
           FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
           style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.

           IF AVAIL eb THEN ASSIGN
            ld-len = eb.len * ld-metric
            ld-wid = eb.wid * ld-metric
            ld-dep = eb.dep * ld-metric.
           ELSE ASSIGN ld-len = 0
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
      lv-part-dscr2 = IF ll-prt-dscr2 THEN xqitm.part-dscr2 ELSE style-dscr.
      PUT  xquo.q-no trim-size AT 8 FORM "x(21)" lv-part-dscr2 FORM "x(30)" .
    END.
    ELSE
    IF i EQ 3 THEN DO:
       /* Output color and die description */
        lv-i-coldscr = IF ll-prt-dscr2 THEN style-dscr ELSE xqitm.i-coldscr.
      PUT     "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)"
              lv-i-coldscr  AT 29 FORM "x(33)".
    END.
    ELSE
    IF i EQ 4 THEN DO:
       /* Output CAD or Board Description */
       PUT "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)". 
       IF ll-prt-dscr2 THEN DO:
         chrX = xqitm.i-coldscr.
         PUT xqitm.i-coldscr AT 29 FORM "x(33)".
       END.
       ELSE IF v-boardDescription EQ 'Est' THEN DO:
          chrX = "".
          IF NOT logSetPrinting THEN DO:
              IF AVAILABLE(ef) THEN
                chrX =  IF AVAILABLE(ef) 
                        THEN ef.brd-dscr 
                        ELSE "" .
          END.
          
          PUT chrX AT 29 FORMAT "x(28)".
       END.
       ELSE IF v-boardDescription EQ 'Quote' THEN DO:
         chrX = "".
         IF NOT logSetPrinting THEN
           chrX = xqitm.i-dscr.
         
         PUT chrX AT 29 FORMAT "x(28)".
       END.
    END.
    ELSE
    IF i EQ 5 THEN DO:
       /* Output FG# */
       ASSIGN
         v-board = ""
         adder-print = NO.

       IF (NOT logSetPrinting OR NOT s-print-comp) AND s-print-2nd-dscr THEN
         IF v-boardDescription EQ 'Est' THEN
           v-board = IF AVAIL ef THEN ef.brd-dscr ELSE "".
         ELSE
           v-board = xqitm.i-dscr.

       ELSE
       IF NOT logSetPrinting AND AVAIL ef THEN DO:
         v-board = ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                   ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6].
         adder-print = YES.
       END.         
       
       RELEASE bf-eb.
       IF AVAILABLE(est) AND est.est-type = 6 THEN
          FIND FIRST bf-eb WHERE bf-eb.company EQ xquo.company
                             AND bf-eb.est-no  EQ xquo.est-no
                             AND bf-eb.form-no EQ 0
                             NO-LOCK NO-ERROR.

       lv-fg# = IF AVAILABLE(est) AND est.est-type EQ 6 
                                  AND AVAIL bf-eb THEN bf-eb.stock-no
                ELSE IF AVAIL eb THEN eb.stock-no
                ELSE xqitm.part-no.

       IF logSetPrinting THEN
         PUT "FG#: " + lv-fg# AT 8 FORM "x(21)" v-board FORM "x(28)".
       ELSE DO:
         /*don't print board description twice*/
         IF v-board EQ chrX THEN DO:
            v-board = "".
            IF AVAIL ef THEN
              ASSIGN
                v-board = ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                          ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                adder-print = YES.
         END.
         
         PUT "FG#: " + lv-fg# AT 8 FORM "x(21)" v-board FORM "x(28)".
       END.
    END.

    ELSE
    IF numfit EQ 6 THEN DO:
       /* Output board number */
       v-board = "".
       IF NOT adder-print AND NOT logSetPrinting AND AVAIL ef THEN
         v-board = ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                   ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6].
     
       IF v-board NE "" THEN PUT SPACE(28) v-board FORM "x(28)".
       ELSE numfit = 5.
    END.
    
    /* wfk - don't see what the purpose of this is, was causing a problem */
    /*
    IF i GT numfit THEN        
        PUT SPACE(58) .
    */  
    IF AVAIL xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).
      
       PUT xqqty.qty FORMAT ">>>>>>>9" TO  70 
           xqqty.rels space(5)
           xqqty.price FORM "->>,>>9.99" space(4)
           xqqty.uom.   
       
       v-line-total = v-line-total + xqqty.price.
    END.

    PUT  SKIP.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.

    RUN printHeader (2, OUTPUT v-printline).
  END. /* do numfit */

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
        
      RUN printHeader (1,OUTPUT idummy).
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
      ELSE temp-trim-size = trim-size.
    
      put eb.part-no AT 8 FORM "x(21)" temp-trim-size   SKIP.
    
      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style.      
      PUT eb.part-dscr1 AT 8 FORM "x(21)"
          style-dscr SKIP.
    
      v-board =   ef.board    + " - " +
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                  .
                
      IF SUBSTR(v-board,LENGTH(TRIM(v-board)),1) EQ "-" THEN
        SUBSTR(v-board,LENGTH(TRIM(v-board)),1) = "".

      IF v-board EQ "" THEN DO:
        FIND FIRST item
            WHERE item.company EQ cocode
              AND item.i-no    EQ ef.board
            NO-LOCK NO-ERROR.
        IF AVAIL item THEN
          v-board = IF item.i-name   GT "" THEN item.i-name   ELSE 
                    IF item.est-dscr GT "" THEN item.est-dscr  ELSE
                       item.i-dscr.
      END.
      PUT eb.part-dscr2  AT 8  FORM "x(21)"
          v-board  FORM "x(50)"   SKIP .

       put "color:" AT 28 eb.i-coldscr AT 35 SKIP.   /*Mode 001*/   
      
    END.
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

  RUN printHeader (1,OUTPUT idummy).

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
       PUT xqchg.charge AT 22 SKIP.
      
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN
       PUT xqchg.charge       AT 22           
           lv-chg-amt TO 88 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
       PUT xqchg.charge    AT 22 skip.

    RUN printHeader (2,OUTPUT v-printline).
  END.

  RUN printHeader (1,OUTPUT v-printline).
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

RUN printHeader (2,OUTPUT idummy).


logPrint = FALSE. 
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
     
     lv-chg-amt = xqchg.amt .

     IF xqchg.bill EQ "N" THEN
       PUT xqchg.charge AT 22
          /* "N/C"        TO 80 */ SKIP.
      
     ELSE
       IF INDEX("TML",xqchg.bill) GT 0 THEN
         PUT xqchg.charge       AT 22
             lv-chg-amt         TO 88  
                   "EA"         AT 93 SKIP.
     
     ELSE
       IF xqchg.bill EQ "W" THEN
         PUT xqchg.charge    AT 22 SKIP.


  RUN printHeader (2,OUTPUT v-printline).

  IF LAST-OF(xqchg.qty) THEN
    LEAVE.
END.
do:
  if ch-inst then do:
      RUN printHeader (2,OUTPUT idummy).
      FIND FIRST est WHERE est.company = xquo.company
                       AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
      {custom/notespr2.i job v-inst2 EXTENT(v-inst2) "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept" }
      idx = 0.
      DO i = 1 TO EXTENT(v-dept-inst) /*6*/:
             v-dept-inst[i] = v-inst2[i].
             IF v-dept-inst[i] NE '' THEN idx = idx + 1.
      END.
      IF idx NE 0 THEN DO:
         IF v-notesPageSpan THEN RUN printHeader (1,OUTPUT idummy).
         ELSE RUN printHeader (idx,OUTPUT idummy).
         
         PUT "Department Notes: " SKIP.
         DO idx = 1 TO EXTENT(v-dept-inst):
           IF v-dept-inst[idx] NE "" THEN PUT v-dept-inst[idx] AT 3.
           IF idx NE EXTENT(v-dept-inst) THEN PUT SKIP.
           IF v-notesPageSpan THEN
           RUN printHeader (1,OUTPUT idummy).
         END. /* do idx */
      END. /* if idx */
  end.
  put skip(1).
end.
put skip(1).
  /*  RUN printHeader (2,OUTPUT v-printline).   wfk test 10/24 */
/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
