/* ------------------------------------------- cec/quote/quosoule.i 04/09 GDM */
/* print quote items in xPrint format                                         */
/* -------------------------------------------------------------------------- */

logSetPrinting = FALSE.
FIND FIRST est NO-LOCK 
    WHERE est.company EQ xquo.company
      AND est.est-no  EQ xquo.est-no NO-ERROR.
IF AVAILABLE(est) AND 
   est.est-type EQ 6 /*AND s-print-comp*/ 
  THEN ASSIGN logSetPrinting = TRUE.

FOR EACH xqitm OF xquo NO-LOCK 
    BREAK BY xqitm.part-no:
    
    ASSIGN
        numfit       = 0
        ll-prt-dscr2 = s-print-2nd-dscr AND xqitm.part-dscr2 NE "".

    FOR EACH xqqty OF xqitm NO-LOCK:
        numfit = numfit + 1.
    END.

    RUN printHeader (15, OUTPUT v-printline).

    RELEASE eb.
    RELEASE ef.

    FIND FIRST est NO-LOCK 
        WHERE est.company = xquo.company
        AND est.est-no = xquo.est-no NO-ERROR.
    IF AVAIL est 
      THEN
        FIND FIRST eb NO-LOCK 
          WHERE eb.company EQ est.company
            AND eb.est-no  EQ est.est-no
            AND eb.part-no EQ xqitm.part-no
            AND eb.form-no NE 0 NO-ERROR.
    IF NOT AVAIL eb AND 
       xqitm.est-no <> "" 
      THEN
        IF AVAIL est 
          THEN 
            FIND FIRST eb NO-LOCK 
              WHERE eb.company EQ est.company
                AND eb.est-no  EQ est.est-no
                and eb.form-no NE 0 NO-ERROR.
            IF AVAIL eb THEN DO:
                FIND FIRST ef NO-LOCK
                  WHERE ef.company EQ est.company
                    AND ef.est-no  EQ est.est-no
                    AND ef.form-no EQ eb.form-no  NO-ERROR.
                
                IF AVAIL est AND est.metric 
                  THEN 
                   ASSIGN
                    ld-metric = 25.4
                    lv-format = "->>,>>>mm".
                  ELSE
                   ASSIGN
                    ld-metric = 1
                    lv-format = ">>>>>9.9<<<<".
            END. /* IF AVAIL eb */

    IF numfit LT 5 AND s-print-2nd-dscr 
      THEN numfit = 6.
      ELSE IF numfit LT 5 THEN numfit = 5.

    FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
    FIND FIRST itemfg NO-LOCK 
      WHERE itemfg.company = xqitm.company
        AND itemfg.i-no = xqitm.part-no NO-ERROR.

    DO i = 1 TO numfit /*WITH FRAME item-10p */:
        
        IF i EQ 1 THEN DO:
            
            ASSIGN 
              lv-est-no     = IF AVAIL eb 
                                THEN xquo.est-no ELSE ""
              lv-part-dscr1 = IF AVAIL est         AND 
                                  est.est-type EQ 6 AND 
                                 AVAIL itemfg 
                                THEN itemfg.i-name ELSE xqitm.part-dscr1.
            PUT 
                TRIM(lv-est-no) FORM "x(5)" AT 2
                xqitm.part-no AT 8 FORMAT "x(21)"
                TRIM(lv-part-dscr1) AT 29 FORMAT "x(28)".
        END. /* IF i EQ 1 */
        ELSE
        IF i EQ 2 THEN DO:            
          FIND FIRST style NO-LOCK  WHERE style.company = xqitm.company
                                      AND style.style = xqitm.style NO-ERROR.
               
          ASSIGN style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.

          ASSIGN lv-part-dscr2 = IF ll-prt-dscr2 THEN xqitm.part-dscr2 ELSE style-dscr.

          PUT  
               xquo.q-no  
               IF AVAIL eb 
                 THEN eb.cad-no 
                 ELSE ""            AT 8 FORMAT "x(21)"
               lv-part-dscr2             FORMAT "x(28)" .
        END. /* IF i EQ 2 */
        ELSE
        IF i EQ 3 THEN DO:

            ASSIGN trim-size = "".
            
            IF AVAIL est AND est.est-type = 6 THEN DO: /* set header */
              IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
                  DO cc = 1 TO LENGTH(xqitm.size):

                  IF SUBSTR(xqitm.size,cc,1) NE "" 
                    THEN 
                      ASSIGN trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
              END.
              ELSE ASSIGN trim-size = xqitm.size.

              IF AVAIL eb                                                    
                THEN                                                         
                  FIND FIRST bf-eb NO-LOCK                                   
                    WHERE bf-eb.company = xquo.company                       
                      AND bf-eb.est-no = xquo.est-no                         
                      AND bf-eb.form-no > 0                                  
                      AND RECID(bf-eb) <> RECID(eb) NO-ERROR.                
                  IF NOT AVAIL bf-eb THEN DO: /* 2 piece box RSC */          
                      FIND FIRST style NO-LOCK                               
                        WHERE style.company = xqitm.company                  
                          AND xqitm.style = style.style NO-ERROR.            
                      ASSIGN style-dscr = IF AVAIL style                     
                                            THEN style.dscr ELSE xqitm.style.
                  END. /* IF NOT AVAIL bf-eb */                              
                  ELSE style-dscr = "".                                      
            END. /* IF AVAIL est AND est.est-type = 6 */
            ELSE DO:
              
/*               FIND FIRST style NO-LOCK                                */
/*                   WHERE style.company = xqitm.company                 */
/*                     AND xqitm.style = style.style NO-ERROR.           */
/*                                                                       */
/*               ASSIGN style-dscr = IF AVAIL style                      */
/*                                     THEN style.dscr ELSE xqitm.style. */

              IF AVAIL eb 
                THEN 
                  ASSIGN
                    ld-len = eb.len * ld-metric
                    ld-wid = eb.wid * ld-metric
                    ld-dep = eb.dep * ld-metric.
                ELSE 
                  ASSIGN 
                    ld-len = 0
                    ld-wid = 0
                    ld-dep = 0.

              IF AVAIL est AND est.metric THEN DO:
                  {sys/inc/roundup.i ld-len}
                  {sys/inc/roundup.i ld-wid}
                  {sys/inc/roundup.i ld-dep}
              END. /* IF AVAIL est AND est.metric */

              IF AVAIL eb AND 
                 AVAIL style AND 
                  int(style.industry) > 1 AND 
                  NOT est.metric 
                THEN
                 ASSIGN 
                  trim-size = TRIM(STRING({sys/inc/k16v.i ld-len})) + "x" +
                              TRIM(STRING({sys/inc/k16v.i ld-wid})) + "x" +
                              TRIM(STRING({sys/inc/k16v.i ld-dep})).
                ELSE
                 IF AVAIL eb 
                  THEN
                   ASSIGN 
                    trim-size = TRIM(STRING(ld-len,lv-format)) + "x" +
                                TRIM(STRING(ld-wid,lv-format)) + "x" +
                                TRIM(STRING(ld-dep,lv-format)).
                  ELSE ASSIGN trim-size = "".
            END. /* ELSE DO: */

            ASSIGN lv-i-coldscr = IF ll-prt-dscr2 
                                    THEN style-dscr ELSE xqitm.i-coldscr.
            IF TRIM(lv-i-coldscr) NE "" 
              THEN
               PUT     
                trim-size  AT 8 FORMAT "x(21)"
                lv-i-coldscr  AT 29 FORM "x(28)".
/*             "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)" */
                
        END. /* IF i EQ 3*/
        ELSE
        IF i EQ 4 THEN DO:
         
/*         PUT  "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8 */
/*                   FORMAT "x(21)"                                 
          /*  rdb 01/31/07 12060608 */
            IF ll-prt-dscr2 
              THEN PUT xqitm.i-coldscr AT 29 FORM "x(30)".
              ELSE 
              IF v-boardDescription EQ 'Est' 
               THEN PUT IF AVAIL ef THEN ef.brd-dscr /*xqitm.i-dscr*/ ELSE "" AT 29 FORMAT "x(30)".
               ELSE 
               IF v-boardDescription EQ 'Quote' 
                THEN PUT xqitm.i-dscr AT 29 FORMAT "x(30)".
*/   

          IF ll-prt-dscr2 THEN DO:
            ASSIGN chrX = xqitm.i-coldscr.
            IF TRIM(xqitm.i-coldscr) NE "" 
              THEN
               PUT xqitm.i-coldscr AT 29 FORM "x(28)".
          END.
          ELSE 
          IF v-boardDescription EQ 'Est' THEN DO:
            ASSIGN chrX = "".
            IF NOT logSetPrinting THEN DO:
              IF AVAILABLE(ef) 
                THEN ASSIGN chrX =  IF AVAILABLE(ef) 
                                        THEN ef.brd-dscr  ELSE "" .
            END.
            IF TRIM(chrX) NE "" 
              THEN PUT chrX AT 29 FORMAT "x(30)".
          END.
          ELSE 
          IF v-boardDescription EQ 'Quote' THEN DO:
            ASSIGN chrX = "".
            IF NOT logSetPrinting 
              THEN ASSIGN chrX = xqitm.i-dscr.
            IF TRIM(chrX) NE "" 
              THEN PUT chrX AT 29 FORMAT "x(28)".
          END.
        END. /* IF i EQ 4 */
        ELSE
        IF i EQ 5 /* AND numfit GE 5 */ THEN DO:
          ASSIGN
            v-board = ""
            adder-print = NO.

          IF NOT logSetPrinting AND s-print-2nd-dscr 
            THEN
             IF v-boardDescription EQ 'Est' 
              THEN ASSIGN v-board = IF AVAIL ef THEN ef.brd-dscr ELSE "".
              ELSE ASSIGN v-board = xqitm.i-dscr.
            ELSE
            IF NOT logSetPrinting AND AVAIL ef THEN DO:
              ASSIGN 
               v-board = ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3]
                       + " " +
                         ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
               adder-print = YES.
            END. /* IF NOT logSetPrinting */          

            RELEASE bf-eb.

            IF AVAIL(est) AND 
               est.est-type = 6 
              THEN
               FIND FIRST bf-eb NO-LOCK 
                 WHERE bf-eb.company EQ xquo.company
                   AND bf-eb.est-no  EQ xquo.est-no
                   AND bf-eb.form-no EQ 0 NO-ERROR.
               
               ASSIGN lv-fg# = IF AVAIL(est) AND 
                                   est.est-type EQ 6 AND 
                                  AVAIL bf-eb 
                                 THEN bf-eb.stock-no
                                 ELSE 
                                  IF AVAIL eb 
                                    THEN eb.stock-no
                                    ELSE xqitm.part-no.

            IF logSetPrinting 
              THEN 
                PUT FILL(" ",21) AT 8.
/*                PUT "FG#: " + lv-fg# AT 8 FORM "x(21)". */
              ELSE DO:
               /*don't print board description twice*/
               IF v-board EQ chrX THEN DO:
                 ASSIGN  v-board = "".
                 IF AVAIL ef 
                   THEN
                    ASSIGN
                     v-board = ef.adder[1] + " " + ef.adder[2] + " " + 
                               ef.adder[3] + " " + ef.adder[4] + " " + 
                               ef.adder[5] + " " + ef.adder[6]
                     adder-print = YES.
               END.

               IF TRIM(v-board) NE "" 
                 THEN
                  PUT 
                   FILL(" ",21) AT 8
/*                    "FG#: " + lv-fg# AT 8 FORM "x(21)" */
                   v-board FORM "x(28)".

              END.
        END. /* IF numfit EQ 5 */
        ELSE
        IF numfit EQ 6 THEN DO:
          ASSIGN v-board = "".
          
          IF NOT adder-print AND 
             NOT logSetPrinting AND 
             AVAIL ef 
            THEN
             ASSIGN
              v-board = ef.adder[1] + " " + ef.adder[2] + " " + 
                        ef.adder[3] + " " + ef.adder[4] + " " + 
                        ef.adder[5] + " " + ef.adder[6].

          IF v-board NE "" 
            THEN PUT SPACE(28) v-board FORM "x(28)".
            ELSE numfit = 5.
        END.

        IF i GT numfit THEN PUT SPACE(58) .

        IF AVAIL xqqty THEN DO:
          ASSIGN 
           xxx    = IF xqqty.uom EQ "L" 
                     THEN xqqty.price    
                     ELSE
                      IF xqqty.uom EQ "C" 
                       THEN ((xqqty.qty / 100) * xqqty.price)     
                       ELSE 
                        IF xqqty.uom EQ "M" 
                         THEN ((xqqty.qty / 1000) * xqqty.price)    
                         ELSE (xqqty.qty * xqqty.price).

          /* gdm - 11040801 - added 1 char(int) to format to fit 10 million + qty */
          PUT
              xqqty.qty FORMAT ">>>>>>>9" TO 65
              xqqty.rels space(5)
              xqqty.price FORM "->>>,>>9.999" space(6)
              xqqty.uom.   

          v-line-total = v-line-total + xqqty.price.
        END. /* IF AVAIL xqqty */

        /*  IF i NE 3 THEN DOWN. */
        PUT  SKIP.

        FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.

        RUN printHeader (2, OUTPUT v-printline).
          
    END.  /* do numfit */

    IF LINE-COUNTER GT PAGE-SIZE - 22 THEN DO:

        PAGE.  
        {cec/quote/quosoule2.i}

    END.

    IF s-print-comp THEN DO :      /* Print components of a set */
      
      FIND FIRST est NO-LOCK
        WHERE est.company EQ xquo.company
          AND est.est-no  EQ xquo.est-no  NO-ERROR.
      IF AVAIL est 
        THEN
         FIND FIRST ef NO-LOCK
           WHERE ef.company EQ xquo.company
             AND ef.est-no  EQ xquo.est-no NO-ERROR. 

      IF AVAIL(est) AND 
         AVAIL(ef)  AND 
          est.est-type EQ 6 AND /* this field identifies if a set is to be print */
          (est.form-qty GT 1 OR NOT can-find(eb OF ef)) 
        THEN
         FOR EACH ef NO-LOCK
           WHERE ef.company EQ est.company
             AND ef.est-no  EQ est.est-no,
           EACH eb OF ef NO-LOCK BREAK BY ef.form-no :

           RUN printHeader (1,OUTPUT idummy).

           IF FIRST(ef.form-no) THEN DO:

             IF LINE-COUNTER GT PAGE-SIZE - 22 THEN DO:
               PAGE.  
               {cec/quote/quosoule2.i}
             END.

             PUT "Components" AT 5 SKIP.
           END.
           ELSE 
            IF LINE-COUNTER GT PAGE-SIZE - 22 THEN DO:
               PAGE.  
               {cec/quote/quosoule2.i}
            END.

           FIND FIRST style NO-LOCK 
             WHERE style.company = eb.company
               AND style.style = eb.style NO-ERROR.

           ASSIGN
            ld-len = eb.len * ld-metric
            ld-wid = eb.wid * ld-metric
            ld-dep = eb.dep * ld-metric.

           IF est.metric THEN DO:
             {sys/inc/roundup.i ld-len}
             {sys/inc/roundup.i ld-wid}
             {sys/inc/roundup.i ld-dep}
           END.

           IF AVAIL style AND 
               INT(style.industry) > 1 AND 
               NOT est.metric 
             THEN
              ASSIGN 
               trim-size = TRIM(STRING({sys/inc/k16v.i ld-len})) + "x" +
                           TRIM(STRING({sys/inc/k16v.i ld-wid})) + "x" +
                           TRIM(STRING({sys/inc/k16v.i ld-dep})).
             ELSE
              ASSIGN
               trim-size = TRIM(STRING(ld-len,lv-format)) + "x" +
                           TRIM(STRING(ld-wid,lv-format)) + "x" +
                           TRIM(STRING(ld-dep,lv-format)).

           IF LENGTH(TRIM(trim-size)) GT 24 
             THEN
               DO cc = 1 TO LENGTH(trim-size):
                 IF SUBSTR(trim-size,cc,1) NE "" 
                   THEN ASSIGN temp-trim-size = temp-trim-size + 
                                                SUBST(trim-size,cc,1).
           END.
           ELSE ASSIGN temp-trim-size = trim-size.

           PUT 
               eb.part-no AT 8 FORMAT "x(21)" temp-trim-size   SKIP.

           FIND FIRST style NO-LOCK
             WHERE style.company EQ cocode
               AND style.style   EQ eb.style NO-ERROR.
           ASSIGN style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style.

           PUT 
               eb.part-dscr1 AT 8 FORM "x(21)"
               style-dscr SKIP.

           ASSIGN
            v-board = /*IF AVAIL ef THEN*/
                     ef.board    + " - " + ef.adder[1] + " " + 
                     ef.adder[2] + " " + ef.adder[3] + " " +
                     ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6].

           IF SUBSTR(v-board,LENGTH(TRIM(v-board)),1) EQ "-" 
             THEN ASSIGN SUBSTR(v-board,LENGTH(TRIM(v-board)),1) = "".

           IF v-board EQ "" THEN DO:
               FIND FIRST ITEM NO-LOCK 
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ ef.board NO-ERROR.
               IF AVAIL item 
                 THEN
                  ASSIGN 
                   v-board = IF item.i-name GT "" 
                               THEN item.i-name   
                               ELSE 
                                IF item.est-dscr GT "" 
                                  THEN item.est-dscr ELSE item.i-dscr.
           END. /* IF v-board EQ "" */

           PUT 
              eb.part-dscr2  AT 8  FORM "x(21)"
              v-board  FORM "x(50)"   SKIP .

           PUT eb.i-coldscr   AT 30 SKIP.
      END.

    END.    /* disp components */

    ASSIGN numfit = 0.

    FOR EACH xqqty OF xqitm NO-LOCK,
        EACH xqchg NO-LOCK
        WHERE xqchg.company EQ xqqty.company
          AND xqchg.loc     EQ xqqty.loc
          AND xqchg.q-no    EQ xqqty.q-no
          AND xqchg.line    EQ xqqty.line
          AND xqchg.qty     EQ xqqty.qty
        BREAK BY xqchg.qty 
              BY xqchg.charge:

        IF FIRST-OF(xqchg.qty) THEN DO:
            IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
        END.
        
        ASSIGN numfit = numfit + 1.
    END.

    RUN printHeader (1,OUTPUT idummy).

    FOR EACH xqqty OF xqitm NO-LOCK,
        EACH xqchg  NO-LOCK
        WHERE xqchg.company = xqqty.company
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
        BREAK BY xqchg.qty
              BY xqchg.charge:

        IF FIRST-OF(xqchg.qty) THEN DO:
            
            IF FIRST(xqchg.qty) THEN PUT SKIP(1).

            PUT "For QTY: " AT 5
                 TRIM(STRING(xqchg.qty,">>>>>>>")).
        END. 

        ASSIGN lv-chg-amt = xqchg.amt.   

        IF xqchg.bill EQ "N" 
          THEN PUT xqchg.charge AT 22  SKIP.
          ELSE
           IF INDEX("TML",xqchg.bill) GT 0 
             THEN
              PUT 
                 xqchg.charge       AT 22
                 lv-chg-amt   TO 80 SKIP.
             ELSE
              IF xqchg.bill EQ "W" 
                THEN PUT xqchg.charge    AT 22 SKIP.

        RUN printHeader (2,OUTPUT v-printline).
    END. /* FOR EACH xqqty */

    RUN printHeader (1,OUTPUT v-printline).

    PUT SKIP(1).

END. /* FOR EACH xqitm */

ASSIGN numfit = 0.

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

  ASSIGN numfit = numfit + 1.

END.

RUN printHeader (2,OUTPUT idummy).


ASSIGN logPrint = FALSE. /* rdb 02/02/07 01310703 */

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
    
    /*
    IF FIRST-OF(xqchg.charge) THEN DO: 
    IF NOT logPrint THEN DO:
      PUT SKIP(1).
      logprint = TRUE.
    END.
    */   
    /*IF xqchg.qty EQ 0 OR FIRST-OF(xqchg.b-num) THEN DO:*/

    ASSIGN lv-chg-amt = xqchg.amt .

    IF xqchg.bill EQ "N" 
      THEN
       PUT xqchg.charge AT 22
          /* "N/C"        TO 80 */ SKIP.
      
      ELSE
       IF INDEX("TML",xqchg.bill) GT 0 
        THEN
         PUT 
            xqchg.charge       AT 22
            lv-chg-amt         TO 83  
            "EA"               AT 90 SKIP.
        ELSE
         IF xqchg.bill EQ "W" 
          THEN
           PUT xqchg.charge    AT 22 SKIP.

    RUN printHeader (2,OUTPUT v-printline).

    IF LAST-OF(xqchg.qty) THEN LEAVE.
END.

/*if (ch-multi and v-last) or (not ch-multi) then */ 
DO:
    IF ch-inst THEN DO:
        
        RUN printHeader (2,OUTPUT idummy).

        FIND FIRST est NO-LOCK 
            WHERE est.company = xquo.company
              AND est.est-no = xquo.est-no NO-ERROR.

        {custom/notespr2.i job v-inst2 EXTENT(v-inst2) 
            "notes.rec_key = est.rec_key and 
            notes.note_code >= fdept and notes.note_code <= tdept" }

        ASSIGN idx = 0.

        DO i = 1 TO EXTENT(v-dept-inst) /*6*/:
          ASSIGN v-dept-inst[i] = v-inst2[i].
          IF v-dept-inst[i] NE '' THEN idx = idx + 1.
        END.

        IF idx NE 0 THEN DO:
         IF v-notesPageSpan 
           THEN RUN printHeader (1,OUTPUT idummy).
           ELSE RUN printHeader (idx,OUTPUT idummy).

         PUT "Department Notes: " SKIP.

         DO idx = 1 TO EXTENT(v-dept-inst):
           IF v-dept-inst[idx] NE "" THEN PUT v-dept-inst[idx] AT 3.
           IF idx NE EXTENT(v-dept-inst) THEN PUT SKIP.
           IF v-notesPageSpan THEN
           RUN printHeader (1,OUTPUT idummy).
         END. /* do idx */
        END. /* if idx */
    END. 
    

END.

PUT SKIP(1).

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
