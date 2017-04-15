/* ------------------------------------------- cec/quote/quosult.i 04/09 GDM */
/* print quote items in Sultana format                                         */
/* -------------------------------------------------------------------------- */
ASSIGN v-boardDescription = "Est"
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
        iNumAdders   = 0
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
                IF AVAIL ef THEN DO: 
                    DO i = 1 TO 4:
                        IF ef.adder[i + 6] NE "" THEN iNumAdders = iNumAdders + 1.
                    END.
                END.
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

/*     IF numfit LT 5 AND s-print-2nd-dscr */
/*       THEN numfit = 6.                  */
/*       ELSE                              */
    IF numfit LT 5 + iNumAdders THEN numfit = 5 + iNumAdders.
    
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
/*                 TRIM(lv-est-no) FORM "x(5)" AT 2 */
                xqitm.part-no AT 1 FORMAT "x(21)"
                TRIM(lv-part-dscr1) AT 22 FORMAT "x(28)".
        END. /* IF i EQ 1 */
        ELSE IF i EQ 2 THEN DO:            
            trim-size = "".
              IF AVAIL eb THEN
                /* trim-size = STRING({sys/inc/k16v.i eb.len}) + "X" + STRING({sys/inc/k16v.i eb.wid})
                          + "X" + STRING({sys/inc/k16v.i eb.dep})*/
                   trim-size = trim(STRING(eb.len)) + " x " + trim(STRING(eb.wid))
                          + " x " + trim(STRING(eb.dep)).
              ELSE trim-size = xqitm.SIZE /*""*/ .
              PUT     trim-size AT 22 FORM "x(30)"
                      /*xqitm.style*/ /* style-dscr */  .
        END. /* IF i EQ 3 */
        ELSE IF i EQ 3 THEN DO:            
          FIND FIRST style NO-LOCK  WHERE style.company = xqitm.company
                                      AND style.style = xqitm.style NO-ERROR.
               
          ASSIGN style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.

          ASSIGN lv-part-dscr2 = IF xqitm.part-dscr2 NE "" THEN xqitm.part-dscr2 
              ELSE IF AVAIL itemfg THEN itemfg.i-dscr ELSE style-dscr.

/*          IF TRIM(xqitm.i-coldscr) NE "" THEN       */
/*             ASSIGN lv-i-coldscr = xqitm.i-coldscr. */
/*          IF lv-i-coldscr NE "" THEN                */
             PUT  
/*                xquo.q-no */
               /*IF AVAIL eb 
                 THEN eb.cad-no 
                 ELSE ""        AT 1 FORMAT "x(21)"*/
               lv-part-dscr2    AT 22 FORMAT "x(28)".
/*                 lv-i-coldscr AT 22 FORM "x(28)". */
        END. /* IF i EQ 3 */
        ELSE IF i EQ 4 THEN DO:
            PUT    /* "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)"*/
            xqitm.i-coldscr  AT 22 FORM "x(40)" .
        END. /* IF i EQ 4 */
        ELSE IF i EQ 5 THEN DO:
            PUT /*"CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)"         */
                IF AVAIL ef THEN ef.brd-dscr  ELSE xqitm.i-dscr  AT 22 FORMAT "x(30)" .
        END. /* IF i EQ 5 */
        ELSE IF i EQ 6 THEN DO:
            PUT IF AVAIL ef AND iNumAdders GE 1 THEN ef.adder[7] ELSE "" AT 22 FORM "x(30)".
        END.
        ELSE IF i EQ 7 THEN DO:
            PUT IF AVAIL ef AND iNumAdders GE 2 THEN ef.adder[8] ELSE "" AT 22 FORM "x(30)".
        END.
        ELSE IF i EQ 8 THEN DO:
            PUT IF AVAIL ef AND iNumAdders GE 3 THEN ef.adder[9] ELSE "" AT 22 FORM "x(30)".
        END.
        ELSE IF i EQ 9 THEN DO:
            PUT IF AVAIL ef AND iNumAdders GE 4 THEN ef.adder[10] ELSE "" AT 22 FORM "x(30)".
        END.


/*         IF i GT numfit THEN DO: */
/*             PUT SPACE(58) .     */
/*                                 */
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
              "<C48>" xqqty.qty FORMAT ">>>>>>>9" 
              "<C57>" xqqty.rels /*space(5)*/
              "<C62>" xqqty.price FORM "->>>,>>9.999"  /*space(6)*/
              "<C75>" xqqty.uom .   

          v-line-total = v-line-total + xqqty.price.
        END. /* IF AVAIL xqqty */

        /*  IF i NE 3 THEN DOWN. */
        PUT  SKIP.

        FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.

        RUN printHeader (2, OUTPUT v-printline).
          
    END.  /* do numfit */

    IF LINE-COUNTER GT PAGE-SIZE - 36 THEN DO:

        PAGE.  
        {cec/quote/quosult2.i}

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

             IF LINE-COUNTER GT PAGE-SIZE - 32 THEN DO:
               PAGE.  
               {cec/quote/quosult2.i}
             END.

             PUT "Components" AT 5 SKIP.
           END.
           ELSE 
            IF LINE-COUNTER GT PAGE-SIZE - 32 THEN DO:
               PAGE.  
               {cec/quote/quosult2.i}
            END.

           PUT 
               eb.part-no AT 8 FORMAT "x(21)" eb.part-dscr1 AT 30 FORMAT "x(30)" SKIP.


           IF eb.part-dscr2 NE "" AND ll-prt-dscr2 THEN
           PUT 
               eb.part-dscr2 AT 30 FORM "x(30)" SKIP.
               
           FIND FIRST ITEM NO-LOCK 
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ ef.board NO-ERROR.
               IF AVAIL item 
                 THEN
                  ASSIGN 
                   v-board = /*IF item.i-name GT "" 
                               THEN item.i-name   
                               ELSE */
                                IF item.est-dscr GT "" 
                                  THEN item.est-dscr ELSE item.i-dscr.
/*            END. /* IF v-board EQ "" */ */

           PUT 
/*               eb.part-dscr2  AT 8  FORM "x(21)" */
              v-board  AT 30 FORM "x(30)"   SKIP .

           PUT eb.i-coldscr   AT 30 SKIP.
        IF LINE-COUNTER > PAGE-SIZE - 32 THEN DO:      
            v-printline = 0.
            PAGE.  
            {cec/quote/quosult2.i}
        END.
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
                 "<C62>" lv-chg-amt  FORM "->>>,>>9.999"  SKIP.
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
            "<C62>" lv-chg-amt  FORM "->>>,>>9.999" 
            "<C75>EA"               SKIP.
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

