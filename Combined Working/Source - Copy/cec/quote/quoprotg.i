/* ------------------------------------------- cec/quote/quoprotg.i */
/* print quote items in Fibre Xprint format                                          */
/* -------------------------------------------------------------------------- */

v-boardDescription = 'Quote'.

EMPTY TEMP-TABLE tt-item.

FOR EACH xqitm OF xquo NO-LOCK,
    FIRST eb fields(form-no blank-no) WHERE
          eb.company EQ est.company AND
          eb.est-no  EQ est.est-no AND
          eb.part-no EQ xqitm.part-no
          NO-LOCK:

    CREATE tt-item.
    ASSIGN
       tt-item.part-no = xqitm.part-no
       tt-item.form-no = eb.form-no
       tt-item.blank-no = eb.blank-no
       tt-item.ROWID = ROWID(xqitm).
    RELEASE tt-item.
END.


FOR EACH tt-item,
    FIRST xqitm WHERE
          ROWID(xqitm) EQ tt-item.ROWID
    NO-LOCK
    BY tt-item.form-no
    BY tt-item.blank-no:

  numfit = 0.

  FOR EACH xqqty OF xqitm NO-LOCK:
      numfit = numfit + 1.
  END.
  IF numfit < 5 THEN numfit = 5.

  RUN printHeader (6,OUTPUT v-printline).
  /*IF AVAIL est AND est.est-type GE 3 AND est.est-type LE 4 THEN numfit = 4.*/
         
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
       ASSIGN ld-metric = 25.4
              lv-format = "->>,>>>mm".
    ELSE ASSIGN ld-metric = 1
                lv-format = ">>>>>9.9<<<<".

    IF AVAIL ef AND ef.adder[2] <> "" AND
/*        ((AVAIL est AND est.est-type GT 4 and est.est-type NE 6) OR NOT AVAIL est) THEN numfit = numfit + 1. rtc */
           ((AVAIL est AND est.est-type GT 4) OR NOT AVAIL est) THEN numfit = numfit + 1.
    IF AVAIL ef AND ef.adder[3] <> "" AND
/*        ((AVAIL est AND est.est-type GT 4 and est.est-type NE 6) OR NOT AVAIL est) THEN numfit = numfit + 1. rtc */
       ((AVAIL est AND est.est-type GT 4) OR NOT AVAIL est) THEN numfit = numfit + 1.
    IF AVAIL ef AND ef.adder[4] <> "" AND
/*        ((AVAIL est AND est.est-type GT 4 and est.est-type NE 6) OR NOT AVAIL est) THEN numfit = numfit + 1. rtc */
       ((AVAIL est AND est.est-type GT 4) OR NOT AVAIL est) THEN numfit = numfit + 1.
    IF AVAIL ef AND ef.adder[5] <> "" AND
/*        ((AVAIL est AND est.est-type GT 4 and est.est-type NE 6) OR NOT AVAIL est) THEN numfit = numfit + 1. rtc */
       ((AVAIL est AND est.est-type GT 4) OR NOT AVAIL est) THEN numfit = numfit + 1.
  END.
  
  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  FIND FIRST itemfg WHERE itemfg.company = xqitm.company
                      AND itemfg.i-no = xqitm.part-no NO-LOCK NO-ERROR.
  IF s-print-2nd-dscr AND xqitm.part-dscr2 EQ "" 
      THEN s-print-2nd-dscr = NO.

  /*lv-two-box = NO.
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
  END.*/

  DO i = 1 TO numfit /*WITH FRAME item-10p */:

    IF i EQ 1 THEN DO:
      /*      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.  */

      ASSIGN
         lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE ""
         lv-part-dscr1 = IF AVAIL est AND est.est-type EQ 6 AND AVAIL itemfg THEN itemfg.i-name
                         ELSE xqitm.part-dscr1.
      /*lv-part-dscr1 = IF s-print-2nd-dscr AND AVAIL itemfg THEN ITEMfg.i-NAME ELSE lv-part-dscr1.*/
      put trim(lv-est-no) FORM "x(6)" SPACE(1) 
          xqitm.part-no space(1) lv-part-dscr1 .
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
           FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
           style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.

           IF AVAIL eb THEN ASSIGN ld-len = eb.len * ld-metric
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
           run sys/inc/dec-frac.p (ld-len,64, output v-len-str).
           run sys/inc/dec-frac.p (ld-wid,64, output v-wid-str).
           run sys/inc/dec-frac.p (ld-dep,64, output v-dep-str).
           /*
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
           */
           trim-size = v-len-str + "x" + v-wid-str + "x" + v-dep-str.
      END.
      lv-part-dscr2 = IF s-print-2nd-dscr THEN xqitm.part-dscr2 ELSE style-dscr.
      PUT  xquo.q-no FORM ">>>>>9" trim-size AT 8 FORM "x(21)"
              /*xqitm.style*/ lv-part-dscr2 /* style-dscr*/  FORM "x(28)" .
    END.
    ELSE                /*not a set estimate*/
/*     IF i EQ 3 AND NOT (AVAIL est AND (est.est-type = 2 OR est.est-type = 6)) THEN DO: rtc */
    IF i EQ 3 AND NOT (AVAIL est AND (est.est-type = 2 OR est.est-type = 6)) THEN DO:
      lv-i-coldscr = IF s-print-2nd-dscr THEN style-dscr ELSE xqitm.i-coldscr.
      PUT     "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)"
              lv-i-coldscr  AT 29 FORM "x(28)".
    END.
    ELSE
/*     IF i EQ 4 AND NOT (AVAIL est AND (est.est-type = 2 OR est.est-type = 6)) THEN DO: rtc */
    IF i EQ 4 AND NOT (AVAIL est AND (est.est-type = 2 OR est.est-type = 6))  THEN DO:
       PUT "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)".
      
       IF AVAIL est AND est.est-type NE 6 THEN
       DO:
          IF s-print-2nd-dscr THEN PUT xqitm.i-coldscr AT 29 FORM "x(28)".
          ELSE PUT xqitm.i-dscr AT 29 FORMAT "x(28)".
       END.
    END.
    ELSE
/*     IF i EQ 5 AND NOT (AVAIL est AND (est.est-type = 2 OR est.est-type = 6)) THEN DO: rtc */
    IF i EQ 5 AND NOT (AVAIL est AND est.est-type = 2)  THEN DO:
       v-board = "".
       IF s-print-2nd-dscr AND v-boardDescription EQ 'Est' THEN
          v-board = IF AVAIL ef THEN ef.brd-dscr  ELSE "".
       ELSE IF s-print-2nd-dscr AND v-boardDescription EQ 'Quote' THEN
          v-board = xqitm.i-dscr.
       ELSE do:
/*            IF AVAIL ef AND ((AVAIL est AND est.est-type GT 4 and est.est-type NE 6) OR NOT AVAIL est) AND ef.adder[1] <> "" THEN DO: rtc */
           IF AVAIL ef AND ((AVAIL est AND est.est-type GT 4 AND est.est-type NE 6) OR NOT AVAIL est) AND ef.adder[1] <> "" THEN DO:
              FIND FIRST ITEM WHERE ITEM.company = xquo.company
                                AND ITEM.i-no = ef.adder[1] NO-LOCK NO-ERROR.
              v-board = IF AVAIL ITEM AND item.i-dscr <> "" THEN ITEM.i-dscr
                        ELSE ef.adder[1].
           END.
/*            ELSE                         rtc           */
/*            IF AVAIL est AND est.est-type EQ 6 THEN */
/*               v-board = xqitm.i-dscr.              */
/*            ELSE                                    */
/*               v-board = "".                        */
       END.

       IF AVAIL est AND est.est-type = 6 THEN
          FIND FIRST bf-eb WHERE bf-eb.company EQ xquo.company
                             AND bf-eb.est-no  EQ xquo.est-no
                             AND bf-eb.form-no EQ 0
                             NO-LOCK NO-ERROR.

       lv-fg# = IF AVAIL est AND est.est-type EQ 6 AND AVAIL bf-eb THEN bf-eb.stock-no
                ELSE IF AVAIL eb THEN eb.stock-no ELSE xqitm.part-no.

         IF est.est-type NE 6 THEN
            put "FG#: " + lv-fg# AT 8 FORM "x(21)"
                  v-board FORM "x(28)".    
    END.
    IF i > 5 THEN do:
       v-board = "".
       IF i > 16 THEN 
          ASSIGN ll = 16. 
       ELSE
          ASSIGN ll = i.
/*        IF AVAIL ef AND ((AVAIL est AND est.est-type GT 4 and est.est-type NE 6) OR NOT AVAIL est) AND ef.adder[i - 4] <> "" THEN DO: rtc */
       IF AVAIL ef AND ((AVAIL est AND est.est-type GT 4) OR NOT AVAIL est) AND ef.adder[ll - 4] <> "" THEN DO:
          FIND FIRST ITEM WHERE ITEM.company = xquo.company
                                AND ITEM.i-no = ef.adder[ll - 4] NO-LOCK NO-ERROR.
          v-board = IF AVAIL ITEM AND item.i-dscr <> "" THEN ITEM.i-dscr
                    ELSE ef.adder[ll - 4].
          PUT SPACE(28) v-board FORM "x(28)" .
       END.
       ELSE PUT SPACE(58) .     
    END.
    IF AVAIL xqqty THEN DO:
       ASSIGN 
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price)
       v-pricea = IF xqqty.uom EQ "EA" THEN string(xqqty.price,">>>>9.9999") 
                                       ELSE string(xqqty.price,"->>,>>9.99").
      
        PUT xqqty.qty           TO 63   FORMAT ">>>>>>9"
            xqqty.rels          TO 67   FORMAT ">>9"   
            v-pricea            TO 77   /*FORMAT "->>,>>9.99"*/.

        IF xqqty.uom = "EA" OR xqqty.uom = "L" OR xqqty.uom = "M" THEN DO:        
            IF xqqty.uom NE "L" THEN
               PUT xqqty.uom TO 82.
            ELSE
               PUT "LOT" TO 82.
        END.

        PUT xxx TO 94 FORMAT ">>>,>>9.99".
              
        v-line-total = v-line-total + xqqty.price.
    END.

/*  IF i NE 3 THEN DOWN. */
    PUT  SKIP.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
    RUN printHeader (2,OUTPUT v-printline).
  END. /* do numfit */

  IF s-print-comp THEN DO :      /* Print components of a set */
    FIND FIRST est
        WHERE est.company EQ xquo.company
          AND est.est-no  EQ xquo.est-no
        NO-LOCK NO-ERROR.
     IF AVAIL est then
        FIND FIRST ef WHERE ef.company EQ xquo.company
                        AND ef.est-no  EQ xquo.est-no NO-LOCK NO-ERROR. 

    IF AVAIL est AND AVAIL ef AND (est.est-type EQ 6 OR est.est-type EQ 2) AND 
       (est.form-qty GT 1 OR NOT can-find(eb OF ef)) THEN

    FOR EACH ef WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est.est-no NO-LOCK,      
        EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
        
        RUN printHeader (1,OUTPUT idummy).
        IF FIRST(ef.form-no) THEN DO:
           IF LINE-COUNTER GT PAGE-SIZE - 2 THEN
           DO:
              v-printline = 0.
              PAGE.
              {cec/quote/quoprotg2.i}
           END.
           PUT "Components" AT 5 SKIP.
        END.
        ELSE IF LINE-COUNTER GT PAGE-SIZE - 2 THEN
        DO:
           v-printline = 0.
           PAGE.
           {cec/quote/quoprotg2.i}
        END.
        
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
        
        IF est.est-type EQ 6 THEN
           put eb.part-no AT 8 FORM "x(21)"
               "  Qty Per Set "
               TRIM(STRING(eb.yld-qty,"->>>,>>>"))
               SKIP.
        ELSE
           put eb.part-no AT 8 FORM "x(21)"
               "  Qty Per Set "
               TRIM(STRING(eb.cust-%,"->>>,>>>"))
               SKIP.
        
        IF LINE-COUNTER GT PAGE-SIZE - 2 THEN
        DO:
           v-printline = 0.
           PAGE.
           {cec/quote/quoprotg2.i}
        END.

        PUT temp-trim-size AT 8 SKIP.
        
        FIND FIRST style
            WHERE style.company EQ cocode
              AND style.style   EQ eb.style
            NO-LOCK NO-ERROR.
        
        ASSIGN
           style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style
           v-board = ef.board.
        
        IF LINE-COUNTER GT PAGE-SIZE - 2 THEN
        DO:
           v-printline = 0.
           PAGE.
           {cec/quote/quoprotg2.i}
        END.

        PUT eb.part-dscr1 AT 8 FORM "x(21)"
            style-dscr SKIP.
        
        IF ((AVAIL est AND est.est-type GT 4) OR NOT AVAIL est) THEN
           v-board = v-board + " - " +
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

        IF LINE-COUNTER GT PAGE-SIZE - 2 THEN
        DO:
           v-printline = 0.
           PAGE.
           {cec/quote/quoprotg2.i}
        END.

        PUT eb.part-dscr2  AT 8  FORM "x(21)"
            v-board  FORM "x(50)"   SKIP .
        put eb.i-coldscr   AT 30 SKIP.
    END. /*end for each*/
  END.    /* disp components */

  numfit = 0.
  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg NO-LOCK
      WHERE xqchg.company EQ xqqty.company
        AND xqchg.loc     EQ xqqty.loc
        AND xqchg.q-no    EQ xqqty.q-no
        AND xqchg.line    EQ xqqty.line
        AND xqchg.qty     EQ xqqty.qty
        AND NOT CAN-FIND(FIRST est-prep
                         WHERE est-prep.company EQ xquo.company
                           AND est-prep.est-no  EQ xquo.est-no
                           AND est-prep.s-num   EQ xqchg.s-num
                           AND est-prep.b-num   EQ xqchg.b-num
                           AND est-prep.dscr    EQ xqchg.charge)
      BREAK BY xqchg.qty
            BY xqchg.charge:

    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
    END.
    numfit = numfit + 1.
  END.

  RUN printHeader (1,OUTPUT idummy).

  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg
      WHERE xqchg.company EQ xqqty.company
        AND xqchg.loc     EQ xqqty.loc
        AND xqchg.q-no    EQ xqqty.q-no
        AND xqchg.line    EQ xqqty.line
        AND xqchg.qty     EQ xqqty.qty
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
           lv-chg-amt /*xqchg.amt*/  TO 83 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
       PUT xqchg.charge    AT 22
           /*  "Will Advise "  TO 45 */  skip.

    RUN printHeader (2,OUTPUT v-printline).
  END.

  RUN printHeader (1,OUTPUT v-printline).
  
END.  /*each xqitm*/

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
    BREAK BY xqchg.qty
          BY xqchg.charge
          BY xqchg.s-num
          BY xqchg.b-num:

  IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.

  IF LAST-OF(xqchg.qty) THEN LEAVE.
END.

RUN printHeader (2,OUTPUT idummy).

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
          lv-chg-amt          TO 80 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
      PUT xqchg.charge    AT 22
        /*"Will Advise "  TO 45 */ SKIP.
  /*END.*/

  RUN printHeader (2,OUTPUT v-printline).

  IF LAST-OF(xqchg.qty) THEN LEAVE.
END. /* each xqchg*/

if ch-inst then do:
   RUN printHeader (2,OUTPUT idummy).
   FIND FIRST est WHERE est.company = xquo.company
                    AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
   {custom/notespr2.i job v-inst2 EXTENT(v-inst2) "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept" }
   idx = 0.
   DO i = 1 TO EXTENT(v-dept-inst):
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
  
/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
