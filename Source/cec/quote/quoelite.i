/* ------------------------------------------- cec/quote/quoelite.i 05/05 YSK */
/* print quote items in Elite format                                          */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.              */
/* -------------------------------------------------------------------------- */


FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:

  numfit = 0.

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.
  IF numfit LT 4 THEN numfit = 4.

  IF LINE-COUNTER > PAGE-SIZE - 6 THEN DO:
       page.  
       {cec/quote/quoelit2.i}
       v-printline = 0.
    END.
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
    ASSIGN
     ld-metric = 25.4
     lv-format = "->>,>>>mm".
  ELSE
    ASSIGN
     ld-metric = 1
     lv-format = ">>>>>9.9<<<<".
          
    /* IF numfit LE 4                            AND
       (eb.cad-no NE "" OR eb.plate-no ne "") THEN numfit = 5. 
     numfit = 5. */
  END.
  IF numfit < 5 THEN numfit = 5.

  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  FIND FIRST itemfg WHERE itemfg.company = xqitm.company
                      AND itemfg.i-no = xqitm.part-no NO-LOCK NO-ERROR.
 
  DO i = 1 TO numfit /*WITH FRAME item-10p */:
    IF i EQ 1 THEN DO:
      /*      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.  */
      lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".
      lv-part-dscr1 = IF AVAIL est AND est.est-type EQ 6 AND AVAIL itemfg THEN itemfg.i-name
                      ELSE xqitm.part-dscr1.
/*       put trim(lv-est-no) FORM "x(8)" SPACE(1)  */
/*           xqitm.part-no space(1) lv-part-dscr1. */

      IF LENGTH(xqitm.part-no) LE 20 THEN
        PUT  "<C1>" TRIM(lv-est-no) FORM "x(8)"
            "<C8.5>" xqitm.part-no  FORMAT "x(20)"           
            "<C26>" TRIM(lv-part-dscr1) FORMAT "x(30)".
      ELSE do: 
         PUT "<C1>" TRIM(lv-est-no) FORM "x(8)"
          "<C8.5>" xqitm.part-no  FORMAT "x(30)".
         IF lv-part-dscr1 NE "" THEN
          PUT SKIP "<C26>" TRIM(lv-part-dscr1) FORMAT "x(30)". 
      END.

         
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
             trim-size = TRIM(STRING(ld-len,lv-format)) + " x " +
                         TRIM(STRING(ld-wid,lv-format)) + " x " +
                         TRIM(STRING(ld-dep,lv-format)).

           ELSE trim-size = "".
      END.
      PUT  xquo.q-no  trim-size AT 10 FORM "x(21)"
              /*xqitm.style*/  style-dscr   .
    END.
    ELSE
    IF i EQ 3 THEN
      PUT     "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 10 FORM "x(21)"
              xqitm.i-coldscr  AT 31 FORM "x(30)".
    ELSE
    IF i EQ 4 THEN DO:
      
       PUT "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 10  FORM "x(21)"         
           IF AVAIL ef THEN ef.brd-dscr /*xqitm.i-dscr*/ ELSE ""  AT 31 FORMAT "x(30)".
    END.

    ELSE
    IF i EQ 5 /* AND numfit GE 5 */ THEN DO:
       v-board = IF AVAIL ef THEN
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                ELSE "".

       lv-fg# = /*IF est.est-type EQ 6 THEN xqitm.part-no 
                ELSE IF AVAIL eb THEN*/ IF AVAIL eb THEN eb.stock-no ELSE xqitm.part-no
                /*ELSE ""*/ .
       put "FG#: " + lv-fg# AT 10 FORM "x(21)"
           /*"PLATE#: " + (IF AVAIL eb THEN eb.plate-no  ELSE "")*/
            v-board FORM "x(30)" /*SKIP(1)*/.                                           


    END.
    IF i > 5 THEN PUT SPACE(58) .

    IF AVAIL xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).

    /*  IF i EQ 3 THEN
        PUT xqqty.qty           TO 70   FORMAT ">>>>>>9"
            xqqty.rels          TO 85   FORMAT ">>9"   
            xqqty.price         TO 95   FORMAT ">>,>>9.99" 
            xqqty.uom           TO 100  .
 
      ELSE*/ 

       put xqqty.qty SPACE(1) xqqty.rels space(5)
               xqqty.price FORM "->>,>>9.99" space(5)
               xqqty.uom  .   
              
       v-line-total = v-line-total + xqqty.price.
    END.

/*  IF i NE 3 THEN DOWN. */
    PUT  SKIP.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
    IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quoelit2.i}
       v-printline = 0.
    END.
    
  END. /* do numfit */


  IF s-print-comp THEN DO :      /* Print components of a set */
  
    FIND FIRST est
        WHERE est.company EQ xquo.company
          AND est.est-no  EQ xquo.est-no
        NO-LOCK NO-ERROR.
    IF AVAIL est AND est.est-type EQ 6 AND est.form-qty GT 1 THEN
    FOR EACH ef WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est.est-no NO-LOCK,      
        EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
        
      IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
          page.  
          {cec/quote/quoelit2.i}
      END.
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
    
      put eb.part-no AT 10 FORM "x(21)" temp-trim-size   SKIP.
    
      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style.      
      PUT eb.part-dscr1 AT 10 FORM "x(21)"
          style-dscr SKIP.
    
      v-board = IF AVAIL ef THEN
                  ef.board    + " - " +
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                ELSE "-".
              
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
    
      PUT eb.part-dscr2  AT 10  FORM "x(21)"
          v-board     SKIP .
    
      put eb.i-coldscr   AT 30 SKIP.
    END.
  END.    /* disp components */

  numfit = 0.
  
  FOR EACH xqqty OF xqitm NO-LOCK,
      /* EACH xqchg OF xqqty NO-LOCK */
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

  IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
     page.  
     {cec/quote/quoelit2.i}
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
           /*  "Will Advise "  TO 45 */  skip.

    IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quoelit2.i}
       v-printline = 0.
    END.
  END.

  IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
     page.  
     {cec/quote/quoelit2.i}
     v-printline = 0.
  END.
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

IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
   page.  
   {cec/quote/quoelit2.i}
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
    BREAK BY xqchg.charge
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

  IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
    PAGE.  
    {cec/quote/quoelit2.i}
    v-printline = 0.
  END.   
END.

/*if (ch-multi and v-last) or (not ch-multi) then */ do:

  if ch-inst then do:
      /* old
    for each est-inst
        where est-inst.company = xquo.company
          AND est-inst.est-no eq xquo.est-no
          and est-inst.dept  ge fdept
          and est-inst.dept  le tdept
        no-lock break by est-inst.est-no by est-inst.dept:

      IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
          page.  
          {cec/quote/quoxpnt2.i}
      END.
      if first-of(est-inst.est-no) then do:
        put skip(1).
        put "Dept" at 6 "Department Manufacturing Instructions" at 11 skip.
        put "----" at 6 "-------------------------------------" at 11 skip.
      end.
      put est-inst.dept at 7 est-inst.inst[1] at 11.
      put est-inst.inst[2] at 11.
      put est-inst.inst[3] at 11 skip.
    end.
    =====*/
      IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
          page.  
          {cec/quote/quoelit2.i}
      END.
      FIND FIRST est WHERE est.company = xquo.company
                       AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
      {custom/notespr2.i job v-inst2 6 "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept" }
      DO i = 1 TO 6:
             v-dept-inst[i] = v-inst2[i].
      END.
      IF v-dept-inst[1] <> "" OR v-dept-inst[2] <> "" THEN DO:
         IF LINE-COUNTER > PAGE-SIZE - 6 THEN DO:
            page.  
            {cec/quote/quoelit2.i}
         END.
         PUT "Department Notes: " SKIP.
         IF v-dept-inst[1] <> "" THEN PUT v-dept-inst[1] AT 3 SKIP.
         IF v-dept-inst[2] <> "" THEN PUT v-dept-inst[2] AT 3 SKIP.
         IF v-dept-inst[3] <> "" THEN PUT v-dept-inst[3] AT 3 SKIP.
         IF v-dept-inst[4] <> "" THEN PUT v-dept-inst[4] AT 3 SKIP.
         IF v-dept-inst[5] <> "" THEN PUT v-dept-inst[5] AT 3 SKIP.
         IF v-dept-inst[6] <> "" THEN PUT v-dept-inst[6] AT 3 .
      END.
      
  end.

 
  put skip(1).
  
end.
    
put skip(1).
  

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
