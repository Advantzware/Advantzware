/* ------------------------------------------- cec/quote/quoxapc.i            */
/* print quote items in Xprint Marketing format                                          */
/* -------------------------------------------------------------------------- */

FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:

    IF s-print-2nd-dscr THEN
        ASSIGN i-dscr2 = xqitm.part-dscr2 .
    ELSE
        ASSIGN i-dscr2 = "" .
  numfit = 0.

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.
  IF numfit LT 6 THEN numfit = 6.

  /*IF AVAIL est AND est.est-type GE 3 AND est.est-type LE 4 THEN numfit = 4.*/
         
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
        
  IF AVAIL eb THEN DO:
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ eb.form-no
        NO-LOCK NO-ERROR.
          
    /*IF numfit LE 4                            AND
       (eb.cad-no NE "" OR eb.plate-no ne "") THEN numfit = 5. */
     IF numfit LE 4 THEN numfit = 5.
  END.

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
  END.
  DO i = 1 TO numfit /*WITH FRAME item-10p */:
    IF i EQ 1 THEN DO:
      /*      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.  */
      lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".

      put trim(lv-est-no) FORM "x(6)" SPACE(1) 
          xqitm.part-no SPACE(1).
      IF xqitm.part-dscr1 NE "" THEN
          PUT xqitm.part-dscr1  .  
      ELSE
          PUT i-dscr2  .  
    END.

    ELSE
    IF i EQ 2 THEN DO:
      trim-size = "".
    /*
      IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
      DO cc = 1 TO LENGTH(xqitm.size):
        IF SUBSTR(xqitm.size,cc,1) NE "" THEN
          trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
      end.

      ELSE trim-size = xqitm.size.
      */
      IF AVAIL eb THEN
         trim-size = STRING({sys/inc/k16v.i eb.len}) + "X" + STRING({sys/inc/k16v.i eb.wid})
                  + "X" + STRING({sys/inc/k16v.i eb.dep}).
      ELSE trim-size = "".

      FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.
      if ch-multi THEN PUT xquo.q-no SPACE(1) .
      PUT     trim-size AT 8 FORM "x(21)" .
              /*xqitm.style*/  
      IF xqitm.part-dscr1 NE "" AND i-dscr2 NE "" THEN
          PUT i-dscr2 AT 29. 
      ELSE do: 
          PUT style-dscr   .
          ASSIGN style-dscr = "" .
      END.
    END.
    ELSE
    IF i EQ 3 THEN do:
        col-dscr = xqitm.i-coldscr .

      PUT     "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)" .
      IF i-dscr2 NE "" AND style-dscr NE "" THEN do:
          PUT style-dscr  AT 29 FORM "x(30)".
          ASSIGN style-dscr = "" .
      END.
      ELSE DO:
          PUT col-dscr AT 29 FORM "x(30)".
          ASSIGN col-dscr = "" .
      END.

      /*ASSIGN style-dscr = "" .*/
    END.
    ELSE
    IF i EQ 4 THEN DO:
        brd-dscr = IF AVAIL ef THEN ef.brd-dscr /*xqitm.i-dscr*/ ELSE "" .

      PUT "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)"         .
      IF i-dscr2 NE "" AND col-dscr NE ""  THEN do:
          PUT col-dscr AT 29 FORMAT "x(30)".
          ASSIGN col-dscr = "" .
      END.
      ELSE do:
           PUT brd-dscr AT 29 FORMAT "x(30)".
           ASSIGN brd-dscr = "" .
      END.
  /*    ASSIGN col-dscr = "" .*/
    END.                              
    ELSE
    IF i EQ 5 /* AND numfit GE 5 */ THEN DO:
       v-board = IF AVAIL ef THEN
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6] 
                 ELSE "" .
       IF est.est-type = 6 THEN DO:
          FIND FIRST bf-eb WHERE bf-eb.company = est.company AND
                                 bf-eb.est-no = est.est-no AND
                                 bf-eb.form-no = 0 AND
                                 bf-eb.blank-no = 0 NO-LOCK NO-ERROR.
          v-board = IF AVAIL bf-eb THEN (bf-eb.part-dscr2) ELSE v-board.

       END.

       IF est.est-type = 6 THEN
          FIND FIRST bf-eb WHERE bf-eb.company EQ xquo.company
                             AND bf-eb.est-no  EQ xquo.est-no
                             AND bf-eb.form-no EQ 0
                             NO-LOCK NO-ERROR.

       lv-fg# = IF est.est-type EQ 6 AND AVAIL bf-eb THEN bf-eb.stock-no
                ELSE IF AVAIL eb THEN eb.stock-no ELSE xqitm.part-no.

       put "FG#: " + lv-fg# /*(IF AVAIL eb THEN eb.stock-no ELSE "")*/  AT 8 FORM "x(21)"  .
           /*"PLATE#: " + (IF AVAIL eb THEN eb.plate-no  ELSE "")*/
        IF i-dscr2 NE "" AND brd-dscr NE "" THEN do:
            PUT brd-dscr FORM "x(30)".  
            ASSIGN brd-dscr = "" .
        END.
        ELSE do:
             PUT v-board FORM "x(30)".         
             ASSIGN v-board = "" .
        END.
      /*  ASSIGN brd-dscr = "" .*/
    END.
    IF i EQ 6 THEN DO:
        IF i-dscr2 NE "" AND v-board NE "" THEN do:
            PUT v-board AT 29 FORM "x(30)".
            ASSIGN v-board = "" .
        END.
        ELSE PUT SPACE(58) .
    END.
    IF i > 6 THEN PUT SPACE(58) .

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
                
           lv-uom = IF est.est-type = 6 AND NOT lv-two-box THEN "SET" ELSE xqqty.uom.
           put xqqty.qty xqqty.rels space(5)
               xqqty.price FORM "->>>,>>9.99<<" space(4)
               lv-uom.  
      

      v-line-total = v-line-total + xqqty.price.
    END.

/*  IF i NE 3 THEN DOWN. */
    PUT SKIP.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
  END.

  numfit = 0.
  
  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg NO-LOCK WHERE xqchg.company = xqqty.company
                           AND xqchg.loc = xqqty.loc
                           AND xqchg.q-no = xqqty.q-no
                           AND xqchg.LINE = xqqty.LINE
                           AND xqchg.qty = xqqty.qty
                           AND xqchg.quote-date = xqqty.quote-date
      BREAK BY xqchg.qty
            BY xqchg.charge:

    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
    END.
    numfit = numfit + 1.
  END.

  IF LINE-COUNTER >= PAGE-SIZE - 1 THEN DO:
     page.  
     {cec/quote/quopaci2.i}
  END.

/*  IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE. */
  v-prep-printed = NO.
  v-prep-prt-list = "".
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
          "N/C"        TO 80 SKIP.
      
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN
      PUT xqchg.charge       AT 22
          "Time & Materials" AT 45
          lv-chg-amt /*xqchg.amt */         TO 80 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
      PUT xqchg.charge    AT 22
          "Will Advise "  TO 45 skip.

    IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quopaci2.i}
       v-printline = 0.
    END.
  END.
END.

IF LINE-COUNTER >= PAGE-SIZE - 1 THEN DO:
   page.  
   {cec/quote/quopaci2.i}
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
    BREAK BY xqchg.charge
          BY xqchg.s-num
          BY xqchg.b-num:

  IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.
END.

/*
IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.
*/

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

  IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quopaci2.i}
       v-printline = 0.
  END.

  IF FIRST(xqchg.charge) THEN PUT SKIP(1).

  /*IF xqchg.qty EQ 0 OR FIRST-OF(xqchg.b-num) THEN DO:*/
    lv-chg-amt = xqchg.amt.

    IF xqchg.bill EQ "N" THEN
      PUT xqchg.charge AT 22
          "N/C"        TO 80 SKIP.
      
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN
      PUT xqchg.charge       AT 22
          "Time & Materials" AT 45
          xqchg.amt          TO 80 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
      PUT xqchg.charge    AT 22
          "Will Advise "  TO 45 skip.
  /*END.*/
END.

if (ch-multi and (v-last or s-sep-page)) OR
   (not ch-multi) then do:
  if ch-inst then do:
    for each est-inst
        where est-inst.company = xquo.company
          AND est-inst.est-no eq xquo.est-no
          and est-inst.dept  ge fdept
          and est-inst.dept  le tdept
        no-lock break by est-inst.est-no by est-inst.dept:
      if first-of(est-inst.est-no) then do:
        put skip(1).
        put "Dept" at 6 "Department Manufacturing Instructions" at 11 skip.
        put "----" at 6 "-------------------------------------" at 11 skip.
      end.
      put est-inst.dept at 7 est-inst.inst[1] at 11.
      put est-inst.inst[2] at 11.
      put est-inst.inst[3] at 11 skip.
    end.
  end.

  put skip(1).

end.
    
put skip(1).
  

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
