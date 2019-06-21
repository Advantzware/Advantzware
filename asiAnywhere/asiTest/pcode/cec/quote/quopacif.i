/* ------------------------------------------- cec/quote/quofibre.i 11/00 JLF */
/* print quote items in Fibre format                                          */
/* -------------------------------------------------------------------------- */

FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no :
  numfit = 0.
  FOR EACH tt-qty:
      DELETE tt-qty.
  END.
  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
    CREATE tt-qty.
    ASSIGN tt-qty.tt-recid = RECID(xqqty)
           tt-qty.qty = xqqty.qty.
  END.
  IF numfit LT 4 THEN numfit = 4.

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

  /*FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.*/
  FIND FIRST tt-qty NO-LOCK NO-ERROR.
  IF AVAIL tt-qty THEN 
      FIND FIRST xqqty WHERE RECID(xqqty) = tt-qty.tt-recid NO-LOCK NO-ERROR.
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
          xqitm.part-no space(1) xqitm.part-dscr1.           
    END.
    ELSE IF i EQ 2 THEN DO:
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

      IF est.est-type NE 5 AND s-print-comp THEN
         ASSIGN trim-size = ""
                style-dscr = "".

      PUT     trim-size AT 8 FORM "x(21)"
              /*xqitm.style*/  style-dscr   .
    END.
    ELSE IF i EQ 3 THEN DO:
         PUT "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)".
         IF NOT s-print-comp OR est.est-type EQ 5 THEN PUT xqitm.i-coldscr  AT 29 FORM "x(30)".
         ELSE PUT "                              " AT 29.
    END.
    ELSE IF i EQ 4 THEN DO:      
       PUT "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)"         .
       IF AVAIL ef AND est.est-type NE 6 THEN PUT ef.brd-dscr AT 29 FORMAT "x(30)".
       ELSE PUT "                              " AT 29.
    END.
    ELSE IF i EQ 5 /* AND numfit GE 5 */ THEN DO:
       v-board = IF AVAIL ef THEN
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                 ELSE "".

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

       put "FG#: " + lv-fg# AT 8 FORM "x(21)"
           v-board FORM "x(30)".                                           
    END.
    IF i > 5 THEN PUT SPACE(58) .

    IF AVAIL tt-qty AND AVAIL xqqty THEN DO:
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
                
           lv-uom = /*IF est.est-type = 6 AND NOT lv-two-box THEN "SET" ELSE ??? Task# 03240607*/ xqqty.uom.
           put xqqty.qty xqqty.rels space(5)
               xqqty.price FORM "->>,>>9.99" space(6)
               lv-uom .  
      

      v-line-total = v-line-total + xqqty.price.
    END.

/*  IF i NE 3 THEN DOWN. */
    PUT SKIP.
    FIND next tt-qty NO-LOCK NO-ERROR.
    IF AVAIL tt-qty THEN 
      FIND FIRST xqqty WHERE RECID(xqqty) = tt-qty.tt-recid NO-LOCK NO-ERROR.
    /*fIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.*/

  END.
  IF s-print-comp THEN DO :      /* Print components of a set */
  
    IF AVAIL est AND est.est-type EQ 6 AND est.form-qty GT 1 THEN
    FOR EACH ef WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est.est-no NO-LOCK,      
        EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
        
      RUN printHeader (1,OUTPUT idummy).
      IF FIRST(ef.form-no) THEN DO:
        IF LINE-COUNTER GT PAGE-SIZE - 10 THEN do:
           PAGE.
           {cec/quote/quopaci2.i}
        END.
        PUT "Components" AT 5 SKIP.
      END.
      ELSE IF LINE-COUNTER > PAGE-SIZE - 10 THEN DO:
         page.  
         {cec/quote/quopaci2.i}
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
    
      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style.

      put eb.part-no AT 8 FORM "x(21)" eb.part-dscr1 AT 29 SKIP.
      PUT temp-trim-size AT 8 FORM "X(21)" style-dscr AT 29 SKIP.
      
      v-board = /*IF AVAIL ef THEN*/
                  ef.board    + " - " +
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                /*ELSE "-"*/.
              
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
    
      PUT eb.part-dscr2 AT 8 FORM "X(21)" v-board AT 29 FORM "X(50)" SKIP.
      
      put eb.i-coldscr   AT 30 SKIP.
      IF LINE-COUNTER > PAGE-SIZE - 11 THEN DO:
         page.  
         {cec/quote/quopaci2.i}
      END.
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
      BREAK BY xqchg.qty DESCENDING
            BY xqchg.charge:

    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
    END.
    numfit = numfit + 1.
  END.

  IF LINE-COUNTER > PAGE-SIZE - 11 THEN DO:
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

    IF LINE-COUNTER > PAGE-SIZE - 11 THEN DO:
       page.  
       {cec/quote/quopaci2.i}
       v-printline = 0.
    END.
  END.
END.  /*xqitm*/

IF LINE-COUNTER > PAGE-SIZE - 11 THEN DO:
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

  IF xqchg.qty EQ 0 OR FIRST-OF(xqchg.b-num) THEN numfit = numfit + 1.
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
    BREAK BY xqchg.qty
          BY xqchg.charge
          BY xqchg.s-num
          BY xqchg.b-num:

  IF LINE-COUNTER > PAGE-SIZE - 11 THEN DO:
       page.  
       {cec/quote/quopaci2.i}
       v-printline = 0.
  END.

  IF FIRST(xqchg.charge) THEN PUT SKIP(1).

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

    IF LAST-OF(xqchg.qty) THEN LEAVE.
END.

if (ch-multi and (v-last OR s-sep-page)) or (not ch-multi) then do:
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

  do i = 1 to 5:
    if xquo.comment[i] ne "" then do:
      put skip(1).
      leave.
    end.
  end.

  do i = 1 to 5:
    if xquo.comment[i] ne "" then put xquo.comment[i] at 6 skip.
  end.

end.
    
put skip(1).
  

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
