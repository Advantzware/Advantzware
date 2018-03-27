/* ------------------------------------------- cec/quote/quooracl.i  04/04 YSK */
/* print quote items in Oracle                                          */
/* -------------------------------------------------------------------------- */



FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:

  numfit = 0.

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
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
     numfit = 5.
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
    lv-yes-printed = NO.
    IF i EQ 1 THEN DO:
      /*      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.  */
      lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".

      put lv-est-no FORM "x(6)" AT 1  /*SPACE(1) */
          xqitm.part-dscr1 AT 8 /*space(1)*/
          xqitm.part-no AT 40 .  
      
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
        /* trim-size = STRING({sys/inc/k16v.i eb.len}) + "X" + STRING({sys/inc/k16v.i eb.wid})
                  + "X" + STRING({sys/inc/k16v.i eb.dep})*/
           trim-size = trim(STRING(eb.len)) + " x " + trim(STRING(eb.wid))
                  + " x " + trim(STRING(eb.dep)).
      ELSE trim-size = "".

      FIND FIRST style WHERE style.company = xqitm.company
                         AND xqitm.style = style.style NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE xqitm.style.
      PUT     trim-size AT 8 FORM "x(30)"
              /*xqitm.style*/ /* style-dscr */  .
    END.
    ELSE
    IF i EQ 3 THEN   do:
        PUT style-dscr AT 8 .
    END.
    ELSE
    IF i EQ 4 THEN
      PUT    /* "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)"*/
            xqitm.i-coldscr  AT 8 FORM "x(40)" .
    ELSE
    IF i EQ 5 THEN DO:
      
       PUT /*"CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)"         */
           IF AVAIL ef THEN ef.brd-dscr  ELSE ""  AT 8 FORMAT "x(30)" .
    END.
  /*
    ELSE
    IF i EQ 5 /* AND numfit GE 5 */ THEN DO:
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

       
       put "FG#: " + (IF AVAIL eb THEN eb.stock-no ELSE "") AT 8 FORM "x(21)"
           /*"PLATE#: " + (IF AVAIL eb THEN eb.plate-no  ELSE "")*/
            v-board FORM "x(30)".                                           
    END.
    */
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
           put xqqty.qty  AT 61 
               space(2) xqqty.rels space(3)
               xqqty.price FORM "$->>,>>9.99" space(5)
               lv-uom .   
      

      v-line-total = v-line-total + xqqty.price.
    END.

/*  IF i NE 3 THEN DOWN. */
   /* IF lv-yes-printed THEN*/  PUT SKIP.
    IF i = 5 THEN PUT SKIP(1).

    IF i >= 5 THEN DO:  /* displ film */
       IF est.est-type = 4 THEN
       FOR EACH est-flm WHERE est-flm.company = eb.company
                          AND est-flm.est-no = eb.est-no
                          AND est-flm.snum = eb.form-no
                          AND est-flm.bnum = eb.blank-no
           NO-LOCK.
           PUT est-flm.dscr AT 9 est-flm.len "x" est-flm.wid SKIP.

       END.
       ELSE DO i = 1 TO 4:
          IF ef.leaf-dscr[i] <> "" THEN 
             PUT ef.leaf-dscr[i] AT 9 ef.leaf-l[i] "x" ef.leaf-w[i] SKIP.
       END.

    END.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
  END.
/*== don't print now, print after print all items 
  numfit = 0.
  
  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg OF xqqty NO-LOCK
      BREAK BY xqchg.qty
            BY xqchg.charge:

    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
    END.
    numfit = numfit + 1.
  END.

/*  IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE. */

  FOR EACH xqchg OF xqitm NO-LOCK
      /*EACH xqchg OF xqqty NO-LOCK*/
      BREAK /* BY xqchg.qty */
            BY xqchg.charge:
/*
    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.qty) THEN PUT SKIP(1).

      PUT "For QTY: " AT 5
          TRIM(STRING(xqchg.qty,">>>>>>>")).
    END.
 */
    IF FIRST-OF(xqchg.charge) THEN DO:

       lv-chg-amt = IF xqchg.mkup <> 0 THEN xqchg.amt / (1 - (xqchg.mkup / 100))
                 ELSE xqchg.amt 
                 .
       IF xqchg.bill EQ "N" THEN
         PUT xqchg.charge AT 8
             xqchg.prep-qty TO 65
             "N/C"        TO 84 SKIP.
       ELSE IF INDEX("TML",xqchg.bill) GT 0 THEN
           PUT xqchg.charge       AT 8
               "Time & Materials" /*AT 45*/
               xqchg.prep-qty TO 65 
               lv-chg-amt /*xqchg.amt */   FORM "$->>,>>9.99" TO 84 "      EA" SKIP.
       ELSE IF xqchg.bill EQ "W" THEN
            PUT xqchg.charge    AT 8
                xqchg.prep-qty TO 65
                "Will Advise"  TO 84 skip.
    END.
  END.
===*/
  IF LINE-COUNTER > PAGE-SIZE - 10 THEN DO:
     page.
     {cec/quote/quoorac1.i}
  END.

END.
numfit = 0.

FOR EACH xqqty OF xquo NO-LOCK,
    /* EACH xqchg OF xqqty NO-LOCK */
    EACH xqchg NO-LOCK
    WHERE xqchg.company EQ xqqty.company
      AND xqchg.loc EQ xqqty.loc
      AND xqchg.q-no EQ xqqty.q-no
      AND xqchg.line EQ xqqty.line
      AND xqchg.qty EQ xqqty.qty
    BREAK BY xqchg.charge:

  IF FIRST-OF(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.
END.

/*
IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.
*/

FOR EACH xqchg OF xquo NO-LOCK
    BREAK BY xqchg.charge:

  IF FIRST-OF(xqchg.charge) THEN do:
      /*PUT SKIP(1).*/
  lv-chg-amt = /*IF xqchg.mkup <> 0 THEN xqchg.amt / (1 - (xqchg.mkup / 100))
               ELSE*/ xqchg.amt 
               .

  IF xqchg.bill EQ "N" THEN
    PUT xqchg.charge AT 8
        xqchg.prep-qty at 61
       /* "N/C"        at 88 */ SKIP.
  ELSE
  IF INDEX("TML",xqchg.bill) GT 0 THEN DO:
     PUT xqchg.charge       AT 9 SPACE(1)
   /*     "Time & Materials" /*AT 45*/ */
        xqchg.prep-qty at 61
        xqchg.amt / xqchg.prep-qty FORMAT "$->>,>>9.99" at 76
        "     EA" SKIP.
  END.
  ELSE
  IF xqchg.bill EQ "W" THEN
    PUT xqchg.charge    AT 8
        xqchg.prep-qty AT 61
        /*"Will Advise"   at 88 */ skip.
  END.
 
END.

numfit = 0.
/*
FOR EACH xqchg OF xquo
    WHERE xqchg.qty  EQ 0
      AND xqchg.line EQ 0
    NO-LOCK
    BREAK BY xqchg.charge:

  IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.
END.

/*
IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.
*/

FOR EACH xqchg OF xquo
    WHERE xqchg.qty  EQ 0
      AND xqchg.line EQ 0
    NO-LOCK
    BREAK BY xqchg.charge:

  IF FIRST(xqchg.charge) THEN PUT SKIP(1).
  lv-chg-amt = IF xqchg.mkup <> 0 THEN xqchg.amt / (1 - (xqchg.mkup / 100))
               ELSE xqchg.amt 
               .

  IF xqchg.bill EQ "N" THEN
    PUT xqchg.charge AT 8
        xqchg.prep-qty to 65
        "N/C"        TO 84 SKIP.
      
  ELSE
  IF INDEX("TML",xqchg.bill) GT 0 THEN
    PUT xqchg.charge       AT 8
        "Time & Materials" /*AT 45*/
        xqchg.prep-qty to 65
        xqchg.amt       FORM "$->>,>>9.99"   TO 84 "      EA" SKIP.

  ELSE
  IF xqchg.bill EQ "W" THEN
    PUT xqchg.charge    AT 8
        xqchg.prep-qty TO 65
        "Will Advise"   TO 84 skip.
END.
*/
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
