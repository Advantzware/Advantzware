/* ------------------------------------------- cec/quote/quofibre.i 11/00 JLF */
/* print quote items in 10 Pitch format                                       */
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
  IF NOT AVAIL eb THEN
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
  FIND FIRST style WHERE style.company = xqitm.company AND
                         style.style = xqitm.style NO-LOCK NO-ERROR.

  DO i = 1 TO numfit WITH FRAME detail:
    IF i EQ 1 THEN DO:
      trim-size = "".
    
      IF LENGTH(TRIM(xqitm.size)) GT 19 THEN
      DO cc = 1 TO LENGTH(xqitm.size):
        IF SUBSTR(xqitm.size,cc,1) NE "" THEN
          trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
      end.

      ELSE trim-size = xqitm.size.

      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.

      DISPLAY TRIM(xquo.est-no) @ xquo.est-no
              xqitm.part-no
              trim-size.
    END.

    ELSE
    IF i EQ 2 THEN
      DISPLAY xqitm.part-dscr1              @ xqitm.part-no
              xqitm.style                   @ trim-size.

    ELSE
    IF i EQ 3 THEN DO:
      v-board = IF AVAIL ef THEN
                  ef.board    + " - " +
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                ELSE "-".

      IF SUBSTR(v-board,LENGTH(TRIM(v-board)),1) EQ "-" THEN
        SUBSTR(v-board,LENGTH(TRIM(v-board)),1) = "".

      IF v-board EQ "" THEN v-board = xqitm.i-dscr.

      PUT xqitm.part-dscr2                  TO 28       FORMAT "x(19)"
          v-board                           TO 47       FORMAT "x(18)".
    END.

    ELSE
    IF i EQ 4 AND xqitm.i-coldscr NE "" THEN
	  DISPLAY xqitm.i-coldscr               @ trim-size.

    IF AVAIL xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).
              
      IF i EQ 3 THEN
        PUT xqqty.qty           TO 55   FORMAT ">>>>>>9"
            xqqty.price         TO 65   FORMAT ">>,>>9.99"
            xqqty.uom           TO 69
	        xxx                 TO 80   FORMAT ">>>,>>9.99".

      ELSE DISPLAY xqqty.qty xqqty.price xqqty.uom xxx.
    END.

    IF i NE 3 THEN DOWN.

    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
  END.

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

  IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.

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
      IF FIRST(xqchg.qty) THEN PUT SKIP(1).

      PUT "For QTY: " AT 5
          TRIM(STRING(xqchg.qty,">>>>>>>")).
    END.

    IF xqchg.bill EQ "N" THEN
      PUT xqchg.charge AT 26
          "N/C"        TO 80 SKIP.
      
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN
      PUT xqchg.charge       AT 26
          "Time & Materials" AT 45
          xqchg.amt          TO 80 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
      PUT xqchg.charge    AT 26
          "Will Advise "  TO 45 skip.
  END.
END.

numfit = 0.
  
FOR EACH xqchg OF xquo
    WHERE xqchg.qty  EQ 0
      AND xqchg.line EQ 0
    NO-LOCK
    BREAK BY xqchg.charge:

  IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.
END.

IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.

FOR EACH xqchg OF xquo
    WHERE xqchg.qty  EQ 0
      AND xqchg.line EQ 0
    NO-LOCK
    BREAK BY xqchg.charge:

  IF FIRST(xqchg.charge) THEN PUT SKIP(1).

  IF xqchg.bill EQ "N" THEN
    PUT xqchg.charge AT 26
        "N/C"        TO 80 SKIP.
      
  ELSE
  IF INDEX("TML",xqchg.bill) GT 0 THEN
    PUT xqchg.charge       AT 26
        "Time & Materials" AT 47
        xqchg.amt          TO 80 SKIP.

  ELSE
  IF xqchg.bill EQ "W" THEN
    PUT xqchg.charge    AT 26
        "Will Advise "  TO 47 skip.
END.

if (ch-multi and v-last) OR (ch-multi AND s-sep-page) OR
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
