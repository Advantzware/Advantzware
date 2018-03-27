/* --------------------------------------------- cec/quote/quocsc.i 11/97 FWK */
/* print quote items in CSC format                                            */
/* -------------------------------------------------------------------------- */

FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:
  numfit = 0.

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.
  IF numfit LT 4 THEN numfit = 4.

  comfit = 0.
  IF (ch-multi AND (v-last OR s-sep-page)) OR
     (NOT ch-multi) THEN DO:
    IF ch-inst THEN
    FOR EACH est-inst
        WHERE est-inst.e-num eq xquo.e-num
          AND est-inst.dept  ge fdept
          AND est-inst.dept  le tdept
        NO-LOCK
        BREAK BY est-inst.e-num
              BY est-inst.dept:
      
      IF FIRST-OF(est-inst.e-num) THEN comfit = comfit + 3.
    
      comfit = comfit + 3.
    END.
  
    comfit = comfit + 1.

    IF v-print-fmt EQ "ContSrvc" THEN DO:
      DO i = 1 TO 5:
        IF xquo.comment[i] NE "" THEN DO:
          comfit = comfit + 1.
          LEAVE.
        END.
      END.
  
      DO i = 1 TO 5:
        IF xquo.comment[i] NE "" THEN comfit = comfit + 1.
      END.
  
      comfit = comfit + 5.
    END.  
  END.

  ASSIGN
   linfit = numfit
   numfit = numfit + comfit.

  IF AVAIL est THEN
           FIND FIRST eb WHERE eb.company EQ est.company
                AND eb.est-no  EQ est.est-no
                AND eb.part-no EQ xqitm.part-no
                AND eb.form-no NE 0
                NO-LOCK NO-ERROR.
  IF NOT AVAIL eb AND xqitm.est-no <> "" THEN
           FIND FIRST eb WHERE eb.company EQ est.company
                AND eb.est-no  EQ est.est-no
                and eb.form-no NE 0
                NO-LOCK NO-ERROR.

  IF AVAIL eb THEN 
           FIND FIRST ef WHERE ef.company EQ est.company
             AND ef.est-no  EQ est.est-no
             AND ef.form-no EQ eb.form-no NO-LOCK NO-ERROR.

  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  v-style = if xqitm.style <> "" THEN xqitm.style
            ELSE IF AVAIL eb THEN eb.style
            ELSE "".

  FIND FIRST style WHERE style.company = xqitm.company AND
                         style.style = v-style NO-LOCK NO-ERROR.
  style-name = IF AVAIL style THEN style.dscr ELSE xqitm.style.
    
  DO i = 1 TO linfit WITH FRAME item-10p:
    IF i EQ 1 THEN DO:
      trim-size = "".
      /*
      IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
      DO cc = 1 TO LENGTH(xqitm.size):
        IF SUBSTR(xqitm.size,cc,1) NE "" THEN
          trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
      END.

      ELSE trim-size = xqitm.size.
      */
      
      IF AVAIL eb AND AVAIL style AND int(style.industry) > 1 THEN
         trim-size = STRING({sys/inc/k16v.i eb.len}) + "x" + STRING({sys/inc/k16v.i eb.wid})
                  + "x" + STRING({sys/inc/k16v.i eb.dep}).

      ELSE IF AVAIL eb THEN trim-size = STRING(eb.len) + "x" + STRING(eb.wid)
                  + "x" + STRING(eb.dep).

      ELSE trim-size = "".

      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.

      DISPLAY trim(xquo.est-no) @ xquo.est-no
              xqitm.part-no
              trim-size.
    END.

    ELSE
    IF i EQ 2 THEN
      DISPLAY xqitm.part-dscr1              @ xqitm.part-no
              style-name                    @ trim-size.

    ELSE
    IF i EQ 3 THEN DO:
      v-board = "".
    
      IF AVAIL est AND (est.est-type NE 6 OR est.form-qty EQ 1) THEN DO:
        RELEASE eb.
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
              AND eb.form-no NE 0
            NO-LOCK NO-ERROR.

        RELEASE ef.
        IF AVAIL eb THEN
         FIND FIRST ef WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no
            AND ef.form-no EQ eb.form-no NO-LOCK NO-ERROR.

        v-board = IF AVAIL ef THEN
                    ef.board    + " - " +
                    ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                    ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                  ELSE "-".

        IF SUBSTR(v-board,LENGTH(TRIM(v-board)),1) EQ "-" THEN
          SUBSTR(v-board,LENGTH(TRIM(v-board)),1) = "".

        IF v-board EQ "" THEN v-board = xqitm.i-dscr.
      END.

      PUT xqitm.part-dscr2                  TO 31       format "x(25)"
          v-board                           TO 57       format "x(24)".
    END.

    ELSE
    IF i EQ 4 THEN DO:
      IF AVAIL est THEN
         FIND FIRST eb WHERE eb.company EQ est.company
              AND eb.est-no  EQ est.est-no
              AND eb.part-no EQ xqitm.part-no
              AND eb.form-no NE 0
              NO-LOCK NO-ERROR.
      IF NOT AVAIL eb AND xqitm.est-no <> "" THEN
         FIND FIRST eb WHERE eb.company EQ est.company
              AND eb.est-no  EQ est.est-no
              and eb.form-no NE 0
              NO-LOCK NO-ERROR.

      IF xqitm.i-coldscr <> "" THEN
         DISPLAY xqitm.i-coldscr               @ trim-size.
      ELSE IF AVAIL eb THEN DISPLAY eb.i-coldscr @ trim-size.
    END.
      

    IF AVAIL xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).
              
      IF i EQ 3 THEN
        PUT xqqty.qty           to 64   format ">>>>>>9"
            xqqty.price         to 75   format ">>,>>9.99"
            xqqty.uom           TO 80.

      ELSE DISPLAY xqqty.qty xqqty.price xqqty.uom.        
    END.

    IF i NE 3 THEN DOWN.

    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
  END.

  IF v-print-fmt EQ "ContSrvc" THEN DO:      /* Print components of a set */
    FIND FIRST est
        WHERE est.company EQ xquo.company
          AND est.est-no  EQ xquo.est-no
        NO-LOCK NO-ERROR.
    IF AVAIL est AND est.est-type EQ 6 AND est.form-qty GT 1 THEN
    FOR EACH ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
        NO-LOCK,
      
        EACH eb OF ef NO-LOCK
      
        BREAK BY ef.form-no
      
        WITH FRAME item-10p:
      
      IF FIRST(ef.form-no) THEN DO:
        IF LINE-COUNTER + 7 GT PAGE-SIZE - 2 THEN PAGE.
      
        PUT "Components" AT 5 SKIP.
      END.
    
      ELSE
      IF LINE-COUNTER + 5 GT PAGE-SIZE - 2 THEN PAGE.
    
      PUT SKIP(1).
    
      trim-size =
          TRIM(STRING(TRUNC(eb.len,0) + ((eb.len - TRUNC(eb.len,0)) /
                  k_frac),">>>9.99")) + " x " +
          TRIM(STRING(TRUNC(eb.wid,0) + ((eb.wid - TRUNC(eb.wid,0)) /
                  k_frac),">>>9.99")) + " x " +
          TRIM(STRING(TRUNC(eb.dep,0) + ((eb.dep - TRUNC(eb.dep,0)) /
                  k_frac),">>>9.99")).
                
      IF LENGTH(TRIM(trim-size)) GT 24 THEN
      DO cc = 1 TO LENGTH(trim-size):
        IF SUBSTR(trim-size,cc,1) NE "" THEN
          temp-trim-size = temp-trim-size + SUBST(trim-size,cc,1).
      END.
      ELSE temp-trim-size = trim-size.
    
      DISPLAY eb.part-no                      @ xqitm.part-no
              temp-trim-size                  @ trim-size.
      DOWN.
    
      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
            
      DISPLAY eb.part-dscr1                   @ xqitm.part-no
              eb.style                        @ trim-size
              style.dscr WHEN AVAIL style     @ trim-size.
      DOWN.
    
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
    
      DISPLAY eb.part-dscr2                   @ xqitm.part-no
              v-board                         @ trim-size.
      DOWN.
    
      DISPLAY eb.i-coldscr                    @ trim-size.
      DOWN.
    END.
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
END.

if (ch-multi and (v-last OR s-sep-page)) OR
   (not ch-multi) then do:
  if ch-inst then do:
    for each est-inst
        where est-inst.company eq xquo.company
          and est-inst.est-no  eq xquo.est-no
          and est-inst.dept    ge fdept
          and est-inst.dept    le tdept
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

  if v-print-fmt eq "ContSrvc" then do:
    do i = 1 to 5:
      if xquo.comment[i] ne "" then do:
        put skip(1).
        leave.
      end.
    end.

    do i = 1 to 5:
      if xquo.comment[i] ne "" then put xquo.comment[i] at 6 skip.
    end.

    display skip(1) "Sincerely,"  at 6
            skip(2) sman.sname    at 6 skip
        with no-box no-underline no-labels no-attr-space frame f-salute{1} STREAM-IO.
  end.
end.
    
put skip(1).
    
/* end ---------------------------------- copr. 1997  Advanced Software, Inc. */
