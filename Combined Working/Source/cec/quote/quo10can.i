/* ------------------------------------------- cec/quote/quohawl.i  */
/* print quote items in Fibre format                                          */
/* -------------------------------------------------------------------------- */


FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:

  numfit = 0.
  ll-prt-dscr2 = s-print-2nd-dscr AND xqitm.part-dscr2 NE "".

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.
  IF numfit LT 4 THEN numfit = 4.

  IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
       page.  
       {cec/quote/quo10can2.i}
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
          
    /* IF numfit LE 4                            AND
       (eb.cad-no NE "" OR eb.plate-no ne "") THEN numfit = 5. 
     numfit = 5. */
  END.
  IF numfit < 9 THEN numfit = 9.

  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  FIND FIRST itemfg WHERE itemfg.company = xqitm.company
                      AND itemfg.i-no = xqitm.part-no NO-LOCK NO-ERROR.
 
  DO i = 1 TO numfit /*WITH FRAME item-10p */:
    IF i EQ 1 THEN DO:
      /*      IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.  */
      lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".
      lv-part-dscr1 = IF AVAIL est AND est.est-type EQ 6 AND AVAIL itemfg THEN itemfg.i-name
                      ELSE xqitm.part-dscr1.
      PUT SPACE(2) trim(lv-est-no) FORM "x(6)" SPACE(1) 
          xqitm.part-no space(1) lv-part-dscr1.  
         
    END.
    ELSE
    IF i EQ 2 THEN DO:
      trim-size = "".
      IF AVAIL est AND est.est-type = 6 THEN DO: /* set header */
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
           
      END.
      PUT "<C26>" style-dscr  FORM "x(30)" .
     
    END.
    ELSE IF i EQ 3 THEN DO:  
      IF ll-prt-dscr2 AND xqitm.part-dscr2 NE "" THEN do:
           PUT "<C26>" xqitm.part-dscr2  .
      END.
       ELSE NEXT .
    END.
     ELSE
    IF i EQ 4 THEN DO:
      trim-size = "".
      IF AVAIL est AND est.est-type = 6 THEN DO: /* set header */
         IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
         DO cc = 1 TO LENGTH(xqitm.size):
            IF SUBSTR(xqitm.size,cc,1) NE "" THEN
               trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
         end.

         ELSE trim-size = xqitm.size.
      END.
      ELSE DO:
           
           IF AVAIL eb AND AVAIL style AND int(style.industry) > 1 THEN
             trim-size = STRING({sys/inc/k16v.i eb.len},">>9.99") + " x " + STRING({sys/inc/k16v.i eb.wid},">>9.99")
                  + " x " + STRING({sys/inc/k16v.i eb.dep},">>9.99").

           ELSE IF AVAIL eb THEN trim-size = STRING(eb.len) + " x " + STRING(eb.wid)
                  + " x " + STRING(eb.dep).

           ELSE trim-size = "".
      END.
     IF trim-size NE "" THEN
         PUT  "<C26>" trim-size  FORM "x(30)" .
     ELSE NEXT .
                 
    END.
    ELSE
    IF i EQ 5 THEN do:
         IF xqitm.i-coldscr NE "" THEN
         PUT "<C26>" xqitm.i-coldscr   FORM "x(30)".
         ELSE NEXT .
    END.
    ELSE
    IF i EQ 6 THEN DO:
        IF xqitm.i-dscr NE "" THEN
        PUT "<C26>" IF xqitm.i-dscr NE "" THEN xqitm.i-dscr ELSE IF AVAIL ef THEN ef.brd-dscr ELSE ""   FORMAT "x(30)".
        ELSE NEXT .
    END.

    ELSE
    IF i EQ 7 /* AND numfit GE 6 */ THEN DO:
       lv-fg# = IF AVAIL eb THEN eb.stock-no ELSE xqitm.part-no .
       put "<C26>FG#: " + lv-fg#  FORM "x(30)" .
    END.
    ELSE
    IF i EQ 8 /* AND numfit GE 7 */ THEN DO:
       PUT     "<C26>DIE#: " + IF AVAIL eb THEN eb.die-no ELSE ""  FORM "x(30)" .

    END.
    ELSE
    IF i EQ 9 /* AND numfit GE 8 */ THEN DO:
       v-board = IF AVAIL ef THEN
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                ELSE "".
      
       PUT "<C26>CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "")  FORM "x(30)" .
           IF v-board NE "" THEN
               PUT "<C26>" v-board  FORM "x(30)" /*SKIP(1)*/  .  

    END.
    IF i > 9 THEN PUT SPACE(58) .

    IF AVAIL xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).

       PUT "<C51>" xqqty.qty /* xqqty.rels*/ space(9)
               xqqty.price FORM "->>,>>9.99" space(6)
               xqqty.uom  .
              
       v-line-total = v-line-total + xqqty.price.
    END.

/*  IF i NE 3 THEN DOWN. */
    PUT  SKIP.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
    IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quo10can2.i}
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
        
      
      IF FIRST(ef.form-no) THEN DO:
        IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO: 
           page.  
           {cec/quote/quo10can2.i}
           v-printline = 0.
        END.
        PUT "Components" AT 5 SKIP.
      END.
      IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO: 
           page.  
           {cec/quote/quo10can2.i}
           v-printline = 0.
      END.
              
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
    
      put eb.part-no AT 10 FORM "x(21)" temp-trim-size   SKIP.
      IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO: 
           page.  
           {cec/quote/quo10can2.i}
           v-printline = 0.
      END.

      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style.      
      PUT eb.part-dscr1 AT 10 FORM "x(21)"
          style-dscr SKIP.
      IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO: 
           page.  
           {cec/quote/quo10can2.i}
           v-printline = 0.
      END.

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
      IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO: 
            page.  
            {cec/quote/quo10can2.i}
            v-printline = 0.
      END.

      PUT eb.part-dscr2  AT 10  FORM "x(21)"
          v-board     SKIP .

      IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO: 
                 page.  
                 {cec/quote/quo10can2.i}
                 v-printline = 0.
            END.
    
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

/*  IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE. */
  IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
     page.  
     {cec/quote/quo10can2.i}
  END.
  v-prep-printed = NO.
  v-prep-prt-list = "".
  FOR EACH xqqty OF xqitm NO-LOCK,
      /*EACH xqchg OF xqqty NO-LOCK */
      EACH xqchg WHERE xqchg.company = xqqty.company
                   AND xqchg.loc = xqqty.loc
                   AND xqchg.q-no = xqqty.q-no
                   AND xqchg.LINE = xqqty.LINE
                   AND xqchg.qty = xqqty.qty NO-LOCK 
      BREAK BY xqchg.s-num BY xqchg.qty
           /* BY xqchg.charge*/ :

    FIND FIRST est-prep WHERE est-prep.company EQ xqqty.company
              AND est-prep.est-no  EQ xquo.est-no
              AND est-prep.s-num   EQ xqchg.s-num
              AND est-prep.b-num   EQ xqchg.b-num
              AND est-prep.dscr = xqchg.charge
            NO-LOCK NO-ERROR.
  
    IF FIRST-OF(xqchg.s-num) THEN v-prep-prt-list = "".
    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.qty) THEN PUT SKIP(1).
   /*   PUT "For QTY: " AT 5
          TRIM(STRING(xqchg.qty,">>>>>>>")).  */
    END.

    IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quo10can2.i}
       v-printline = 0.
    END.

    IF NOT AVAIL est-prep then 
         PUT "For QTY: " AT 5
          TRIM(STRING(xqchg.qty,">>>>>>>")).
                                            
    lv-chg-amt = /* already calculated from quote creation
                    iF xqchg.mkup <> 0 THEN xqchg.amt / (1 - (xqchg.mkup / 100))
                    ELSE */ xqchg.amt .
    IF NOT AVAIL est-prep OR
       LOOKUP(xqchg.charge,v-prep-prt-list) <= 0 
    THEN DO:
      v-prep-prt-list = v-prep-prt-list + xqchg.charge + ",".   

      IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quo10can2.i}
       v-printline = 0.
      END.
      IF xqchg.bill EQ "N" THEN
         PUT xqchg.charge AT 22
          /*"N/C"        TO 80*/ SKIP.
      
      ELSE IF INDEX("TML",xqchg.bill) GT 0 THEN
         PUT xqchg.charge       AT 22
             /*   "Time & Materials" AT 45  */
             lv-chg-amt /*xqchg.amt*/          TO 80 SKIP.

      ELSE IF xqchg.bill EQ "W" THEN
           PUT xqchg.charge    AT 22
           /*  "Will Advise "  TO 45 */ skip.
    END.
    /*IF AVAIL est-prep THEN v-prep-printed = YES.*/
    IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quo10can2.i}
       v-printline = 0.
    END.

  END.
  IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
     page.  
     {cec/quote/quo10can2.i}
     v-printline = 0.
  END.
  IF NOT LAST(xqitm.part-no) THEN
  put skip(1) .
END.

numfit = 0.

FOR EACH xqchg OF xquo NO-LOCK
    /* WHERE NOT CAN-FIND(FIRST xqqty OF xqchg) */
    WHERE NOT CAN-FIND(FIRST xqqty
    WHERE xqqty.company EQ xqchg.company
      AND xqqty.loc EQ xqchg.loc
      AND xqqty.q-no EQ xqchg.q-no
      AND xqqty.line EQ xqchg.line
      AND xqqty.qty EQ xqchg.qty)
    BREAK BY xqchg.charge:

  IF FIRST(xqchg.charge) THEN numfit = numfit + 1.
  numfit = numfit + 1.
END.

/*
IF LINE-COUNTER + numfit GT PAGE-SIZE - 2 THEN PAGE.
*/
IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
   page.  
   {cec/quote/quo10can2.i}
END.
FOR EACH xqchg OF xquo
   /* WHERE NOT CAN-FIND(FIRST xqqty OF xqchg) */
    WHERE xqchg.qty  EQ 0
      AND xqchg.line EQ 0
    NO-LOCK
    BREAK BY xqchg.charge:

   IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
       page.  
       {cec/quote/quo10can2.i}
       v-printline = 0.
    END.
  IF FIRST(xqchg.charge) THEN PUT SKIP(1).

  lv-chg-amt = /*already calculated from quote creation
                 IF xqchg.mkup <> 0 THEN xqchg.amt / (1 - (xqchg.mkup / 100))
                 ELSE*/ xqchg.amt .

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
        /*"Will Advise "  TO 45 */ skip.
  IF LINE-COUNTER > PAGE-SIZE - 2 THEN DO:
      page.  
      {cec/quote/quo10can2.i}
      v-printline = 0.
   END.   
END.

if (ch-multi and (v-last OR s-sep-page)) or (not ch-multi) then do:

  if ch-inst then do:
      
      IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:
          page.  
          {cec/quote/quo10can2.i}
      END.
      FIND FIRST est WHERE est.company = xquo.company
                       AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
      {custom/notespr2.i job v-inst2 6 "notes.rec_key = est.rec_key " }
      DO i = 1 TO 6:
             v-dept-inst[i] = v-inst2[i].
      END.
      IF v-dept-inst[1] <> "" OR v-dept-inst[2] <> "" THEN DO:
         IF LINE-COUNTER > PAGE-SIZE - 6 THEN DO:
            page.  
            {cec/quote/quo10can2.i}
         END.
         PUT "Department Notes: " SKIP
           v-dept-inst[1] AT 3 SKIP
           v-dept-inst[2] AT 3 SKIP
           v-dept-inst[3] AT 3 SKIP
           v-dept-inst[4] AT 3 SKIP
           v-dept-inst[5] AT 3 SKIP
           v-dept-inst[6] AT 3 .
      END.
      
  end.

 
  put skip(1).
  /* moved to quoxprnt.p 
  do i = 1 to 5:
    if xquo.comment[i] ne "" then do:
      put skip(1).
      leave.
    end.
  end.

  do i = 1 to 5:
    if xquo.comment[i] ne "" then put xquo.comment[i] at 6 skip.
  end.
 */

end.
    
put skip(1).
  

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
