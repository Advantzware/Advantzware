/* ------------------------------------------- cec/quote/quoxprem.i  */
/* print quote items in premierX format                                          */
/* -------------------------------------------------------------------------- */

FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:

  numfit = 0.

  FOR EACH xqqty OF xqitm NO-LOCK:
    numfit = numfit + 1.
  END.
  IF numfit LT 4 THEN numfit = 4.

  IF LINE-COUNTER > 55 THEN DO:
       page.  
       {cec/quote/quoxprm2.i}
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
  IF numfit < 5 THEN numfit = 5.

  FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
  FIND FIRST itemfg WHERE itemfg.company = xqitm.company
                      AND itemfg.i-no = xqitm.part-no NO-LOCK NO-ERROR.
 
  DO i = 1 TO numfit /*WITH FRAME item-10p */:
    IF i EQ 1 THEN DO:
      lv-est-no = IF AVAIL eb THEN xquo.est-no ELSE "".
      lv-part-dscr1 = IF AVAIL est AND est.est-type EQ 6 AND AVAIL itemfg THEN itemfg.i-name
                      ELSE xqitm.part-dscr1.
      PUT TRIM(lv-est-no) FORM "x(6)" SPACE(1) 
          xqitm.part-no space(1) lv-part-dscr1.  
    END.
    ELSE
    IF i EQ 2 THEN DO:
      trim-size = "".
      IF AVAIL est AND est.est-type = 6 THEN DO: /* set header */
         IF LENGTH(TRIM(xqitm.size)) GT 24 THEN
         DO cc = 1 TO LENGTH(xqitm.size):
            IF SUBSTR(xqitm.size,cc,1) NE "" THEN
               trim-size = trim-size + SUBSTR(xqitm.size,cc,1).
         END.
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

           IF AVAIL eb AND AVAIL style AND int(style.industry) > 1 THEN
             trim-size = STRING({sys/inc/k16v.i eb.len}) + "x" + STRING({sys/inc/k16v.i eb.wid})
                  + "x" + STRING({sys/inc/k16v.i eb.dep}).

           ELSE IF AVAIL eb THEN trim-size = STRING(eb.len) + "x" + STRING(eb.wid)
                  + "x" + STRING(eb.dep).

           ELSE trim-size = "".
      END.
      PUT  xquo.q-no  trim-size AT 8 FORM "x(21)"
              /*xqitm.style*/  style-dscr   .
    END.
    ELSE
    IF i EQ 3 THEN
      PUT     "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 8 FORM "x(21)"
              xqitm.i-coldscr  AT 29 FORM "x(30)".
    ELSE
    IF i EQ 4 THEN DO:
       PUT "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 8  FORM "x(21)"         
           IF AVAIL ef THEN ef.brd-dscr /*xqitm.i-dscr*/ ELSE ""  AT 29 FORMAT "x(30)".
    END.
    ELSE
    IF i EQ 5 /* AND numfit GE 5 */ THEN DO:
       v-board = IF AVAIL ef THEN
                  ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                  ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                ELSE "".
       IF est.est-type = 6 THEN DO:
          FIND FIRST bf-eb WHERE bf-eb.company EQ est.company
                              AND bf-eb.est-no  EQ est.est-no
                              AND bf-eb.form-no EQ 0 NO-LOCK NO-ERROR.
          lv-fg# = IF AVAIL bf-eb THEN bf-eb.stock-no ELSE "".
       END.
       ELSE lv-fg# = IF AVAIL eb THEN eb.stock-no ELSE xqitm.part-no.
             
       PUT "FG#: " + lv-fg# AT 8 FORM "x(21)"
           /*"PLATE#: " + (IF AVAIL eb THEN eb.plate-no  ELSE "")*/
            v-board FORM "x(30)" /*SKIP(1)*/.                                           
    END.
    IF i > 5 THEN PUT SPACE(58) .

    /* rstark 05181205 */
    IF i LE 5 THEN DO:
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
    END.
    IF i EQ 1 THEN DO:
      RUN XMLOutput (lXMLOutput,'Column_1',lv-est-no,'Col').
      RUN XMLOutput (lXMLOutput,'Column_2',xqitm.part-no,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',lv-part-dscr1,'Col').
    END. ELSE
    IF i EQ 2 THEN DO:
      RUN XMLOutput (lXMLOutput,'Column_1',xquo.q-no,'Col').
      RUN XMLOutput (lXMLOutput,'Column_2',trim-size,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',style-dscr,'Col').
    END. ELSE
    IF i EQ 3 THEN DO:
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      IF AVAIL eb THEN
      RUN XMLOutput (lXMLOutput,'Column_2','DIE#:' + eb.die-no,'Col').
      ELSE RUN XMLOutput (lXMLOutput,'Column_2','DIE#:','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',xqitm.i-coldscr,'Col').
    END. ELSE
    IF i EQ 4 THEN DO:
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      IF AVAIL eb THEN
      RUN XMLOutput (lXMLOutput,'Column_2','CAD#:' + eb.cad-no,'Col').
      ELSE RUN XMLOutput (lXMLOutput,'Column_2','CAD#:','Col').
      IF AVAIL ef THEN
      RUN XMLOutput (lXMLOutput,'Column_3',ef.brd-dscr,'Col').
      ELSE RUN XMLOutput (lXMLOutput,'Column_3','','Col').
    END. ELSE
    IF i EQ 5 THEN DO:
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','FG#:' + lv-fg#,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',v-board,'Col').
    END.
    /* rstark 05181205 */

    IF AVAIL xqqty THEN DO:
       xxx    = IF xqqty.uom EQ "L" THEN xqqty.price    ELSE
                IF xqqty.uom EQ "C" THEN
                  ((xqqty.qty / 100) * xqqty.price)     ELSE
                IF xqqty.uom EQ "M" THEN
                  ((xqqty.qty / 1000) * xqqty.price)    ELSE
                  (xqqty.qty * xqqty.price).

       PUT xqqty.qty xqqty.rels SPACE(5)
               xqqty.price FORM "->>,>>9.99" SPACE(6)
               xqqty.uom .   
              
       v-line-total = v-line-total + xqqty.price.
       
       /* rstark 05181205 */
       RUN XMLOutput (lXMLOutput,'Column_4',xqqty.qty,'Col').
       RUN XMLOutput (lXMLOutput,'Column_5',xqqty.rels,'Col').
       RUN XMLOutput (lXMLOutput,'Column_6',xqqty.price,'Col').
       RUN XMLOutput (lXMLOutput,'Column_7',xqqty.uom,'Col').
       /* rstark 05181205 */

    END.

    /* rstark 05181205 */
    ELSE DO:
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
    END.
    RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
    /* rstark 05181205 */

    PUT  SKIP.
    FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
    IF LINE-COUNTER > 55 THEN DO:
       page.  
       {cec/quote/quoxprm2.i}
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
        
      IF LINE-COUNTER > 55 THEN DO:
          page.  
          {cec/quote/quoxprm2.i}
      END.
      IF FIRST(ef.form-no) THEN DO:
        IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO:
          PAGE.
          RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */
        END.
        PUT "Components" AT 5 SKIP.

        /* rstark 05181205 */
        XMLLineNumber = XMLLineNumber + 1.
        RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
        RUN XMLOutput (lXMLOutput,'Column_1','Components','Col').
        RUN XMLOutput (lXMLOutput,'Column_2','','Col').
        RUN XMLOutput (lXMLOutput,'Column_3','','Col').
        RUN XMLOutput (lXMLOutput,'Column_4','','Col').
        RUN XMLOutput (lXMLOutput,'Column_5','','Col').
        RUN XMLOutput (lXMLOutput,'Column_6','','Col').
        RUN XMLOutput (lXMLOutput,'Column_7','','Col').
        RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
        /* rstark 05181205 */
      
      END.
      ELSE IF LINE-COUNTER GT PAGE-SIZE - 2 THEN DO:
        PAGE.
        RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */
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
    
      PUT eb.part-no AT 8 FORM "x(21)" temp-trim-size   SKIP.
    
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2',eb.part-no,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',temp-trim-size,'Col').
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
      
      FIND FIRST style
          WHERE style.company EQ cocode
            AND style.style   EQ eb.style
          NO-LOCK NO-ERROR.
      style-dscr = IF AVAIL style THEN style.dscr ELSE eb.style.      
      PUT eb.part-dscr1 AT 8 FORM "x(21)"
          style-dscr SKIP.
    
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2',eb.part-dscr1,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',style-dscr,'Col').
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
      
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
    
      PUT eb.part-dscr2 AT 8 FORM "x(21)" v-board SKIP .
    
      PUT eb.i-coldscr AT 30 SKIP.
      
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2',eb.part-dscr2,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',v-board,'Col').
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',eb.i-coldscr,'Col').
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */

    END.  /* for ef*/
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

  IF LINE-COUNTER > 55 THEN DO:
     page.  
     {cec/quote/quoxprm2.i}
  END.

  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg WHERE xqchg.company = xqqty.company
                   AND xqchg.loc = xqqty.loc
                   AND xqchg.q-no =  xqqty.q-no
                   AND xqchg.LINE = xqqty.LINE
                   AND xqchg.qty = xqqty.qty
                   AND (NOT CAN-FIND(FIRST est-prep
                                    WHERE est-prep.company EQ xquo.company
                                      AND est-prep.est-no  EQ xquo.est-no
                                      AND est-prep.s-num   EQ xqchg.s-num
                                      AND est-prep.b-num   EQ xqchg.b-num
                                      AND est-prep.dscr    EQ xqchg.charge
                                      AND INDEX(est-prep.dscr, "Plate") EQ 0)
                                      AND INDEX(xqchg.charge, "plate") EQ 0

                        OR CAN-FIND(FIRST est-prep
                                    WHERE est-prep.company EQ xquo.company
                                      AND est-prep.est-no  EQ xquo.est-no
                                      AND est-prep.s-num   EQ xqchg.s-num
                                      AND est-prep.b-num   EQ xqchg.b-num
                                      AND INDEX(est-prep.dscr, "plate") GT 0
                                      AND INDEX(xqchg.charge, "plate") GT 0
                                      AND est-prep.cost NE xqchg.cost)

                        OR CAN-FIND(FIRST b-qchg 
                                    WHERE b-qchg.company EQ xqchg.company
                                      AND b-qchg.q-no    EQ xqchg.q-no
                                      AND b-qchg.s-num   EQ xqchg.s-num
                                      AND b-qchg.b-num   EQ xqchg.b-num
                                      AND INDEX(b-qchg.charge, "plate") GT 0
                                      AND INDEX(xqchg.charge, "plate") GT 0
                                      AND b-qchg.cost   NE xqchg.cost)
                                             
                        
                        )
      NO-LOCK 
      BREAK BY xqchg.qty
            BY xqchg.charge:
    
    IF FIRST-OF(xqchg.qty) THEN DO:
      IF FIRST(xqchg.qty) THEN DO:
        PUT SKIP(1).
        XMLLineNumber = XMLLineNumber + 1.
      END.
      PUT "For QTY: " AT 5
          TRIM(STRING(xqchg.qty,">>>>>>>")).
      RUN XMLOutput (lXMLOutput,'QuoteLineQtyCharge','','Row'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Quantity',TRIM(STRING(xqchg.qty)),'Col'). /* rstark 05181205 */
    END.
                                            
    lv-chg-amt = xqchg.amt.

    IF xqchg.bill EQ "N" THEN DO:
      PUT xqchg.charge AT 22 SKIP.
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',TRIM(STRING(xqchg.charge)),'Col'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
    END.
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN DO:
      PUT xqchg.charge AT 22 lv-chg-amt TO 80 SKIP.
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',TRIM(STRING(xqchg.charge)),'Col'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7',TRIM(STRING(lv-chg-amt)),'Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
    END.
    ELSE
    IF xqchg.bill EQ "W" THEN DO:
      PUT xqchg.charge AT 22 SKIP.
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',TRIM(STRING(xqchg.charge)),'Col'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
    END.
    
    IF LINE-COUNTER > 55 THEN DO:
       page.  
       {cec/quote/quoxprm2.i}
       v-printline = 0.
    END.
    
  END.

  IF LINE-COUNTER > 55 THEN DO:
     page.  
     {cec/quote/quoxprm2.i}
     v-printline = 0.
  END.
  PUT SKIP(1).
  
  XMLLineNumber = XMLLineNumber + 1. /* rstark 05181205 */

END.

numfit = 0.

FOR EACH xqchg OF xquo NO-LOCK
    WHERE (xqchg.qty EQ 0 AND xqchg.line EQ 0)
       OR (CAN-FIND(FIRST est-prep
                   WHERE est-prep.company EQ xquo.company
                     AND est-prep.est-no  EQ xquo.est-no
                     AND est-prep.s-num   EQ xqchg.s-num
                     AND est-prep.b-num   EQ xqchg.b-num
                     AND INDEX(est-prep.dscr, "Plate") EQ 0
                     AND INDEX(xqchg.charge, "plate") EQ 0
                     AND est-prep.dscr    EQ xqchg.charge)
       OR 
           (CAN-FIND(FIRST est-prep   WHERE est-prep.company EQ xquo.company
                                                AND est-prep.est-no  EQ xquo.est-no
                                                AND est-prep.s-num   EQ xqchg.s-num
                                                AND est-prep.b-num   EQ xqchg.b-num
                                                AND INDEX(est-prep.dscr, "plate") GT 0
                                                AND INDEX(xqchg.charge, "plate") GT 0)

           AND NOT CAN-FIND(FIRST est-prep   WHERE est-prep.company EQ xquo.company
                                                AND est-prep.est-no  EQ xquo.est-no
                                                AND est-prep.s-num   EQ xqchg.s-num
                                                AND est-prep.b-num   EQ xqchg.b-num
                                                AND INDEX(est-prep.dscr, "plate") GT 0
                                                AND INDEX(xqchg.charge, "plate") GT 0
                                                AND est-prep.cost NE xqchg.cost)
           AND NOT CAN-FIND(FIRST b-qchg 
                                    WHERE b-qchg.company EQ xqchg.company
                                      AND b-qchg.q-no    EQ xqchg.q-no
                                      AND b-qchg.s-num   EQ xqchg.s-num
                                      AND b-qchg.b-num   EQ xqchg.b-num
                                      AND INDEX(b-qchg.charge, "plate") GT 0
                                      AND INDEX(xqchg.charge, "plate") GT 0
                                      AND b-qchg.cost   NE xqchg.cost))
           )
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

IF LINE-COUNTER > 55 THEN DO:
   page.  
   {cec/quote/quoxprm2.i}
END.

FOR EACH xqchg OF xquo NO-LOCK
    WHERE (xqchg.qty EQ 0 AND xqchg.line EQ 0)
       OR (CAN-FIND(FIRST est-prep
                   WHERE est-prep.company EQ xquo.company
                     AND est-prep.est-no  EQ xquo.est-no
                     AND est-prep.s-num   EQ xqchg.s-num
                     AND est-prep.b-num   EQ xqchg.b-num
                     AND INDEX(est-prep.dscr, "Plate") EQ 0
                     AND INDEX(xqchg.charge, "plate") EQ 0
                     AND est-prep.dscr    EQ xqchg.charge)
       OR 
           (CAN-FIND(FIRST est-prep   WHERE est-prep.company EQ xquo.company
                                                AND est-prep.est-no  EQ xquo.est-no
                                                AND est-prep.s-num   EQ xqchg.s-num
                                                AND est-prep.b-num   EQ xqchg.b-num
                                                AND INDEX(est-prep.dscr, "plate") GT 0
                                                AND INDEX(xqchg.charge, "plate") GT 0)

           AND NOT CAN-FIND(FIRST est-prep   WHERE est-prep.company EQ xquo.company
                                                AND est-prep.est-no  EQ xquo.est-no
                                                AND est-prep.s-num   EQ xqchg.s-num
                                                AND est-prep.b-num   EQ xqchg.b-num
                                                AND INDEX(est-prep.dscr, "plate") GT 0
                                                AND INDEX(xqchg.charge, "plate") GT 0
                                                AND est-prep.cost NE xqchg.cost)
           AND NOT CAN-FIND(FIRST b-qchg 
                                    WHERE b-qchg.company EQ xqchg.company
                                      AND b-qchg.q-no    EQ xqchg.q-no
                                      AND b-qchg.s-num   EQ xqchg.s-num
                                      AND b-qchg.b-num   EQ xqchg.b-num
                                      AND INDEX(b-qchg.charge, "plate") GT 0
                                      AND INDEX(xqchg.charge, "plate") GT 0
                                      AND b-qchg.cost   NE xqchg.cost))
           )
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
    
    lv-chg-amt = xqchg.amt .

    IF xqchg.bill EQ "N" THEN DO:
      PUT xqchg.charge AT 22 SKIP.
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',TRIM(STRING(xqchg.charge)),'Col'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
    END.
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN DO:
      PUT xqchg.charge AT 22 lv-chg-amt TO 80 SKIP.
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',TRIM(STRING(xqchg.charge)),'Col'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7',TRIM(STRING(lv-chg-amt)),'Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
    END.
    ELSE
    IF xqchg.bill EQ "W" THEN DO:
      PUT xqchg.charge AT 22 SKIP.
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1','','Col').
      RUN XMLOutput (lXMLOutput,'Column_2','','Col').
      RUN XMLOutput (lXMLOutput,'Column_3',TRIM(STRING(xqchg.charge)),'Col'). /* rstark 05181205 */
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'Column_7','','Col').
      RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
    END.
    
    IF LAST-OF(xqchg.qty) THEN LEAVE.
  END.

  IF LINE-COUNTER > 55 THEN DO:
    PAGE.  
    {cec/quote/quoxprm2.i}
    v-printline = 0.
  /*END.*/
END.

if ch-inst then do:
    IF LINE-COUNTER > 55 THEN DO:
        page.  
        {cec/quote/quoxprm2.i}
    END.
      
    FIND FIRST est WHERE est.company = xquo.company
                     AND est.est-no = xquo.est-no NO-LOCK NO-ERROR.
    {custom/notespr2.i job v-inst2 6 "notes.rec_key = est.rec_key and notes.note_code ge fdept and notes.note_code le tdept " }
    DO i = 1 TO 6:
           v-dept-inst[i] = v-inst2[i].
    END.
    IF v-dept-inst[1] <> "" OR v-dept-inst[2] <> "" THEN DO:
       IF LINE-COUNTER > 55 THEN DO:
          page.  
          {cec/quote/quoxprm2.i}
       END.
       PUT "Department Notes: " SKIP.

       /* rstark 05181205 */
       XMLLineNumber = XMLLineNumber + 1.
       RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
       RUN XMLOutput (lXMLOutput,'Column_1','Department Notes:','Col').
       RUN XMLOutput (lXMLOutput,'Column_2','','Col').
       RUN XMLOutput (lXMLOutput,'Column_3','','Col'). /* rstark 05181205 */
       RUN XMLOutput (lXMLOutput,'Column_4','','Col').
       RUN XMLOutput (lXMLOutput,'Column_5','','Col').
       RUN XMLOutput (lXMLOutput,'Column_6','','Col').
       RUN XMLOutput (lXMLOutput,'Column_7','','Col').
       RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
       /* rstark 05181205 */

       DO i = 1 TO 6:
        IF v-dept-inst[i] > "" THEN DO:
          PUT UNFORMATTED "   " + v-dept-inst[i] SKIP.

          /* rstark 05181205 */
          XMLLineNumber = XMLLineNumber + 1.
          RUN XMLOutput (lXMLOutput,'QuoteLine_' + STRING(XMLLineNumber),'','Row').
          RUN XMLOutput (lXMLOutput,'Column_1',v-dept-inst[i],'Col').
          RUN XMLOutput (lXMLOutput,'Column_2','','Col').
          RUN XMLOutput (lXMLOutput,'Column_3','','Col'). /* rstark 05181205 */
          RUN XMLOutput (lXMLOutput,'Column_4','','Col').
          RUN XMLOutput (lXMLOutput,'Column_5','','Col').
          RUN XMLOutput (lXMLOutput,'Column_6','','Col').
          RUN XMLOutput (lXMLOutput,'Column_7','','Col').
          RUN XMLOutput (lXMLOutput,'/QuoteLine_' + STRING(XMLLineNumber),'','Row').
          /* rstark 05181205 */

        END.
       END.
       PUT SKIP(2).

       XMLLineNumber = XMLLineNumber + 2. /* rstark 05181205 */

    END.
  
  PUT SKIP(1).

  XMLLineNumber = XMLLineNumber + 1. /* rstark 05181205 */

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

END.
    
PUT SKIP(1).

XMLLineNumber = XMLLineNumber + 1. /* rstark 05181205 */
  
/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
