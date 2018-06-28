/* bolprem23.i*/

IF FIRST-OF(tt-boll.LINE) THEN DO:
  FOR EACH w2.
      DELETE w2.
  END.
  ASSIGN
     i = 0
     v-tot-case-qty = 0
     v-tot-cases = 0
     v-total-weight = 0.

  FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
                       AND bf-ttboll.po-no = tt-boll.po-no
                       AND bf-ttboll.ord-no = tt-boll.ord-no
                       AND bf-ttboll.LINE = tt-boll.LINE
                BREAK BY bf-ttboll.cases DESC.
      find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.

      find first oe-ord where oe-ord.company eq cocode
         and oe-ord.ord-no  eq tt-boll.ord-no no-lock no-error.
       IF lGenerateCXML  THEN DO:
            IF AVAIL oe-ordl AND oe-ordl.spare-char-2 NE '' THEN DO:
                ASSIGN 
                    dOrigQty = oe-ordl.spare-dec-1
                    cOrigUom = oe-ordl.spare-char-2
                    .
                IF cOrigUom EQ 'CS' 
                    AND dOrigQty NE tt-boll.qty 
                    AND oe-ordl.cas-cnt NE 0 THEN DO:
                    dOrigQty = tt-boll.qty / oe-ordl.cas-cnt.
                END.
                ELSE dOrigQty = tt-boll.qty.
            END.
            IF dOrigQty EQ 0 THEN dOrigQty = tt-boll.qty.
            IF cOrigUom EQ "" THEN cOrigUom = "EA".
            ciXMLOutput = ciXMLOutput + 1.
            RUN cXMLOutput (clXMLOutput,'ShipNoticeItem lineNumber="' + STRING(tt-boll.LINE) + '" quantity="' + STRING(dOrigQty) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',cOrigUom,'Col').
            RUN cXMLOutput (clXMLOutput,'/ShipNoticeItem','','Row').
       /* rstark 05291402 */
        END.
      ASSIGN
         v-tot-case-qty = v-tot-case-qty + bf-ttboll.qty
         v-total-weight = v-total-weight + bf-ttboll.weight
         i = i + 1.
      find first w2 where w2.cas-cnt eq bf-ttboll.qty-case no-error.
      if not avail w2 then create w2.
      ASSIGN 
             w2.ord-po-no = ""
             w2.job-po = ""
             w2.i-no = ""
             w2.job-po = ""
             w2.cas-cnt = bf-ttboll.qty-case
             w2.cases   = w2.cases + bf-ttboll.cases
             w2.rec-id = RECID(bf-ttboll).

      IF i = 1 THEN
         ASSIGN
            w2.i-no = oe-ordl.i-no
            w2.ord-po-no = tt-boll.po-no
            w2.dscr = oe-ordl.i-name
            w2.qty = oe-ordl.qty.
      ELSE IF i = 2 THEN 
         ASSIGN
            w2.i-no = string(oe-ordl.ord-no)
            w2.ord-po-no = if oe-ordl.job-no eq "" then "" else
                           (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
            w2.dscr = oe-ordl.part-no.                          
            
      else if i eq 3 then
         ASSIGN w2.dscr = oe-ordl.part-dscr1.
      ELSE if i eq 4 then
         ASSIGN w2.dscr = oe-ordl.part-dscr2.

  END.
  IF i < 4 THEN DO i = i TO 4:
     CREATE w2.
  END.
  ASSIGN
     i = 0
     comp-ctr = 0.

  FOR EACH w2  BREAK BY w2.cases DESC:
    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.  
    i = i + 1.
    IF w2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       w2.i-no = "".
       IF i = 2 THEN 
          ASSIGN 
             w2.i-no = string(oe-ordl.ord-no)
             w2.dscr = oe-ordl.part-no
             w2.ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                            ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")).
       ELSE if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
       ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
    END.
    IF w2.cases NE 0 THEN comp-ctr = comp-ctr + 1.
/*     IF w2.cas-cnt LE 0 AND comp-ctr = 0 THEN           */
/*        ASSIGN comp-ctr = (IF i > 1 THEN i - 1 ELSE 1). */
    IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND w2.cas-cnt = 0 THEN DELETE w2.
  END.

  ASSIGN i = 0 v-case-tot = 0.
  IF comp-ctr = 0 THEN ASSIGN comp-ctr = 4.
  FOR EACH w2 BREAK BY w2.cases DESC:
    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.    
    i = i + 1.

    IF w2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       w2.i-no = "".
       IF i = 2 THEN 
          ASSIGN 
            w2.i-no = string(oe-ordl.ord-no)
            w2.dscr = oe-ordl.part-no
            w2.ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                           ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")).
       ELSE if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
       ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
    END.
    IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND w2.cas-cnt = 0 AND NOT last(w2.cases) THEN .
    ELSE DO:    
       ASSIGN v-case-tot =  v-case-tot +  (w2.cases * w2.cas-cnt).
       
       DISPLAY 
            w2.i-no
            w2.ord-po-no
            w2.job-po
            w2.dscr
            w2.cases    WHEN w2.cases > 0   
            w2.cas-cnt  WHEN w2.cases > 0
            v-case-tot  WHEN /*last(w2.cases)*/ i = comp-ctr
           with frame bol-mid.
       DOWN WITH FRAME bol-mid.  
       v-printline = v-printline + 1.

       ASSIGN v-summ-case-tot = v-summ-case-tot + (w2.cases * w2.cas-cnt).

       /* rstark 05181205 */
       XMLLineNumber = XMLLineNumber + 1.
       RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
       RUN XMLOutput (lXMLOutput,'Column_1',w2.i-no,'Col').
       RUN XMLOutput (lXMLOutput,'Column_2',w2.ord-po-no,'Col').
       RUN XMLOutput (lXMLOutput,'Column_3',w2.dscr,'Col').
       IF w2.cases GT 0 THEN DO:
         RUN XMLOutput (lXMLOutput,'Column_4',w2.cases,'Col').
         RUN XMLOutput (lXMLOutput,'Column_5',w2.cas-cnt,'Col').
       END. ELSE DO:
         RUN XMLOutput (lXMLOutput,'Column_4','','Col').
         RUN XMLOutput (lXMLOutput,'Column_5','','Col').
       END.
       IF i EQ comp-ctr THEN
       RUN XMLOutput (lXMLOutput,'Column_6',v-case-tot,'Col').
       ELSE
       RUN XMLOutput (lXMLOutput,'Column_6','','Col').
       RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
       /* rstark 05181205 */

    END.
    
   
    IF j >= 30 THEN do:
        IF v-printline >= 62 THEN DO: 
           v-printline = 0.
           j = j - 30.
           PAGE {1}.
           {oe/rep/bolprem2.i}
        END.
    END.
    ELSE do: 
         IF v-printline >= 50 THEN DO: 
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolprem2.i}
      END.
    END.

    ASSIGN
      v-tot-cases = v-tot-cases + w2.cases.
          /*delete w2. */
  end. /* each w2 */
  v-grand-total-cases = v-grand-total-cases + v-tot-cases.
  PUT {1}
         SKIP(1)
         "P.O.#:" AT 10
          tt-boll.po-no AT 17
          "Total" AT 34
         v-summ-case-tot    FORMAT "->>,>>>,>>z"      AT 85 
        SKIP.
     
      v-printline = v-printline + 3.

  /* rstark 05181205 */
  XMLLineNumber = XMLLineNumber + 2.
  RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
  RUN XMLOutput (lXMLOutput,'Column_1','P.O.#:','Col').
  RUN XMLOutput (lXMLOutput,'Column_2',tt-boll.po-no,'Col').
  RUN XMLOutput (lXMLOutput,'Column_3','Total','Col').
  RUN XMLOutput (lXMLOutput,'Column_4','','Col').
  RUN XMLOutput (lXMLOutput,'Column_5','','Col').
  RUN XMLOutput (lXMLOutput,'Column_6',v-summ-case-tot,'Col').
  RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
  XMLLineNumber = XMLLineNumber + 1.
  /* rstark 05181205 */
      
  put {1} skip(1).

  ASSIGN
     v-printline = v-printline + 1
     tt-boll.printed = yes.
  
  if v-print-components AND itemfg.alloc NE YES THEN DO:

    for each fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq tt-boll.i-no
        no-lock,
        
        first b-itemfg
        where b-itemfg.company eq cocode
          and b-itemfg.i-no    eq fg-set.part-no
        no-lock
        
        break by fg-set.set-no:
        
      {sys/inc/part-qty.i v-part-qty fg-set}
  
    IF j >= 30 THEN do:
        IF v-printline >= 62 THEN DO: 
           v-printline = 0.
           j = j - 30.
           PAGE {1}.
           {oe/rep/bolprem2.i}
        END.
    END.
    ELSE do:  
         IF v-printline >= 50 THEN DO: 
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolprem2.i}
      END.
    END.
  
     /* rstark 05181205 */
     XMLLineNumber = XMLLineNumber + 1.
     RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
     RUN XMLOutput (lXMLOutput,'Column_1',STRING(oe-ordl.qty * v-part-qty),'Col').
     RUN XMLOutput (lXMLOutput,'Column_2','','Col').
     RUN XMLOutput (lXMLOutput,'Column_3',b-itemfg.part-no,'Col').
     RUN XMLOutput (lXMLOutput,'Column_4','','Col').
     RUN XMLOutput (lXMLOutput,'Column_5','','Col').
     RUN XMLOutput (lXMLOutput,'Column_6',STRING(tt-boll.qty * v-part-qty),'Col').
     RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
     /* rstark 05181205 */
  
      display {1}
              trim(string(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                      @ w2.i-no
              b-itemfg.part-no                        @ w2.dscr
              tt-boll.qty * v-part-qty                @ /*tt-boll.qty */ v-case-tot
          with frame bol-mid.
      down {1} with frame bol-mid.
      v-printline = v-printline + 1.
  
      /* rstark 05181205 */
      XMLLineNumber = XMLLineNumber + 1.
      RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
      RUN XMLOutput (lXMLOutput,'Column_1',fg-set.part-no,'Col').
      RUN XMLOutput (lXMLOutput,'Column_2',v-job-po,'Col').
      RUN XMLOutput (lXMLOutput,'Column_3',b-itemfg.i-name,'Col').
      RUN XMLOutput (lXMLOutput,'Column_4','','Col').
      RUN XMLOutput (lXMLOutput,'Column_5','','Col').
      RUN XMLOutput (lXMLOutput,'Column_6','','Col').
      RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
      /* rstark 05181205 */
      
      display {1}
              fg-set.part-no                          @ w2.i-no
              v-job-po                                @ /*w2.job-po*/ w2.ord-po-no
              b-itemfg.i-name                         @ w2.dscr
          with frame bol-mid.
      down {1} with frame bol-mid.
      
      put {1} skip(1).
      v-printline = v-printline + 2.
    END.

  END.
END. /* first-of(tt-boll.line) */

