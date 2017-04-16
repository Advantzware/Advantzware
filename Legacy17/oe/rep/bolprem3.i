/* bolprem3.i */

ASSIGN
   v-tot-cases = 0
   v-total-weight = 0
   v-grand-total-cases = 0
   v-summ-case-tot = 0.

FOR EACH tt-boll,
   FIRST itemfg WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ tt-boll.i-no NO-LOCK
   BREAK BY tt-boll.po-no /* po-no first like premier */
         BY tt-boll.i-no
         BY tt-boll.ord-no
         BY tt-boll.line
         BY tt-boll.cases DESC:

   IF FIRST-OF(tt-boll.po-no) THEN
      ASSIGN v-tot-cases = 0
             v-summ-case-tot = 0.
   
   

   IF ll-consol-bolls THEN DO:
      {oe/rep/bolprem23.i}
   END.
   ELSE DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                           AND oe-ordl.ord-no  EQ tt-boll.ord-no
                           AND oe-ordl.i-no    EQ tt-boll.i-no
                           AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

      FIND first oe-ord WHERE oe-ord.company EQ cocode
                          AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.
        
      /* rstark 05291402 */
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
      IF v-printline >= 40 THEN DO:
         v-printline = 0.
         PAGE {1}.
         {oe/rep/bolprem2.i}
      END.
      IF tt-boll.qty-case NE 0 AND tt-boll.cases NE 0 THEN DO:
         FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.qty-case NO-ERROR.
         IF NOT AVAIL w2 THEN 
            CREATE w2.
         ASSIGN
            w2.cas-cnt = tt-boll.qty-case
            w2.cases   = w2.cases + tt-boll.cases.
      END.

      IF tt-boll.partial NE 0 THEN DO:
         FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.partial NO-ERROR.
         IF NOT AVAIL w2 THEN 
            CREATE w2.
         ASSIGN
            w2.cas-cnt = tt-boll.partial
            w2.cases   = w2.cases + 1.
      END.

      v-lines = 0.
      FOR EACH w2 BREAK BY w2.cases:
         v-lines = v-lines + 1.
      END. 
  
      DO i = v-lines + 1 TO 4:
         ASSIGN
            v-part-dscr = ""
            v-job-po    = ""
            v-ord-po-no = ""
            v-item-part-no = "".

         IF i EQ 1 THEN
            ASSIGN
               v-part-dscr = oe-ordl.i-name
               v-job-po    = IF oe-ordl.job-no EQ "" THEN "" 
                             ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99"))
               v-ord-po-no = STRING(tt-boll.ord-no)
               v-item-part-no = oe-ordl.i-no.
         ELSE
            IF i EQ 2 THEN
               ASSIGN
                  v-part-dscr = oe-ordl.part-no
                  v-ord-po-no = STRING(tt-boll.po-no)
                  v-item-part-no = oe-ordl.part-no.
    
         ELSE
            IF i EQ 3 THEN 
               v-part-dscr = oe-ordl.part-dscr1.
         ELSE
            IF i EQ 4 THEN 
               v-part-dscr = oe-ordl.part-dscr2.
    
         IF v-part-dscr NE "" OR v-job-po NE "" OR i LE 2 THEN 
            v-lines = v-lines + 1.
      END.
  
      v-lines = v-lines + 1.

      ASSIGN i = 0 v-case-tot = 0.
      FOR EACH w2 BREAK BY w2.cases:
         i = i + 1.

         ASSIGN
            v-part-dscr = ""
            v-job-po    = ""
            v-ord-po-no = ""
            v-item-part-no = "".

         /* on first line display item#, po#, cust part # */
         IF i EQ 1 THEN
         ASSIGN
            v-part-dscr = oe-ordl.i-name
            v-ord-po-no = STRING(tt-boll.po-no)
            v-item-part-no = oe-ordl.i-no.
         ELSE IF i EQ 2 THEN
         /* on 2nd line display order#, job#, prod descr */
         ASSIGN
            v-part-dscr = oe-ordl.part-no
            v-ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                          ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99"))
            v-item-part-no = STRING(tt-boll.ord-no).
         ELSE IF i EQ 3 THEN v-part-dscr = oe-ordl.part-dscr1.
         ELSE IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr2.

         v-case-tot = /* v-case-tot + */ (w2.cases * w2.cas-cnt).

         /* rstark 05181205 */
         XMLLineNumber = XMLLineNumber + 1.
         RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
         RUN XMLOutput (lXMLOutput,'Column_1',v-item-part-no,'Col').
         RUN XMLOutput (lXMLOutput,'Column_2',v-ord-po-no,'Col').
         RUN XMLOutput (lXMLOutput,'Column_3',v-part-dscr,'Col').
         RUN XMLOutput (lXMLOutput,'Column_4',w2.cases,'Col').
         RUN XMLOutput (lXMLOutput,'Column_5',w2.cas-cnt,'Col').
         IF LAST(w2.cases) THEN
         RUN XMLOutput (lXMLOutput,'Column_6',v-case-tot,'Col').
         ELSE
         RUN XMLOutput (lXMLOutput,'Column_6','','Col').
         RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
         /* rstark 05181205 */
         
         DISPLAY 
            v-item-part-no
            v-ord-po-no
            v-part-dscr
            w2.cases
            w2.cas-cnt
            v-case-tot /* WHEN LAST(w2.cases) */
         WITH FRAME bol-mid2.
         DOWN  WITH FRAME bol-mid2. 

         ASSIGN
         v-grand-total-cases  = v-grand-total-cases + w2.cases
         v-printline = v-printline + 1.
         
         IF v-printline >= 40 THEN DO:
            v-printline = 0.
            PAGE {1}.
            {oe/rep/bolprem2.i}
         END.
         v-tot-cases = v-tot-cases + (w2.cases * w2.cas-cnt).
         DELETE w2.    
      END. /* each w2 */

      IF i < 4 THEN
         DO i = i + 1 TO 4:
            CLEAR FRAME bol-mid2 NO-PAUSE.
   
            ASSIGN
               v-part-dscr = ""
               v-job-po    = ""
               v-ord-po-no = ""
               v-item-part-no = "".

            /* on first line display item#, po#, cust part # */
            IF i EQ 1 THEN
            ASSIGN
               v-part-dscr = oe-ordl.i-name
               v-ord-po-no = STRING(tt-boll.po-no)
               v-item-part-no = oe-ordl.i-no.
            /* on 2nd line display order#, job#, prod descr */
            ELSE IF i EQ 2 THEN
            ASSIGN
               v-part-dscr = oe-ordl.part-no
               v-ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                             ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99"))
               v-item-part-no = STRING(tt-boll.ord-no).
            ELSE IF i EQ 3 THEN v-part-dscr = oe-ordl.part-dscr1.
            ELSE IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr2.

            IF v-part-dscr NE "" OR v-ord-po-no NE "" OR i LE 2 THEN DO:
              IF v-printline >= 40 THEN DO:
                 v-printline = 0.
                 PAGE {1}.
                 {oe/rep/bolprem2.i}
              END.

              /* rstark 05181205 */
              XMLLineNumber = XMLLineNumber + 1.
              RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
              RUN XMLOutput (lXMLOutput,'Column_1',v-item-part-no,'Col').
              RUN XMLOutput (lXMLOutput,'Column_2',v-ord-po-no,'Col').
              RUN XMLOutput (lXMLOutput,'Column_3',v-part-dscr,'Col').
              RUN XMLOutput (lXMLOutput,'Column_4','','Col').
              RUN XMLOutput (lXMLOutput,'Column_5','','Col').
              RUN XMLOutput (lXMLOutput,'Column_6','','Col').
              RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
              /* rstark 05181205 */
               
              DISPLAY {1}
                v-item-part-no
                v-ord-po-no
                v-part-dscr
              WITH FRAME bol-mid2.
              DOWN {1} WITH FRAME bol-mid2. 
              v-printline = v-printline + 1.
               
            END.
         END.

      PUT {1} SKIP(1).

      ASSIGN
         v-printline = v-printline + 1
         tt-boll.printed = yes.
  
      IF v-print-components AND itemfg.alloc NE YES THEN DO:

         FOR EACH fg-set WHERE fg-set.company EQ cocode
                           AND fg-set.set-no  EQ tt-boll.i-no NO-LOCK,  
            FIRST b-itemfg WHERE b-itemfg.company EQ cocode
                             AND b-itemfg.i-no    EQ fg-set.part-no NO-LOCK
            BREAK BY fg-set.set-no:
      
            {sys/inc/part-qty.i v-part-qty fg-set}

            IF v-printline >= 40 THEN DO:
               v-printline = 0.
               PAGE {1}.
               {oe/rep/bolprem2.i}
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
            
            DISPLAY {1}
               TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                       @ v-item-part-no
               b-itemfg.part-no                        @ v-part-dscr
               tt-boll.qty * v-part-qty                @ v-case-tot
            WITH FRAME bol-mid2.
            DOWN {1} WITH FRAME bol-mid2.
    
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
            
            v-printline = v-printline + 1.
            DISPLAY {1}
               fg-set.part-no                          @ v-item-part-no
               v-job-po                                @ v-ord-po-no
               b-itemfg.i-name                         @ v-part-dscr
            WITH FRAME bol-mid2.
            DOWN {1} WITH FRAME bol-mid2.
    
            put {1} skip(1).
            v-printline = v-printline + 2.
            
         END.

      END.

      IF LAST-OF(tt-boll.po-no) THEN DO:

        /* rstark 05181205 */
        XMLLineNumber = XMLLineNumber + 2.
        RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
        RUN XMLOutput (lXMLOutput,'Column_1','P.O.#:','Col').
        RUN XMLOutput (lXMLOutput,'Column_2',tt-boll.po-no,'Col').
        RUN XMLOutput (lXMLOutput,'Column_3','Total','Col').
        RUN XMLOutput (lXMLOutput,'Column_4','','Col').
        RUN XMLOutput (lXMLOutput,'Column_5','','Col').
        RUN XMLOutput (lXMLOutput,'Column_6',v-tot-cases,'Col').
        RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
        XMLLineNumber = XMLLineNumber + 1.
        /* rstark 05181205 */
         
        PUT {1}  
             SKIP
             "P.O.#:" AT 10
             tt-boll.po-no AT 17
             "Total" AT 34
             v-tot-cases             FORMAT "->>,>>>,>>z"       AT 85
             SKIP(1).
         v-printline = v-printline + 2.
         
      END.
      
     END. /* else */

END. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.
