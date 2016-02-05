/* ---------------------------------------------- oe/rep/bolkdwl3.i           */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */
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
      {oe/rep/bolkdw23.i}
   END.
   ELSE DO:
      
      FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                           AND oe-ordl.ord-no  EQ tt-boll.ord-no
                           AND oe-ordl.i-no    EQ tt-boll.i-no
                           AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

      FIND first oe-ord WHERE oe-ord.company EQ cocode
                          AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.

      IF v-printline >= 40 THEN DO:
         v-printline = 0.
         PAGE {1}.
         {oe/rep/bolkdwl2.i}
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
         ELSE
         /* on 2nd line display order#, job#, prod descr */
            IF i EQ 2 THEN
               ASSIGN
                  v-part-dscr = oe-ordl.part-no
                  v-ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                                ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99"))
                  v-item-part-no = STRING(tt-boll.ord-no).

            ELSE 
               IF i EQ 3 THEN 
                  v-part-dscr = oe-ordl.part-dscr1.
               ELSE 
                  IF i EQ 4 THEN 
                     v-part-dscr = oe-ordl.part-dscr2.

         v-case-tot = v-case-tot + (w2.cases * w2.cas-cnt).

         DISPLAY 
            v-item-part-no
            v-ord-po-no
            v-part-dscr
            w2.cases
            w2.cas-cnt
            v-case-tot  WHEN LAST(w2.cases)
         WITH FRAME bol-mid2.
         DOWN  WITH FRAME bol-mid2. 
         ASSIGN
         v-grand-total-cases  = v-grand-total-cases + w2.cases
         v-printline = v-printline + 1.
         
         IF v-printline >= 40 THEN DO:
            v-printline = 0.
            PAGE {1}.
            {oe/rep/bolkdwl2.i}
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
            ELSE
            /* on 2nd line display order#, job#, prod descr */
               IF i EQ 2 THEN
                  ASSIGN
                     v-part-dscr = oe-ordl.part-no
                     v-ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                                   ELSE (TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99"))
                     v-item-part-no = STRING(tt-boll.ord-no).

            ELSE 
               IF i EQ 3 THEN 
                  v-part-dscr = oe-ordl.part-dscr1.
            ELSE 
               IF i EQ 4 THEN 
                  v-part-dscr = oe-ordl.part-dscr2.
   
            IF v-part-dscr NE "" OR v-ord-po-no NE "" OR i LE 2 THEN DO:
               IF v-printline >= 40 THEN DO:
                  v-printline = 0.
                  PAGE {1}.
                  {oe/rep/bolkdwl2.i}
               END.
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
  
      IF v-print-components AND itemfg.alloc NE YES THEN
         FOR EACH fg-set WHERE fg-set.company EQ cocode
                           AND fg-set.set-no  EQ tt-boll.i-no NO-LOCK,  
            FIRST b-itemfg WHERE b-itemfg.company EQ cocode
                             AND b-itemfg.i-no    EQ fg-set.part-no NO-LOCK
            BREAK BY fg-set.set-no:
      
            {sys/inc/part-qty.i v-part-qty fg-set}

            IF v-printline >= 40 THEN DO:
               v-printline = 0.
               PAGE {1}.
               {oe/rep/bolkdwl2.i}
            END.

            DISPLAY {1}
               TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                       @ v-item-part-no
               b-itemfg.part-no                        @ v-part-dscr
               tt-boll.qty * v-part-qty                @ v-case-tot
            WITH FRAME bol-mid2.
            DOWN {1} WITH FRAME bol-mid2.
    
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

      IF LAST-OF(tt-boll.po-no) THEN DO:
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

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
