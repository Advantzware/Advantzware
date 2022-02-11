/* ---------------------------------------------- oe/rep/bolptree3.i          */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
ASSIGN
   v-total-weight = 0.

FOR EACH tt-boll,
   FIRST itemfg WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ tt-boll.i-no NO-LOCK
   BREAK BY tt-boll.po-no /* po-no first like premier */
         BY tt-boll.i-no
         BY tt-boll.ord-no
         BY tt-boll.line
         BY tt-boll.cases DESC:

   IF ll-consol-bolls THEN DO:       
      {oe/rep/bolptree23.i}
   END.
   ELSE DO:      

      FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                           AND oe-ordl.ord-no  EQ tt-boll.ord-no
                           AND oe-ordl.i-no    EQ tt-boll.i-no
                           AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

      FIND first oe-ord WHERE oe-ord.company EQ cocode
                          AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.

      IF v-printline + 4 >= 46 THEN DO:
         ASSIGN
            v-pg-num = v-pg-num + 1
            v-printline = 0.
         PAGE {1}.
         {oe/rep/bolptreebc2.i}
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
                             ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2)))
               v-ord-po-no = STRING(tt-boll.ord-no)
               v-item-part-no = oe-ordl.i-no.
         ELSE
            IF i EQ 2 THEN
               ASSIGN
                  v-part-dscr = oe-ordl.part-no
                  v-ord-po-no = IF lLot THEN tt-boll.lot-no ELSE STRING(tt-boll.po-no)
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
  
      ASSIGN
         v-lines = v-lines + 1
         i = 0
         v-case-tot = 0.

      FOR EACH w2 BREAK BY w2.cases:
         
         ASSIGN
            i = i + 1
            v-part-dscr = ""
            v-job-po    = ""
            v-ord-po-no = ""
            v-item-part-no = "".

         /* on first line display item#, po#, cust part # */
         IF i EQ 1 THEN
            ASSIGN
               v-part-dscr = oe-ordl.i-name
               v-ord-po-no = IF lLot THEN tt-boll.lot-no ELSE STRING(tt-boll.po-no)
               v-item-part-no = oe-ordl.i-no.
         ELSE
         /* on 2nd line display order#, job#, prod descr */
            IF i EQ 2 THEN
               ASSIGN
                  v-part-dscr = oe-ordl.part-no
                  v-ord-po-no = IF oe-ordl.job-no EQ "" THEN "" 
                                ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2)))
                  v-item-part-no = STRING(tt-boll.ord-no).

         ELSE 
            IF i EQ 3 THEN 
               v-part-dscr = oe-ordl.part-dscr1.
            ELSE 
               IF i EQ 4 THEN 
                  v-part-dscr = oe-ordl.part-dscr2.

         ASSIGN
            v-case-tot = v-case-tot + (w2.cases * w2.cas-cnt)
            v-printline = v-printline + 1.

         DISPLAY 
            v-item-part-no
            v-ord-po-no
            v-part-dscr
            w2.cases
            w2.cas-cnt
            v-case-tot  WHEN LAST(w2.cases) 
            w2.partl
         WITH FRAME bol-mid2.
         DOWN WITH FRAME bol-mid2. 

         IF v-printline >= 46 THEN DO:
            ASSIGN
            v-pg-num = v-pg-num + 1
            v-printline = 0.
            PAGE {1}.
            {oe/rep/bolptreebc2.i}
         END.
        
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
                                   ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2)))
                     v-item-part-no = STRING(tt-boll.ord-no).
               ELSE 
                  IF i EQ 3 THEN 
                     v-part-dscr = oe-ordl.part-dscr1.
                  ELSE 
                     IF i EQ 4 THEN 
                        v-part-dscr = oe-ordl.part-dscr2.

            IF v-part-dscr NE "" OR v-ord-po-no NE "" OR i LE 2 THEN DO:
               IF v-printline >= 46 THEN DO:
                  ASSIGN
                  v-pg-num = v-pg-num + 1
                  v-printline = 0.
                  PAGE {1}.
                  {oe/rep/bolptreebc2.i}
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

            IF v-printline >= 46 THEN DO:
               ASSIGN
                  v-pg-num = v-pg-num + 1
                  v-printline = 0.
               PAGE {1}.
               {oe/rep/bolptreebc2.i}
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
      IF itemfg.alloc EQ YES THEN
      DO:
         RUN fg/fullset.p (ROWID(itemfg)).

         FOR EACH tt-fg-set,
             FIRST b-itemfg WHERE
                   b-itemfg.company EQ itemfg.company AND
                   b-itemfg.i-no    EQ tt-fg-set.part-no
                   NO-LOCK:
        
             IF v-printline >= 46 THEN DO:
                ASSIGN
                v-pg-num = v-pg-num + 1
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolptreebc2.i}
             END.

             DISPLAY {1}
                TRIM(STRING(oe-ordl.qty * tt-fg-set.part-qty-dec,">>>,>>>,>>>")) 
                                                        @ v-item-part-no
                b-itemfg.part-no                        @ v-part-dscr
                tt-boll.qty * tt-fg-set.part-qty-dec                @ v-case-tot
             WITH FRAME bol-mid2.
             DOWN {1} WITH FRAME bol-mid2.
    
             v-printline = v-printline + 1.
             DISPLAY {1}
               tt-fg-set.part-no                       @ v-item-part-no
               v-job-po                                @ v-ord-po-no
               b-itemfg.i-name                         @ v-part-dscr
             WITH FRAME bol-mid2.
             DOWN {1} WITH FRAME bol-mid2.
    
             put {1} skip(1).

             v-printline = v-printline + 2.
         END.
      END.

      PUT "<|3><C1><FROM><C80><LINE>".
      v-printline = v-printline + 1.
      
   END. /* else */

END. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
