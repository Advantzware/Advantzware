/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.    */
/* bolHarwell3.i */
PUT "<FCourier New><P10>" SKIP(1).

ASSIGN
    v-tot-cases         = 0
    v-total-weight      = 0
    v-grand-total-cases = 0
    v-summ-case-tot     = 0.

FOR EACH tt-boll,
    FIRST itemfg WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ tt-boll.i-no NO-LOCK
    BREAK BY tt-boll.po-no /* po-no first like premier */
    BY tt-boll.i-no
    BY tt-boll.ord-no
    BY tt-boll.line
    BY tt-boll.cases DESC:

    IF FIRST-OF(tt-boll.po-no) THEN
        ASSIGN v-tot-cases     = 0
            v-summ-case-tot = 0
            v-tot-pkgs      = 0.
   
   

/*    IF ll-consol-bolls THEN    */
/*    DO:                        */
/*        {oe/rep/bolHarwell23.i}*/
/*    END.                       */
/*    ELSE                       */
    DO:
        FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ tt-boll.ord-no
            AND oe-ordl.i-no    EQ tt-boll.i-no
            AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

        FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.
        
        IF NOT LAST(tt-boll.cases) THEN 
        DO:
            IF v-printline >= 60 THEN 
            DO: 
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolHarwell2.i}
                PUT "<FCourier New><P10>" SKIP(1).
            END.
        END.
        ELSE 
        DO:  
            IF v-printline >= 48 THEN 
            DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolHarwell2.i}
                PUT "<FCourier New><P10>" SKIP(1).
            END.
        END.

        IF tt-boll.qty-case NE 0 
        AND tt-boll.cases NE 0 THEN 
        DO:
            FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.qty-case NO-ERROR.
            IF NOT AVAIL w2 THEN 
                CREATE w2.
            ASSIGN
                w2.iBolLine = tt-boll.line
                w2.cas-cnt = tt-boll.qty-case
                w2.cases   = w2.cases + tt-boll.cases.
        END.

        IF tt-boll.partial NE 0 THEN 
        DO:
            FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.partial NO-ERROR.
            IF NOT AVAIL w2 THEN 
                CREATE w2.
            ASSIGN
                w2.iBolLine = tt-boll.line
                w2.cas-cnt = tt-boll.partial
                w2.cases   = w2.cases + 1.
        END.

        ASSIGN
            v-tot-pkgs = 0 
            v-lines = 0.
        FOR EACH w2 BREAK BY w2.cases:
            ASSIGN 
                v-lines    = v-lines + 1
                v-tot-pkgs = v-tot-pkgs + w2.cases.
        END. 
  
        DO i = v-lines + 1 TO 4:
            ASSIGN
                v-part-dscr    = ""
                v-job-po       = ""
                v-ord-po-no    = ""
                v-item-part-no = "".

            IF i EQ 1 THEN ASSIGN
                v-part-dscr    = oe-ordl.i-name
                v-ord-po-no    = STRING(tt-boll.ord-no)
                v-item-part-no = oe-ordl.i-no.
            ELSE IF i EQ 2 THEN ASSIGN
                v-part-dscr    = oe-ordl.i-name
                v-ord-po-no    = ""
                v-item-part-no = "".
            ELSE IF i EQ 3 THEN ASSIGN  
                v-part-dscr = oe-ordl.part-no.
            ELSE IF i EQ 4 THEN ASSIGN  
                v-part-dscr = oe-ordl.i-no.
    
            IF v-part-dscr NE "" OR i LE 2 THEN 
                v-lines = v-lines + 1.
        END.
  
        ASSIGN 
            i          = 0 
            v-case-tot = 0.
      
        FOR EACH w2 BREAK BY w2.iBolLine BY w2.cas-cnt:
            ASSIGN
                i = i + 1
                v-part-dscr    = ""
                v-job-po       = ""
                v-ord-po-no    = ""
                v-item-part-no = "".

            /* on first line display item#, po#, cust part # */
            IF i EQ 1 THEN ASSIGN
                v-part-dscr    = TRIM(STRING(oe-ordl.ord-no))
                v-ord-po-no    = STRING(tt-boll.po-no)
                v-item-part-no = oe-ordl.i-no.
            ELSE IF i EQ 2 THEN ASSIGN
                v-part-dscr    = oe-ordl.i-name
                v-ord-po-no    = ""
                v-item-part-no = "".
            ELSE IF i EQ 3 THEN ASSIGN 
                v-part-dscr = oe-ordl.part-no.
            ELSE IF i EQ 4 THEN ASSIGN 
                v-part-dscr = oe-ordl.i-no.

            v-case-tot = /* v-case-tot + */ (w2.cases * w2.cas-cnt).

  
            DISPLAY  
                v-tot-pkgs      WHEN i EQ 1
                oe-ordl.qty     WHEN i EQ 1
                v-part-dscr   
                tt-boll.po-no   WHEN i EQ 1 
                w2.cases        WHEN AVAIL w2
                w2.cas-cnt      WHEN AVAIL w2
                tt-boll.qty     WHEN LAST-OF(w2.iBolLine)
                tt-boll.p-c     WHEN LAST-OF(w2.iBolLine) 
                tt-boll.weight  WHEN LAST-OF(w2.iBolLine)
                WITH FRAME bol-mid1.
            DOWN WITH FRAME bol-mid1.

            ASSIGN
                v-grand-total-cases = v-grand-total-cases + w2.cases
                v-printline         = v-printline + 1.
         
            IF NOT LAST(tt-boll.cases) THEN 
            DO:
                IF v-printline >= 62 THEN 
                DO: 
                    v-printline = 0.
                    j = j - 30.
                    PAGE {1}.
                    {oe/rep/bolHarwell2.i}
                    PUT "<FCourier New><P10>" SKIP(1).
                END.
            END.
            ELSE 
            DO:                           
                IF v-printline >= 45 THEN 
                DO: 
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bolHarwell2.i}
                    PUT "<FCourier New><P10>" SKIP(1).
                END.
            END. 
            v-tot-cases = v-tot-cases + (w2.cases * w2.cas-cnt).
            DELETE w2.    
        END. /* each w2 */
        
        IF i < 4 THEN DO i = i + 1 TO 4:
            CLEAR FRAME bol-mid2 NO-PAUSE.
   
            ASSIGN
                v-part-dscr = ""
                v-job-po    = ""
                v-ord-po-no = ""
                v-item-part-no = "".

            /* on first line display item#, po#, cust part # */
            IF i EQ 1 THEN ASSIGN
                v-part-dscr    = TRIM(STRING(oe-ordl.ord-no))
                v-ord-po-no    = STRING(tt-boll.po-no)
                v-item-part-no = oe-ordl.i-no.
            ELSE IF i EQ 2 THEN ASSIGN
                v-part-dscr    = oe-ordl.i-name
                v-ord-po-no    = ""
                v-item-part-no = "".
            ELSE IF i EQ 3 THEN ASSIGN 
                v-part-dscr = oe-ordl.part-no.
            ELSE IF i EQ 4 THEN ASSIGN 
                v-part-dscr = oe-ordl.i-no.

            IF v-part-dscr NE "" OR i LE 2 THEN DO:
                DISPLAY 
                    v-part-dscr
                    WITH FRAME bol-mid1.
                DOWN WITH FRAME bol-mid1. 
                v-printline = v-printline + 1.
            END.
        END.
        
        PUT SKIP.

        ASSIGN
            v-printline     = v-printline + 1
            tt-boll.printed = YES.

        IF NOT LAST(tt-boll.cases) THEN 
        DO:
            IF v-printline >= 60 THEN 
            DO: 
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolHarwell2.i}
                PUT "<FCourier New><P10>" SKIP(1).
            END.
        END.
        ELSE 
        DO:   
            IF v-printline >= 49 THEN 
            DO: 
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolHarwell2.i}
                PUT "<FCourier New><P10>" SKIP(1).
            END.
        END.
  
        IF v-print-components AND itemfg.alloc NE YES THEN 
        DO:

            FOR EACH fg-set WHERE fg-set.company EQ cocode
                AND fg-set.set-no  EQ tt-boll.i-no NO-LOCK,  
                FIRST b-itemfg WHERE b-itemfg.company EQ cocode
                AND b-itemfg.i-no    EQ fg-set.part-no NO-LOCK
                BREAK BY fg-set.set-no:
      
                {sys/inc/part-qty.i v-part-qty fg-set}

                IF NOT LAST(tt-boll.cases) THEN 
                DO:
                    IF v-printline >= 60 THEN 
                    DO: 
                        v-printline = 0.
                        PAGE {1}.
                        {oe/rep/bolHarwell2.i}
                    END.
                END.
                ELSE 
                DO:                           
                    IF v-printline >= 48 THEN 
                    DO: 
                        v-printline = 0.
                        PAGE {1}.
                        {oe/rep/bolHarwell2.i}
                    END.
                END.

            /*            DISPLAY {1} SPACE(1)                                       */
            /*               TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>"))    */
            /*                                                       @ v-item-part-no*/
            /*               b-itemfg.part-no                        @ v-part-dscr   */
            /*               tt-boll.qty * v-part-qty                @ v-case-tot    */
            /*            WITH FRAME bol-mid2.                                       */
            /*            DOWN {1} WITH FRAME bol-mid2.                              */
    
            /*            v-printline = v-printline + 1.                             */
            /*            DISPLAY {1} SPACE 1                                        */
            /*               fg-set.part-no                          @ v-item-part-no*/
            /*               v-job-po                                @ v-ord-po-no   */
            /*               b-itemfg.i-name                         @ v-part-dscr   */
            /*            WITH FRAME bol-mid2.                                       */
            /*            DOWN {1} WITH FRAME bol-mid2.                              */
            /*                                                                       */
            /*            put {1} skip(1).                                           */
            /*            v-printline = v-printline + 2.                             */
            
            END.

        END.

        IF LAST-OF(tt-boll.po-no) THEN 
        DO:

            /*        PUT {1}                                                      */
            /*             SKIP                                                    */
            /*             "P.O.#:" AT 10                                          */
            /*             tt-boll.po-no AT 17                                     */
            /*             "Total" AT 34                                           */
            /*             v-tot-cases             FORMAT "->>,>>>,>>z"       AT 85*/
            /*             SKIP(1).                                                */
            v-printline = v-printline + 3.
         
        END.
      
    END. /* else */

END. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.