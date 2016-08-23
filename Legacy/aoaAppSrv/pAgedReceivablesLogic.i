/* pAgedReceivablesLogic.i */

    /* local variables */
    DEFINE VARIABLE v-cr-db-amt          AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-disc-amt           AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-type               AS CHARACTER FORMAT "x(2)" NO-UNDO.
    DEFINE VARIABLE v-first-cust         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE d                    AS INTEGER   LABEL "Days" NO-UNDO.
    DEFINE VARIABLE ni                   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cust-t               AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE cust-t-pri           AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE cust-t-fc            AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE sman-t               AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE sman-t-pri           AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE sman-t-fc            AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-current-trend-days AS INTEGER   FORMAT "->>9" NO-UNDO.
    DEFINE VARIABLE curr-t               AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE curr-t-pri           AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE curr-t-fc            AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE s                    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ag                   AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE amt                  LIKE ag NO-UNDO.
    DEFINE VARIABLE paid-amt             LIKE ag NO-UNDO.
    DEFINE VARIABLE c1                   AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE c1-pri               AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE c1-fc                AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE m2                   AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE m3                   AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE t1                   AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE t1-pri               AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE t1-fc                AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE unapp                LIKE cust-t NO-UNDO.
    DEFINE VARIABLE first-unapp          AS LOGICAL   INITIAL YES NO-UNDO.
    DEFINE VARIABLE v-disc-type          AS CHARACTER FORMAT "x(4)" NO-UNDO.
    DEFINE VARIABLE v-sman               AS CHARACTER FORMAT "x(24)" NO-UNDO.
    DEFINE VARIABLE v-int                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-dec                AS DECIMAL   EXTENT 4 NO-UNDO.                 
    DEFINE VARIABLE ll-valid-cust        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE ll-mult-curr         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-neg-text           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tr-dscr            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-check-date         AS DATE      NO-UNDO.
    DEFINE VARIABLE v-gltrans-desc       AS CHARACTER FORMAT "X(60)" NO-UNDO.
    DEFINE VARIABLE cPoNo                LIKE ar-inv.po-no NO-UNDO.
    DEFINE VARIABLE cJobStr              AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE i                    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-text              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-Inv-note           AS CHARACTER FORMAT "x(80)" EXTENT 8 NO-UNDO.
    DEFINE VARIABLE v-Collection-note    AS CHARACTER FORMAT "x(80)" EXTENT 8 NO-UNDO.
    
&SCOPED-DEFINE for-each-arinv ~
    FOR EACH ar-inv ~
        FIELDS(company posted cust-no inv-date ~
               terms x-no due-date net gross ~
               freight tax-amt inv-no) ~
        NO-LOCK ~
        WHERE ar-inv.company     EQ cust.company ~
          AND ar-inv.posted      EQ YES ~
          AND ar-inv.cust-no     EQ cust.cust-no ~
          AND ar-inv.inv-date    LE dtAsofDate ~
          AND ar-inv.terms       NE "CASH"         

&SCOPED-DEFINE for-each-arcsh ~
    FOR EACH ar-cash ~
        FIELDS(company cust-no posted check-date c-no check-no) ~
        NO-LOCK ~
        WHERE ar-cash.company     EQ cust.company ~
          AND ar-cash.cust-no     EQ cust.cust-no ~
          AND (ar-cash.check-date LE dtAsofDate OR ~
               ar-cash.check-date EQ ?) ~
          AND ar-cash.posted      EQ YES ~
        USE-INDEX ar-cash, ~
        EACH ar-cashl ~
        FIELDS(check-no c-no posted inv-no company ~
               cust-no memo amt-disc amt-paid on-account rec_key) ~
        NO-LOCK ~
        WHERE ar-cashl.c-no       EQ ar-cash.c-no ~
          AND ar-cashl.posted     EQ YES ~
        USE-INDEX c-no: ~
        IF ar-cashl.inv-no NE 0 THEN DO: ~
            FIND FIRST ar-inv NO-LOCK  ~
                 WHERE ar-inv.company     EQ cust.company ~
                   AND ar-inv.inv-no      EQ ar-cashl.inv-no ~
                   AND ar-inv.inv-date    GT dtAsofDate ~
                 USE-INDEX inv-no NO-ERROR. ~
            IF NOT AVAILABLE ar-inv THEN NEXT. ~
        END. ~
        IF ar-cashl.amt-paid GT 0 THEN DO: ~
            FIND FIRST reftable NO-LOCK ~
                 WHERE reftable.reftable EQ "ARCASHLVDDATE" ~
                   AND reftable.rec_key EQ ar-cashl.rec_key ~
                 USE-INDEX rec_key NO-ERROR. ~
            IF AVAILABLE reftable THEN ~
            v-check-date = DATE(reftable.CODE). ~
            ELSE DO: ~
                v-gltrans-desc = "VOID " + cust.cust-no + " " ~
                               + STRING(ar-cash.check-no,"9999999999") ~
                               + " Inv# " + STRING(ar-cashl.inv-no). ~
                FIND FIRST gltrans NO-LOCK ~
                     WHERE gltrans.company EQ cust.company ~
                       AND gltrans.jrnl EQ "CASHRVD" ~
                       AND gltrans.tr-dscr EQ v-gltrans-desc ~
                     NO-ERROR. ~
                v-check-date = IF AVAILABLE gltrans THEN gltrans.tr-date ~
                               ELSE ar-cash.check-date. ~
            END. ~
        END. ~
        ELSE v-check-date = ar-cash.check-date. ~
        IF v-check-date NE ? AND v-check-date GT dtAsofDate THEN NEXT.

&SCOPED-DEFINE valid-factored ~
    IF NOT lIncludeFactoredFGItems AND ~
       CAN-FIND(FIRST tt-factored ~
                  WHERE tt-factored.x-no EQ ar-inv.x-no) THEN ~
       NEXT.
    
    FOR EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "FACTORED"
          AND reftable.code2    EQ "YES"
        :
        /* Note: code2 index exists on reftable */
        FIND FIRST tt-factored NO-LOCK WHERE tt-factored.i-no EQ reftable.code NO-ERROR.
        IF NOT AVAILABLE tt-factored THEN
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.company EQ ipcCompany
              AND ar-invl.i-no    EQ reftable.code
            :
            CREATE tt-factored.
            ASSIGN
                tt-factored.company = reftable.company
                tt-factored.i-no    = reftable.code
                .
        END. /* END FOR-EACH ar-inv1 */
    END. /* END FOR EACH reftable */
        
    FOR EACH company NO-LOCK
        WHERE company.company GE cStartCompany
          AND company.company LE cEndCompany,
        EACH cust FIELDS(company cust-no sman curr-code name area-code phone terms fax cr-lim contact addr city state zip) NO-LOCK
        WHERE cust.company EQ company.company
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
        /*AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)*/
          AND cust.sman    GE cStartSalesRep
          AND cust.sman    LE cEndSalesRep
          AND cust.terms   GE cStartTerms
          AND cust.terms   LE cEndTerms
          AND (cust.ACTIVE NE "I" OR lInactiveCustomers)
          AND ((cust.curr-code  GE cStartCurrency
          AND   cust.curr-code  LE cEndCurrency)
           OR (cust.curr-code   EQ ""
          AND company.curr-code GE cStartCurrency
          AND company.curr-code LE cEndCurrency))
        :
        ll-valid-cust = NO.
        IF NOT ll-valid-cust THEN
        {&for-each-arinv}:
                {&valid-factored}
                ll-valid-cust = YES.
                LEAVE.
        END. /* not ll-valid-cust */
    
        IF NOT ll-valid-cust THEN
        {&for-each-arcsh}
            ll-valid-cust = YES.
            LEAVE.
        END. /* not ll-valid-cust */
    
        IF ll-valid-cust THEN DO:
            CREATE tt-cust.
            ASSIGN
                tt-cust.curr-code = IF cust.curr-code EQ "" THEN company.curr-code
                                    ELSE cust.curr-code
                tt-cust.sorter    = IF cSort1 EQ "Customer No" THEN cust.cust-no
                               ELSE IF cSort1 EQ "Name" THEN cust.name
                               ELSE cust.sman
                tt-cust.row-id    = ROWID(cust)
                .
            IF tt-cust.curr-code NE company.curr-code THEN
            ll-mult-curr = YES.
        END. /* if ll-valid-cust */
    END. /* each company */
    
    FOR EACH tt-cust,
        FIRST cust FIELDS(company cust-no sman curr-code name area-code phone terms fax cr-lim contact addr city state zip) NO-LOCK
        WHERE ROWID(cust) EQ tt-cust.row-id
        BREAK BY tt-cust.curr-code
              BY tt-cust.sorter
        :
        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ cust.company
              AND sman.sman    EQ cust.sman
            NO-ERROR.
        ASSIGN
            v-sman       = cust.sman + "-" + (IF AVAILABLE sman THEN sman.sname
                                              ELSE "Slsmn not on file")
            v-first-cust = YES
            .
        EMPTY TEMP-TABLE tt-inv.
        IF lIncludePaidInvoices OR dtAsofDate NE TODAY THEN
        {&for-each-arinv}:
            {&valid-factored}
            CREATE tt-inv.
            ASSIGN
                tt-inv.sorter = IF cSort2 EQ "Due Date" THEN INTEGER(ar-inv.due-date)
                           ELSE IF cSort2 EQ "Invoice Date" THEN INTEGER(ar-inv.inv-date)
                           ELSE ar-inv.inv-no
                tt-inv.inv-no = ar-inv.inv-no
                tt-inv.row-id = ROWID(ar-inv)
                .
        END. /* for each arinv */
        ELSE DO:
            {&for-each-arinv} AND ar-inv.due LT 0 USE-INDEX posted-due:
                {&valid-factored}
                CREATE tt-inv.
                ASSIGN
                    tt-inv.sorter = IF cSort2 EQ "Due Date" THEN INTEGER(ar-inv.due-date)
                               ELSE IF cSort2 EQ "Invoice Date" THEN INTEGER(ar-inv.inv-date)
                               ELSE ar-inv.inv-no
                    tt-inv.inv-no = ar-inv.inv-no
                    tt-inv.row-id = ROWID(ar-inv)
                    .
            END. /* for each arinv */
        
            {&for-each-arinv} AND ar-inv.due GT 0 USE-INDEX posted-due:
                {&valid-factored}
                CREATE tt-inv.
                ASSIGN
                    tt-inv.sorter = IF cSort2 EQ "Due Date" THEN INTEGER(ar-inv.due-date)
                               ELSE IF cSort2 EQ "Invoice Date" THEN INTEGER(ar-inv.inv-date)
                               ELSE ar-inv.inv-no
                    tt-inv.inv-no = ar-inv.inv-no
                    tt-inv.row-id = ROWID(ar-inv)
                    .
            END. /* for each arinv */
        END. /* else */
        
        FOR EACH tt-inv,
            FIRST ar-inv NO-LOCK
            WHERE ROWID(ar-inv) EQ tt-inv.row-id
            BREAK BY tt-inv.sorter
            BY tt-inv.inv-no
            :
            /* Inserted because AR stores gross wrong */
            amt = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN ar-inv.net
            ELSE ar-inv.gross.
            IF amt EQ ? THEN amt = 0.
            
            /* if fuel surcharge should not be aged, get it out of 'amt' */
            IF NOT lIncludeFuelSurchages THEN 
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.x-no EQ ar-inv.x-no
                  AND CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ ar-invl.company
                  AND itemfg.i-no    EQ ar-invl.i-no
                  AND itemfg.procat  EQ "FS")
                :
                amt = amt - ar-invl.amt.
            END. /* each ar-invl */
        
            ASSIGN
                cPoNo   = ""
                cJobStr = ""
                .
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.x-no EQ ar-inv.x-no
                :
                IF ar-invl.po-no  GT "" THEN cPoNo   = ar-invl.po-no.
                IF ar-invl.job-no GT "" THEN cJobStr = ar-invl.job-no + "-" + STRING(ar-invl.job-no2, "99").
            END. /* each ar-invl */
        
            ASSIGN
                ag     = amt
                d      = dtAsofDate - ar-inv.due-date
                ni     = ni + 1
                v-type = IF ar-inv.terms EQ "FCHG" THEN "FC" ELSE "IN"
                .
        
            FOR EACH ar-cashl NO-LOCK
                WHERE ar-cashl.company  EQ ar-inv.company
                  AND ar-cashl.posted   EQ YES 
                  AND ar-cashl.cust-no  EQ ar-inv.cust-no
                  AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no,
                FIRST ar-cash NO-LOCK
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                  AND ar-cash.check-date LE dtAsofDate
                USE-INDEX c-no BY ar-cashl.rec_key
                :
                IF ar-cashl.amt-paid GT 0 THEN DO:
                    FIND FIRST reftable NO-LOCK
                        WHERE reftable.reftable EQ "ARCASHLVDDATE"
                          AND reftable.rec_key EQ ar-cashl.rec_key
                        USE-INDEX rec_key NO-ERROR.
                    IF AVAILABLE reftable THEN
                    v-check-date = DATE(reftable.code).
                    ELSE DO:
                        v-gltrans-desc = "VOID " + cust.cust-no + " "
                                       + STRING(ar-cash.check-no,"9999999999")
                                       + " Inv# " + STRING(ar-cashl.inv-no).
                        FIND FIRST gltrans NO-LOCK
                            WHERE gltrans.company EQ cust.company
                              AND gltrans.jrnl EQ "CASHRVD"
                              AND gltrans.tr-dscr EQ v-gltrans-desc
                            NO-ERROR.
                        v-check-date = IF AVAILABLE gltrans THEN gltrans.tr-date
                                       ELSE ar-cash.check-date.
                    END. /* else */
                END. /* if ar-cashl.amt-paid */
                ELSE v-check-date = ar-cash.check-date.
        
                IF v-check-date NE ? AND v-check-date GT dtAsofDate THEN NEXT.
                    
                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-disc NE 0 AND ar-cashl.amt-paid EQ 0 THEN 
                    ag = ag - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN 
                        ag = ag + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                        ag = ag + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                ag = ag + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
            END. /* each ar-cashl */
                
            IF ag NE 0 OR
              (lIncludePaidInvoices AND
               ar-inv.inv-date GE dtStartInvoiceDate AND
               ar-inv.inv-date LE dtEndInvoiceDate) THEN DO:
                IF v-first-cust THEN DO:
                    ASSIGN
                        paid-amt = 0
                        m3       = ""
                        ni       = 0
                        .
                    IF cust.area-code NE "" THEN m3 = STRING(cust.area-code,"(999) ").
                    m3 = m3 + STRING(cust.phone,"999-9999").
                    FIND FIRST terms NO-LOCK
                         WHERE terms.company EQ cust.company
                           AND terms.t-code  EQ cust.terms
                         NO-ERROR.
                    /* If input trend days entered, THEN DO the trend days calculation. */
                    IF iRecenTrendDays GT 0 THEN
                    RUN pGetTrendDays (INPUT iRecenTrendDays, OUTPUT v-current-trend-days).
                    IF cType EQ "Detail" THEN DO:
                    END. /* if cType */
                    v-first-cust = NO.
                END. /* if v-first-cust */
        
                IF d GE iPeriodDays3 THEN v-int = 4.
                ELSE IF d GE iPeriodDays2 THEN v-int = 3.
                ELSE IF d GE iPeriodDays1 THEN v-int = 2.
                ELSE v-int = 1.
        
                ASSIGN
                    cust-t[v-int] = cust-t[v-int] + ag
                    v-dec         = 0
                    v-dec[v-int]  = ag
                    .
        
                IF lSeparateFinanceCharges THEN DO:
                    IF v-type NE "FC" THEN cust-t-pri[v-int] = cust-t-pri[v-int] + ag.
                    ELSE cust-t-fc[v-int] = cust-t-fc[v-int] + ag.
                END. /* IF lSeparateFinanceCharges THEN */
        
                IF cType EQ "Detail" THEN DO:
                    CREATE ttAgedReceivables.
                    ASSIGN
                        ttAgedReceivables.custNo      = cust.cust-no
                        ttAgedReceivables.custName    = cust.name
                        ttAgedReceivables.contact     = cust.contact
                        ttAgedReceivables.salesRep    = v-sman
                        ttAgedReceivables.terms       = IF AVAILABLE terms THEN terms.dscr ELSE ""
                        ttAgedReceivables.address1    = cust.addr[1] 
                        ttAgedReceivables.address2    = cust.addr[2] 
                        ttAgedReceivables.city        = cust.city    
                        ttAgedReceivables.state       = cust.state   
                        ttAgedReceivables.zip         = cust.zip     
                        ttAgedReceivables.creditLimit = cust.cr-lim  
                        ttAgedReceivables.phone       = TRIM(STRING(cust.area-code,"(xxx)") + STRING(cust.phone,"xxx-xxxx")) 
                        ttAgedReceivables.fax         = TRIM(STRING(SUBSTRING(cust.fax,1,3),"(xxx)") + STRING(SUBSTRING(cust.fax,4,7),"xxx-xxxx")) 
                        ttAgedReceivables.checkMemo   = "0"                          
                        ttAgedReceivables.daysOld     = d                            
                        ttAgedReceivables.vType       = v-type                       
                        ttAgedReceivables.invoiceNo   = ar-inv.inv-no                
                        ttAgedReceivables.invoiceDate = ar-inv.inv-date              
                        ttAgedReceivables.amount      = amt                          
                        ttAgedReceivables.vCurrent    = v-dec[1]                     
                        ttAgedReceivables.adtp        = cust.avg-pay                 
                        ttAgedReceivables.td          = v-current-trend-days         
                        ttAgedReceivables.periodDay1  = v-dec[2]                     
                        ttAgedReceivables.periodDay2  = v-dec[3]                     
                        ttAgedReceivables.periodDay3  = v-dec[4]                     
                        ttAgedReceivables.custPoNo    = cPoNo                        
                        ttAgedReceivables.jobNo       = cJobStr                      
                        ttAgedReceivables.invoiceNote = ""                           
                        ttAgedReceivables.collNote    = ""
                        .
                            
                    FOR EACH notes NO-LOCK
                        WHERE notes.rec_key EQ ar-inv.rec_key
                          AND notes.note_type EQ "I"
                        :
                        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                    END. /* each notes */
        
                    ASSIGN
                        ttAgedReceivables.invoiceNote = lv-text
                        lv-text                       = ""
                        .
        
                    FOR EACH notes NO-LOCK
                        WHERE notes.rec_key    EQ cust.rec_key
                          AND notes.note_type  EQ "G"
                          AND notes.note_group EQ "Collection"
                        :
                        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                    END. /*end  FOR EACH notes */
                    ttAgedReceivables.collNote = lv-text.
                END.  /* if cType EQ "Detail" THEN */
        
                FOR EACH ar-cashl NO-LOCK
                    WHERE ar-cashl.company EQ ar-inv.company
                      AND ar-cashl.posted  EQ YES 
                      AND ar-cashl.cust-no EQ ar-inv.cust-no
                      AND ar-cashl.inv-no  EQ ar-inv.inv-no
                    USE-INDEX inv-no,
                    FIRST ar-cash NO-LOCK
                    WHERE ar-cash.c-no       EQ ar-cashl.c-no
                      AND ar-cash.check-date LE dtAsofDate
                    USE-INDEX c-no
                    :
                    IF ar-cashl.amt-paid GT 0 THEN DO:
                        FIND FIRST reftable NO-LOCK
                            WHERE reftable.reftable EQ "ARCASHLVDDATE"
                              AND reftable.rec_key  EQ ar-cashl.rec_key
                            USE-INDEX rec_key NO-ERROR.
                        IF AVAILABLE reftable THEN                             
                        v-check-date = DATE(reftable.code).
                        ELSE DO:
                            v-gltrans-desc = "VOID " + cust.cust-no + " "
                                           + STRING(ar-cash.check-no,"9999999999")
                                           + " Inv# " + STRING(ar-cashl.inv-no).
                            FIND FIRST gltrans NO-LOCK
                                 WHERE gltrans.company EQ cust.company
                                   AND gltrans.jrnl EQ "CASHRVD"
                                   AND gltrans.tr-dscr EQ v-gltrans-desc
                                 NO-ERROR.
                            v-check-date = IF AVAILABLE gltrans THEN gltrans.tr-date
                            ELSE ar-cash.check-date.
                        END. /* else */
                    END. /* if ar-cashl.amt-paid GT 0 */
                    ELSE v-check-date = ar-cash.check-date.
        
                    IF v-check-date NE ? AND v-check-date GT dtAsofDate THEN NEXT.
        
                    IF ar-cashl.memo THEN
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                        ASSIGN
                            v-type      = "DM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = ar-cashl.amt-disc
                            .
                        ELSE
                        ASSIGN
                            v-type      = "CM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = - (ar-cashl.amt-disc)
                            .
                    ELSE DO:
                        v-tr-dscr = "VOID " + cust.cust-no + " "
                            + STRING(ar-cash.check-no,"9999999999")
                            + " Inv# " + STRING(ar-cashl.inv-no).
                        IF ar-cashl.amt-paid GT 0 AND
                            (CAN-FIND(FIRST reftable
                                      WHERE reftable.reftable EQ "ARCASHLVDDATE"
                                        AND reftable.rec_key EQ ar-cashl.rec_key
                                      USE-INDEX rec_key) OR
                             CAN-FIND(FIRST gltrans
                                      WHERE gltrans.company EQ cust.company
                                        AND gltrans.jrnl EQ "CASHRVD"
                                        AND gltrans.tr-dscr EQ v-tr-dscr)) THEN
                        v-type = "VD".
                        ELSE v-type = "PY".
                        ASSIGN
                            v-cr-db-amt = ar-cashl.amt-paid * -1
                            v-disc-amt  = ar-cashl.amt-disc * -1
                            .
                        IF v-type EQ "VD" AND v-cr-db-amt LT 0 THEN
                            v-cr-db-amt = v-cr-db-amt * -1.
                    END. /* else */
        
                    IF v-disc-amt NE 0 THEN DO:
                        v-disc-type = "DISC".
                        IF ar-cashl.memo THEN
                        ASSIGN 
                            v-disc-type = "RETN"
                            v-disc-amt  = - v-disc-amt
                            .
                        IF cType EQ "Detail" THEN DO:
                            IF v-disc-type EQ "DISC" THEN DO:
                                CREATE ttAgedReceivables.
                                ASSIGN
                                    ttAgedReceivables.custNo      = cust.cust-no
                                    ttAgedReceivables.custName    = cust.NAME
                                    ttAgedReceivables.contact     = cust.contact
                                    ttAgedReceivables.salesRep    = v-sman
                                    ttAgedReceivables.terms       = IF AVAILABLE terms THEN STRING(terms.dscr) ELSE ""
                                    ttAgedReceivables.address1    = cust.addr[1]
                                    ttAgedReceivables.address2    = cust.addr[2]
                                    ttAgedReceivables.city        = cust.city
                                    ttAgedReceivables.state       = cust.state
                                    ttAgedReceivables.zip         = cust.zip
                                    ttAgedReceivables.creditLimit = cust.cr-lim
                                    ttAgedReceivables.phone       = TRIM(STRING(cust.area-code,"(xxx)") +  STRING(cust.phone,"xxx-xxxx")) 
                                    ttAgedReceivables.fax         = TRIM(STRING(SUBSTRING(cust.fax,1,3),"(xxx)") + STRING(SUBSTRING(cust.fax,4,7),"xxx-xxxx"))
                                    ttAgedReceivables.checkMemo   = STRING(ar-cashl.check-no)
                                    ttAgedReceivables.daysOld     = 0  
                                    ttAgedReceivables.vType       = v-type
                                    ttAgedReceivables.invoiceNo   = ar-cashl.inv-no
                                    ttAgedReceivables.invoiceDate = ar-cashl.inv-date
                                    ttAgedReceivables.amount      = v-cr-db-amt
                                    ttAgedReceivables.vCurrent    = 0
                                    ttAgedReceivables.adtp        = cust.avg-pay
                                    ttAgedReceivables.td          = v-current-trend-days
                                    ttAgedReceivables.periodDay1  = 0
                                    ttAgedReceivables.periodDay2  = 0
                                    ttAgedReceivables.periodDay3  = 0
                                    ttAgedReceivables.custPoNo    = cPoNo
                                    ttAgedReceivables.jobNo       = cJobStr
                                    ttAgedReceivables.invoiceNote = ""
                                    ttAgedReceivables.collNote    = ""
                                    .
                                FOR EACH notes NO-LOCK
                                    WHERE notes.rec_key   EQ ar-inv.rec_key
                                      AND notes.note_type EQ "I"
                                    :
                                    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                                END. /* each notes*/
                                ttAgedReceivables.invoiceNote =  lv-text.
                            END.  /* v-disc-type */
        
                            CREATE ttAgedReceivables.
                            ASSIGN
                                ttAgedReceivables.custNo      = cust.cust-no
                                ttAgedReceivables.custName    = cust.NAME
                                ttAgedReceivables.contact     = cust.contact
                                ttAgedReceivables.salesRep    = v-sman
                                ttAgedReceivables.terms       = IF AVAILABLE terms THEN STRING(terms.dscr) ELSE ""
                                ttAgedReceivables.address1    = cust.addr[1]
                                ttAgedReceivables.address2    = cust.addr[2]
                                ttAgedReceivables.city        = cust.city
                                ttAgedReceivables.state       = cust.state
                                ttAgedReceivables.zip         = cust.zip
                                ttAgedReceivables.creditLimit = cust.cr-lim
                                ttAgedReceivables.phone       = TRIM(STRING(cust.area-code,"(xxx)") +  STRING(cust.phone,"xxx-xxxx")) 
                                ttAgedReceivables.fax         = TRIM(STRING(SUBSTRING(cust.fax,1,3),"(xxx)") + STRING(SUBSTRING(cust.fax,4,7),"xxx-xxxx"))
                                ttAgedReceivables.checkMemo   = STRING(ar-cashl.check-no)
                                ttAgedReceivables.daysOld     = 0
                                ttAgedReceivables.vType       = v-type
                                ttAgedReceivables.invoiceNo   = ar-cashl.inv-no
                                ttAgedReceivables.invoiceDate = ar-cashl.inv-date
                                ttAgedReceivables.amount      = v-disc-amt
                                ttAgedReceivables.vCurrent    = 0
                                ttAgedReceivables.adtp        = cust.avg-pay
                                ttAgedReceivables.td          = v-current-trend-days
                                ttAgedReceivables.periodDay1  = 0
                                ttAgedReceivables.periodDay2  = 0
                                ttAgedReceivables.periodDay3  = 0
                                ttAgedReceivables.custPoNo    = cPoNo
                                ttAgedReceivables.jobNo       = cJobStr
                                ttAgedReceivables.invoiceNote = ""
                                ttAgedReceivables.collNote    = ""
                                .
                        
                            FOR EACH notes NO-LOCK
                                WHERE notes.rec_key   EQ ar-inv.rec_key
                                  AND notes.note_type EQ "I"
                                :
                                lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                            END. /* each notes */
                            ttAgedReceivables.invoiceNote = lv-text.
                        END. /* IF cType EQ "Detail" */
                    END. /* IF v-disc-amt NE 0*/
                    ELSE IF cType EQ "Detail" THEN DO:
                        IF v-type EQ "VD" THEN DO:
                            FIND FIRST reftable NO-LOCK
                                WHERE reftable.reftable EQ "ARCASHLVDDATE"
                                  AND reftable.rec_key  EQ ar-cashl.rec_key
                                USE-INDEX rec_key NO-ERROR.
                            IF AVAILABLE reftable THEN
                            v-check-date = DATE(reftable.code).
                            ELSE DO:
                                v-gltrans-desc = "VOID " + cust.cust-no + " "
                                               + STRING(ar-cash.check-no,"9999999999")
                                               + " Inv# " + STRING(ar-cashl.inv-no).
                                FIND FIRST gltrans NO-LOCK
                                     WHERE gltrans.company EQ cust.company
                                       AND gltrans.jrnl    EQ "CASHRVD"
                                       AND gltrans.tr-dscr EQ v-gltrans-desc
                                     NO-ERROR.
                                v-check-date = IF AVAILABLE gltrans THEN gltrans.tr-date
                                               ELSE ar-cash.check-date.
                            END. /* else */
                        END. /* IF v-type EQ "VD" */
                        ELSE v-check-date = ar-cash.check-date.

                        IF cType EQ "Detail" THEN DO:
                            CREATE ttAgedReceivables.
                            ASSIGN
                                ttAgedReceivables.custNo      = cust.cust-no
                                ttAgedReceivables.custName    = cust.NAME
                                ttAgedReceivables.contact     = cust.contact
                                ttAgedReceivables.salesRep    = v-sman
                                ttAgedReceivables.terms       = IF AVAILABLE terms THEN STRING(terms.dscr) ELSE ""
                                ttAgedReceivables.address1    = cust.addr[1]
                                ttAgedReceivables.address2    = cust.addr[2]
                                ttAgedReceivables.city        = cust.city
                                ttAgedReceivables.state       = cust.state
                                ttAgedReceivables.zip         = cust.zip
                                ttAgedReceivables.creditLimit = cust.cr-lim
                                ttAgedReceivables.phone       = TRIM(STRING(cust.area-code,"(xxx)") +  STRING(cust.phone,"xxx-xxxx")) 
                                ttAgedReceivables.fax         = TRIM(STRING(SUBSTRING(cust.fax,1,3),"(xxx)") + STRING(SUBSTRING(cust.fax,4,7),"xxx-xxxx"))
                                ttAgedReceivables.checkMemo   = STRING(ar-cashl.check-no)
                                ttAgedReceivables.daysOld     = 0
                                ttAgedReceivables.vType       = v-type
                                ttAgedReceivables.invoiceNo   = ar-cashl.inv-no
                                ttAgedReceivables.invoiceDate = v-check-date
                                ttAgedReceivables.amount      = v-cr-db-amt
                                ttAgedReceivables.vCurrent    = 0
                                ttAgedReceivables.adtp        = cust.avg-pay
                                ttAgedReceivables.td          = v-current-trend-days
                                ttAgedReceivables.periodDay1  = 0
                                ttAgedReceivables.periodDay2  = 0
                                ttAgedReceivables.periodDay3  = 0
                                ttAgedReceivables.custPoNo    = cPoNo
                                ttAgedReceivables.jobNo       = cJobStr
                                ttAgedReceivables.invoiceNote = ""
                                ttAgedReceivables.collNote    = ""
                                .
                            FOR EACH notes NO-LOCK
                                WHERE notes.rec_key   EQ ar-inv.rec_key
                                  AND notes.note_type EQ "I"
                                :
                                lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                            END. /* FOR EACH notes*/                                                           
                            ttAgedReceivables.invoiceNote    =  lv-text . 
                        END. /*  IF cType EQ "Detail"*/
                    END. /*  IF cType EQ "Detail" */
                END. /* for each ar-cashl record */
            END. /* IF ag NE 0  */
        END. /* for each tt-inv, ar-inv */
    
        ASSIGN
            unapp[1] = 0
            unapp[2] = 0
            unapp[3] = 0
            unapp[4] = 0
            .
    
        /* This loop finds all unapplied balances and totals by age */
        {&for-each-arcsh}
            IF ar-cashl.memo THEN DO:
                /* CTS CM/DM signs are reversed *****************************/
                IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                ASSIGN
                    v-type      = "DM"
                    v-cr-db-amt = ar-cashl.amt-paid
                    v-disc-amt  = ar-cashl.amt-disc
                    .
                ELSE
                ASSIGN
                    v-type      = "CM"
                    v-cr-db-amt = ar-cashl.amt-paid
                    v-disc-amt  = ar-cashl.amt-disc
                    .
            END. /* IF ar-cashl.memo THEN DO */
            ELSE
            ASSIGN
                v-cr-db-amt = ar-cashl.amt-paid * -1
                v-disc-amt  = ar-cashl.amt-disc * -1
                .
            
            d = dtAsofDate - ar-cash.check-date.
        
            IF d GE iPeriodDays3 THEN
            unapp[4] = unapp[4] + v-cr-db-amt - v-disc-amt.
            ELSE IF d GE iPeriodDays2 AND d LT iPeriodDays3 THEN
                    unapp[3] = unapp[3] + v-cr-db-amt - v-disc-amt.
            ELSE IF d GE iPeriodDays1 AND d LT iPeriodDays2 THEN 
                    unapp[2] = unapp[2] + v-cr-db-amt - v-disc-amt.
            ELSE IF d LT iPeriodDays1 THEN 
                    unapp[1] = unapp[1] + v-cr-db-amt - v-disc-amt.
        END. /* for each ar-cashl record */
    
        first-unapp = YES.
        /* this loop displays all unapplied balances */
        {&for-each-arcsh}
            IF v-first-cust THEN DO:
                ASSIGN
                    paid-amt   = 0
                    cust-t     = 0
                    m3         = ""
                    ni         = 0
                    cust-t-pri = 0
                    cust-t-fc  = 0
                    .
            
                IF cust.area-code NE "" THEN
                m3 = STRING(cust.area-code,"(999) ").
            
                m3 = m3 + STRING(cust.phone,"999-9999").
            
                FIND FIRST terms NO-LOCK
                    WHERE terms.company EQ cust.company
                      AND terms.t-code  EQ cust.terms
                    NO-ERROR.
                v-first-cust = NO.
            END. /*  IF v-first-cust THEN DO */
        
            v-neg-text = "ON ACCT".
        
            IF ar-cashl.memo EQ TRUE THEN DO:
                IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                ASSIGN
                    v-type      = "DM"
                    v-cr-db-amt = ar-cashl.amt-paid
                    v-disc-amt  = ar-cashl.amt-disc
                    .
                ELSE
                ASSIGN
                    v-type      = "CM"
                    v-cr-db-amt = ar-cashl.amt-paid
                    v-disc-amt  = ar-cashl.amt-disc
                    .
            END. /* IF ar-cashl.memo EQ TRUE */
            ELSE DO:
                v-tr-dscr = "VOID " + cust.cust-no + " "
                    + STRING(ar-cash.check-no,"9999999999")
                    + " Inv# " + STRING(ar-cashl.inv-no).
                IF CAN-FIND(FIRST reftable
                            WHERE reftable.reftable EQ "ARCASHLVDDATE"
                              AND reftable.rec_key  EQ ar-cashl.rec_key
                            USE-INDEX rec_key) OR
                   CAN-FIND(FIRST gltrans
                            WHERE gltrans.company EQ cust.company
                              AND gltrans.jrnl    EQ "CASHRVD"
                              AND gltrans.tr-dscr EQ v-tr-dscr) THEN DO:
                    ASSIGN
                        v-type     = "VD"
                        v-neg-text = "VOID"
                        .
                    RELEASE reftable.
                END. /* do */
                ELSE v-type = "PY".
            
                ASSIGN
                    v-cr-db-amt = ar-cashl.amt-paid * -1
                    v-disc-amt  = ar-cashl.amt-disc * -1
                    .
        
                IF v-type EQ "VD" AND v-cr-db-amt LT 0 THEN
                v-cr-db-amt = v-cr-db-amt * -1.
            END. /* ELSE DO */
        
            IF first-unapp THEN DO:
                IF v-type EQ "VD" THEN DO:
                    FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "ARCASHLVDDATE"
                           AND reftable.rec_key  EQ ar-cashl.rec_key
                         USE-INDEX rec_key NO-ERROR.
                    IF AVAILABLE reftable THEN
                    v-check-date = DATE(reftable.code).
                    ELSE DO:
                        v-gltrans-desc = "VOID " + cust.cust-no + " "
                                       + STRING(ar-cash.check-no,"9999999999")
                                       + " Inv# " + STRING(ar-cashl.inv-no).
                        FIND FIRST gltrans NO-LOCK
                             WHERE gltrans.company EQ cust.company
                               AND gltrans.jrnl    EQ "CASHRVD"
                               AND gltrans.tr-dscr EQ v-gltrans-desc
                             NO-ERROR.
                        v-check-date = IF AVAILABLE gltrans THEN gltrans.tr-date
                                       ELSE ar-cash.check-date.
                    END. /* ELSE DO */
                END. /*  IF v-type EQ "VD" */
                ELSE v-check-date = ar-cash.check-date.
                   
                IF cType EQ "Detail" THEN DO:
                    CREATE ttAgedReceivables.
                    ASSIGN
                        ttAgedReceivables.custNo      = cust.cust-no 
                        ttAgedReceivables.custName    = cust.NAME                                                  
                        ttAgedReceivables.contact     = cust.contact                                               
                        ttAgedReceivables.salesRep    = v-sman                                                     
                        ttAgedReceivables.terms       = IF AVAILABLE terms THEN STRING(terms.dscr) ELSE ""             
                        ttAgedReceivables.address1    = cust.addr[1]                                               
                        ttAgedReceivables.address2    = cust.addr[2]                                               
                        ttAgedReceivables.city        = cust.city                                                  
                        ttAgedReceivables.state       = cust.state                                                 
                        ttAgedReceivables.zip         = cust.zip                                                   
                        ttAgedReceivables.creditLimit = cust.cr-lim                                                
                        ttAgedReceivables.phone       = TRIM(STRING(cust.area-code,"(xxx)") +  STRING(cust.phone,"xxx-xxxx")) 
                        ttAgedReceivables.fax         = TRIM(STRING(SUBSTRING(cust.fax,1,3),"(xxx)") + STRING(SUBSTRING(cust.fax,4,7),"xxx-xxxx"))  
                        ttAgedReceivables.checkMemo   = STRING(ar-cashl.check-no)                                                             
                        ttAgedReceivables.daysOld     = 0                                                                                     
                        ttAgedReceivables.vType       = v-type                                                                                
                        ttAgedReceivables.invoiceNo   = 0
                        ttAgedReceivables.invoiceDate = v-check-date                                                                          
                        ttAgedReceivables.amount      = v-cr-db-amt + v-disc-amt                                                              
                        ttAgedReceivables.vCurrent    = unapp[1]                                                                              
                        ttAgedReceivables.adtp        = cust.avg-pay                                                                          
                        ttAgedReceivables.td          = v-current-trend-days                                                                  
                        ttAgedReceivables.periodDay1  = unapp[2]                                                                              
                        ttAgedReceivables.periodDay2  = unapp[3]                                                                              
                        ttAgedReceivables.periodDay3  = unapp[4]                                                                              
                        ttAgedReceivables.custPoNo    = cPoNo                                                                                 
                        ttAgedReceivables.jobNo       = cJobStr                                                                               
                        ttAgedReceivables.invoiceNote = v-neg-text
                        ttAgedReceivables.collNote    = ""
                        . 
                END. /*IF cType EQ "Detail" THEN DO */
                
                ASSIGN
                    cust-t[4] = cust-t[4] + unapp[4]
                    cust-t[3] = cust-t[3] + unapp[3]
                    cust-t[2] = cust-t[2] + unapp[2]
                    cust-t[1] = cust-t[1] + unapp[1]
                    .
            
                IF lSeparateFinanceCharges THEN
                ASSIGN
                    cust-t-pri[4] = cust-t-pri[4] + unapp[4]
                    cust-t-pri[3] = cust-t-pri[3] + unapp[3]
                    cust-t-pri[2] = cust-t-pri[2] + unapp[2]
                    cust-t-pri[1] = cust-t-pri[1] + unapp[1]
                    .
            END. /*  IF first-unapp THEN DO */
        
            IF first-unapp THEN first-unapp = NO.
            ELSE DO:
                IF v-type EQ "VD" THEN DO:
                    FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "ARCASHLVDDATE"
                           AND reftable.rec_key EQ ar-cashl.rec_key
                         USE-INDEX rec_key NO-ERROR.
                      
                    IF AVAILABLE reftable THEN
                    v-check-date = DATE(reftable.CODE).
                    ELSE DO:
                        v-gltrans-desc = "VOID " + cust.cust-no + " "
                                       + STRING(ar-cash.check-no,"9999999999")
                                       + " Inv# " + STRING(ar-cashl.inv-no).
            
                        FIND FIRST gltrans NO-LOCK
                             WHERE gltrans.company EQ cust.company
                               AND gltrans.jrnl EQ "CASHRVD"
                               AND gltrans.tr-dscr EQ v-gltrans-desc
                             NO-ERROR.
                         
                        v-check-date = IF AVAILABLE gltrans THEN gltrans.tr-date
                                       ELSE ar-cash.check-date.
                    END. /* ELSE DO  */
                END. /* IF v-type EQ "VD" */
                ELSE v-check-date = ar-cash.check-date.
                  
                IF cType EQ "Detail" THEN DO:
                    CREATE ttAgedReceivables .
                    ASSIGN
                        ttAgedReceivables.custNo      = cust.cust-no
                        ttAgedReceivables.custName    = cust.NAME
                        ttAgedReceivables.contact     = cust.contact
                        ttAgedReceivables.salesRep    = v-sman
                        ttAgedReceivables.terms       = IF AVAILABLE terms THEN STRING(terms.dscr) ELSE ""
                        ttAgedReceivables.address1    = cust.addr[1]
                        ttAgedReceivables.address2    = cust.addr[2]
                        ttAgedReceivables.city        = cust.city
                        ttAgedReceivables.state       = cust.state
                        ttAgedReceivables.zip         = cust.zip
                        ttAgedReceivables.creditLimit = cust.cr-lim
                        ttAgedReceivables.phone       = TRIM(STRING(cust.area-code,"(xxx)") +  STRING(cust.phone,"xxx-xxxx")) 
                        ttAgedReceivables.fax         = TRIM(STRING(SUBSTRING(cust.fax,1,3),"(xxx)") + STRING(SUBSTRING(cust.fax,4,7),"xxx-xxxx"))
                        ttAgedReceivables.checkMemo   = STRING(ar-cashl.check-no)
                        ttAgedReceivables.daysOld     = 0
                        ttAgedReceivables.vType       = v-type
                        ttAgedReceivables.invoiceNo   = 0
                        ttAgedReceivables.invoiceDate = v-check-date
                        ttAgedReceivables.amount      = v-cr-db-amt + v-disc-amt
                        ttAgedReceivables.vCurrent    = 0
                        ttAgedReceivables.adtp        = cust.avg-pay
                        ttAgedReceivables.td          = v-current-trend-days
                        ttAgedReceivables.periodDay1  = 0
                        ttAgedReceivables.periodDay2  = 0 
                        ttAgedReceivables.periodDay3  = 0
                        ttAgedReceivables.custPoNo    = cPoNo
                        ttAgedReceivables.jobNo       = cJobStr
                        ttAgedReceivables.invoiceNote = v-neg-text
                        ttAgedReceivables.collNote    = ""
                        . 
                END. /*IF cType EQ "Detail" THEN */
            END. /* ELSE DO: */
        END. /* for each ar-cashl record */
    
        c1 = cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4].
        
        IF (NOT v-first-cust) OR c1 NE 0 THEN DO:
            IF cType EQ "Detail" THEN DO:
                IF lSeparateFinanceCharges THEN DO:
                    ASSIGN
                        c1-pri = cust-t-pri[1] + cust-t-pri[2] + cust-t-pri[3] + cust-t-pri[4]
                        c1-fc  = cust-t-fc[1] + cust-t-fc[2] + cust-t-fc[3] + cust-t-fc[4].
                END. /* IF lSeparateFinanceCharges THEN */
            END. /*  IF cType EQ "Detail" THEN DO */
                   
            DO i = 1 TO 4:
                ASSIGN
                    sman-t[i] = sman-t[i] + cust-t[i]
                    cust-t[i] = 0.
        
                IF lSeparateFinanceCharges THEN
                    ASSIGN
                        sman-t-pri[i] = sman-t-pri[i] + cust-t-pri[i]
                        sman-t-fc[i]  = sman-t-fc[i] + cust-t-fc[i]
                        cust-t-pri[i] = 0
                        cust-t-fc[i]  = 0.
            END. /*  DO i = 1 TO 4: */
        END. /* IF (NOT v-first-cust) OR c1 NE 0 THEN DO */
        
        IF LAST-OF(tt-cust.sorter) THEN DO:
            c1 = sman-t[1] + sman-t[2] + sman-t[3] + sman-t[4].
            DO i = 1 TO 4:
                ASSIGN
                    curr-t[i]     = curr-t[i] + sman-t[i]
                    sman-t[i]     = 0
                    curr-t-pri[i] = curr-t-pri[i] + sman-t-pri[i]
                    curr-t-fc[i]  = curr-t-fc[i] + sman-t-fc[i]
                    sman-t-pri[i] = 0
                    sman-t-fc[i]  = 0.
            END. /*  DO i = 1 TO 4: */
        END. /* IF LAST-OF(tt-cust.sorter) */
        
        IF LAST-OF(tt-cust.curr-code) THEN DO:
            IF ll-mult-curr THEN DO:
                c1 = curr-t[1] + curr-t[2] + curr-t[3] + curr-t[4].
            END. /* IF ll-mult-curr */
        END. /*  IF LAST-OF(tt-cust.curr-code) */
        
        m3 = "".
        IF ni EQ 1 THEN m3 = m2.
        ASSIGN
            v-cr-db-amt = 0
            v-disc-amt  = 0
            .
    END. /* each tt-cust */
