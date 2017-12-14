/* r-araged.i - used in aoa/BL/r-araged.p & aoa/BL/agedtot.p */

/* local variables */
DEFINE VARIABLE dCreditDebitAmt   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDiscAmt          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cvType            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lFirstCust        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iD                AS INTEGER   NO-UNDO.
DEFINE VARIABLE idx               AS INTEGER   NO-UNDO.
DEFINE VARIABLE dCustT            AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dCustTPri         AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dCustTFc          AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dSManT            AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dSManTPri         AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dSManTFc          AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dGrandT           AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dGrandTPri        AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dGrandTFc         AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE iCurrentTrendDays AS INTEGER   NO-UNDO.
DEFINE VARIABLE dCurrT            AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dCurrTPri         AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE dCurrTFc          AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE s                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE dAg               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dAmt              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPaidAmt          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dC1               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dC1Pri            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dC1Fc             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dT1               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dT1Pri            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dT1Fc             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cM2               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cM3               AS CHARACTER NO-UNDO.
DEFINE VARIABLE dUnapp            AS DECIMAL   NO-UNDO EXTENT 6.
DEFINE VARIABLE lFirstUnapp       AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE cDiscType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSalesRep         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt              AS INTEGER   NO-UNDO.
DEFINE VARIABLE dDec              AS DECIMAL   NO-UNDO EXTENT 4.
DEFINE VARIABLE lValidCust        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMultCurr         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cInvoiceNote      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTrDscr           AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtInvoiceDate     AS DATE      NO-UNDO.
DEFINE VARIABLE cGltransDesc      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoNo             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobStr           AS CHARACTER NO-UNDO.
DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLvText           AS CHARACTER NO-UNDO.

&SCOPED-DEFINE for-each-arinv ~
FOR EACH ar-inv ~
    FIELDS(company posted cust-no inv-date ~
           terms x-no due-date net gross ~
           freight tax-amt inv-no) ~
    NO-LOCK ~
    WHERE ar-inv.company  EQ cust.company ~
      AND ar-inv.posted   EQ YES ~
      AND ar-inv.cust-no  EQ cust.cust-no ~
      AND ar-inv.inv-date LE dtAsofDate ~
      AND ar-inv.terms    NE "CASH"         

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
    WHERE ar-cashl.c-no   EQ ar-cash.c-no ~
      AND ar-cashl.posted EQ YES ~
    USE-INDEX c-no: ~
    IF ar-cashl.inv-no NE 0 THEN DO: ~
        FIND FIRST ar-inv NO-LOCK ~
             WHERE ar-inv.company  EQ cust.company ~
               AND ar-inv.inv-no   EQ ar-cashl.inv-no ~
               AND ar-inv.inv-date GT dtAsofDate ~
             USE-INDEX inv-no NO-ERROR. ~
        IF NOT AVAILABLE ar-inv THEN NEXT. ~
    END. ~
    IF ar-cashl.amt-paid GT 0 THEN DO: ~
        FIND FIRST reftable NO-LOCK ~
             WHERE reftable.reftable EQ "ARCASHLVDDATE" ~
               AND reftable.rec_key  EQ ar-cashl.rec_key ~
             USE-INDEX rec_key NO-ERROR. ~
        IF AVAILABLE reftable THEN ~
        dtInvoiceDate = DATE(reftable.CODE). ~
        ELSE DO: ~
            cGltransDesc = "VOID " + cust.cust-no + " " ~
                         + STRING(ar-cash.check-no,"9999999999") ~
                         + " Inv# " + STRING(ar-cashl.inv-no). ~
            FIND FIRST gltrans NO-LOCK ~
                 WHERE gltrans.company EQ cust.company ~
                   AND gltrans.jrnl    EQ "CASHRVD" ~
                   AND gltrans.tr-dscr EQ cGltransDesc ~
                 NO-ERROR. ~
            dtInvoiceDate = IF AVAILABLE gltrans THEN gltrans.tr-date ~
                            ELSE ar-cash.check-date. ~
        END. ~
    END. ~
    ELSE dtInvoiceDate = ar-cash.check-date. ~
    IF dtInvoiceDate NE ? AND dtInvoiceDate GT dtAsofDate THEN NEXT.

&SCOPED-DEFINE valid-factored ~
IF NOT lIncludeFactoredFGItems AND ~
   CAN-FIND(FIRST tt-factored ~
            WHERE tt-factored.x-no EQ ar-inv.x-no) THEN ~
   NEXT.

FOR EACH itemfg NO-LOCK
    WHERE itemfg.factored    EQ YES
    :
    /* Note: code2 index exists on reftable */
    FIND FIRST tt-factored NO-LOCK WHERE tt-factored.i-no EQ itemfg.i-no NO-ERROR.
    IF NOT AVAILABLE tt-factored THEN
    FOR EACH ar-invl NO-LOCK
        WHERE ar-invl.company EQ ipcCompany
          AND ar-invl.i-no    EQ itemfg.i-no
        :
        CREATE tt-factored.
        ASSIGN
            tt-factored.company = itemfg.company
            tt-factored.i-no    = itemfg.i-no
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
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ cust.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    lValidCust = NO.
    IF NOT lValidCust THEN
    {&for-each-arinv}:
        {&valid-factored}
        lValidCust = YES.
        LEAVE.
    END. /* not lValidCust */

    IF NOT lValidCust THEN
    {&for-each-arcsh}
        lValidCust = YES.
        LEAVE.
    END. /* not lValidCust */

    IF lValidCust THEN DO:
        CREATE tt-cust.
        ASSIGN
            tt-cust.curr-code = IF cust.curr-code EQ "" THEN company.curr-code
                                ELSE cust.curr-code
            tt-cust.sorter    = IF cSort1 EQ "Customer No" THEN cust.cust-no
                           ELSE IF cSort1 EQ "Name" THEN cust.name
                           ELSE cust.sman
            tt-cust.row-id    = ROWID(cust) .
        IF tt-cust.curr-code NE company.curr-code THEN
        lMultCurr = YES.
    END. /* if lValidCust */
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
        cSalesRep  = cust.sman + "-" + (IF AVAILABLE sman THEN sman.sname
                                        ELSE "Slsmn not on file")
        lFirstCust = YES
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
        dAmt = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN ar-inv.net
        ELSE ar-inv.gross.
        IF dAmt EQ ? THEN dAmt = 0.
        
        /* if fuel surcharge should not be aged, get it out of 'amt' */
        IF NOT lIncludeFuelSurchages THEN 
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND CAN-FIND(FIRST itemfg
            WHERE itemfg.company EQ ar-invl.company
              AND itemfg.i-no    EQ ar-invl.i-no
              AND itemfg.procat  EQ "FS")
            :
            dAmt = dAmt - ar-invl.amt.
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
            dAg    = dAmt
            iD     = dtAsofDate - ar-inv.due-date
            idx    = idx + 1
            cvType = IF ar-inv.terms EQ "FCHG" THEN "FC" ELSE "IN"
            .
    
        FOR EACH ar-cashl NO-LOCK
            WHERE ar-cashl.company EQ ar-inv.company
              AND ar-cashl.posted  EQ YES 
              AND ar-cashl.cust-no EQ ar-inv.cust-no
              AND ar-cashl.inv-no  EQ ar-inv.inv-no
            USE-INDEX inv-no,
            FIRST ar-cash NO-LOCK
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
              AND ar-cash.check-date LE dtAsofDate
            USE-INDEX c-no BY ar-cashl.rec_key
            :
            IF ar-cashl.amt-paid GT 0 THEN DO:
                FIND FIRST reftable NO-LOCK
                    WHERE reftable.reftable EQ "ARCASHLVDDATE"
                      AND reftable.rec_key  EQ ar-cashl.rec_key
                    USE-INDEX rec_key NO-ERROR.
                IF AVAILABLE reftable THEN
                dtInvoiceDate = DATE(reftable.code).
                ELSE DO:
                    cGltransDesc = "VOID " + cust.cust-no + " "
                                 + STRING(ar-cash.check-no,"9999999999")
                                 + " Inv# " + STRING(ar-cashl.inv-no).
                    FIND FIRST gltrans NO-LOCK
                        WHERE gltrans.company EQ cust.company
                          AND gltrans.jrnl    EQ "CASHRVD"
                          AND gltrans.tr-dscr EQ cGltransDesc
                        NO-ERROR.
                    dtInvoiceDate = IF AVAILABLE gltrans THEN gltrans.tr-date
                                    ELSE ar-cash.check-date.
                END. /* else */
            END. /* if ar-cashl.amt-paid */
            ELSE dtInvoiceDate = ar-cash.check-date.
    
            IF dtInvoiceDate NE ? AND dtInvoiceDate GT dtAsofDate THEN NEXT.
                
            IF ar-cashl.memo THEN
                IF ar-cashl.amt-disc NE 0 AND ar-cashl.amt-paid EQ 0 THEN 
                dAg = dAg - ar-cashl.amt-disc.
                ELSE
                    IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN 
                    dAg = dAg + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                    ELSE
                    dAg = dAg + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
            ELSE
            dAg = dAg + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
        END. /* each ar-cashl */
            
        IF dAg NE 0 OR
          (lIncludePaidInvoices AND
           ar-inv.inv-date GE dtStartInvoiceDate AND
           ar-inv.inv-date LE dtEndInvoiceDate) THEN DO:
            IF lFirstCust THEN DO:
                ASSIGN
                    dPaidAmt = 0
                    cM3       = ""
                    idx       = 0
                    .
                IF cust.area-code NE "" THEN cM3 = STRING(cust.area-code,"(999) ").
                cM3 = cM3 + STRING(cust.phone,"999-9999").
                FIND FIRST terms NO-LOCK
                     WHERE terms.company EQ cust.company
                       AND terms.t-code  EQ cust.terms
                     NO-ERROR.
                /* If input trend days entered, THEN DO the trend days calculation. */
                IF iRecenTrendDays GT 0 THEN
                RUN pGetTrendDays (INPUT iRecenTrendDays, OUTPUT iCurrentTrendDays).
                IF cType EQ "Detail" THEN DO:
                END. /* if cType */
                lFirstCust = NO.
            END. /* if lFirstCust */
    
            IF iD GE iPeriodDays3 THEN iInt = 4.
            ELSE IF iD GE iPeriodDays2 THEN iInt = 3.
            ELSE IF iD GE iPeriodDays1 THEN iInt = 2.
            ELSE iInt = 1.
    
            ASSIGN
                dCustT[iInt] = dCustT[iInt] + dAg
                dDec         = 0
                dDec[iInt]   = dAg
                .
    
            IF lSeparateFinanceCharges THEN DO:
                IF cvType NE "FC" THEN dCustTPri[iInt] = dCustTPri[iInt] + dAg.
                ELSE dCustTFc[iInt] = dCustTFc[iInt] + dAg.
            END. /* if lSeparateFinanceCharges */
    
            IF cType EQ "Detail" THEN DO:
                CREATE ttAgedReceivables.
                ASSIGN
                    ttAgedReceivables.custNo      = cust.cust-no
                    ttAgedReceivables.custName    = cust.name
                    ttAgedReceivables.contact     = cust.contact
                    ttAgedReceivables.salesRep    = cSalesRep
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
                    ttAgedReceivables.daysOld     = iD                            
                    ttAgedReceivables.vType       = cvType                       
                    ttAgedReceivables.invoiceNo   = ar-inv.inv-no                
                    ttAgedReceivables.invoiceDate = ar-inv.inv-date              
                    ttAgedReceivables.amount      = dAmt                          
                    ttAgedReceivables.vCurrent    = dDec[1]                     
                    ttAgedReceivables.adtp        = cust.avg-pay                 
                    ttAgedReceivables.td          = iCurrentTrendDays         
                    ttAgedReceivables.periodDay1  = dDec[2]                     
                    ttAgedReceivables.periodDay2  = dDec[3]                     
                    ttAgedReceivables.periodDay3  = dDec[4]                     
                    ttAgedReceivables.custPoNo    = cPoNo                        
                    ttAgedReceivables.jobNo       = cJobStr                      
                    ttAgedReceivables.invoiceNote = ""                           
                    ttAgedReceivables.collNote    = ""
                    ttAgedReceivables.xxSort1     = tt-cust.sorter
                    ttAgedReceivables.xxSort2     = tt-inv.sorter
                    .
                        
                FOR EACH notes NO-LOCK
                    WHERE notes.rec_key EQ ar-inv.rec_key
                      AND notes.note_type EQ "I"
                    :
                    cLvText = cLvText + " " + TRIM(notes.note_text) + CHR(10).
                END. /* each notes */
    
                ASSIGN
                    ttAgedReceivables.invoiceNote = cLvText
                    cLvText                       = ""
                    .
    
                FOR EACH notes NO-LOCK
                    WHERE notes.rec_key    EQ cust.rec_key
                      AND notes.note_type  EQ "G"
                      AND notes.note_group EQ "Collection" :
                    cLvText = cLvText + " " + TRIM(notes.note_text) + CHR(10).
                END. /*end  FOR EACH notes */
                ttAgedReceivables.collNote = cLvText.
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
                    dtInvoiceDate = DATE(reftable.code).
                    ELSE DO:
                        cGltransDesc = "VOID " + cust.cust-no + " "
                                     + STRING(ar-cash.check-no,"9999999999")
                                     + " Inv# " + STRING(ar-cashl.inv-no).
                        FIND FIRST gltrans NO-LOCK
                             WHERE gltrans.company EQ cust.company
                               AND gltrans.jrnl    EQ "CASHRVD"
                               AND gltrans.tr-dscr EQ cGltransDesc
                             NO-ERROR.
                        dtInvoiceDate = IF AVAILABLE gltrans THEN gltrans.tr-date
                        ELSE ar-cash.check-date.
                    END. /* else */
                END. /* if ar-cashl.amt-paid GT 0 */
                ELSE dtInvoiceDate = ar-cash.check-date.
    
                IF dtInvoiceDate NE ? AND dtInvoiceDate GT dtAsofDate THEN NEXT.
    
                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                    ASSIGN
                        cvType          = "DM"
                        dCreditDebitAmt = ar-cashl.amt-paid
                        dDiscAmt        = ar-cashl.amt-disc
                        .
                    ELSE
                    ASSIGN
                        cvType          = "CM"
                        dCreditDebitAmt = ar-cashl.amt-paid
                        dDiscAmt        = - (ar-cashl.amt-disc)
                        .
                ELSE DO:
                    cTrDscr = "VOID " + cust.cust-no + " "
                            + STRING(ar-cash.check-no,"9999999999")
                            + " Inv# " + STRING(ar-cashl.inv-no).
                    IF ar-cashl.amt-paid GT 0 AND
                        (CAN-FIND(FIRST reftable
                                  WHERE reftable.reftable EQ "ARCASHLVDDATE"
                                    AND reftable.rec_key  EQ ar-cashl.rec_key
                                  USE-INDEX rec_key) OR
                         CAN-FIND(FIRST gltrans
                                  WHERE gltrans.company EQ cust.company
                                    AND gltrans.jrnl    EQ "CASHRVD"
                                    AND gltrans.tr-dscr EQ cTrDscr)) THEN
                    cvType = "VD".
                    ELSE cvType = "PY".
                    ASSIGN
                        dCreditDebitAmt = ar-cashl.amt-paid * -1
                        dDiscAmt        = ar-cashl.amt-disc * -1
                        .
                    IF cvType EQ "VD" AND dCreditDebitAmt LT 0 THEN
                    dCreditDebitAmt = dCreditDebitAmt * -1.
                END. /* else */
    
                IF dDiscAmt NE 0 THEN DO:
                    cDiscType = "DISC".
                    IF ar-cashl.memo THEN
                    ASSIGN 
                        cDiscType = "RETN"
                        dDiscAmt  = - dDiscAmt
                        .
                    IF cType EQ "Detail" THEN DO:
                        IF cDiscType EQ "DISC" THEN DO:
                            CREATE ttAgedReceivables.
                            ASSIGN
                                ttAgedReceivables.custNo      = cust.cust-no
                                ttAgedReceivables.custName    = cust.name
                                ttAgedReceivables.contact     = cust.contact
                                ttAgedReceivables.salesRep    = cSalesRep
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
                                ttAgedReceivables.vType       = cvType
                                ttAgedReceivables.invoiceNo   = ar-cashl.inv-no
                                ttAgedReceivables.invoiceDate = ar-cashl.inv-date
                                ttAgedReceivables.amount      = dCreditDebitAmt
                                ttAgedReceivables.vCurrent    = 0
                                ttAgedReceivables.adtp        = cust.avg-pay
                                ttAgedReceivables.td          = iCurrentTrendDays
                                ttAgedReceivables.periodDay1  = 0
                                ttAgedReceivables.periodDay2  = 0
                                ttAgedReceivables.periodDay3  = 0
                                ttAgedReceivables.custPoNo    = cPoNo
                                ttAgedReceivables.jobNo       = cJobStr
                                ttAgedReceivables.invoiceNote = ""
                                ttAgedReceivables.collNote    = ""
                                ttAgedReceivables.xxSort1     = tt-cust.sorter
                                ttAgedReceivables.xxSort2     = tt-inv.sorter
                                .
                            FOR EACH notes NO-LOCK
                                WHERE notes.rec_key   EQ ar-inv.rec_key
                                  AND notes.note_type EQ "I"
                                :
                                cLvText = cLvText + " " + TRIM(notes.note_text) + CHR(10).
                            END. /* each notes*/
                            ttAgedReceivables.invoiceNote = cLvText.
                        END.  /* v-disc-type */
    
                        CREATE ttAgedReceivables.
                        ASSIGN
                            ttAgedReceivables.custNo      = cust.cust-no
                            ttAgedReceivables.custName    = cust.name
                            ttAgedReceivables.contact     = cust.contact
                            ttAgedReceivables.salesRep    = cSalesRep
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
                            ttAgedReceivables.vType       = cvType
                            ttAgedReceivables.invoiceNo   = ar-cashl.inv-no
                            ttAgedReceivables.invoiceDate = ar-cashl.inv-date
                            ttAgedReceivables.amount      = dDiscAmt
                            ttAgedReceivables.vCurrent    = 0
                            ttAgedReceivables.adtp        = cust.avg-pay
                            ttAgedReceivables.td          = iCurrentTrendDays
                            ttAgedReceivables.periodDay1  = 0
                            ttAgedReceivables.periodDay2  = 0
                            ttAgedReceivables.periodDay3  = 0
                            ttAgedReceivables.custPoNo    = cPoNo
                            ttAgedReceivables.jobNo       = cJobStr
                            ttAgedReceivables.invoiceNote = ""
                            ttAgedReceivables.collNote    = ""
                            ttAgedReceivables.xxSort1     = tt-cust.sorter
                            ttAgedReceivables.xxSort2     = tt-inv.sorter
                            .
                    
                        FOR EACH notes NO-LOCK
                            WHERE notes.rec_key   EQ ar-inv.rec_key
                              AND notes.note_type EQ "I"
                            :
                            cLvText = cLvText + " " + TRIM(notes.note_text) + CHR(10).
                        END. /* each notes */
                        ttAgedReceivables.invoiceNote = cLvText.
                    END. /* IF cType EQ "Detail" */
                END. /* IF v-disc-amt NE 0*/
                ELSE IF cType EQ "Detail" THEN DO:
                    IF cvType EQ "VD" THEN DO:
                        FIND FIRST reftable NO-LOCK
                            WHERE reftable.reftable EQ "ARCASHLVDDATE"
                              AND reftable.rec_key  EQ ar-cashl.rec_key
                            USE-INDEX rec_key NO-ERROR.
                        IF AVAILABLE reftable THEN
                        dtInvoiceDate = DATE(reftable.code).
                        ELSE DO:
                            cGltransDesc = "VOID " + cust.cust-no + " "
                                         + STRING(ar-cash.check-no,"9999999999")
                                         + " Inv# " + STRING(ar-cashl.inv-no).
                            FIND FIRST gltrans NO-LOCK
                                 WHERE gltrans.company EQ cust.company
                                   AND gltrans.jrnl    EQ "CASHRVD"
                                   AND gltrans.tr-dscr EQ cGltransDesc
                                 NO-ERROR.
                            dtInvoiceDate = IF AVAILABLE gltrans THEN gltrans.tr-date
                                            ELSE ar-cash.check-date.
                        END. /* else */
                    END. /* IF v-type EQ "VD" */
                    ELSE dtInvoiceDate = ar-cash.check-date.

                    IF cType EQ "Detail" THEN DO:
                        CREATE ttAgedReceivables.
                        ASSIGN
                            ttAgedReceivables.custNo      = cust.cust-no
                            ttAgedReceivables.custName    = cust.name
                            ttAgedReceivables.contact     = cust.contact
                            ttAgedReceivables.salesRep    = cSalesRep
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
                            ttAgedReceivables.vType       = cvType
                            ttAgedReceivables.invoiceNo   = ar-cashl.inv-no
                            ttAgedReceivables.invoiceDate = dtInvoiceDate
                            ttAgedReceivables.amount      = dCreditDebitAmt
                            ttAgedReceivables.vCurrent    = 0
                            ttAgedReceivables.adtp        = cust.avg-pay
                            ttAgedReceivables.td          = iCurrentTrendDays
                            ttAgedReceivables.periodDay1  = 0
                            ttAgedReceivables.periodDay2  = 0
                            ttAgedReceivables.periodDay3  = 0
                            ttAgedReceivables.custPoNo    = cPoNo
                            ttAgedReceivables.jobNo       = cJobStr
                            ttAgedReceivables.invoiceNote = ""
                            ttAgedReceivables.collNote    = ""
                            ttAgedReceivables.xxSort1     = tt-cust.sorter
                            ttAgedReceivables.xxSort2     = tt-inv.sorter
                            .
                        FOR EACH notes NO-LOCK
                            WHERE notes.rec_key   EQ ar-inv.rec_key
                              AND notes.note_type EQ "I"
                            :
                            cLvText = cLvText + " " + TRIM(notes.note_text) + CHR(10).
                        END. /* FOR EACH notes*/                                                           
                        ttAgedReceivables.invoiceNote = cLvText.
                    END. /*  IF cType EQ "Detail"*/
                END. /*  IF cType EQ "Detail" */
            END. /* for each ar-cashl record */
        END. /* IF ag NE 0  */
    END. /* for each tt-inv, ar-inv */

    ASSIGN dUnapp = 0.

    /* This loop finds all unapplied balances and totals by age */
    {&for-each-arcsh}
        IF ar-cashl.memo THEN DO:
            /* CTS CM/DM signs are reversed *****************************/
            IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
            ASSIGN
                cvType          = "DM"
                dCreditDebitAmt = ar-cashl.amt-paid
                dDiscAmt        = ar-cashl.amt-disc
                .
            ELSE
            ASSIGN
                cvType          = "CM"
                dCreditDebitAmt = ar-cashl.amt-paid
                dDiscAmt        = ar-cashl.amt-disc
                .
        END. /* IF ar-cashl.memo THEN DO */
        ELSE
        ASSIGN
            dCreditDebitAmt = ar-cashl.amt-paid * -1
            dDiscAmt        = ar-cashl.amt-disc * -1
            .
        iD = dtAsofDate - ar-cash.check-date.
    
        IF iD GE iPeriodDays3 THEN
        dUnapp[4] = dUnapp[4] + dCreditDebitAmt - dDiscAmt.
        ELSE IF iD GE iPeriodDays2 AND iD LT iPeriodDays3 THEN
                dUnapp[3] = dUnapp[3] + dCreditDebitAmt - dDiscAmt.
        ELSE IF iD GE iPeriodDays1 AND iD LT iPeriodDays2 THEN 
                dUnapp[2] = dUnapp[2] + dCreditDebitAmt - dDiscAmt.
        ELSE IF iD LT iPeriodDays1 THEN 
                dUnapp[1] = dUnapp[1] + dCreditDebitAmt - dDiscAmt.
    END. /* for each ar-cashl record */

    lFirstUnapp = YES.
    /* this loop displays all unapplied balances */
    {&for-each-arcsh}
        IF lFirstCust THEN DO:
            ASSIGN
                dPaidAmt  = 0
                dCustT    = 0
                cM3       = ""
                idx       = 0
                dCustTPri = 0
                dCustTFc  = 0
                .
            IF cust.area-code NE "" THEN
            cM3 = STRING(cust.area-code,"(999) ").
        
            cM3 = cM3 + STRING(cust.phone,"999-9999").
        
            FIND FIRST terms NO-LOCK
                WHERE terms.company EQ cust.company
                  AND terms.t-code  EQ cust.terms
                NO-ERROR.
            lFirstCust = NO.
        END. /*  IF lFirstCust THEN DO */
    
        cInvoiceNote = "ON ACCT".
    
        IF ar-cashl.memo EQ TRUE THEN DO:
            IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
            ASSIGN
                cvType          = "DM"
                dCreditDebitAmt = ar-cashl.amt-paid
                dDiscAmt        = ar-cashl.amt-disc 
                .
            ELSE
            ASSIGN
                cvType          = "CM"
                dCreditDebitAmt = ar-cashl.amt-paid
                dDiscAmt        = ar-cashl.amt-disc
                .
        END. /* IF ar-cashl.memo EQ TRUE */
        ELSE DO:
            cTrDscr = "VOID " + cust.cust-no + " "
                + STRING(ar-cash.check-no,"9999999999")
                + " Inv# " + STRING(ar-cashl.inv-no).
            IF CAN-FIND(FIRST reftable
                        WHERE reftable.reftable EQ "ARCASHLVDDATE"
                          AND reftable.rec_key  EQ ar-cashl.rec_key
                        USE-INDEX rec_key) OR
               CAN-FIND(FIRST gltrans
                        WHERE gltrans.company EQ cust.company
                          AND gltrans.jrnl    EQ "CASHRVD"
                          AND gltrans.tr-dscr EQ cTrDscr) THEN DO:
                ASSIGN
                    cvType       = "VD"
                    cInvoiceNote = "VOID"
                    .
                RELEASE reftable.
            END. /* do */
            ELSE cvType = "PY".
        
            ASSIGN
                dCreditDebitAmt = ar-cashl.amt-paid * -1
                dDiscAmt        = ar-cashl.amt-disc * -1
                .
    
            IF cvType EQ "VD" AND dCreditDebitAmt LT 0 THEN
            dCreditDebitAmt = dCreditDebitAmt * -1.
        END. /* ELSE DO */
    
        IF lFirstUnapp THEN DO:
            IF cvType EQ "VD" THEN DO:
                FIND FIRST reftable NO-LOCK
                     WHERE reftable.reftable EQ "ARCASHLVDDATE"
                       AND reftable.rec_key  EQ ar-cashl.rec_key
                     USE-INDEX rec_key NO-ERROR.
                IF AVAILABLE reftable THEN
                dtInvoiceDate = DATE(reftable.code).
                ELSE DO:
                    cGltransDesc = "VOID " + cust.cust-no + " "
                                 + STRING(ar-cash.check-no,"9999999999")
                                 + " Inv# " + STRING(ar-cashl.inv-no).
                    FIND FIRST gltrans NO-LOCK
                         WHERE gltrans.company EQ cust.company
                           AND gltrans.jrnl    EQ "CASHRVD"
                           AND gltrans.tr-dscr EQ cGltransDesc
                         NO-ERROR.
                    dtInvoiceDate = IF AVAILABLE gltrans THEN gltrans.tr-date
                                    ELSE ar-cash.check-date.
                END. /* ELSE DO */
            END. /*  IF v-type EQ "VD" */
            ELSE dtInvoiceDate = ar-cash.check-date.
               
            IF cType EQ "Detail" THEN DO:
                CREATE ttAgedReceivables.
                ASSIGN
                    ttAgedReceivables.custNo      = cust.cust-no 
                    ttAgedReceivables.custName    = cust.name                                                  
                    ttAgedReceivables.contact     = cust.contact                                               
                    ttAgedReceivables.salesRep    = cSalesRep                                                     
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
                    ttAgedReceivables.vType       = cvType                                                                                
                    ttAgedReceivables.invoiceNo   = 0
                    ttAgedReceivables.invoiceDate = dtInvoiceDate                                                                          
                    ttAgedReceivables.amount      = dCreditDebitAmt + dDiscAmt                                                              
                    ttAgedReceivables.vCurrent    = dUnapp[1]                                                                              
                    ttAgedReceivables.adtp        = cust.avg-pay                                                                          
                    ttAgedReceivables.td          = iCurrentTrendDays                                                                  
                    ttAgedReceivables.periodDay1  = dUnapp[2]                                                                              
                    ttAgedReceivables.periodDay2  = dUnapp[3]                                                                              
                    ttAgedReceivables.periodDay3  = dUnapp[4]                                                                              
                    ttAgedReceivables.custPoNo    = cPoNo                                                                                 
                    ttAgedReceivables.jobNo       = cJobStr                                                                               
                    ttAgedReceivables.invoiceNote = cInvoiceNote
                    ttAgedReceivables.collNote    = ""
                    ttAgedReceivables.xxSort1     = tt-cust.sorter
                    ttAgedReceivables.xxSort2     = IF cSort2 EQ "Invoice Date" THEN INTEGER(dtInvoiceDate) ELSE 0
                    . 
            END. /*IF cType EQ "Detail" THEN DO */
            
            ASSIGN
                dCustT[4] = dCustT[4] + dUnapp[4]
                dCustT[3] = dCustT[3] + dUnapp[3]
                dCustT[2] = dCustT[2] + dUnapp[2]
                dCustT[1] = dCustT[1] + dUnapp[1]
                .
        
            IF lSeparateFinanceCharges THEN
            ASSIGN
                dCustTPri[4] = dCustTPri[4] + dUnapp[4]
                dCustTPri[3] = dCustTPri[3] + dUnapp[3]
                dCustTPri[2] = dCustTPri[2] + dUnapp[2]
                dCustTPri[1] = dCustTPri[1] + dUnapp[1]
                .
        END. /*  IF first-unapp THEN DO */
    
        IF lFirstUnapp THEN lFirstUnapp = NO.
        ELSE DO:
            IF cvType EQ "VD" THEN DO:
                FIND FIRST reftable NO-LOCK
                     WHERE reftable.reftable EQ "ARCASHLVDDATE"
                       AND reftable.rec_key  EQ ar-cashl.rec_key
                     USE-INDEX rec_key NO-ERROR.
                  
                IF AVAILABLE reftable THEN
                dtInvoiceDate = DATE(reftable.CODE).
                ELSE DO:
                    cGltransDesc = "VOID " + cust.cust-no + " "
                                 + STRING(ar-cash.check-no,"9999999999")
                                 + " Inv# " + STRING(ar-cashl.inv-no).
                    FIND FIRST gltrans NO-LOCK
                         WHERE gltrans.company EQ cust.company
                           AND gltrans.jrnl EQ "CASHRVD"
                           AND gltrans.tr-dscr EQ cGltransDesc
                         NO-ERROR.
                     
                    dtInvoiceDate = IF AVAILABLE gltrans THEN gltrans.tr-date
                                    ELSE ar-cash.check-date.
                END. /* ELSE DO  */
            END. /* IF v-type EQ "VD" */
            ELSE dtInvoiceDate = ar-cash.check-date.
              
            IF cType EQ "Detail" THEN DO:
                CREATE ttAgedReceivables.
                ASSIGN
                    ttAgedReceivables.custNo      = cust.cust-no
                    ttAgedReceivables.custName    = cust.name
                    ttAgedReceivables.contact     = cust.contact
                    ttAgedReceivables.salesRep    = cSalesRep
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
                    ttAgedReceivables.vType       = cvType
                    ttAgedReceivables.invoiceNo   = 0
                    ttAgedReceivables.invoiceDate = dtInvoiceDate
                    ttAgedReceivables.amount      = dCreditDebitAmt + dDiscAmt
                    ttAgedReceivables.vCurrent    = 0
                    ttAgedReceivables.adtp        = cust.avg-pay
                    ttAgedReceivables.td          = iCurrentTrendDays
                    ttAgedReceivables.periodDay1  = 0
                    ttAgedReceivables.periodDay2  = 0 
                    ttAgedReceivables.periodDay3  = 0
                    ttAgedReceivables.custPoNo    = cPoNo
                    ttAgedReceivables.jobNo       = cJobStr
                    ttAgedReceivables.invoiceNote = cInvoiceNote
                    ttAgedReceivables.collNote    = ""
                    ttAgedReceivables.xxSort1     = tt-cust.sorter
                    ttAgedReceivables.xxSort2     = IF cSort2 EQ "Invoice Date" THEN INTEGER(dtInvoiceDate) ELSE 0
                    . 
            END. /* if cType eq "Detail" */
        END. /* else do: */
    END. /* each ar-cashl */

    dC1 = dCustT[1] + dCustT[2] + dCustT[3] + dCustT[4].
    
    IF NOT lFirstCust OR dC1 NE 0 THEN DO:
        IF cType EQ "Detail" THEN DO:
            RUN AgedReceivablesCreateTotals (
                cust.cust-no,
                cust.name,
                cSalesRep,
                "Customer Totals",
                dC1,
                dCustT[1],
                dCustT[2],
                dCustT[3],
                dCustT[4],
                tt-cust.sorter
                ).
            IF lSeparateFinanceCharges THEN DO:
                ASSIGN
                    dC1Pri = dCustTPri[1] + dCustTPri[2] + dCustTPri[3] + dCustTPri[4]
                    dC1Fc  = dCustTFc[1]  + dCustTFc[2]  + dCustTFc[3]  + dCustTFc[4].
                RUN AgedReceivablesCreateTotals (
                    "",
                    "",
                    "",
                    "Principal Amount",
                    dC1Pri,
                    dCustTPri[1],
                    dCustTPri[2],
                    dCustTPri[3],
                    dCustTPri[4],
                    tt-cust.sorter
                    ).
                RUN AgedReceivablesCreateTotals (
                    "",
                    "",
                    "",
                    "Finance Charges",
                    dC1Fc,
                    dCustTFc[1],
                    dCustTFc[2],
                    dCustTFc[3],
                    dCustTFc[4],
                    tt-cust.sorter
                    ).
            END. /* IF lSeparateFinanceCharges THEN */
        END. /*  if cType eq "Detail" */
        ELSE IF cType EQ "Summary" THEN DO:
            RUN AgedReceivablesCreateTotals (
                cust.cust-no,
                cust.name,
                cSalesRep,
                "Summary Totals",
                dC1,
                dCustT[1],
                dCustT[2],
                dCustT[3],
                dCustT[4],
                tt-cust.sorter
                ).
        END. /* if cType eq "Summary" */
               
        DO i = 1 TO 4:
            ASSIGN
                dSManT[i] = dSManT[i] + dCustT[i]
                dCustT[i] = 0
                .
            IF lSeparateFinanceCharges THEN
            ASSIGN
                dSManTPri[i] = dSManTPri[i] + dCustTPri[i]
                dSManTFc[i]  = dSManTFc[i]  + dCustTFc[i]
                dCustTPri[i] = 0
                dCustTFc[i]  = 0
                .
        END. /* do i */
    END. /* if not lFirstCust */
    
    IF LAST-OF(tt-cust.sorter) THEN DO:
        dC1 = dSManT[1] + dSManT[2] + dSManT[3] + dSManT[4].
        IF cSort1 EQ "Name" THEN DO:
            IF cType NE "Totals Only" THEN
            RUN AgedReceivablesCreateTotals (
                cust.cust-no,
                cust.name,
                cSalesRep,
                "Salesrep Totals",
                dC1,
                dSManT[1],
                dSManT[2],
                dSManT[3],
                dSManT[4],
                tt-cust.sorter
                ).
        END. /* sort by customer no */
        DO i = 1 TO 4:
            ASSIGN
                dCurrT[i]    = dCurrT[i]    + dSManT[i]
                dCurrTPri[i] = dCurrTPri[i] + dSManTPri[i]
                dCurrTFc[i]  = dCurrTFc[i]  + dSManTFc[i]
                dSManT[i]    = 0
                dSManTPri[i] = 0
                dSManTFc[i]  = 0
                .
        END. /*  DO i = 1 TO 4: */
    END. /* IF LAST-OF(tt-cust.sorter) */
    
    IF LAST-OF(tt-cust.curr-code) THEN DO:
        IF lMultCurr THEN DO:
            dC1 = dCurrT[1] + dCurrT[2] + dCurrT[3] + dCurrT[4].
            IF cType NE "Totals Only" THEN
            RUN AgedReceivablesCreateTotals (
                cust.cust-no,
                cust.name,
                cSalesRep,
                "Currency Totals",
                dC1,
                dCurrT[1],
                dCurrT[2],
                dCurrT[3],
                dCurrT[4],
                tt-cust.sorter
                ).
            RUN AgedReceivablesCreateTotals (
                cust.cust-no,
                cust.name,
                cSalesRep,
                "Percentage Composition",
                0,
                (IF dC1 NE 0 THEN (dCurrT[1] / dC1) * 100 ELSE 0),
                (IF dC1 NE 0 THEN (dCurrT[2] / dC1) * 100 ELSE 0),
                (IF dC1 NE 0 THEN (dCurrT[3] / dC1) * 100 ELSE 0),
                (IF dC1 NE 0 THEN (dCurrT[4] / dC1) * 100 ELSE 0),
                tt-cust.sorter
                ).
        END. /* IF lMultCurr */
        DO i = 1 TO 4:
            ASSIGN
                dGrandT[i] = dGrandT[i] + dCurrT[i]
                dCurrT[i]  = 0
                .
        END. /* do i */
        IF lSeparateFinanceCharges THEN
        ASSIGN
            dGrandTPri[i] = dGrandTPri[i] + dCustTPri[i]
            dGrandTFc[i]  = dGrandTFc[i]  + dCustTFc[i]
            dCustTPri[i]  = 0
            dCustTFc[i]   = 0
            .
    END. /*  IF LAST-OF(tt-cust.curr-code) */
    
    cM3 = "".
    IF idx EQ 1 THEN cM3 = cM2.
    ASSIGN
        dCreditDebitAmt = 0
        dDiscAmt        = 0
        .
END. /* each tt-cust */

dT1 = dGrandT[1] + dGrandT[2] + dGrandT[3] + dGrandT[4].

RUN AgedReceivablesCreateTotals (
    "",
    "",
    "",
    "Grand Total",
    dT1,
    dGrandT[1],
    dGrandT[2],
    dGrandT[3],
    dGrandT[4],
    tt-cust.sorter
    ).
RUN AgedReceivablesCreateTotals (
    "",
    "",
    "",
    "Percentage Composition",
    0,
    (IF dT1 NE 0 THEN (dGrandT[1] / dT1) * 100 ELSE 0),
    (IF dT1 NE 0 THEN (dGrandT[2] / dT1) * 100 ELSE 0),
    (IF dT1 NE 0 THEN (dGrandT[3] / dT1) * 100 ELSE 0),
    (IF dT1 NE 0 THEN (dGrandT[4] / dT1) * 100 ELSE 0),
    tt-cust.sorter
    ).

IF lSeparateFinanceCharges THEN DO:
    ASSIGN
        dT1Pri = dGrandTPri[1] + dGrandTPri[2] + dGrandTPri[3] + dGrandTPri[4]
        dT1Fc  = dGrandTFc[1]  + dGrandTFc[2]  + dGrandTFc[3]  + dGrandTFc[4]
        .
    RUN AgedReceivablesCreateTotals (
        "",
        "",
        "",
        "Principal Amount",
        dT1Pri,
        dGrandTPri[1],
        dGrandTPri[2],
        dGrandTPri[3],
        dGrandTPri[4],
        tt-cust.sorter
        ).
    RUN AgedReceivablesCreateTotals (
        "",
        "",
        "",
        "Finance Charges",
        dT1Fc,
        dGrandTFc[1],
        dGrandTFc[2],
        dGrandTFc[3],
        dGrandTFc[4],
        tt-cust.sorter
        ).
END. /* if separate finance charges */


PROCEDURE AgedReceivablesCreateTotals:
    DEFINE INPUT  PARAMETER ipcTotCustNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTotCustName    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTotSalesRep    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTotDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTotAmount      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTotCurrent     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTotPeriod1     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTotPeriod2     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTotPeriod3     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSort           AS CHARACTER NO-UNDO.

    CREATE ttAgedReceivablesTotals.
    ASSIGN
        ttAgedReceivablesTotals.totCustNo      = ipcTotCustNo
        ttAgedReceivablesTotals.totCustName    = ipcTotCustName
        ttAgedReceivablesTotals.totSalesRep    = ipcTotSalesRep
        ttAgedReceivablesTotals.totDescription = ipcTotDescription
        ttAgedReceivablesTotals.totAmount      = ipdTotAmount
        ttAgedReceivablesTotals.totCurrent     = ipdTotCurrent
        ttAgedReceivablesTotals.totPeriodDay1  = ipdTotPeriod1
        ttAgedReceivablesTotals.totPeriodDay2  = ipdTotPeriod2
        ttAgedReceivablesTotals.totPeriodDay3  = ipdTotPeriod3
        ttAgedReceivablesTotals.xxSort         = ipcSort
        . 
END PROCEDURE.

PROCEDURE pGetTrendDays:
    DEFINE INPUT  PARAMETER ipiTrendDays AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTrendDays AS INTEGER NO-UNDO.

    DEFINE BUFFER bARInv FOR ar-inv.

    DEFINE VARIABLE iDays    AS   INTEGER      NO-UNDO.
    DEFINE VARIABLE iInvs    AS   INTEGER      NO-UNDO.
    DEFINE VARIABLE dAvgDays LIKE cust.avg-pay NO-UNDO.

    /* If zero trend days, then abort calculation. */
    IF ipiTrendDays = 0 THEN RETURN.

    ASSIGN
        iDays = 0
        iInvs = 0
        .

    FOR EACH bARInv NO-LOCK
        WHERE bARInv.company  EQ cust.company
          AND bARInv.posted   EQ YES
          AND bARInv.cust-no  EQ cust.cust-no
          AND bARInv.due      LE 0
          AND bARInv.pay-date GE (TODAY - ipiTrendDays)
        USE-INDEX posted-due
        :
        ASSIGN
            iDays = iDays + (bARInv.pay-date - bARInv.inv-date)
            iInvs = iInvs + 1
            .
    END. /*  each barinv */
    
    ASSIGN dAvgDays = iDays / iInvs. 
  
    IF dAvgDays LT 1 OR dAvgDays EQ ? THEN dAvgDays = 1.
    ASSIGN opiTrendDays = (cust.avg-pay - dAvgDays).
END PROCEDURE.

{aoa/BL/pBuildCustList.i}
