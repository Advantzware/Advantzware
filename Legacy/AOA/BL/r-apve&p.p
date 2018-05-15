/*------------------------------------------------------------------------
  File: r-apve&p.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* AP Invoice Posting.rpa */
{aoa/tempTable/ttAPInvoicePosting.i}
{aoa/tempTable/ttAPInvoicePostingGL.i}
{aoa/tempTable/ttAPInvoicePostingSummary.i}
{aoa/tempTable/ttAPInvoicePostingMsg.i}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD actnum   LIKE account.actnum
    FIELD ex-rate  LIKE currency.ex-rate INIT 1
    FIELD curr-amt LIKE ap-inv.net
    .
DEFINE TEMP-TABLE tt-ap-invl NO-UNDO
    FIELD row-id    AS ROWID
    FIELD actnum  LIKE account.actnum
    FIELD unit-pr LIKE ap-invl.unit-pr
    FIELD amt     LIKE ap-invl.amt
        INDEX row-id row-id
        .
DEFINE TEMP-TABLE tt-ap-tax NO-UNDO
    FIELD row-id     AS ROWID
    FIELD actnum   LIKE account.actnum
    FIELD amt      LIKE ap-invl.amt
    FIELD curr-amt LIKE ap-invl.amt
        INDEX row-id row-id
        .
/* Parameters Definitions */
DEFINE OUTPUT PARAMETER TABLE FOR ttAPInvoicePosting.
DEFINE OUTPUT PARAMETER TABLE FOR ttAPInvoicePostingGL.
DEFINE OUTPUT PARAMETER TABLE FOR ttAPInvoicePostingSummary.
{aoa/includes/pAPInvoicePosting.i}

/* Local Variables */
DEFINE VARIABLE iTransNum  AS INTEGER         NO-UNDO.
DEFINE VARIABLE cCurrency  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE cAPTax   LIKE account.actnum  NO-UNDO.
DEFINE VARIABLE cFrtAcct LIKE ap-ctrl.freight NO-UNDO.
DEFINE VARIABLE cAPAcct  LIKE account.actnum  NO-UNDO.
DEFINE VARIABLE cCashAcct  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE cBankCode  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE cBankAcct  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE cListFile  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE lFGPostGL  AS LOGICAL         NO-UNDO.
DEFINE VARIABLE cAuditDir  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE lFGPostDir AS LOGICAL         NO-UNDO.
DEFINE VARIABLE lInvalid   AS LOGICAL         NO-UNDO.
DEFINE VARIABLE lPostOK    AS LOGICAL         NO-UNDO.

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cocode    AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc     AS CHARACTER NO-UNDO.

DEFINE BUFFER bAPChk FOR ap-chk.

DEFINE STREAM sEditReport.

FUNCTION fErrorMsg RETURNS LOGICAL (ipcErrorMsg AS CHARACTER):
    IF ipcErrorMsg NE "" THEN DO: 
        CREATE ttAPInvoicePostingMsg.
        ttAPInvoicePostingMsg.Msg = ipcErrorMsg.
    END.
    RETURN ipcErrorMsg NE "".
END FUNCTION.

/* Business Logic Prep */
FIND FIRST ap-ctrl NO-LOCK
     WHERE ap-ctrl.company EQ ipcCompany
     NO-ERROR.
IF AVAILABLE ap-ctrl THEN
ASSIGN 
    cAPAcct   = ap-ctrl.payables
    cAPTax    = ap-ctrl.stax
    cFrtAcct  = ap-ctrl.freight
    cCashAcct = ap-ctrl.cash-act
    .
ELSE DO:
    fErrorMsg("No AP control record found").
    RETURN.
END. /* else */
RELEASE ap-ctrl.

FIND FIRST bank NO-LOCK
     WHERE bank.company EQ ipcCompany
       AND bank.actnum  EQ cCashAcct
     NO-ERROR.
IF AVAILABLE bank THEN DO:
    ASSIGN
        cBankCode = bank.bank-code
        cBankAcct = bank.actnum
        .
    RELEASE bank.
END. /* avail bank */

FIND FIRST company NO-LOCK
     WHERE company.company EQ ipcCompany
     NO-ERROR.
IF AVAILABLE company THEN
cCurrency = company.curr-code.

DO TRANSACTION:
    ASSIGN
        g_company = ipcCompany 
        cocode    = ipcCompany
        g_loc     = cLocation
        .
    {sys/inc/fgpostgl.i}
    lFGPostGL = fgpostgl NE "None".
    {sys/inc/rmpostgl.i}
    {sys/inc/postdate.i}
    {sys/inc/apsecure.i}
    {sys/inc/apautocheck.i}
    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ ipcCompany
           AND sys-ctrl.name    EQ "AUDITDIR"
         NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = ipcCompany
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails Directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS"
            .
    END. /* not avail sys-ctrl */
    cAuditDir = sys-ctrl.char-fld.
    IF LOOKUP(SUBSTR(cAuditDir,LENGTH(cAuditDir),1),"/,\") GT 0 THEN
    cAuditDir = SUBSTR(cAuditDir,1,LENGTH(cAuditDir) - 1).
    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ ipcCompany
           AND sys-ctrl.name    EQ "GLPOST"
         NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = ipcCompany
            sys-ctrl.name     = "GLPOST"
            sys-ctrl.descrip  = "Post AP Invoices within current period only? Default to NO "
            sys-ctrl.char-fld = "User Defined Password to be entered when Logical Value = YES "
            sys-ctrl.log-fld  = NO
            .
    END. /* not avail sys-ctrl */
    lFGPostDir = sys-ctrl.log-fld.
END. /* do trans */

/* ********************************************************************************************* */
/* Business Logic Main Block ******************************************************************* */
/* ********************************************************************************************* */

RUN pCheckDate (dtPostDate, OUTPUT iPeriod, OUTPUT lInvalid).
IF lInvalid THEN RETURN.
IF lFGPostDir THEN DO:
    RUN pCheckInvDate (dtStartInvoiceDate, OUTPUT lInvalid).
    IF lInvalid THEN RETURN.
    RUN pCheckInvDate (dtEndInvoiceDate, OUTPUT lInvalid).
    IF lInvalid THEN RETURN.
END. /* if lfgpostdir */
RUN pGetTransNum.
RUN pEditReport.
IF lPostOK THEN DO:
    IF lPost THEN DO: 
        RUN pPostGL.
        RUN pCopyReportToAuditDir.
        RUN pClearAP.
    END. /* lpost */
    ELSE RUN pUndoTransNum.
END. /* if lpostok */
ELSE DO:
     fErrorMsg("No Invoices available for posting.").
     RUN pUndoTransNum.
END. /* else */

FOR EACH ttAPInvoicePostingMsg:
    ttAPInvoicePostingMsg.xxID = 0.
END. /* ttAPInvoicePostingMsg */

{AOA/BL/exportTempTable.i ttAPInvoicePostingMsg}

/* ********************************************************************************************* */
/* ********************************************************************************************* */
/* ********************************************************************************************* */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pCheckDate:
    DEFINE INPUT  PARAMETER ipdtDate  AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPeriod AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplInvalid AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cErrorMsg AS CHARACTER NO-UNDO.
     
    DO TRANSACTION:
        {sys/inc/postdate.i}
    END.
  
    FIND FIRST period NO-LOCK                   
        WHERE period.company EQ ipcCompany
          AND period.pst     LE ipdtDate
          AND period.pend    GE ipdtDate
        NO-ERROR.
    IF NOT AVAILABLE period THEN DO:
        IF ipdtDate NE ? THEN
        cErrorMsg = "No defined period exists for " + STRING(ipdtDate,"99/99/9999").
        ELSE
        cErrorMsg = "No defined period exists for ?".
    END.
    ELSE IF period.pstat EQ NO THEN
         cErrorMsg = "Period for " + STRING(ipdtDate,"99/99/9999") + " is already closed".
         ELSE IF postdate-dat NE ? AND ipdtDate LE postdate-dat THEN
              cErrorMsg = "Transaction Date must be after " + STRING(ipdtDate,"99/99/9999").
    oplInvalid = fErrorMsg(cErrorMsg).
    IF oplInvalid EQ NO THEN
    opiPeriod = period.pnum.
END PROCEDURE.

PROCEDURE pCheckInvDate:
    DEFINE INPUT  PARAMETER ipdtDate   AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oplInvalid AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cErrorMsg AS CHARACTER NO-UNDO.

    FIND FIRST period NO-LOCK                   
         WHERE period.company EQ ipcCompany
           AND period.pst     LE ipdtDate
           AND period.pend    GE ipdtDate
           AND period.pnum    EQ MONTH(ipdtDate)
         NO-ERROR.
    IF NOT AVAILABLE period THEN DO:
        IF lPostOutOfPeriod EQ NO THEN
        cErrorMsg = "Can Not POST Out Of Period".
    END. /* not avail period */
    ELSE IF period.pstat EQ NO THEN DO:
        IF lPostIntoClosedPeriod EQ NO THEN
        cErrorMsg = "Period for " + STRING(ipdtDate,"99/99/9999") + " is already closed".
    END. /* else if */
    oplInvalid = fErrorMsg(cErrorMsg).
END PROCEDURE.

PROCEDURE pClearAP:
    FOR EACH ap-inv
        WHERE ap-inv.company EQ ipcCompany
          AND ap-inv.posted  EQ NO
          AND ap-inv.user-id EQ USERID("ASI")
        :
        IF NOT CAN-FIND(FIRST ap-invl
                        WHERE ap-invl.i-no EQ ap-inv.i-no) THEN
        DELETE ap-inv.
    END.
END PROCEDURE.

PROCEDURE pCopyReportToAuditDir:
    DEFINE VARIABLE cAuditFile AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE cDirLevel1 AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE cDirLevel2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE cDirLevel3 AS CHARACTER FORMAT "X(20)" NO-UNDO.

    ASSIGN 
        cListFile  = "AOA.APInvoicePostingRpt."
                   + ipcCompany       + "."
                   + STRING(ipiBatch) + "."
                   + ipcUserID        + ".dat"
        cAuditFile = cAuditDir + "\AP\VU3\Run#"
                   + STRING(iTransNum) + ".txt"
        cDirLevel1 = cAuditDir
        cDirLevel2 = cAuditDir + "\AP"
        cDirLevel3 = cAuditDir + "\AP\VU3"
        .

    OUTPUT STREAM sEditReport TO VALUE (cListFile).
    PUT STREAM sEditReport UNFORMATTED
        "AP Invoice Posting Edit Report - "
        STRING(TODAY,"99/99/9999") " @ "
        STRING(TIME,"hh:mm:ss am") " for Period "
        iPeriod
        SKIP 
        FILL("=",95)        
        SKIP. 
    FOR EACH ttAPInvoicePosting
        WHERE ttAPInvoicePosting.rowType EQ "Data"
        BREAK BY ttAPInvoicePosting.xxID
        WITH STREAM-IO WIDTH 100:
        DISPLAY STREAM sEditReport
            ttAPInvoicePosting.vendNo
            ttAPInvoicePosting.vendName
            ttAPInvoicePosting.invNo
            ttAPInvoicePosting.invDate
            ttAPInvoicePosting.dueDate
            ttAPInvoicePosting.amount
            .
        FOR EACH ttAPInvoicePostingGL
            WHERE ttAPInvoicePostingGL.xxID EQ ttAPInvoicePosting.xxID
            BREAK BY ttAPInvoicePostingGL.xxID
            WITH STREAM-IO WIDTH 100:
            DISPLAY STREAM sEditReport
                ttAPInvoicePostingGL.glAcct AT 11
                ttAPInvoicePostingGL.glDscr
                ttAPInvoicePostingGL.glAmount
                .
            ACCUM ttAPInvoicePostingGL.glAmount (TOTAL BY ttAPInvoicePostingGL.xxID). 
            IF LAST(ttAPInvoicePostingGL.xxID) THEN
            PUT STREAM sEditReport UNFORMATTED SKIP
                "* VENDOR TOTAL" TO 81
                ACCUM TOTAL BY ttAPInvoicePostingGL.xxID ttAPInvoicePostingGL.glAmount FORMAT "->,>>>,>>9.99" TO 95
                " *"
                .  
        END. /* each ttAPInvoicePostingGL */
        ACCUM ttAPInvoicePosting.amount (TOTAL).
        IF LAST(ttAPInvoicePosting.xxID) THEN
        PUT STREAM sEditReport UNFORMATTED SKIP(1) 
            "** GRAND TOTAL" TO 81
            ACCUM TOTAL ttAPInvoicePosting.amount FORMAT "->,>>>,>>9.99" TO 95
            " **"
            .
    END. /* each ttAPInvoicePosting */
    PUT STREAM sEditReport UNFORMATTED SKIP(1)
        FILL("=",133) SKIP 
        "Period " iPeriod " - Summary by Account" SKIP
        FILL("=",133)
        SKIP(1). 
    FOR EACH ttAPInvoicePostingSummary
        WHERE ttAPInvoicePostingSummary.xxID LT 999999
        BREAK BY ttAPInvoicePostingSummary.xxOrder
              BY ttAPInvoicePostingSummary.glAcct
        WITH FRAME fSummary STREAM-IO WIDTH 500 NO-BOX:
        IF FIRST(ttAPInvoicePostingSummary.xxOrder) THEN 
        VIEW STREAM sEditReport FRAME fSummary.
        IF FIRST-OF(ttAPInvoicePostingSummary.glAcct) THEN 
        PUT STREAM sEditReport UNFORMATTED "Account: " AT 1 
            ttAPInvoicePostingSummary.glAcct " - "
            ttAPInvoicePostingSummary.glDscr
            .
        DISPLAY STREAM sEditReport
            ttAPInvoicePostingSummary.poNo AT 10
            ttAPInvoicePostingSummary.invDate
            ttAPInvoicePostingSummary.vendor
            ttAPInvoicePostingSummary.invoice
            ttAPInvoicePostingSummary.lineNo
            ttAPInvoicePostingSummary.dscr
            ttAPInvoicePostingSummary.qty
            ttAPInvoicePostingSummary.unitPrice
            ttAPInvoicePostingSummary.glAmount
            .
        ACCUM ttAPInvoicePostingSummary.glAmount (TOTAL BY ttAPInvoicePostingSummary.glAcct). 
        IF LAST-OF(ttAPInvoicePostingSummary.glAcct) THEN
        PUT STREAM sEditReport UNFORMATTED SKIP
            "** TOTAL" TO 119
            ACCUM TOTAL BY ttAPInvoicePostingSummary.glAcct ttAPInvoicePostingSummary.glAmount FORMAT "->,>>>,>>9.99" TO 133
            " *"
            .  
        ACCUM ttAPInvoicePostingSummary.glAmount (TOTAL).
        IF LAST(ttAPInvoicePostingSummary.xxOrder) THEN
        PUT STREAM sEditReport UNFORMATTED SKIP(1)
            "** TOTAL for ALL ACCOUNTS" TO 119
            ACCUM TOTAL ttAPInvoicePostingSummary.glAmount FORMAT "->,>>>,>>9.99" TO 133
            " **"
            .
    END. /* each ttAPInvoicePostingSummary */
    OUTPUT STREAM sEditReport CLOSE.

    OS-COPY VALUE(cListFile) VALUE(cAuditFile).

    IF SEARCH(cAuditFile) EQ ? THEN DO:
        OS-CREATE-DIR VALUE(cDirLevel1).
        OS-CREATE-DIR VALUE(cDirLevel2).
        OS-CREATE-DIR VALUE(cDirLevel3).
        OS-COPY VALUE(cListFile) VALUE (cAuditFile).
    END. /* if search */

END PROCEDURE.

PROCEDURE pCreateManualCheck:
    DEFINE VARIABLE iCNo     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCheckNo AS INTEGER NO-UNDO.

    iCheckNo = INTEGER(ap-inv.receiver-no).

    IF CAN-FIND(FIRST ap-pay
                WHERE ap-pay.company    EQ ipcCompany
                  AND ap-pay.check-no   EQ iCheckNo
                  AND (ap-pay.check-act EQ cBankAcct
                   OR ap-pay.bank-code  EQ cBankCode)
                  AND ap-pay.posted     EQ YES) THEN DO:
        fErrorMsg("Check Number " + STRING(iCheckNo) + " has already been posted, Cannot Create Manual Check").
        LEAVE.
    END.

    FOR EACH bAPChk NO-LOCK BY bAPChk.c-no DESCENDING:
        iCNo = bAPChk.c-no.
        LEAVE.
    END.

    CREATE ap-chk.
    ASSIGN 
        ap-chk.bank-code  = cBankCode
        ap-chk.check-no   = iCheckNo
        ap-chk.man-check  = YES   
        ap-chk.check-date = ap-inv.inv-date
        ap-chk.c-no       = iCNo + 1
        ap-chk.company    = ipcCompany
        ap-chk.vend-no    = ap-inv.vend-no
        ap-chk.check-amt  = ap-inv.due.

    FIND FIRST bank
         WHERE bank.company   EQ ipcCompany
           AND bank.bank-code EQ ap-chk.bank-code
         NO-ERROR.
    IF AVAILABLE bank THEN DO:
        IF ap-chk.check-no GT bank.last-chk THEN
        bank.last-chk = ap-chk.check-no.
        ap-chk.check-act = bank.actnum.
        RELEASE bank.
    END.

    CREATE ap-sel.
    ASSIGN 
        ap-sel.company   = ipcCompany
        ap-sel.vend-no   = ap-chk.vend-no
        ap-sel.check-no  = ap-chk.check-no
        ap-sel.bank-code = ap-chk.bank-code
        ap-sel.man-check = YES
        ap-sel.pre-date  = ap-chk.check-date
        ap-sel.actnum    = IF cBankAcct NE "" THEN cBankAcct ELSE "NO Account"
        ap-sel.inv-no    = ap-inv.inv-no
        ap-sel.due-date  = ap-inv.due-date
        ap-sel.inv-bal   = ap-inv.due
        ap-sel.amt-paid  = ap-inv.due
        .
    IF ap-sel.pre-date - ap-inv.inv-date LE ap-inv.disc-days THEN
    ap-sel.disc-amt = ROUND(ap-inv.disc-% * ap-inv.net / 100,2).

    RELEASE ap-sel.
    RELEASE ap-chk.

END PROCEDURE.

PROCEDURE pEditReport:
    DEFINE VARIABLE lUpdate    AS LOGICAL         NO-UNDO.
    DEFINE VARIABLE cPONum     AS CHARACTER       NO-UNDO.
    DEFINE VARIABLE dTotTax    AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE cTaxAcct   AS CHARACTER       NO-UNDO.
    DEFINE VARIABLE iLastOne   AS INTEGER         NO-UNDO.
    DEFINE VARIABLE dJDTaxAmt  AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE dTaxRate   AS DECIMAL         NO-UNDO DECIMALS 8.
    DEFINE VARIABLE idx        AS INTEGER         NO-UNDO.
    DEFINE VARIABLE dFrtTotal  AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE iID        AS INTEGER         NO-UNDO.
    DEFINE VARIABLE iOrder     AS INTEGER         NO-UNDO.
    
    DEFINE BUFFER xap-inv FOR ap-inv.
    
    FOR EACH xap-inv NO-LOCK
        WHERE xap-inv.company  EQ ipcCompany
          AND xap-inv.posted   EQ NO
          AND xap-inv.recur    EQ NO
          AND xap-inv.stat     EQ "R"
          AND xap-inv.inv-date GE dtStartInvoiceDate
          AND xap-inv.inv-date LE dtEndInvoiceDate 
          AND xap-inv.vend-no  GE cStartVendNo
          AND xap-inv.vend-no  LE cEndVendNo
          AND xap-inv.user-id  GE cStartUserID
          AND xap-inv.user-id  LE cEndUserID
          AND CAN-FIND(FIRST ap-invl
                       WHERE ap-invl.i-no EQ xap-inv.i-no
                       USE-INDEX i-no)
          AND NOT CAN-FIND(FIRST ap-invl
                           WHERE ap-invl.i-no EQ xap-inv.i-no
                             AND ap-invl.actnum EQ ""
                           USE-INDEX i-no)
        USE-INDEX posted
        TRANSACTION:
        FIND FIRST ap-inv EXCLUSIVE-LOCK
             WHERE ROWID(ap-inv) EQ ROWID(xap-inv)
             NO-WAIT NO-ERROR.
        IF AVAILABLE ap-inv THEN DO:
            lUpdate = YES.
    
            FOR EACH ap-invl NO-LOCK
                WHERE ap-invl.i-no      EQ ap-inv.i-no,     
                FIRST po-ordl NO-LOCK
                WHERE po-ordl.company   EQ ipcCompany
                  AND po-ordl.po-no     EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                                                                ELSE ap-invl.po-no)
                  AND po-ordl.line      EQ {ap/invlline.i -1}
                  AND po-ordl.item-type EQ NO
                USE-INDEX po-no:
                 
                cPONum = TRIM(STRING(po-ordl.po-no,">>>>>>>>>>")).
                   
                FIND FIRST fg-rcpth NO-LOCK
                     WHERE fg-rcpth.company   EQ ipcCompany
                       AND fg-rcpth.i-no      EQ po-ordl.i-no
                       AND fg-rcpth.po-no     EQ cPONum
                       AND fg-rcpth.rita-code EQ "R"
                    USE-INDEX item-po NO-ERROR.
                IF NOT AVAILABLE fg-rcpth THEN DO:
                    lUpdate = NO.
                    LEAVE.
                END.  
               
                FIND FIRST fg-rcpts NO-LOCK
                    WHERE fg-rcpts.company   EQ ipcCompany
                      AND fg-rcpts.i-no      EQ po-ordl.i-no
                      AND fg-rcpts.po-no     EQ cPONum
                      AND fg-rcpts.rita-code EQ "R"
                    USE-INDEX i-no NO-ERROR.
                IF AVAILABLE fg-rcpts THEN DO:
                    lUpdate = NO. 
                    LEAVE.
                END.
            END. /* each ap-invl */
    
            IF lUpdate THEN DO:
                CREATE tt-report.
                tt-report.rec-id = RECID(ap-inv).
            END.
        END. /* avail ap-inv */
    END. /* each xap-inv */
    
    FIND CURRENT ap-inv NO-LOCK NO-ERROR.
    
    FOR EACH tt-report,
        FIRST ap-inv NO-LOCK
        WHERE RECID(ap-inv) EQ tt-report.rec-id
        BREAK BY ap-inv.vend-no
              BY ap-inv.inv-no
        :
        lPostOK = YES.
        IF FIRST-OF(ap-inv.vend-no) THEN
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ ap-inv.company
               AND vend.vend-no EQ ap-inv.vend-no
             USE-INDEX vend NO-ERROR.
        CREATE ttAPInvoicePosting.
        ASSIGN
            iID                         = iID + 1
            ttAPInvoicePosting.xxID     = iID
            ttAPInvoicePosting.vendNo   = ap-inv.vend-no
            ttAPInvoicePosting.vendName = vend.name
            ttAPInvoicePosting.invNo    = ap-inv.inv-no
            .
        RELEASE currency.
        IF cCurrency NE "" AND cCurrency NE ap-inv.curr-code[1] THEN
        FIND FIRST currency NO-LOCK
             WHERE currency.company     EQ ap-inv.company
               AND currency.c-code      EQ ap-inv.curr-code[1]
               AND currency.ar-ast-acct NE ""
               AND currency.ex-rate     GT 0
             NO-ERROR.
        IF AVAILABLE currency THEN
        ASSIGN
            tt-report.actnum  = currency.ar-ast-acct
            tt-report.ex-rate = currency.ex-rate
            .
        ASSIGN
            tt-report.curr-amt         = (ap-inv.net + ap-inv.freight) * tt-report.ex-rate
            ttAPInvoicePosting.invDate = ap-inv.inv-date
            ttAPInvoicePosting.dueDate = ap-inv.due-date
            ttAPInvoicePosting.amount  = tt-report.curr-amt
            .
        FOR EACH ap-invl NO-LOCK
            WHERE ap-invl.i-no EQ ap-inv.i-no
            USE-INDEX i-no
            :
            CREATE tt-ap-invl.
            ASSIGN
                tt-ap-invl.row-id  = ROWID(ap-invl)
                tt-ap-invl.actnum  = ap-invl.actnum
                tt-ap-invl.unit-pr = ap-invl.unit-pr
                tt-ap-invl.amt     = ap-invl.amt
                .
            IF AVAILABLE currency THEN
            ASSIGN
                tt-ap-invl.unit-pr = tt-ap-invl.unit-pr * currency.ex-rate
                tt-ap-invl.amt     = tt-ap-invl.amt     * currency.ex-rate
                .
            FIND FIRST account NO-LOCK
                 WHERE account.company EQ ipcCompany
                   AND account.actnum  EQ ap-invl.actnum
                 NO-ERROR.
            CREATE ttAPInvoicePostingGL.
            ASSIGN 
                ttAPInvoicePostingGL.xxID     = iID
                ttAPInvoicePostingGL.glAcct   = ap-invl.actnum
                ttAPInvoicePostingGL.glAmount = tt-ap-invl.amt
                ttAPInvoicePostingGL.glDscr   = IF AVAILABLE account THEN account.dscr ELSE ""
                .
        END. /* each ap-invl */
        
        ACCUM ap-inv.freight * tt-report.ex-rate (TOTAL BY ap-inv.vend-no).
    
        IF ap-inv.tax-amt NE 0 THEN DO:
            FIND FIRST stax NO-LOCK 
                 WHERE stax.company   EQ ap-inv.company
                   AND stax.tax-group EQ ap-inv.tax-gr
                 NO-ERROR.
            IF AVAILABLE stax THEN DO:
    
                ASSIGN
                    dTaxRate = 0
                    dTotTax = ap-inv.tax-amt
                    .
                DO idx = 1 TO EXTENT(stax.tax-rate1):
                    dTaxRate = dTaxRate + stax.tax-rate1[idx].
                    IF stax.tax-rate1[idx] NE 0 THEN
                    iLastOne = idx.
                END.
                
                DO idx = 1 TO EXTENT(stax.tax-rate1):
                    IF stax.tax-rate1[idx] NE 0 THEN DO:
                        FIND FIRST account NO-LOCK
                             WHERE account.company EQ ipcCompany
                               AND account.actnum  EQ stax.tax-acc1[idx]
                             NO-ERROR.
                        ASSIGN
                            cTaxAcct  = IF AVAILABLE account THEN stax.tax-acc1[idx] ELSE cAPTax
                            dJDTaxAmt = ROUND((stax.tax-rate1[idx] / dTaxRate) * ap-inv.tax-amt,2)
                            dTotTax     = dTotTax - dJDTaxAmt
                            .
                        /* add in any residual amount */
                        IF idx EQ iLastOne THEN
                        dJDTaxAmt = dJDTaxAmt + dTotTax.
                        CREATE tt-ap-tax.
                        ASSIGN
                            tt-ap-tax.row-id   = ROWID(ap-inv)
                            tt-ap-tax.actnum   = cTaxAcct
                            tt-ap-tax.amt      = dJDTaxAmt
                            tt-ap-tax.curr-amt = dJDTaxAmt * (IF AVAILABLE currency THEN currency.ex-rate ELSE 1)
                            .
                        CREATE ttAPInvoicePostingGL.
                        ASSIGN 
                            ttAPInvoicePostingGL.xxID     = iID
                            ttAPInvoicePostingGL.glAcct   = tt-ap-tax.actnum
                            ttAPInvoicePostingGL.glAmount = tt-ap-tax.curr-amt
                            .
                    END. /* if tax-rate1 ne 0 */
                END. /* do idx */
            END. /* avail stax */
            ELSE DO:
                CREATE tt-ap-tax.
                ASSIGN
                    tt-ap-tax.row-id   = ROWID(ap-inv)
                    tt-ap-tax.actnum   = cAPTax
                    tt-ap-tax.amt      = ap-inv.tax-amt
                    tt-ap-tax.curr-amt = ap-inv.tax-amt * (IF AVAILABLE currency THEN currency.ex-rate ELSE 1)
                    .
                CREATE ttAPInvoicePostingGL.
                ASSIGN 
                    ttAPInvoicePostingGL.xxID     = iID
                    ttAPInvoicePostingGL.glAcct   = tt-ap-tax.actnum
                    ttAPInvoicePostingGL.glAmount = tt-ap-tax.curr-amt
                    .
            END. /* else */
        END. /* if tax-amt ne 0 */
        IF LAST-OF(ap-inv.vend-no) THEN DO:
            IF (ACCUM TOTAL BY ap-inv.vend-no ap-inv.freight * tt-report.ex-rate) NE 0 THEN DO:
                FIND FIRST account NO-LOCK
                     WHERE account.company EQ ipcCompany
                       AND account.actnum  EQ cFrtAcct
                     NO-ERROR.
                CREATE ttAPInvoicePostingGL.
                ASSIGN 
                    ttAPInvoicePostingGL.xxID     = iID
                    ttAPInvoicePostingGL.glAcct   = cFrtAcct
                    ttAPInvoicePostingGL.glAmount = (ACCUM TOTAL BY ap-inv.vend-no ap-inv.freight * tt-report.ex-rate)
                    ttAPInvoicePostingGL.glDscr   = IF AVAILABLE account THEN account.dscr ELSE ""
                    .
            END. /* if last-of */    
        END. /* if last-of */
    END. /* each tt-report */
    
    {AOA/BL/exportTempTable.i ttAPInvoicePostingGL}
    
    FOR EACH tt-report,
        FIRST ap-inv NO-LOCK
        WHERE RECID(ap-inv) EQ tt-report.rec-id,
        FIRST vend NO-LOCK 
        WHERE vend.company EQ ipcCompany
          AND vend.vend-no EQ ap-inv.vend-no
        USE-INDEX vend    
        BREAK BY ap-inv.vend-no
        :
        FOR EACH ap-invl NO-LOCK
            WHERE ap-invl.i-no   EQ ap-inv.i-no
              AND ap-invl.posted EQ NO
            USE-INDEX i-no,
            FIRST tt-ap-invl
            WHERE tt-ap-invl.row-id EQ ROWID(ap-invl)
            :
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company EQ ipcCompany
                   AND po-ordl.po-no   EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                                                                 ELSE ap-invl.po-no)
                   AND po-ordl.line    EQ {ap/invlline.i -1}
                 USE-INDEX po-no NO-ERROR.
            IF AVAILABLE po-ordl AND po-ordl.item-type AND ap-invl.amt NE 0 AND rmpostgl THEN DO:
                FIND FIRST item
                     WHERE item.company EQ ipcCompany
                       AND item.i-no    EQ po-ordl.i-no
                     NO-ERROR.
                RELEASE costtype.
                IF AVAILABLE item THEN
                FIND FIRST costtype
                     WHERE costtype.company   EQ ipcCompany
                       AND costtype.cost-type EQ item.cost-type
                     NO-ERROR.
                IF AVAILABLE costtype AND costtype.ap-accrued NE "" THEN /* Debit RM AP Accrued */
                tt-ap-invl.actnum = costtype.ap-accrued.
            END. /* avail po-ordl */
        END. /* each ap-invl */
        IF ap-inv.freight NE 0 THEN DO:
            FIND FIRST account NO-LOCK 
                 WHERE account.company EQ ipcCompany
                   AND account.actnum  EQ cFrtAcct
                 NO-ERROR.
            CREATE ttAPInvoicePostingSummary.
            ASSIGN
                ttAPInvoicePostingSummary.xxID      = iID
                ttAPInvoicePostingSummary.xxOrder   = 1
                ttAPInvoicePostingSummary.glAcct    = cFrtAcct
                ttAPInvoicePostingSummary.invDate   = ap-inv.inv-date
                ttAPInvoicePostingSummary.vendor    = ap-inv.vend-no
                ttAPInvoicePostingSummary.invoice   = ap-inv.inv-no
                ttAPInvoicePostingSummary.lineNo    = 0
                ttAPInvoicePostingSummary.dscr      = "Freight"
                ttAPInvoicePostingSummary.qty       = 1.0
                ttAPInvoicePostingSummary.unitPrice = ap-inv.freight
                ttAPInvoicePostingSummary.glAmount  = ap-inv.freight
                ttAPInvoicePostingSummary.glDscr    = IF AVAILABLE account THEN account.dscr ELSE ""
                .                .
        END. /* freight ne 0 */
        ACCUMULATE ap-inv.freight (TOTAL).
    END.
    
    FOR EACH tt-ap-tax,
        FIRST ap-inv NO-LOCK
        WHERE ROWID(ap-inv) EQ tt-ap-tax.row-id
        BREAK BY tt-ap-tax.actnum
              BY ap-inv.inv-no
        :
        IF FIRST-OF(tt-ap-tax.actnum) THEN
        FIND FIRST account NO-LOCK 
             WHERE account.company EQ ipcCompany
               AND account.actnum  EQ tt-ap-tax.actnum
             NO-ERROR.
        CREATE ttAPInvoicePostingSummary.
        ASSIGN
            ttAPInvoicePostingSummary.xxID      = iID
            ttAPInvoicePostingSummary.xxOrder   = 2
            ttAPInvoicePostingSummary.glAcct    = tt-ap-tax.actnum
            ttAPInvoicePostingSummary.invDate   = ap-inv.inv-date
            ttAPInvoicePostingSummary.vendor    = ap-inv.vend-no
            ttAPInvoicePostingSummary.invoice   = ap-inv.inv-no
            ttAPInvoicePostingSummary.lineNo    = 0
            ttAPInvoicePostingSummary.dscr      = "Tax"
            ttAPInvoicePostingSummary.qty       = 1.0
            ttAPInvoicePostingSummary.unitPrice = tt-ap-tax.amt
            ttAPInvoicePostingSummary.glAmount  = tt-ap-tax.amt
            ttAPInvoicePostingSummary.glDscr    = IF AVAILABLE account THEN account.dscr ELSE ""
            .                .
        ACCUMULATE tt-ap-tax.amt (TOTAL).
    END. /* each tt-ap-tax */
    
    FOR EACH tt-report,
        FIRST ap-inv NO-LOCK
        WHERE RECID(ap-inv)  EQ tt-report.rec-id,    
        EACH ap-invl NO-LOCK
        WHERE ap-invl.i-no   EQ ap-inv.i-no
          AND ap-invl.posted EQ NO
        USE-INDEX i-no,
        FIRST tt-ap-invl
        WHERE tt-ap-invl.row-id EQ rowid(ap-invl)
        BREAK BY tt-ap-invl.actnum
              BY ap-inv.inv-no
              BY ap-invl.line
        :
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ ipcCompany
               AND vend.vend-no EQ ap-inv.vend-no
             USE-INDEX vend NO-ERROR.
        IF FIRST-OF(tt-ap-invl.actnum) THEN
        FIND FIRST account NO-LOCK
             WHERE account.company EQ ipcCompany
               AND account.actnum  EQ tt-ap-invl.actnum
             NO-ERROR.        
        CREATE ttAPInvoicePostingSummary.
        ASSIGN 
            ttAPInvoicePostingSummary.xxID      = iID
            ttAPInvoicePostingSummary.xxOrder   = 3
            ttAPInvoicePostingSummary.glAcct    = tt-ap-invl.actnum
            ttAPInvoicePostingSummary.poNo      = ap-invl.po-no
            ttAPInvoicePostingSummary.invDate   = ap-inv.inv-date
            ttAPInvoicePostingSummary.vendor    = ap-inv.vend-no
            ttAPInvoicePostingSummary.invoice   = ap-inv.inv-no
            ttAPInvoicePostingSummary.lineNo    = {ap/invlline.i -1}
            ttAPInvoicePostingSummary.dscr      = ap-invl.dscr
            ttAPInvoicePostingSummary.qty       = ap-invl.qty
            ttAPInvoicePostingSummary.unitPrice = ap-invl.unit-pr
            ttAPInvoicePostingSummary.glAmount  = ap-invl.amt
            ttAPInvoicePostingSummary.glDscr    = IF AVAILABLE account THEN account.dscr ELSE ""
            .                .
        ACCUMULATE ap-invl.amt (TOTAL BY tt-ap-invl.actnum).
        ACCUMULATE ap-invl.amt (TOTAL).  
    END.
    
    FOR EACH tt-report
        WHERE tt-report.actnum NE "",
        FIRST ap-inv NO-LOCK
        WHERE RECID(ap-inv) EQ tt-report.rec-id
        BREAK BY tt-report.actnum
              BY ap-inv.inv-no
        :
        FIND FIRST vend NO-LOCK 
             WHERE vend.company EQ ipcCompany
               AND vend.vend-no EQ ap-inv.vend-no
          USE-INDEX vend NO-ERROR.      
        IF FIRST-OF(tt-report.actnum) THEN
        FIND FIRST account NO-LOCK
             WHERE account.company EQ ipcCompany
               AND account.actnum  EQ tt-report.actnum
             NO-ERROR.
        CREATE ttAPInvoicePostingSummary.
        ASSIGN 
            ttAPInvoicePostingSummary.xxID      = iID
            ttAPInvoicePostingSummary.xxOrder   = 4
            ttAPInvoicePostingSummary.glAcct    = tt-report.actnum
            ttAPInvoicePostingSummary.invDate   = ap-inv.inv-date
            ttAPInvoicePostingSummary.vendor    = ap-inv.vend-no
            ttAPInvoicePostingSummary.invoice   = ap-inv.inv-no
            ttAPInvoicePostingSummary.lineNo    = 0
            ttAPInvoicePostingSummary.dscr      = ""
            ttAPInvoicePostingSummary.qty       = 1.0
            ttAPInvoicePostingSummary.unitPrice = 0.0
            ttAPInvoicePostingSummary.glAmount  = tt-report.curr-amt - (ap-inv.net + ap-inv.freight)
            ttAPInvoicePostingSummary.glDscr    = IF AVAILABLE account THEN account.dscr ELSE ""
            .                .
        ACCUMULATE tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (TOTAL BY tt-report.actnum).
        ACCUMULATE tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (TOTAL).
    END. /* last-of */
    dFrtTotal = (ACCUM TOTAL ap-inv.freight).
    
    {AOA/BL/exportTempTable.i ttAPInvoicePostingSummary}
    
END PROCEDURE.

PROCEDURE pGetTransNum:
    /** get next g/l trans. posting number **/
    DO TRANSACTION:
        loop:
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                 WHERE gl-ctrl.company EQ ipcCompany
                 NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN DO:
                ASSIGN 
                    iTransNum     = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = iTransNum
                    .
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE loop.
            END. /* avail gl-ctrl */
        END. /* repeat */
    END. /* do trans */
END PROCEDURE.

PROCEDURE pPostGL:
    DEFINE VARIABLE dFrtTotal   AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE dAmount     AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE dAmt        AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE lUpdate     AS LOGICAL         NO-UNDO.
    DEFINE VARIABLE cPONum    LIKE fg-rcpth.po-no  NO-UNDO.
    DEFINE VARIABLE dTotalMSF LIKE ap-invl.amt-msf NO-UNDO.
    DEFINE VARIABLE iQty      LIKE ap-invl.qty     NO-UNDO.
    DEFINE VARIABLE iQty1     LIKE iQty            NO-UNDO.
    DEFINE VARIABLE iQty2     LIKE iQty            NO-UNDO.
    DEFINE VARIABLE iQty3     LIKE iQty            NO-UNDO.
    DEFINE VARIABLE dCost     LIKE fg-rdtlh.cost   NO-UNDO.
    DEFINE VARIABLE dDepth      AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE lRcpth      AS LOGICAL         NO-UNDO.
    DEFINE VARIABLE v-wid       AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE v-len       AS DECIMAL         NO-UNDO.
    DEFINE VARIABLE v-bwt       AS DECIMAL         NO-UNDO.

    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    postit:
    DO TRANSACTION ON ERROR UNDO postit:
        dAmount = 0.
        FOR EACH tt-report
            WHERE CAN-FIND(FIRST ap-inv
                           WHERE RECID(ap-inv) EQ tt-report.rec-id
                             AND ap-inv.posted EQ NO)
            BREAK BY tt-report.actnum
            :
            FIND FIRST ap-inv EXCLUSIVE-LOCK 
                 WHERE RECID(ap-inv) EQ tt-report.rec-id
                 NO-ERROR NO-WAIT.
            IF NOT AVAILABLE ap-inv THEN DO:
                fErrorMsg("Unable to Post due to Invoice Record being Locked").
                UNDO postit, LEAVE postit.
            END. /* not avail ap-inv */
            ap-inv.period = iPeriod.
            FIND FIRST vend NO-LOCK
                WHERE vend.company EQ ipcCompany
                  AND vend.vend-no EQ ap-inv.vend-no
                USE-INDEX vend.
            FOR EACH ap-invl
                WHERE ap-invl.i-no EQ ap-inv.i-no,
                FIRST tt-ap-invl WHERE tt-ap-invl.row-id EQ rowid(ap-invl)
                :
                CREATE gltrans.
                ASSIGN
                    dAmt            = dAmt + ap-invl.amt
                    dAmount         = dAmount + ap-invl.amt
                    dTotalMSF       = dTotalMSF + ap-invl.amt-msf
                    gltrans.company = ipcCompany
                    gltrans.actnum  = tt-ap-invl.actnum
                    gltrans.jrnl    = "ACPAY"
                    gltrans.tr-dscr = vend.name  + "  " + string(ap-inv.inv-date)
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = tt-ap-invl.amt
                    gltrans.trnum   = iTransNum
                    gltrans.period  = iPeriod
                    ap-invl.posted  = YES
                    .    
                RELEASE gltrans.
                FIND FIRST po-ordl
                     WHERE po-ordl.company EQ ipcCompany
                       AND po-ordl.po-no   EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                                                                     ELSE ap-invl.po-no)
                       AND po-ordl.line    EQ {ap/invlline.i -1}
                     USE-INDEX po-no NO-ERROR.
                IF AVAILABLE po-ordl THEN DO:
                    FIND FIRST reftable NO-LOCK 
                         {ap/apreftbw.i po-ordl.po-no}
                           AND reftable.code2 EQ string(ap-invl.i-no,"9999999999")
                         NO-ERROR.
                    IF NOT AVAILABLE reftable THEN DO:
                        {ap/addreftb.i po-ordl.po-no}
                        RELEASE reftable.
                    END. /* not avail reftable */
                    po-ordl.t-inv-qty = po-ordl.t-inv-qty + ap-invl.qty.
                    RELEASE item.
                    IF po-ordl.item-type THEN
                        FIND FIRST ITEM NO-LOCK
                             WHERE item.company EQ po-ordl.company
                               AND item.i-no    EQ po-ordl.i-no
                             NO-ERROR.
                    IF AVAILABLE item AND
                       item.i-code EQ "R" AND
                       INDEX("MOXY789@",ITEM.mat-type) GT 0 AND
                       item.stocked EQ NO THEN DO:
                        lRcpth = NO.
                        FOR EACH rm-rcpth NO-LOCK
                            WHERE rm-rcpth.company   EQ po-ordl.company
                              AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
                              AND rm-rcpth.i-no      EQ po-ordl.i-no
                              AND rm-rcpth.rita-code EQ "R",
                            EACH rm-rdtlh NO-LOCK
                            WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
                              AND rm-rdtlh.job-no  EQ po-ordl.job-no
                              AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
                              AND rm-rdtlh.s-num   EQ po-ordl.s-num
                            BREAK BY rm-rcpth.company
                            :
                            IF FIRST(rm-rcpth.company) THEN
                            ASSIGN
                                po-ordl.t-rec-qty = 0
                                lRcpth          = YES
                                .
                            iQty = rm-rdtlh.qty.
                            IF rm-rcpth.pur-uom NE po-ordl.cons-uom THEN
                            RUN sys/ref/convquom.p (rm-rcpth.pur-uom, po-ordl.cons-uom,
                                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                iQty, OUTPUT iQty).
                            IF po-ordl.cons-uom EQ "EA" THEN DO:
                                {sys/inc/roundup.i iQty}
                            END.
                            po-ordl.t-rec-qty = po-ordl.t-rec-qty + iQty.
                        END. /* each rm-rcpth */
                        IF NOT lRcpth THEN DO:
                            dDepth = item.s-dep.          
                            {po/pol-dims.i}
                            iQty = ap-invl.qty.
                            IF po-ordl.pr-qty-uom NE po-ordl.cons-uom THEN
                            RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.cons-uom,
                                v-bwt, v-len, v-wid, dDepth,
                                iQty, OUTPUT iQty).
                            po-ordl.t-rec-qty = po-ordl.t-rec-qty + iQty.
                        END. /* not lRcpth */
                        RUN rm/polclose.p (ROWID(po-ordl), ap-invl.qty, po-ordl.pr-qty-uom).
                    END. /* avail item */
                    RUN po/closechk.p (ROWID(po-ordl)).
                    /* Ensure receipts = payables */
                    IF NOT po-ordl.item-type AND lFGPostGL THEN DO:
                        RELEASE prod.
                        FIND FIRST itemfg
                             WHERE itemfg.company EQ ipcCompany
                               AND itemfg.i-no    EQ po-ordl.i-no
                             NO-ERROR.
                        IF AVAILABLE itemfg THEN
                        FIND FIRST prodl NO-LOCK
                             WHERE prodl.company EQ ipcCompany
                               AND prodl.procat  EQ itemfg.procat
                               AND CAN-FIND(FIRST prod
                                            WHERE prod.company EQ ipcCompany
                                              AND prod.prolin  EQ prodl.prolin)
                            NO-ERROR.
                        IF AVAILABLE prodl THEN
                        FIND FIRST prod NO-LOCK
                             WHERE prod.company EQ ipcCompany
                               AND prod.prolin  EQ prodl.prolin
                             NO-ERROR.
                        IF AVAILABLE itemfg THEN DO:
                            RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                ap-invl.qty, OUTPUT iQty1).
                            ASSIGN
                                cPONum = TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
                                iQty   = 0
                                dCost  = ap-invl.amt / (iQty1 / 1000)
                                .
                            FOR EACH fg-rcpth
                                WHERE fg-rcpth.company   EQ ipcCompany
                                  AND fg-rcpth.i-no      EQ po-ordl.i-no
                                  AND fg-rcpth.po-no     EQ cPONum
                                  AND fg-rcpth.rita-code EQ "R"
                                  AND ((fg-rcpth.b-no    EQ ap-invl.i-no AND lFGPostGL)
                                   OR  (fg-rcpth.b-no    EQ 0        AND NOT lFGPostGL))
                                USE-INDEX item-po,
                                EACH fg-rdtlh
                                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                BREAK BY fg-rcpth.trans-date
                                      BY fg-rdtlh.trans-time
                                      BY fg-rcpth.r-no
                                      BY RECID(fg-rdtlh)
                                :
                                ASSIGN
                                    iQty         = iQty + fg-rdtlh.qty
                                    fg-rdtlh.cost = dCost
                                    fg-rcpth.b-no = ap-invl.i-no
                                    .
                                IF LAST(fg-rcpth.trans-date) AND iQty NE iQty1 THEN DO:
/*                                    FIND FIRST fg-bin                                */
/*                                         WHERE fg-bin.company EQ ipcCompany          */
/*                                           AND fg-bin.i-no    EQ fg-rcpth.i-no       */
/*                                           AND fg-bin.loc     EQ fg-rdtlh.loc        */
/*                                           AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin    */
/*                                           AND fg-bin.tag     EQ fg-rdtlh.tag        */
/*                                           AND fg-bin.job-no  EQ fg-rcpth.job-no     */
/*                                           AND fg-bin.job-no2 EQ fg-rcpth.job-no2    */
/*                                         NO-ERROR.                                   */
/*                                    IF NOT AVAILABLE fg-bin THEN DO:                 */
/*                                        CREATE fg-bin.                               */
/*                                        ASSIGN                                       */
/*                                            fg-bin.company      = fg-rdtlh.company   */
/*                                            fg-bin.job-no       = fg-rcpth.job-no    */
/*                                            fg-bin.job-no2      = fg-rcpth.job-no2   */
/*                                            fg-bin.loc          = fg-rdtlh.loc       */
/*                                            fg-bin.loc-bin      = fg-rdtlh.loc-bin   */
/*                                            fg-bin.tag          = fg-rdtlh.tag       */
/*                                            fg-bin.i-no         = fg-rcpth.i-no      */
/*                                            fg-bin.case-count   = itemfg.case-count  */
/*                                            fg-bin.cases-unit   = 1                  */
/*                                            fg-bin.aging-date   = fg-rcpth.trans-date*/
/*                                            fg-bin.pur-uom      = "M"                */
/*                                            fg-bin.std-tot-cost = fg-rdtlh.cost      */
/*                                            fg-bin.std-mat-cost = fg-bin.std-tot-cost*/
/*                                            fg-bin.std-lab-cost = 0                  */
/*                                            fg-bin.std-var-cost = 0                  */
/*                                            fg-bin.std-fix-cost = 0                  */
/*                                            .                                        */
/*                                    END. /* not avail fg-bin */                      */
                                    ASSIGN
                                        iQty1         = iQty1 - iQty
                                        fg-rdtlh.qty   = fg-rdtlh.qty + iQty1
                                        fg-rdtlh.cases = trunc(fg-rdtlh.qty / fg-rdtlh.qty-case,0)
/*                                        fg-bin.qty     = fg-bin.qty + iQty1          */
                                        itemfg.q-onh   = itemfg.q-onh + iQty1
                                        .
                                    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT fg-rdtlh.loc).
                                    FIND FIRST itemfg-loc EXCLUSIVE-LOCK 
                                         WHERE itemfg-loc.company EQ itemfg.company
                                           AND itemfg-loc.i-no    EQ itemfg.i-no
                                           AND itemfg-loc.loc     EQ fg-rdtlh.loc
                                         NO-ERROR.
                                    IF AVAILABLE itemfg-loc THEN
                                    itemfg-loc.q-onh = itemfg-loc.q-onh + iQty1.
                                    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                                END. /* last trans-date */
                            END. /* each fg-rcpth */
/*                            FOR EACH fg-rcpth                                                                                   */
/*                                WHERE fg-rcpth.company   EQ ipcCompany                                                          */
/*                                  AND fg-rcpth.i-no      EQ po-ordl.i-no                                                        */
/*                                  AND fg-rcpth.po-no     EQ cPONum                                                              */
/*                                  AND fg-rcpth.rita-code EQ "R"                                                                 */
/*                                USE-INDEX item-po NO-LOCK,                                                                      */
/*                                EACH fg-rdtlh                                                                                   */
/*                                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no                                                            */
/*                                BREAK BY fg-rcpth.job-no                                                                        */
/*                                      BY fg-rcpth.job-no2                                                                       */
/*                                      BY fg-rdtlh.loc                                                                           */
/*                                      BY fg-rdtlh.loc-bin                                                                       */
/*                                      BY fg-rdtlh.tag                                                                           */
/*                                :                                                                                               */
/*                                IF FIRST-OF(fg-rdtlh.tag) THEN                                                                  */
/*                                ASSIGN                                                                                          */
/*                                    iQty  = 0                                                                                   */
/*                                    dCost = 0                                                                                   */
/*                                    .                                                                                           */
/*                                ASSIGN                                                                                          */
/*                                    iQty  = iQty + fg-rdtlh.qty                                                                 */
/*                                    dCost = dCost + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost)                                       */
/*                                    .                                                                                           */
/*                                IF LAST-OF(fg-rdtlh.tag) THEN DO:                                                               */
/*                                    FIND FIRST fg-bin                                                                           */
/*                                         WHERE fg-bin.company EQ ipcCompany                                                     */
/*                                           AND fg-bin.i-no    EQ fg-rcpth.i-no                                                  */
/*                                           AND fg-bin.loc     EQ fg-rdtlh.loc                                                   */
/*                                           AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin                                               */
/*                                           AND fg-bin.tag     EQ fg-rdtlh.tag                                                   */
/*                                           AND fg-bin.job-no  EQ fg-rcpth.job-no                                                */
/*                                           AND fg-bin.job-no2 EQ fg-rcpth.job-no2                                               */
/*                                         NO-ERROR.                                                                              */
/*                                    IF NOT AVAILABLE fg-bin THEN DO:                                                            */
/*                                        CREATE fg-bin.                                                                          */
/*                                        ASSIGN                                                                                  */
/*                                            fg-bin.company      = fg-rdtlh.company                                              */
/*                                            fg-bin.job-no       = fg-rcpth.job-no                                               */
/*                                            fg-bin.job-no2      = fg-rcpth.job-no2                                              */
/*                                            fg-bin.loc          = fg-rdtlh.loc                                                  */
/*                                            fg-bin.loc-bin      = fg-rdtlh.loc-bin                                              */
/*                                            fg-bin.tag          = fg-rdtlh.tag                                                  */
/*                                            fg-bin.i-no         = fg-rcpth.i-no                                                 */
/*                                            fg-bin.case-count   = itemfg.case-count                                             */
/*                                            fg-bin.cases-unit   = 1                                                             */
/*                                            fg-bin.aging-date   = fg-rcpth.trans-date                                           */
/*                                            fg-bin.pur-uom      = "M"                                                           */
/*                                            fg-bin.std-tot-cost = fg-rdtlh.cost                                                 */
/*                                            fg-bin.std-mat-cost = fg-bin.std-tot-cost                                           */
/*                                            fg-bin.std-lab-cost = 0                                                             */
/*                                            fg-bin.std-var-cost = 0                                                             */
/*                                            fg-bin.std-fix-cost = 0                                                             */
/*                                            .                                                                                   */
/*                                    END. /* not avail fg-bin */                                                                 */
/*                                    dCost = dCost / (iQty / 1000).                                                              */
/*                                    IF fg-bin.pur-uom EQ "M" THEN                                                               */
/*                                    fg-bin.std-tot-cost = dCost.                                                                */
/*                                    ELSE                                                                                        */
/*                                    RUN sys/ref/convcuom.p ("M", fg-bin.pur-uom, 0, 0, 0, 0, dCost, OUTPUT fg-bin.std-tot-cost).*/
/*                                    ASSIGN                                                                                      */
/*                                        fg-bin.std-mat-cost = fg-bin.std-tot-cost                                               */
/*                                        fg-bin.std-lab-cost = 0                                                                 */
/*                                        fg-bin.std-var-cost = 0                                                                 */
/*                                        fg-bin.std-fix-cost = 0                                                                 */
/*                                        .                                                                                       */
/*                                END. /* last-of tag */                                                                          */
/*                            END. /* each fg-rcpth */                                                                            */
                        END. /* avail itemfg */
                        RUN fg/updfgcst.p (po-ordl.i-no).
                    END. /* if not po-ordl.item-type */
                END. /* avail po-ordl */
                IF ap-invl.actnum NE "" THEN
                    FIND FIRST bank EXCLUSIVE-LOCK
                         WHERE bank.company EQ ipcCompany
                          AND bank.actnum  EQ ap-invl.actnum
                         NO-ERROR.
                IF AVAILABLE bank THEN
                bank.bal = bank.bal + ap-invl.amt.
                RELEASE bank.
            END.  /* each line */
            FIND FIRST vend EXCLUSIVE-LOCK
                WHERE vend.company EQ ipcCompany
                  AND vend.vend-no EQ ap-inv.vend-no
                USE-INDEX vend.
            ASSIGN
                vend.purch[iPeriod]   = vend.purch[iPeriod] + dAmt
                vend.n-purch[iPeriod] = vend.n-purch[iPeriod] + 1
                vend.purch[13]        = vend.purch[13] + dAmt
                vend.n-purch[13]      = vend.n-purch[13] + 1
                vend.ptd-msf[iPeriod] = vend.ptd-msf[iPeriod] + dTotalMSF
                vend.ytd-msf          = vend.ytd-msf + dTotalMSF
                vend.acc-bal          = vend.acc-bal + dAmt + ap-inv.tax-amt
                .
            IF vend.acc-bal GE vend.hibal THEN
                ASSIGN
                    vend.hibal      = vend.acc-bal
                    vend.hibal-date = ap-inv.inv-date
                    .
            FIND CURRENT vend     NO-LOCK NO-ERROR.
            FIND CURRENT po-ordl  NO-LOCK NO-ERROR.
            FIND CURRENT itemfg   NO-LOCK NO-ERROR.
/*            FIND CURRENT fg-bin   NO-LOCK NO-ERROR.*/
            FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
            FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.
            CREATE ap-ledger.
            ASSIGN
                ap-ledger.company  = ipcCompany
                ap-ledger.vend-no  = ap-inv.vend-no
                ap-ledger.amt      = ap-inv.net
                ap-ledger.refnum   = "INV# " + ap-inv.inv-no
                ap-ledger.ref-date = ap-inv.inv-date
                ap-ledger.trnum    = iTransNum
                ap-ledger.period   = iPeriod
                ap-ledger.tr-date  = dtPostDate
                .
            RELEASE ap-ledger.
            ASSIGN
                dAmt          = 0
                ap-inv.posted = YES
                .
            IF apautocheck-log AND ap-inv.receiver-no NE "0" THEN
            RUN pCreateManualCheck.
            ACCUM ap-inv.net (TOTAL BY tt-report.actnum).
            ACCUM tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (TOTAL BY tt-report.actnum).
            ACCUM ap-inv.freight * tt-report.ex-rate (TOTAL).
            IF LAST-OF(tt-report.actnum) AND
                tt-report.actnum NE ""   AND
                (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight))
                NE 0    THEN DO:
                CREATE gltrans.
                ASSIGN
                    gltrans.company = ipcCompany
                    gltrans.actnum  = tt-report.actnum
                    gltrans.jrnl    = "ACPAY"
                    gltrans.tr-dscr = "ACCOUNTS PAYABLE CURRENCY GAIN/LOSS"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight)) * -1
                    gltrans.period  = iPeriod
                    gltrans.trnum   = iTransNum
                    .
                RELEASE gltrans.
            END. /* last-of actnum */
        END. /* for each ap-inv */
        dAmount = dAmount + dFrtTotal.
        IF dFrtTotal NE 0 THEN DO:
            CREATE gltrans.
            ASSIGN
                gltrans.company = ipcCompany
                gltrans.actnum  = cFrtAcct
                gltrans.jrnl    = "ACPAY"
                gltrans.tr-dscr = "ACCOUNTS PAYABLE FREIGHT"
                gltrans.tr-date = dtPostDate
                gltrans.tr-amt  = (ACCUM TOTAL ap-inv.freight * tt-report.ex-rate)
                gltrans.period  = iPeriod
                gltrans.trnum   = iTransNum
                .
            RELEASE gltrans.
        END. /* dfrttotal ne 0 */
        FOR EACH tt-ap-tax
            BREAK BY tt-ap-tax.actnum:
            ACCUM tt-ap-tax.curr-amt (TOTAL BY tt-ap-tax.actnum).
            dAmount = dAmount + tt-ap-tax.amt.
            IF LAST-OF(tt-ap-tax.actnum) THEN DO:
                CREATE gltrans.
                ASSIGN
                    gltrans.company = ipcCompany
                    gltrans.actnum  = tt-ap-tax.actnum
                    gltrans.jrnl    = "ACPAY"
                    gltrans.tr-dscr = "ACCOUNTS PAYABLE TAX"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = (ACCUM TOTAL BY tt-ap-tax.actnum tt-ap-tax.curr-amt)
                    gltrans.period  = iPeriod
                    gltrans.trnum   = iTransNum
                    .
                RELEASE gltrans.
            END. /* last-of actnum */
        END. /* each tt-ap-tax */
        CREATE gltrans.
        ASSIGN
            gltrans.company = ipcCompany
            gltrans.actnum  = cAPAcct
            gltrans.jrnl    = "ACPAY"
            gltrans.tr-dscr = "ACCOUNTS PAYABLE INVOICE"
            gltrans.tr-date = dtPostDate
            gltrans.tr-amt  = - dAmount
            gltrans.period  = iPeriod
            gltrans.trnum   = iTransNum
            .
        RELEASE gltrans.
    END. /* postit: transaction */
END PROCEDURE.

PROCEDURE pUndoTransNum:
    REPEAT:
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
             WHERE gl-ctrl.company EQ ipcCompany
             NO-ERROR NO-WAIT.
        IF AVAILABLE gl-ctrl THEN DO:
            IF gl-ctrl.trnum EQ iTransNum THEN
            gl-ctrl.trnum = iTransNum - 1.
            RELEASE gl-ctrl.
            LEAVE.
        END. /* avail gl-ctrl */
    END. /* repeat */
END PROCEDURE.
