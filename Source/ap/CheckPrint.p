
/*------------------------------------------------------------------------
    File        : CheckPrint.p
    Purpose     : 

    Syntax      :

    Description : Prints checks when NK1 chkfmt = "Configurable"

    Author(s)   : BV
    Created     : Tue Feb 18 22:25:03 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtCheckDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcBankID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcVendorIDStart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcVendorIDEnd AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiCheckNumber AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iplSample AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplPreview AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

{ap/CheckPrintDefs.i}
DEFINE VARIABLE hdOutputProcs AS HANDLE.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fFormatDate RETURNS CHARACTER PRIVATE 
    (ipdtDate AS DATE,
    ipcFormat AS CHARACTER) FORWARD.

FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    (ipdNumber AS DECIMAL,
    ipiLeftDigits AS INTEGER,
    ipiRightDigits AS INTEGER,
    iplComma AS LOGICAL) FORWARD.

FUNCTION fFormatString RETURNS CHARACTER PRIVATE
    (ipcString AS CHARACTER,
    ipiCharacters AS INTEGER) FORWARD.
    
/* ***************************  Main Block  *************************** */
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutputProcs).
RUN pBuildCheckConfig(ipcCompany, ipcFile).
FIND FIRST ttCheckConfig NO-LOCK NO-ERROR.
IF AVAILABLE ttCheckConfig THEN 
DO: 
    RUN pBuildCheckData(BUFFER ttCheckConfig, ipdtCheckDate, ipcBankID, ipcVendorIDStart, ipcVendorIDEnd, ipiCheckNumber, iplSample).
    IF CAN-FIND(FIRST ttCheck) THEN DO:
        RUN Output_InitializeXprint(ttCheckConfig.outputFile, iplPreview, YES, ttCheckConfig.defaultFont, ttCheckConfig.defaultFontSize,"") .
        RUN pPrintChecks(BUFFER ttCheckConfig).
        RUN Output_Close.
        RUN Output_PrintXprintFile(ttCheckConfig.outputFile).
        IF ttCheckConfig.stubUseSupplementalReg THEN 
            RUN pPrintSupplementalRegister(BUFFER ttCheckConfig, iplPreview).
        IF NOT iplSample THEN RUN pUpdateData(ipcBankID).
    END.
    ELSE 
        MESSAGE "No Checks are found for printing.  Please build the check selection list first."
        VIEW-AS ALERT-BOX TITLE "No Checks".
END.
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutputProcs).
DELETE OBJECT hdOutputProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddCheck PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given ap-chk and vend buffer, create ttCheck and return writable buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ap-chk FOR ap-chk.
    DEFINE PARAMETER BUFFER ipbf-vend   FOR vend.
    DEFINE INPUT PARAMETER ipiCheckNumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtCheckDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyLine1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyLine2 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyLine3 AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttCheck FOR ttCheck.
    
    DEFINE VARIABLE lUseRemit AS LOGICAL NO-UNDO.
    
    CREATE opbf-ttCheck.
    ASSIGN 
        opbf-ttCheck.amount       = ipbf-ap-chk.check-amt
        opbf-ttCheck.ap-chkRowid  = ROWID(ipbf-ap-chk)
        opbf-ttCheck.checkNo      = ipiCheckNumber
        opbf-ttCheck.companyName  = ipcCompanyName
        opbf-ttCheck.companyLine1 = ipcCompanyLine1
        opbf-ttCheck.companyLine2 = ipcCompanyLine2
        opbf-ttCheck.companyLine3 = ipcCompanyLine3
        opbf-ttCheck.invoiceCount = 0
        lUseRemit                 = ipbf-vend.r-add1 NE "" OR ipbf-vend.r-add2 NE ""
        opbf-ttCheck.remitToName  = IF lUseRemit THEN ipbf-vend.remit ELSE ipbf-vend.name
        opbf-ttCheck.remitToLine1 = IF lUseRemit THEN ipbf-vend.r-add1 ELSE ipbf-vend.add1
        opbf-ttCheck.remitToLine2 = IF lUseRemit THEN ipbf-vend.r-add2 ELSE ipbf-vend.add2
        opbf-ttCheck.remitToLine3 = IF lUseRemit THEN ipbf-vend.r-city + ", " + ipbf-vend.r-state + " " + ipbf-vend.r-zip ELSE ipbf-vend.city + ", " + ipbf-vend.state + " " + ipbf-vend.zip
        opbf-ttCheck.payDate      = ipdtCheckDate
        opbf-ttCheck.memo         = ipbf-ap-chk.check-memo
        opbf-ttCheck.vendorID     = ipbf-ap-chk.vend-no 
        .

END PROCEDURE.

PROCEDURE pBuildCheckConfig PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds the configuration for the check print process
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lLoaded AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCheckConfig.
    
    RUN pLoadConfig(ipcCompany, TEMP-TABLE ttCheckConfig:HANDLE, OUTPUT lLoaded).
    
    IF NOT lLoaded THEN 
    DO:
        CREATE ttCheckConfig.
        ASSIGN 
            ttCheckConfig.company = ipcCompany
            .
    END.
    ELSE 
        FIND FIRST ttCheckConfig EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAILABLE ttCheckConfig THEN 
        IF ipcFile EQ "" THEN 
            ttCheckConfig.outputFile = "C:\tmp\CheckTest.xpr".
        ELSE 
            ttCheckConfig.outputFile = ipcFile.
        
//        RUN Output_TempTableToJSON(TEMP-TABLE ttCheckConfig:HANDLE, "C:\temp\CheckConfig.json").

END PROCEDURE.

PROCEDURE pBuildCheckData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a company, builds the check data from the DB
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCheckConfig FOR ttCheckConfig.
    DEFINE INPUT PARAMETER ipdtCheckDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcBankID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorIDStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorIDEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCheckNumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplSample AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-ttCheck FOR ttCheck.
    
    DEFINE VARIABLE iCheckNumber  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCompanyName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dTotalDisc    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalNet     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalGross   AS DECIMAL   NO-UNDO.
            
    EMPTY TEMP-TABLE ttCheck.
    EMPTY TEMP-TABLE ttCheckInvoice.
    
    IF iplSample THEN 
    DO:
        RUN pBuildCheckDataSample (BUFFER ipbf-ttCheckConfig, ipiCheckNumber).  /*Build Sample Check Data*/
    END.
    ELSE 
    DO:
        IF ipiCheckNumber EQ 0 THEN 
            FIND FIRST bank NO-LOCK 
                WHERE bank.company EQ ipbf-ttCheckConfig.company
                AND bank.bank-code EQ ipcBankID
                NO-ERROR.
        IF AVAILABLE bank THEN 
            iCheckNumber = bank.last-chk.
        ELSE 
            iCheckNumber = ipiCheckNumber - 1.
        FIND FIRST company NO-LOCK 
            WHERE company.company EQ ipbf-ttCheckConfig.company
            NO-ERROR.
        IF AVAILABLE company THEN 
            ASSIGN 
                cCompanyName  = company.name
                cCompanyLine1 = company.addr[1]
                cCompanyLine2 = company.addr[2]
                cCompanyLine3 = company.city + ", " + company.state + " " + company.zip
                .
        FOR EACH ap-chk NO-LOCK 
            WHERE ap-chk.company EQ ipbf-ttCheckConfig.company
            AND ap-chk.vend-no GE ipcVendorIDStart
            AND ap-chk.vend-no LE ipcVendorIDEnd
            AND ap-chk.man-check EQ NO
            AND CAN-FIND(FIRST ap-sel
            WHERE ap-sel.company   EQ ap-chk.company
            AND ap-sel.vend-no   EQ ap-chk.vend-no
            AND ap-sel.man-check EQ NO),
            FIRST vend NO-LOCK
            WHERE vend.company EQ ap-chk.company
            AND vend.vend-no EQ ap-chk.vend-no
            BREAK 
//            BY (IF v-sort-name THEN vend.name ELSE "")
            BY ap-chk.vend-no:
            
            ASSIGN 
                dTotalGross  = 0
                dTotalNet    = 0
                dTotalDisc   = 0
                iCheckNumber = iCheckNumber + 1
                .  
            RUN pAddCheck(BUFFER ap-chk, BUFFER vend, iCheckNumber, ipdtCheckDate, cCompanyName, cCompanyLine1, cCompanyLine2, cCompanyLine3, BUFFER bf-ttCheck).
             
            FOR EACH ap-sel NO-LOCK                                           
                WHERE ap-sel.company EQ ap-chk.company                        
                AND ap-sel.vend-no EQ ap-chk.vend-no             
                AND ap-sel.man-check EQ NO,
                FIRST ap-inv NO-LOCK
                WHERE ap-inv.company EQ ap-sel.company
                AND ap-inv.vend-no EQ ap-sel.vend-no
                AND ap-inv.inv-no  EQ ap-sel.inv-no
                USE-INDEX inv-no                                  
                BREAK BY ap-sel.inv-no:                              
                FIND FIRST ap-invl
                    WHERE ap-invl.company EQ ap-inv.company
                    AND ap-invl.i-no EQ ap-inv.i-no
                    USE-INDEX i-no 
                    NO-ERROR.
                CREATE ttCheckInvoice.
                ASSIGN 
                    ttCheckInvoice.checkNo            = iCheckNumber
                    ttCheckInvoice.ap-selRowid        = ROWID(ap-sel)
                    ttCheckInvoice.ap-invRowid        = ROWID(ap-inv)
                    ttCheckInvoice.ap-chkRowid        = ROWID(ap-chk)
                    ttCheckInvoice.vendorID           = ap-sel.vend-no
                    ttCheckInvoice.invoiceAmtDiscount = ap-sel.disc-amt
                    ttCheckInvoice.invoiceAmtNet      = ap-sel.amt-paid
                    ttCheckInvoice.invoiceAmtGross    = IF ap-inv.gross GT 0 THEN ap-inv.gross ELSE ap-inv.net
                    ttCheckInvoice.invoiceNumber      = ap-inv.inv-no
                    ttCheckInvoice.invoiceDate        = ap-inv.inv-date
                    ttCheckInvoice.invoicePO          = IF AVAILABLE ap-invl THEN ap-invl.po-no ELSE ap-inv.po-no
                    ttCheckInvoice.sequence           = bf-ttCheck.invoiceCount + 1                                                          
                    bf-ttCheck.invoiceCount           = bf-ttCheck.invoiceCount + 1
                    dTotalGross                       = dTotalGross + ttCheckInvoice.invoiceAmtGross
                    dTotalNet                         = dTotalNet + ttCheckInvoice.invoiceAmtNet
                    dTotalDisc                        = dTotalDisc + ttCheckInvoice.invoiceAmtDisc
                    .                                         
                IF bf-ttCheck.invoiceCount EQ ipbf-ttCheckConfig.stubInvoiceLines * ipbf-ttCheckConfig.stubInvoiceColumns
                    AND NOT LAST(ap-sel.inv-no) THEN 
                DO:
                    IF ipbf-ttCheckConfig.stubUseSupplementalReg THEN 
                    DO: 
                        bf-ttCheck.useSupplemental = YES.
                    END. /*supplemental*/
                    ELSE 
                    DO: /*Void last check and make a new one*/
                        bf-ttCheck.isVoid = YES.
                        iCheckNumber = iCheckNumber + 1.
                        RUN pAddCheck(BUFFER ap-chk, BUFFER vend, iCheckNumber, ipdtCheckDate, cCompanyName, cCompanyLine1, cCompanyLine2, cCompanyLine3, BUFFER bf-ttCheck).           
                        .
                    END. /*Not supplementatl - create voind*/
                END. /*Invoice count eq max lines*/ 
               
            END. /*each ap-sel*/
            ASSIGN 
                bf-ttCheck.amountDisc  = dTotalDisc
                bf-ttCheck.amountGross = dTotalGross
                bf-ttCheck.amount      = dTotalNet
                .
        END. /*each ap-Chk*/                                     
    END.  /*Not iplSample*/
    
END PROCEDURE.

PROCEDURE pBuildCheckDataSample PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds sample data for test printing
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCheckConfig FOR ttCheckConfig.
    DEFINE INPUT PARAMETER ipiCheckNumber AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCheckAmount1 AS DECIMAL INITIAL 51231.32.
    DEFINE VARIABLE dCheckAmount2 AS DECIMAL INITIAL 13123.44.
    
    CREATE ttCheck.
    ASSIGN 
        ttCheck.companyName  = "Our Company Name"
        ttCheck.companyLine1 = "9876 Our Address Blvd."
        ttCheck.companyLine2 = ""
        ttCheck.companyLine3 = "Company Town, PA 15238-3333"
        ttCheck.checkNo      = ipiCheckNumber
        ttCheck.remitToName  = "Fake Vendor"
        ttCheck.remitToLine1 = "123 Vendor St."
        ttCheck.remitToLine2 = "Suite 1522"
        ttCheck.remitToLine3 = "Sample City, CA 90210-2341"
        ttCheck.payDate      = TODAY
        ttCheck.memo         = "This is the check memo"
        .
    DO iCount = 1 TO 8:
        CREATE ttCheckInvoice.
        ASSIGN 
            ttCheckInvoice.checkNo            = ttCheck.checkNo
            ttCheckInvoice.sequence           = iCount
            ttCheckInvoice.invoiceNumber      = "123456" + STRING(iCount,"9")
            ttCheckInvoice.invoicePO          = 654000 + iCount * 100
            ttCheckInvoice.invoiceAmtGross    = dCheckAmount1 / 15
            ttCheckInvoice.invoiceAmtDiscount = ttCheckInvoice.invoiceAmtGross * .05
            ttCheckInvoice.invoiceAmtNet      = ttCheckInvoice.invoiceAmtGross - ttCheckInvoice.invoiceAmtDiscount
            ttCheckInvoice.invoiceDate        = 1/1/2020 + iCount
            ttCheck.amount                    = ttCheck.amount + ttCheckInvoice.invoiceAmtNet
            ttCheck.amountGross               = ttCheck.amountGross + ttCheckInvoice.invoiceAmtGross
            ttCheck.amountDisc                = ttCheck.amountDisc + ttCheckInvoice.invoiceAmtDisc
            .
    END.
        
    CREATE ttCheck.
    ASSIGN 
        ttCheck.companyName     = "Our Company Name"
        ttCheck.companyLine1    = "9876 Our Address Blvd."
        ttCheck.companyLine2    = ""
        ttCheck.companyLine3    = "Company Town, PA 15238-3333"
        ttCheck.checkNo         = ipiCheckNumber + 1
        ttCheck.remitToName     = "Vendor with Lots of Invoices"
        ttCheck.remitToLine1    = "333123 I Love to Bill St."
        ttCheck.remitToLine2    = ""
        ttCheck.remitToLine3    = "Sample City, CA 90210-2341"
        ttCheck.payDate         = TODAY
        ttCheck.memo            = "This is the check memo"
        ttCheck.useSupplemental = ipbf-ttCheckConfig.stubUseSupplementalReg
        ttCheck.isVoid          = NOT ttCheck.useSupplemental
        .
    DO iCount = 1 TO 100:
        CREATE ttCheckInvoice.
        ASSIGN 
            ttCheckInvoice.checkNo            = ttCheck.checkNo
            ttCheckInvoice.sequence           = iCount
            ttCheckInvoice.invoiceNumber      = "1236" + STRING(iCount,"999")
            ttCheckInvoice.invoicePO          = 654000 + iCount * 100
            ttCheckInvoice.invoiceAmtGross    = dCheckAmount2 / (iCount / 2)
            ttCheckInvoice.invoiceAmtDiscount = ttCheckInvoice.invoiceAmtGross * .05
            ttCheckInvoice.invoiceAmtNet      = ttCheckInvoice.invoiceAmtGross - ttCheckInvoice.invoiceAmtDiscount
            ttCheckInvoice.invoiceDate        = 1/1/2020 + iCount
            ttCheck.amount                    = ttCheck.amount + ttCheckInvoice.invoiceAmtNet
            ttCheck.amountGross               = ttCheck.amountGross + ttCheckInvoice.invoiceAmtGross
            ttCheck.amountDisc                = ttCheck.amountDisc + ttCheckInvoice.invoiceAmtDisc
            .
    END.
END PROCEDURE.

PROCEDURE pConvertNumberToWords PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an amount and size constraint, output the number in words
     Notes: replaces apchks.p
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipdAmt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiSize  AS   INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER iplDecimalsAsFraction AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplZeroAsNo AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFillChar AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplFillCenter AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdAmtInWords AS   CHARACTER NO-UNDO.

    DEFINE VARIABLE iIntegerDigits AS INT64     NO-UNDO.
    DEFINE VARIABLE cIntegerDigits AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDigitBlock    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iDecimalDigits AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDigit         AS INTEGER   EXTENT 3 NO-UNDO.
    DEFINE VARIABLE cCents         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFrontFill     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE iBlock         AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cAppend        AS CHARACTER NO-UNDO.
    
    /*Constants*/
    DEFINE VARIABLE cWords         AS CHARACTER EXTENT 33 NO-UNDO.
    DEFINE VARIABLE cDashConst     AS CHARACTER NO-UNDO INITIAL "-".
    DEFINE VARIABLE cDollarsConst  AS CHARACTER NO-UNDO INITIAL "Dollars".
    DEFINE VARIABLE cCentsConst    AS CHARACTER NO-UNDO INITIAL "Cents".
    DEFINE VARIABLE cAndConst      AS CHARACTER NO-UNDO INITIAL "and".
    ASSIGN
        cWords[1]  = "One"           
        cWords[2]  = "Two"           
        cWords[3]  = "Three"         
        cWords[4]  = "Four"          
        cWords[5]  = "Five"          
        cWords[6]  = "Six"           
        cWords[7]  = "Seven"         
        cWords[8]  = "Eight"         
        cWords[9]  = "Nine"          
        cWords[10] = "Ten"          
        cWords[11] = "Eleven"       
        cWords[12] = "Twelve"       
        cWords[13] = "Thirteen"     
        cWords[14] = "Fourteen"     
        cWords[15] = "Fifteen"
        cWords[16] = "Sixteen"
        cWords[17] = "Seventeen"
        cWords[18] = "Eighteen"
        cWords[19] = "Nineteen"
        cWords[20] = "Twenty"
        cWords[21] = "Thirty"
        cWords[22] = "Forty"
        cWords[23] = "Fifty"
        cWords[24] = "Sixty"
        cWords[25] = "Seventy"
        cWords[26] = "Eighty"
        cWords[27] = "Ninety"
        cWords[30] = "Hundred"
        cWords[31] = "Million"
        cWords[32] = "Thousand"
        cWords[33] = ""
        .

    ASSIGN 
        iIntegerDigits = TRUNCATE(ipdAmt , 0)
        iDecimalDigits = TRUNCATE(100 * (ipdAmt - iIntegerDigits), 0)
        cIntegerDigits = STRING(iIntegerDigits)
        cCents         = TRIM(STRING(iDecimalDigits))
        .
    
    IF iplZeroAsNo AND iDecimalDigits EQ 0 THEN 
        cCents = "NO".
        
    IF iplDecimalsAsFraction THEN 
        cCents = cCents + "/100".
    ELSE     
        cCents = cCents + " " + cCentsConst.
    cCents = " " + cAndConst + " " + cCents.

    IF iIntegerDigits LE 999999999 THEN 
    DO:
        DO WHILE LENGTH(cIntegerDigits) NE 9:
            cIntegerDigits = "0" + cIntegerDigits.
        END.
        DO iBlock = 1 TO 3:
            cDigitBlock = SUBSTRING(cIntegerDigits, 1 + (iBlock - 1) * 3 , 3).
            IF iBlock LT 3 THEN 
                cAppend = cDashConst + cWords[30 + iBlock] + " ".
            ELSE 
                cAppend = " ".
            IF INTEGER(cDigitBlock) GT 0 THEN 
            DO:
                DO iIndex = 1 TO 3:
                    iDigit[iIndex] = INTEGER(SUBSTRING(cDigitBlock , iIndex , 1)).
                END.

                IF iDigit[1] NE 0 THEN opdAmtInWords = opdAmtInWords + cWords[iDigit[1]] + cDashConst + cWords[30] + " ".

                IF iDigit[2] + iDigit[3] NE 0 THEN 
                DO:
                    IF iDigit[2] LT 2 THEN /* lookup ones and teens from constants*/
                    DO:
                        opdAmtInWords = opdAmtInWords + cWords[iDigit[2] * 10 + iDigit[3]].
                    END.
                    ELSE 
                    DO:  /*Lookup "-ty" values*/
                        opdAmtInWords = opdAmtInWords + cWords[iDigit[2] + 18].
                        IF iDigit[3] EQ 0 THEN 
                            opdAmtInWords = opdAmtInWords + " ".
                        ELSE 
                            opdAmtInWords = opdAmtInWords + cDashConst + cWords[iDigit[3]].
                    END.
                END.
                opdAmtInWords = opdAmtInWords + cAppend.
            END.
        END.    

        opdAmtInWords = opdAmtInWords + cDollarsConst + cCents.


    END.
    IF opdAmtInWords EQ "" OR LENGTH(TRIM(opdAmtInWords)) GT ipiSize THEN
        ASSIGN
            opdAmtInWords = TRIM(STRING(iIntegerDigits)) + " " + cDollarsConst + cCents.
             
    /*Apply Fill characters*/
    opdAmtInWords = TRIM(opdAmtInWords) + " ".
    IF iplFillCenter THEN 
        ASSIGN
            iFrontFill    = INTEGER((ipiSize - LENGTH(opdAmtInWords)) / 2)
            opdAmtInWords = FILL(ipcFillChar,iFrontFill) + opdAmtInWords
            .
    opdAmtInWords = opdAmtInWords + FILL(ipcFillChar,ipiSize - LENGTH(opdAmtInWords)).   
    
        
END PROCEDURE.

PROCEDURE pLoadConfig PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Loads Config file based on NK1 and .json file location
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplLoaded AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.


    RUN sys/ref/nk1look.p (INPUT ipcCompany, "ChkFmtConfig", "C" /* Logical */, NO /* check by cust */, 
        INPUT NO /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cFile, OUTPUT lFound). 
    
    IF lFound AND cFile NE "" THEN 
    DO: 
        
        ASSIGN
            cSourceType = "file"
            cReadMode   = "empty"
            .

        oplLoaded = iphTT:READ-JSON(cSourceType, cFile, cReadMode).
    END.
    
END PROCEDURE.

PROCEDURE pPrintChecks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a config buffer, print the checks currently in temp-table.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCheckConfig FOR ttCheckConfig.
    
    DEFINE VARIABLE iPageCount AS INTEGER.
    DEFINE VARIABLE iRowCount  AS INTEGER.
    
    FOR EACH ttCheck NO-LOCK
        BREAK BY ttCheck.checkNo:
        CASE ipbf-ttCheckConfig.panel1Type:
            WHEN "Check" THEN
            RUN pWriteCheck(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel1Row).
            WHEN "Stub" THEN  
            RUN pWriteStub(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel1Row, NO).
        END CASE.
        CASE ipbf-ttCheckConfig.panel2Type:
            WHEN "Check" THEN
            RUN pWriteCheck(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel2Row).
            WHEN "Stub" THEN  
            RUN pWriteStub(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel2Row, NO).
        END CASE.
        CASE ipbf-ttCheckConfig.panel3Type:
            WHEN "Check" THEN
            RUN pWriteCheck(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel3Row).
            WHEN "Stub" THEN  
            RUN pWriteStub(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel3Row, NO).
        END CASE.
        IF NOT LAST(ttCheck.checkNo) THEN 
            RUN AddPage(INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
    END.
    
END PROCEDURE.

PROCEDURE pPrintSupplementalRegister PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCheckConfig FOR ttCheckConfig.
    DEFINE INPUT PARAMETER iplPreview AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iPageCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRowCount   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFileSuffix AS CHARACTER NO-UNDO INITIAL "supp".
    
    IF CAN-FIND(FIRST ttCheck WHERE ttCheck.useSupplemental) THEN 
    DO:
        MESSAGE "Supplemental Register Required.  Load non-check paper and hit OK to print."
            VIEW-AS ALERT-BOX TITLE "Supplemental Register".
        RUN Output_InitializeXprint(ipbf-ttCheckConfig.outputFile + cFileSuffix, iplPreview, YES, ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize,"") .
        FOR EACH ttCheck NO-LOCK 
            WHERE ttCheck.useSupplemental
            BREAK BY ttCheck.checkNo:
            RUN pWriteStub(BUFFER ipbf-ttCheckConfig, BUFFER ttCheck, ipbf-ttCheckConfig.panel1Row, YES).
            IF NOT LAST(ttCheck.checkNo) THEN 
                RUN AddPage(INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
        END.
        RUN Output_Close.
        RUN Output_PrintXprintFile(ttCheckConfig.outputFile + cFileSuffix).
    END.  /*Supplemental Register*/

END PROCEDURE.

PROCEDURE pSetFont PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the font for the next bit of output
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFont AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFontSize AS INTEGER NO-UNDO.

    RUN Output_WriteToXprintFontChange(ipcFont, ipiFontSize).

END PROCEDURE.

PROCEDURE pUpdateData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Updates the data related to the check process
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcBankID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ap-chk FOR ap-chk.
    DEFINE BUFFER bf-bank   FOR bank.
    DEFINE BUFFER bf-ap-sel FOR ap-sel.
    DEFINE BUFFER bf-ap-inv FOR ap-inv.

    FOR EACH ttCheck
        WHERE NOT ttCheck.isVoid
        BREAK BY ttCheck.checkNo
        :
        FIND bf-ap-chk EXCLUSIVE-LOCK
            WHERE ROWID(bf-ap-chk) EQ ttCheck.ap-chkRowid
            NO-ERROR.
        IF AVAILABLE bf-ap-chk THEN DO:
            ASSIGN 
                bf-ap-chk.bank-code = ipcBankID
                bf-ap-chk.check-date = ttCheck.payDate
                bf-ap-chk.check-no = ttCheck.checkNo. 
            IF LAST(ttCheck.checkNo) THEN DO:
                FIND FIRST bf-bank EXCLUSIVE-LOCK  
                    WHERE bf-bank.company EQ bf-ap-chk.company
                    AND bf-bank.bank-code EQ bf-ap-chk.bank-code
                    NO-ERROR.
                IF AVAILABLE bf-bank THEN 
                    bf-bank.last-chk = bf-ap-chk.check-no.
            END.
        END.
    END.
    FOR EACH ttCheckInvoice,
        FIRST bf-ap-chk NO-LOCK 
            WHERE ROWID(bf-ap-chk) EQ ttCheckInvoice.ap-chkRowid:
        FIND bf-ap-sel EXCLUSIVE-LOCK 
            WHERE ROWID(bf-ap-sel) EQ ttCheckInvoice.ap-selRowid
            NO-ERROR.
            IF AVAILABLE bf-ap-sel THEN 
                ASSIGN 
                    bf-ap-sel.check-no  = bf-ap-chk.check-no
                    bf-ap-sel.bank-code = bf-ap-chk.bank-code
                    bf-ap-sel.actnum    = bf-ap-chk.check-act
                    bf-ap-sel.check-date = bf-ap-chk.check-date
                    .
        FIND FIRST bf-ap-inv EXCLUSIVE-LOCK 
            WHERE ROWID(bf-ap-inv) EQ ttCheckInvoice.ap-invRowid
            NO-ERROR.
        IF AVAILABLE bf-ap-inv THEN 
            ASSIGN 
                bf-ap-inv.check-no = bf-ap-chk.check-no
                bf-ap-inv.pay-date = bf-ap-chk.check-date
                .
    END.
                
END PROCEDURE.

PROCEDURE pWriteCheck PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given configuration buffer and Key information, print the 
     check panel
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCheckConfig FOR ttCheckConfig.
    DEFINE PARAMETER BUFFER ipbf-ttCheck       FOR ttCheck.
    DEFINE INPUT PARAMETER ipdRowStart AS DECIMAL NO-UNDO.
            
    DEFINE VARIABLE cPayDate      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAmtInWords   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRemitToLine1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRemitToLine2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRemitToLine3 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine3 AS CHARACTER NO-UNDO.
       
    RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    
    /*Print Pay Date*/
    IF ipbf-ttCheckConfig.checkPayDateShow THEN 
    DO:
        IF ipbf-ttCheck.payDate = ? THEN 
            cPayDate = "".
        ELSE
            cPayDate = fFormatDate(ipbf-ttCheck.payDate, ipbf-ttCheckConfig.checkPayDateFormat).
        
        RUN pSetFont(ipbf-ttCheckConfig.checkPayDateFont, ipbf-ttCheckConfig.checkPayDateFontSize).   
        RUN pWriteToCoordinatesString(                                  /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.checkPayDateRow,           /*x*/ 
            ipbf-ttCheckConfig.checkPayDateCol,                         /*y*/
            cPayDate,                                                   /*value to print*/
            ipbf-ttCheckConfig.checkPayDateSize,                        /*string size*/ 
            ipbf-ttCheckConfig.checkPayDateBold,                        /*bold*/ 
            ipbf-ttCheckConfig.checkPayDateUnderline,                   /*underline*/ 
            ipbf-ttCheckConfig.checkPayDateItalic,
            ipbf-ttCheckConfig.checkPayDateRightJustify,                /*right justify*/
            ipbf-ttCheckConfig.checkPayDateAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.
    
    /*Print Check No*/
    IF ipbf-ttCheckConfig.checkCheckNumberShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.checkCheckNumberFont, ipbf-ttCheckConfig.checkCheckNumberFontSize).
        RUN pWriteToCoordinatesNum(                             /*number format*/
            ipdRowStart + ipbf-ttCheckConfig.checkCheckNumberRow,   /*x*/ 
            ipbf-ttCheckConfig.checkCheckNumberCol,                 /*y*/
            ipbf-ttCheck.checkNo,                               /*value to print*/
            ipbf-ttCheckConfig.checkCheckNumberNumSize,             /*size to Left of decimal*/ 
            0,                                                  /*decimal places - 0 will suppress decimal*/ 
            NO,                                                 /*include comma*/ 
            YES,                                                /* trim extra spaces */
            ipbf-ttCheckConfig.checkCheckNumberBold,                /*bold*/ 
            ipbf-ttCheckConfig.checkCheckNumberUnderline,           /*underline*/ 
            ipbf-ttCheckConfig.checkCheckNumberItalic,
            ipbf-ttCheckConfig.checkCheckNumberRightJustify,        /*right justify*/
            ""                                                  /*Currency Symbol*/    
            ).
    
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.
        
    /*Print Amt in Words*/
    IF ipbf-ttCheckConfig.checkAmtInWordsShow THEN 
    DO:
        RUN pConvertNumberToWords(ipbf-ttCheck.amount, 
            ipbf-ttCheckConfig.checkAmtInWordsSize, 
            ipbf-ttCheckConfig.checkAmtInWordsAndAsFraction, 
            ipbf-ttCheckConfig.checkAmtInWordsZeroAsNo,
            ipbf-ttCheckConfig.checkAmtInWordsFillChar,
            ipbf-ttCheckConfig.checkAmtInWordsFillCenter, 
            OUTPUT cAmtInWords).
            
        RUN pSetFont(ipbf-ttCheckConfig.checkAmtInWordsFont, ipbf-ttCheckConfig.checkAmtInWordsFontSize).   
        RUN pWriteToCoordinatesString(                                  /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.checkAmtInWordsRow,     /*x*/ 
            ipbf-ttCheckConfig.checkAmtInWordsCol,                   /*y*/
            cAmtInWords,                                             /*value to print*/
            ipbf-ttCheckConfig.checkAmtInWordsSize,                  /*string size*/ 
            ipbf-ttCheckConfig.checkAmtInWordsBold,                  /*bold*/ 
            ipbf-ttCheckConfig.checkAmtInWordsUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.checkAmtInWordsItalic,
            ipbf-ttCheckConfig.checkAmtInWordsRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.checkAmtInWordsAllCaps).
            
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.
        
    /*Print Amt*/
    IF ipbf-ttCheckConfig.checkAmtShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.checkAmtFont, ipbf-ttCheckConfig.checkAmtFontSize).   
        RUN pWriteToCoordinatesNum(                             /*number format*/
            ipdRowStart + ipbf-ttCheckConfig.checkAmtRow,   /*x*/ 
            ipbf-ttCheckConfig.checkAmtCol,                 /*y*/
            ipbf-ttCheck.amount,                               /*value to print*/
            ipbf-ttCheckConfig.checkAmtNumSize,             /*size to Left of decimal*/ 
            ipbf-ttCheckConfig.checkAmtDecimals,            /*decimal places - 0 will suppress decimal*/ 
            ipbf-ttCheckConfig.checkAmtComma,               /*include comma*/ 
            YES,                                                /* trim extra spaces */
            ipbf-ttCheckConfig.checkAmtBold,                /*bold*/ 
            ipbf-ttCheckConfig.checkAmtUnderline,           /*underline*/
            ipbf-ttCheckConfig.checkAmtItalic,  
            ipbf-ttCheckConfig.checkAmtRightJustify,         /*right justify*/
            ipbf-ttCheckConfig.checkAmtCurrSymb        /*Currency Symbol*/
            ).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.

    /*Print Memo*/
    IF ipbf-ttCheckConfig.checkMemoShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.checkMemoFont, ipbf-ttCheckConfig.checkMemoFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.checkMemoRow,     /*x*/ 
            ipbf-ttCheckConfig.checkMemoCol,                   /*y*/
            ipbf-ttCheck.memo,                                 /*value to print*/
            ipbf-ttCheckConfig.checkMemoSize,                  /*string size*/ 
            ipbf-ttCheckConfig.checkMemoBold,                  /*bold*/ 
            ipbf-ttCheckConfig.checkMemoUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.checkMemoItalic,
            ipbf-ttCheckConfig.checkMemoRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.checkMemoAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.
    
    /*Print RemitTo*/
    IF ipbf-ttCheckConfig.checkRemitToNameShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.checkRemitToNameFont, ipbf-ttCheckConfig.checkRemitToNameFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.checkRemitToNameRow,     /*x*/ 
            ipbf-ttCheckConfig.checkRemitToNameCol,                   /*y*/
            ipbf-ttCheck.remitToName,                                 /*value to print*/
            ipbf-ttCheckConfig.checkRemitToNameSize,                  /*string size*/ 
            ipbf-ttCheckConfig.checkRemitToNameBold,                  /*bold*/ 
            ipbf-ttCheckConfig.checkRemitToNameUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.checkRemitToNameItalic, 
            ipbf-ttCheckConfig.checkRemitToNameRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.checkRemitToNameAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.
    
    /*Print RemitTo Address*/
    IF ipbf-ttCheckConfig.checkRemitToAddressShow THEN 
    DO:
        IF ipbf-ttCheckConfig.checkRemitToAddressCompact THEN 
            ASSIGN 
                cRemitToLine1 = ipbf-ttCheck.remitToLine1
                cRemitToLine2 = IF ipbf-ttCheck.remitToLine2 EQ "" THEN ipbf-ttCheck.remitToLine3 ELSE ipbf-ttCheck.remitToLine2
                cRemitToLine3 = IF ipbf-ttCheck.remitToLine2 EQ "" THEN "" ELSE ipbf-ttCheck.remitToLine3
                .
        ELSE 
            ASSIGN 
                cRemitToLine1 = ipbf-ttCheck.remitToLine1
                cRemitToLine2 = ipbf-ttCheck.remitToLine2
                cRemitToLine3 = ipbf-ttCheck.remitToLine3
                .
        RUN pSetFont(ipbf-ttCheckConfig.checkRemitToAddressFont, ipbf-ttCheckConfig.checkRemitToAddressFontSize).   
        IF cRemitToline1 NE "" THEN 
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + ipbf-ttCheckConfig.checkRemitToAddressRow,     /*x*/ 
                ipbf-ttCheckConfig.checkRemitToAddressCol,                   /*y*/
                cRemitToLine1,                                   /*value to print*/
                ipbf-ttCheckConfig.checkRemitToAddressSize,                  /*string size*/ 
                ipbf-ttCheckConfig.checkRemitToAddressBold,                  /*bold*/ 
                ipbf-ttCheckConfig.checkRemitToAddressUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.checkRemitToAddressItalic, 
                ipbf-ttCheckConfig.checkRemitToAddressRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.checkRemitToAddressAllCaps).
        IF cRemitToLine2 NE "" THEN 
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + ipbf-ttCheckConfig.checkRemitToAddressRow + ipbf-ttCheckConfig.checkRemitToAddressRowSpace,     /*x*/ 
                ipbf-ttCheckConfig.checkRemitToAddressCol,                   /*y*/
                cRemitToLine2,                                   /*value to print*/
                ipbf-ttCheckConfig.checkRemitToAddressSize,                  /*string size*/ 
                ipbf-ttCheckConfig.checkRemitToAddressBold,                  /*bold*/ 
                ipbf-ttCheckConfig.checkRemitToAddressUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.checkRemitToAddressItalic,
                ipbf-ttCheckConfig.checkRemitToAddressRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.checkRemitToAddressAllCaps).
        IF cRemitToLine3 NE "" THEN 
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + ipbf-ttCheckConfig.checkRemitToAddressRow + ipbf-ttCheckConfig.checkRemitToAddressRowSpace * 2, /*x*/ 
                ipbf-ttCheckConfig.checkRemitToAddressCol,                   /*y*/
                cRemitToLine3,                                   /*value to print*/
                ipbf-ttCheckConfig.checkRemitToAddressSize,                  /*string size*/ 
                ipbf-ttCheckConfig.checkRemitToAddressBold,                  /*bold*/ 
                ipbf-ttCheckConfig.checkRemitToAddressUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.checkRemitToAddressItalic,
                ipbf-ttCheckConfig.checkRemitToAddressRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.checkRemitToAddressAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.

    /*Print Company*/
    IF ipbf-ttCheckConfig.checkCompanyNameShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.checkCompanyNameFont, ipbf-ttCheckConfig.checkCompanyNameFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.checkCompanyNameRow,     /*x*/ 
            ipbf-ttCheckConfig.checkCompanyNameCol,                   /*y*/
            ipbf-ttCheck.companyName,                                 /*value to print*/
            ipbf-ttCheckConfig.checkCompanyNameSize,                  /*string size*/ 
            ipbf-ttCheckConfig.checkCompanyNameBold,                  /*bold*/ 
            ipbf-ttCheckConfig.checkCompanyNameUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.checkCompanyNameItalic,
            ipbf-ttCheckConfig.checkCompanyNameRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.checkCompanyNameAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.
    
    /*Print Company Address*/
    IF ipbf-ttCheckConfig.checkCompanyAddressShow THEN 
    DO:
        IF ipbf-ttCheckConfig.checkCompanyAddressCompact THEN 
            ASSIGN 
                cCompanyLine1 = ipbf-ttCheck.CompanyLine1
                cCompanyLine2 = IF ipbf-ttCheck.CompanyLine2 EQ "" THEN ipbf-ttCheck.CompanyLine3 ELSE ipbf-ttCheck.CompanyLine2
                cCompanyLine3 = IF ipbf-ttCheck.CompanyLine2 EQ "" THEN "" ELSE ipbf-ttCheck.CompanyLine3
                .
        ELSE 
            ASSIGN 
                cCompanyLine1 = ipbf-ttCheck.CompanyLine1
                cCompanyLine2 = ipbf-ttCheck.CompanyLine2
                cCompanyLine3 = ipbf-ttCheck.CompanyLine3
                .
        RUN pSetFont(ipbf-ttCheckConfig.checkCompanyAddressFont, ipbf-ttCheckConfig.checkCompanyAddressFontSize).   
        IF cCompanyline1 NE "" THEN 
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + ipbf-ttCheckConfig.checkCompanyAddressRow,     /*x*/ 
                ipbf-ttCheckConfig.checkCompanyAddressCol,                   /*y*/
                cCompanyLine1,                                   /*value to print*/
                ipbf-ttCheckConfig.checkCompanyAddressSize,                  /*string size*/ 
                ipbf-ttCheckConfig.checkCompanyAddressBold,                  /*bold*/ 
                ipbf-ttCheckConfig.checkCompanyAddressUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.checkCompanyAddressItalic,
                ipbf-ttCheckConfig.checkCompanyAddressRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.checkCompanyAddressAllCaps).
        IF cCompanyLine2 NE "" THEN 
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + ipbf-ttCheckConfig.checkCompanyAddressRow + ipbf-ttCheckConfig.checkCompanyAddressRowSpace,     /*x*/ 
                ipbf-ttCheckConfig.checkCompanyAddressCol,                   /*y*/
                cCompanyLine2,                                   /*value to print*/
                ipbf-ttCheckConfig.checkCompanyAddressSize,                  /*string size*/ 
                ipbf-ttCheckConfig.checkCompanyAddressBold,                  /*bold*/ 
                ipbf-ttCheckConfig.checkCompanyAddressUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.checkCompanyAddressItalic,
                ipbf-ttCheckConfig.checkCompanyAddressRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.checkCompanyAddressAllCaps).
        IF cCompanyLine3 NE "" THEN 
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + ipbf-ttCheckConfig.checkCompanyAddressRow + ipbf-ttCheckConfig.checkCompanyAddressRowSpace * 2, /*x*/ 
                ipbf-ttCheckConfig.checkCompanyAddressCol,                   /*y*/
                cCompanyLine3,                                   /*value to print*/
                ipbf-ttCheckConfig.checkCompanyAddressSize,                  /*string size*/ 
                ipbf-ttCheckConfig.checkCompanyAddressBold,                  /*bold*/ 
                ipbf-ttCheckConfig.checkCompanyAddressUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.checkCompanyAddressItalic, 
                ipbf-ttCheckConfig.checkCompanyAddressRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.checkCompanyAddressAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.checkFont, ipbf-ttCheckConfig.checkFontSize).
    END.

END PROCEDURE.

PROCEDURE pWriteStub PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given configuration buffer and Key information, print the 
     stub panel
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCheckConfig FOR ttCheckConfig.
    DEFINE PARAMETER BUFFER ipbf-ttCheck       FOR ttCheck.
    DEFINE INPUT PARAMETER ipdRowStart AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplSupplemental AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cPayDate      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRemitToLine1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRemitToLine2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRemitToLine3 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyLine3 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRowCount     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageCount    AS INTEGER   NO-UNDO.
       
    RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).

    /*Print Pay Date*/
    IF ipbf-ttCheckConfig.stubPayDateShow THEN 
    DO:
        IF ipbf-ttCheck.payDate = ? THEN 
            cPayDate = "".
        ELSE
            cPayDate = fFormatDate(ipbf-ttCheck.payDate, ipbf-ttCheckConfig.stubPayDateFormat).
        
        RUN pSetFont(ipbf-ttCheckConfig.stubPayDateFont, ipbf-ttCheckConfig.stubPayDateFontSize).   
        RUN pWriteToCoordinatesString(                                  /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubPayDateRow,           /*x*/ 
            ipbf-ttCheckConfig.stubPayDateCol,                         /*y*/
            cPayDate,                                                   /*value to print*/
            ipbf-ttCheckConfig.stubPayDateSize,                        /*string size*/ 
            ipbf-ttCheckConfig.stubPayDateBold,                        /*bold*/ 
            ipbf-ttCheckConfig.stubPayDateUnderline,                   /*underline*/ 
            ipbf-ttCheckConfig.stubPayDateItalic,
            ipbf-ttCheckConfig.stubPayDateRightJustify,                /*right justify*/
            ipbf-ttCheckConfig.stubPayDateAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
    END.
    
    /*Print Check No*/
    IF ipbf-ttCheckConfig.stubCheckNumberShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.stubCheckNumberFont, ipbf-ttCheckConfig.stubCheckNumberFontSize).
        RUN pWriteToCoordinatesNum(                             /*number format*/
            ipdRowStart + ipbf-ttCheckConfig.stubCheckNumberRow,   /*x*/ 
            ipbf-ttCheckConfig.stubCheckNumberCol,                 /*y*/
            ipbf-ttCheck.checkNo,                               /*value to print*/
            ipbf-ttCheckConfig.stubCheckNumberNumSize,             /*size to Left of decimal*/ 
            0,                                                  /*decimal places - 0 will suppress decimal*/ 
            NO,                                                 /*include comma*/ 
            YES,                                                /* trim extra spaces */
            ipbf-ttCheckConfig.stubCheckNumberBold,                /*bold*/ 
            ipbf-ttCheckConfig.stubCheckNumberUnderline,           /*underline*/ 
            ipbf-ttCheckConfig.stubCheckNumberItalic,
            ipbf-ttCheckConfig.stubCheckNumberRightJustify,        /*right justify*/
            ""                                                  /*Currency Symbol*/    
            ).
    
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
    END.
    
    /*Print Company*/
    IF ipbf-ttCheckConfig.stubCompanyNameShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.stubCompanyNameFont, ipbf-ttCheckConfig.stubCompanyNameFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubCompanyNameRow,     /*x*/ 
            ipbf-ttCheckConfig.stubCompanyNameCol,                   /*y*/
            ipbf-ttCheck.companyName,                                 /*value to print*/
            ipbf-ttCheckConfig.stubCompanyNameSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubCompanyNameBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubCompanyNameUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubCompanyNameItalic,
            ipbf-ttCheckConfig.stubCompanyNameRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubCompanyNameAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
    END.
    
    /*Print RemitTo*/
    IF ipbf-ttCheckConfig.stubRemitToNameShow THEN 
    DO:
        RUN pSetFont(ipbf-ttCheckConfig.stubRemitToNameFont, ipbf-ttCheckConfig.stubRemitToNameFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRemitToNameRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRemitToNameCol,                   /*y*/
            ipbf-ttCheck.remitToName,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRemitToNameSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRemitToNameBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRemitToNameUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRemitToNameItalic,
            ipbf-ttCheckConfig.stubRemitToNameRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRemitToNameAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
    END.
    
    IF NOT ipbf-ttCheck.useSupplemental OR iplSupplemental THEN 
    DO:
        /*Print Register Headers*/
        /*Invoice #*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegInvNoHeadFont, ipbf-ttCheckConfig.stubRegInvNoHeadFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegHeadRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegInvNoCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegInvNoHead,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRegInvNoHeadSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegInvNoHeadBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegInvNoHeadUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegInvNoHeadItalic,
            ipbf-ttCheckConfig.stubRegInvNoHeadRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegInvNoHeadAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
        /*PO #*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegPOHeadFont, ipbf-ttCheckConfig.stubRegPOHeadFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegHeadRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegPOCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegPOHead,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRegPOHeadSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegPOHeadBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegPOHeadUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegPOHeadItalic,
            ipbf-ttCheckConfig.stubRegPOHeadRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegPOHeadAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).

        /*InvDate #*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegInvDateHeadFont, ipbf-ttCheckConfig.stubRegInvDateHeadFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegHeadRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegInvDateCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegInvDateHead,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRegInvDateHeadSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegInvDateHeadBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegInvDateHeadUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegInvDateHeadItalic,
            ipbf-ttCheckConfig.stubRegInvDateHeadRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegInvDateHeadAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).

        /*AmtGross #*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtGrossHeadFont, ipbf-ttCheckConfig.stubRegAmtGrossHeadFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegHeadRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegAmtGrossCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegAmtGrossHead,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRegAmtGrossHeadSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegAmtGrossHeadBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegAmtGrossHeadUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegAmtGrossHeadItalic,
            ipbf-ttCheckConfig.stubRegAmtGrossHeadRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegAmtGrossHeadAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
        /*AmtDisc #*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtDiscHeadFont, ipbf-ttCheckConfig.stubRegAmtDiscHeadFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegHeadRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegAmtDiscCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegAmtDiscHead,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRegAmtDiscHeadSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegAmtDiscHeadBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegAmtDiscHeadUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegAmtDiscHeadItalic,
            ipbf-ttCheckConfig.stubRegAmtDiscHeadRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegAmtDiscHeadAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
        /*AmtNet #*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtNetHeadFont, ipbf-ttCheckConfig.stubRegAmtNetHeadFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegHeadRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegAmtNetCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegAmtNetHead,                                 /*value to print*/
            ipbf-ttCheckConfig.stubRegAmtNetHeadSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegAmtNetHeadBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegAmtNetHeadUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegAmtNetHeadItalic,
            ipbf-ttCheckConfig.stubRegAmtNetHeadRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegAmtNetHeadAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
        ASSIGN 
            iRowCount  = 0
            iPageCount = 0
            .
        IF iplSupplemental THEN  
            RUN AddRowMultiple(ipbf-ttCheckConfig.stubRegDetailRow, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
        ELSE 
            iRowCount = ipbf-ttCheckConfig.stubRegDetailRow.
        /*Print Register Invoice Details*/
        FOR EACH ttCheckInvoice
            WHERE ttCheckInvoice.checkNo EQ ipbf-ttCheck.checkNo
            BY ttCheckInvoice.sequence
            :
            IF iplSupplemental THEN 
                RUN AddRow(INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
            ELSE 
                iRowCount = iRowCount + 1.
            /*Invoice #*/
            RUN pSetFont(ipbf-ttCheckConfig.stubRegInvNoFont, ipbf-ttCheckConfig.stubRegInvNoFontSize).   
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegInvNoCol,                   /*y*/
                ttCheckInvoice.invoiceNumber,                         /*value to print*/
                ipbf-ttCheckConfig.stubRegInvNoSize,                  /*string size*/ 
                ipbf-ttCheckConfig.stubRegInvNoBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegInvNoUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegInvNoItalic,
                ipbf-ttCheckConfig.stubRegInvNoRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.stubRegInvNoAllCaps).
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
            /*PO #*/
            RUN pSetFont(ipbf-ttCheckConfig.stubRegPOFont, ipbf-ttCheckConfig.stubRegPOFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegPOCol,                   /*y*/
                ttCheckInvoice.invoicePO,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegPONumSize,               /*size to Left of decimal*/
                0,                                                 /*decimal places - 0 will suppress decimal*/ 
                NO,                                                /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegPOBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegPOUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegPOItalic, 
                ipbf-ttCheckConfig.stubRegPORightJustify,          /*right justify*/
                "").
            
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).

            /*InvDate #*/
            RUN pSetFont(ipbf-ttCheckConfig.stubRegInvDateFont, ipbf-ttCheckConfig.stubRegInvDateFontSize).   
            RUN pWriteToCoordinatesString(                                /*string format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegInvDateCol,                   /*y*/
                fFormatDate(ttCheckInvoice.invoiceDate, ipbf-ttCheckConfig.stubRegInvDateFormat), /*value to print*/
                ipbf-ttCheckConfig.stubRegInvDateSize,                  /*string size*/ 
                ipbf-ttCheckConfig.stubRegInvDateBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegInvDateUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegInvDateItalic,
                ipbf-ttCheckConfig.stubRegInvDateRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.stubRegInvDateAllCaps).
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).

            /*AmtGross #*/
            RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtGrossFont, ipbf-ttCheckConfig.stubRegAmtGrossFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossCol,                   /*y*/
                ttCheckInvoice.invoiceAmtGross,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegAmtGrossNumSize,               /*size to Left of decimal*/
                ipbf-ttCheckConfig.stubRegAmtGrossDecimals,          /*decimal places - 0 will suppress decimal*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossComma,            /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegAmtGrossBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossItalic, 
                ipbf-ttCheckConfig.stubRegAmtGrossRightJustify,          /*right justify*/
                (IF ipbf-ttCheckConfig.stubRegAmtGrossCurrSymbFirstOnly AND ttCheckInvoice.sequence GT 1 THEN "" ELSE ipbf-ttCheckConfig.stubRegAmtGrossCurrSymb)).
        
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
            /*AmtDisc #*/
            RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtDiscFont, ipbf-ttCheckConfig.stubRegAmtDiscFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscCol,                   /*y*/
                ttCheckInvoice.invoiceAmtDisc,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegAmtDiscNumSize,               /*size to Left of decimal*/
                ipbf-ttCheckConfig.stubRegAmtDiscDecimals,          /*decimal places - 0 will suppress decimal*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscComma,            /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegAmtDiscBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscItalic,
                ipbf-ttCheckConfig.stubRegAmtDiscRightJustify,          /*right justify*/
                (IF ipbf-ttCheckConfig.stubRegAmtDiscCurrSymbFirstOnly AND ttCheckInvoice.sequence GT 1 THEN "" ELSE ipbf-ttCheckConfig.stubRegAmtDiscCurrSymb)).
        
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
            /*AmtNet #*/
            RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtNetFont, ipbf-ttCheckConfig.stubRegAmtNetFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegAmtNetCol,                   /*y*/
                ttCheckInvoice.invoiceAmtNet,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegAmtNetNumSize,               /*size to Left of decimal*/
                ipbf-ttCheckConfig.stubRegAmtNetDecimals,          /*decimal places - 0 will suppress decimal*/ 
                ipbf-ttCheckConfig.stubRegAmtNetComma,            /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegAmtNetBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegAmtNetUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegAmtNetItalic,
                ipbf-ttCheckConfig.stubRegAmtNetRightJustify,          /*right justify*/
                (IF ipbf-ttCheckConfig.stubRegAmtNetCurrSymbFirstOnly AND ttCheckInvoice.sequence GT 1 THEN "" ELSE ipbf-ttCheckConfig.stubRegAmtNetCurrSymb)).
        
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
                      
        END. /*Each ttCheckInvoice*/
        /*Print register Totals*/
        IF iplSupplemental THEN 
            RUN AddRow(INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
        ELSE 
            iRowCount = iRowCount + 1.
        /*Totals Label*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegTotLabelFont, ipbf-ttCheckConfig.stubRegTotLabelFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + iRowCount - 1,     /*x*/ 
            ipbf-ttCheckConfig.stubRegTotLabelCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegTotLabel,                      /*value to print*/
            ipbf-ttCheckConfig.stubRegTotLabelSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegTotLabelBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegTotLabelUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegTotLabelItalic,
            ipbf-ttCheckConfig.stubRegTotLabelRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegTotLabelAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        
        /*AmtGross Total*/
        IF ipbf-ttCheckConfig.stubRegAmtGrossTotShow THEN 
        DO:
            RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtGrossTotFont, ipbf-ttCheckConfig.stubRegAmtGrossTotFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossCol,                   /*y*/
                ipbf-ttCheck.amountGross,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegAmtGrossTotNumSize,               /*size to Left of decimal*/
                ipbf-ttCheckConfig.stubRegAmtGrossTotDecimals,          /*decimal places - 0 will suppress decimal*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossTotComma,            /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegAmtGrossTotBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossTotUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegAmtGrossTotItalic, 
                ipbf-ttCheckConfig.stubRegAmtGrossTotRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.stubRegAmtGrossTotCurrSymb).
        
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        END.
        /*AmtDisc Total*/
        IF ipbf-ttCheckConfig.stubRegAmtDiscTotShow THEN 
        DO:
            RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtDiscTotFont, ipbf-ttCheckConfig.stubRegAmtDiscTotFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscCol,                   /*y*/
                ipbf-ttCheck.amountDisc,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegAmtDiscTotNumSize,               /*size to Left of decimal*/
                ipbf-ttCheckConfig.stubRegAmtDiscTotDecimals,          /*decimal places - 0 will suppress decimal*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscTotComma,            /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegAmtDiscTotBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscTotUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegAmtDiscTotItalic,
                ipbf-ttCheckConfig.stubRegAmtDiscTotRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.stubRegAmtDiscTotCurrSymb).
        
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        END.
        /*AmtNet Total*/
        IF ipbf-ttCheckConfig.stubRegAmtNetTotShow THEN 
        DO:
            RUN pSetFont(ipbf-ttCheckConfig.stubRegAmtNetTotFont, ipbf-ttCheckConfig.stubRegAmtNetTotFontSize).   
            RUN pWriteToCoordinatesNum(                             /*number format*/
                ipdRowStart + iRowCount - 1,     /*x*/ 
                ipbf-ttCheckConfig.stubRegAmtNetCol,                   /*y*/
                ipbf-ttCheck.amount,                          /*value to print*/
                ipbf-ttCheckConfig.stubRegAmtNetTotNumSize,               /*size to Left of decimal*/
                ipbf-ttCheckConfig.stubRegAmtNetTotDecimals,          /*decimal places - 0 will suppress decimal*/ 
                ipbf-ttCheckConfig.stubRegAmtNetTotComma,            /*include comma*/ 
                YES,                                                /* trim extra spaces */ 
                ipbf-ttCheckConfig.stubRegAmtNetTotBold,                  /*bold*/ 
                ipbf-ttCheckConfig.stubRegAmtNetTotUnderline,             /*underline*/ 
                ipbf-ttCheckConfig.stubRegAmtNetTotItalic,
                ipbf-ttCheckConfig.stubRegAmtNetTotRightJustify,          /*right justify*/
                ipbf-ttCheckConfig.stubRegAmtNetTotCurrSymb).
        
            /*Reset and Set Font*/
            RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
            RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
        END.
    END. /*Non supplemental*/
    ELSE 
    DO:
        /*Print supplemental notification*/
        /*Supplemental Notice*/
        RUN pSetFont(ipbf-ttCheckConfig.stubRegSuppNoticeFont, ipbf-ttCheckConfig.stubRegSuppNoticeFontSize).   
        RUN pWriteToCoordinatesString(                                /*string format*/
            ipdRowStart + ipbf-ttCheckConfig.stubRegSuppNoticeRow,     /*x*/ 
            ipbf-ttCheckConfig.stubRegSuppNoticeCol,                   /*y*/
            ipbf-ttCheckConfig.stubRegSuppNotice,                      /*value to print*/
            ipbf-ttCheckConfig.stubRegSuppNoticeSize,                  /*string size*/ 
            ipbf-ttCheckConfig.stubRegSuppNoticeBold,                  /*bold*/ 
            ipbf-ttCheckConfig.stubRegSuppNoticeUnderline,             /*underline*/ 
            ipbf-ttCheckConfig.stubRegSuppNoticeItalic,
            ipbf-ttCheckConfig.stubRegSuppNoticeRightJustify,          /*right justify*/
            ipbf-ttCheckConfig.stubRegSuppNoticeAllCaps).
        /*Reset and Set Font*/
        RUN pSetFont(ipbf-ttCheckConfig.defaultFont, ipbf-ttCheckConfig.defaultFontSize).
        RUN pSetFont(ipbf-ttCheckConfig.stubFont, ipbf-ttCheckConfig.stubFontSize).
    END.
END PROCEDURE.

PROCEDURE pWriteToCoordinates PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplItalic AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    
    RUN Output_WriteToXprint(ipdR, ipdC, ipcText, iplBold, iplUnderline, iplItalic, iplRightJustified).

END PROCEDURE.

PROCEDURE pWriteToCoordinatesNum PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdNumber AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeftDigits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiDecimalDigits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplComma AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplTrim AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplItalic AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcCurrSymb AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    cText = ipcCurrSymb + fFormatNumber(ipdNumber, ipiLeftDigits, ipiDecimalDigits, iplComma).
    IF iplTrim THEN cText = TRIM(cText).
    RUN Output_WriteToXprint(ipdR, ipdC, cText, iplBold, iplUnderline, iplItalic, iplRightJustified).

END PROCEDURE.

PROCEDURE pWriteToCoordinatesString PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCharacters AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplItalic AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplAllCaps AS LOGICAL NO-UNDO.
    
    ipcText = fFormatString(ipcText, ipiCharacters).
    IF iplAllCaps THEN ipcText = CAPS(ipcText).
    RUN Output_WriteToXprint(ipdR, ipdC, ipcText, iplBold, iplUnderline, iplItalic, iplRightJustified).

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fFormatDate RETURNS CHARACTER PRIVATE
    ( ipdtDate AS DATE, ipcFormat AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Given Date and Format as character, return a character string
        of the date formatted.
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cDate       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMonthShort AS CHARACTER EXTENT 12 NO-UNDO INITIAL ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec" ].
    DEFINE VARIABLE cMonthLong  AS CHARACTER EXTENT 12 NO-UNDO INITIAL ["January","February","March","April","May","June","July","August","September","October","November","December" ]. 
    
    CASE ipcFormat:
        WHEN "" THEN
            cDate = "".
        WHEN "MM/DD/YYYY" THEN 
            cDate = STRING(ipdtDate, "99/99/9999").
        WHEN "MM/DD/YY" THEN 
            cDate = STRING(ipdtDate, "99/99/99").
        WHEN "MM-DD-YYYY" THEN 
            cDate = STRING(ipdtDate, "99-99-9999").
        WHEN "MM-DD-YY" THEN 
            cDate = STRING(ipdtDate, "99-99-99").
        WHEN "MM.DD.YYYY" THEN 
            cDate = STRING(ipdtDate, "99.99.9999").
        WHEN "MM-DD-YY" THEN 
            cDate = STRING(ipdtDate, "99.99.99").
        WHEN "M/D/YYYY" THEN
            cDate =  TRIM(STRING(MONTH(ipdtDate),">9")) + "/" + TRIM(STRING(DAY(ipdtDate),">9")) + "/" + STRING(YEAR(ipdtDate)).
        WHEN "DD-MMM-YYYY" THEN
            cDate =  STRING(DAY(ipdtDate),"99") + "-" + cMonthShort[MONTH(ipdtDate)] + "-" + STRING(YEAR(ipdtDate)).
        WHEN "DD-MONTH-YYYY" THEN
            cDate =  STRING(DAY(ipdtDate),"99") + "-" + cMonthLong[MONTH(ipdtDate)] + "-" + STRING(YEAR(ipdtDate)).
        WHEN "MONTH DD, YYYY" THEN
            cDate =  cMonthLong[MONTH(ipdtDate)] + " " + STRING(DAY(ipdtDate),">9") + ", " + STRING(YEAR(ipdtDate)).
        OTHERWISE 
        cDate = STRING(ipdtDate, ipcFormat).
    END CASE.		
    RETURN cDate.
		
END FUNCTION.

FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    ( ipdNumber AS DECIMAL , ipiLeftDigits AS INTEGER , ipiRightDigits AS INTEGER, iplComma AS LOGICAL):
    /*------------------------------------------------------------------------------
     Purpose: Formats a number with left and right digits.  Handles problem when 
     size of number doesn't fit
     Notes:
    ------------------------------------------------------------------------------*/    
    
    RETURN DYNAMIC-FUNCTION("FormatNumber", ipdNumber, ipiLeftDigits, ipiRightDigits, iplComma, YES).
        
END FUNCTION.

FUNCTION fFormatString RETURNS CHARACTER PRIVATE
    ( ipcString AS CHARACTER, ipiCharacters AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose:  Formats string with number of characters.  If string is larger than what fits, 
     it auto adds a "cont" string to end
     Notes:
    ------------------------------------------------------------------------------*/    
    
    RETURN DYNAMIC-FUNCTION("FormatString", ipcString, ipiCharacters).
    
        
END FUNCTION.


