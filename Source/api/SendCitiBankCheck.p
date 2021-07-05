/*------------------------------------------------------------------------
    File        : api/SendCitiBankCheck.p
    Purpose     : Returns the request data for xml check

    Syntax      :

    Description : Returns the request data for xml check

    Author(s)   : Sewa Singh
    Created     : Fr July 2 07:33:22 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

{api/ttArgs.i}
{api/CommonAPIProcs.i}

DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

DEFINE VARIABLE iMessageId                              AS INTEGER      NO-UNDO.
DEFINE VARIABLE cCreateDateTime                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNumberOfTransactions                   AS INTEGER      NO-UNDO.
DEFINE VARIABLE dControlSum                             AS DECIMAL      NO-UNDO.
DEFINE VARIABLE cCompanyName                            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iPaymentInformationIdentification       AS INTEGER      NO-UNDO.
DEFINE VARIABLE cPaymentMethod                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCode                                   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cProprietary                            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTransactionDate                        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCountry                                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCompanyId                              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTransactionId                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCountryOfResidence                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCurrencyType                           AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cBankIdentifierCode                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cMemberIdentification                   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cVendorCountry                          AS CHARACTER    NO-UNDO.

DEFINE VARIABLE iInstructionIdentification              AS INTEGER      NO-UNDO.
DEFINE VARIABLE cChequeMaturityDate                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lcVendorName                            AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cVendorPostCode                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cVendorTownName                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cVendorCountrySubDivision               AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lcVendorPostalAddress1                  AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE lcVendorPostalAddress2                  AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cRemittanceIdentification               AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cUnstructured                           AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cVendorCode                             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iInvoiceNumber                          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dtInvoiceDate                           AS DATE         NO-UNDO.
DEFINE VARIABLE cCreditDebitIndicator                   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAdditionalRemittanceInformation        AS CHARACTER    NO-UNDO.

DEFINE VARIABLE cRequestDataType                        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iSECount                                AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcLineItemsData                         AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE lcConcatLineItemsData                   AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE iPaymentID                              AS INTEGER      NO-UNDO.
DEFINE VARIABLE lPostManual                             AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cCompany                                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dCheckAmount                            AS DECIMAL      NO-UNDO.

DEFINE TEMP-TABLE ttPaymentData NO-UNDO
    FIELD paymentID   AS INTEGER
    FIELD company     AS CHARACTER
    FIELD checkNo     AS INTEGER
    FIELD invNo       AS CHARACTER
    FIELD vendNo      AS CHARACTER
    FIELD bankCode    AS CHARACTER
    FIELD paymentType AS CHARACTER
    FIELD description AS CHARACTER
    FIELD grossAmt    AS DECIMAL
    FIELD adjAmt      AS DECIMAL
    FIELD netAmt      AS DECIMAL
    FIELD Amt         AS DECIMAL
    FIELD checkDate   AS DATE
    FIELD invDate     AS DATE
    .

DEFINE BUFFER bf-APIOutbound                FOR APIOutbound.
DEFINE BUFFER bf-line-APIOutboundDetail     FOR APIOutboundDetail.

IF ipcRequestHandler NE "" THEN DO:
    RUN VALUE(ipcRequestHandler) (
        INPUT TABLE ttArgs,
        INPUT ipiAPIOutboundID,
        INPUT ipiAPIOutboundTriggerID,
        INPUT-OUTPUT ioplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).      
    RETURN.
END.

FIND FIRST bf-APIOutbound NO-LOCK
     WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID
     NO-ERROR.
IF NOT AVAILABLE bf-APIOutbound THEN DO:
    ASSIGN
        opcMessage = "No APIOutbound record found"
        oplSuccess = FALSE
        .
    RETURN.        
END. 

FIND FIRST ttArgs
     WHERE ttArgs.argType  EQ "ROWID"
       AND ttArgs.argKey   EQ "post-manual"
    NO-ERROR.
    
lPostManual = IF AVAIL ttArgs THEN logical(ttArgs.argValue) ELSE NO. 

FIND FIRST ttArgs
     WHERE ttArgs.argType  EQ "ROWID"
       AND ttArgs.argKey   EQ "cocode"
    NO-ERROR.
  
cCompany = IF AVAIL ttArgs THEN ttArgs.argValue ELSE "".
IF NOT AVAILABLE ttArgs THEN DO:
    ASSIGN
        opcMessage = "No valid company record passed to handler"
        oplSuccess = FALSE
        .
    RETURN.
END.  
 
FIND FIRST bf-line-APIOutboundDetail NO-LOCK
     WHERE bf-line-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-line-APIOutboundDetail.detailID      EQ "Detail"
       AND bf-line-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR.

cRequestDataType = bf-APIOutbound.requestDataType. 

FIND FIRST company NO-LOCK
     WHERE company.company EQ cCompany 
     NO-ERROR.
IF AVAILABLE company THEN
    cCompanyName = company.name.

FOR EACH ap-sel NO-LOCK
    WHERE ap-sel.company      EQ cCompany 
      AND ap-sel.check-no     NE ?
      AND ap-sel.check-no     GT 0
      AND ap-sel.man-check    EQ LOGICAL(lPostManual)  
      AND TRIM(ap-sel.inv-no) GT ""
      AND CAN-FIND(FIRST ap-chk
                   WHERE ap-chk.company   EQ ap-sel.company
                     AND ap-chk.check-no  EQ ap-sel.check-no
                     AND ap-chk.man-check EQ ap-sel.man-check),
    FIRST ap-inv NO-LOCK
    WHERE ap-inv.company EQ ap-sel.company
      AND ap-inv.vend-no EQ ap-sel.vend-no
      AND ap-inv.inv-no  EQ ap-sel.inv-no,
    FIRST vend NO-LOCK
    WHERE vend.company EQ ap-sel.company
      AND vend.vend-no EQ ap-sel.vend-no
    BREAK BY ap-sel.check-no:
                            
    iPaymentID = iPaymentID + 1.

    CREATE ttPaymentData.
    ASSIGN
        ttPaymentData.paymentID   = iPaymentID
        ttPaymentData.company     = ap-sel.company
        ttPaymentData.checkNo     = ap-sel.check-no
        ttPaymentData.invNo       = ap-sel.inv-no
        ttPaymentData.vendNo      = vend.vend-no
        ttPaymentData.bankCode    = ap-sel.bank-code
        ttPaymentData.grossAmt    = ap-sel.inv-bal
        ttPaymentData.adjAmt      = ap-sel.disc-amt
        ttPaymentData.netAmt      = ap-sel.amt-paid
        ttPaymentData.paymentType = vend.payment-type
        ttPaymentData.invDate     = ap-inv.inv-date
        ttPaymentData.checkDate   = TODAY
        ttPaymentData.amt         = ap-sel.amt-paid * (ap-sel.amt-paid / (ap-inv.net + ap-inv.freight))
        .    

    FIND FIRST currency NO-LOCK
         WHERE currency.company     EQ ap-inv.company
           AND currency.c-code      EQ ap-inv.curr-code[1]
           AND currency.ar-ast-acct NE ""
           AND currency.ex-rate     GT 0
         NO-ERROR.
    IF AVAILABLE currency THEN
        ASSIGN
            ttPaymentData.grossAmt = ttPaymentData.grossAmt * currency.ex-rate
            ttPaymentData.adjAmt   = ttPaymentData.adjAmt * currency.ex-rate
            ttPaymentData.netAmt   = ttPaymentData.netAmt * currency.ex-rate
            .
END.

FIND FIRST ttPaymentData NO-ERROR.
IF AVAILABLE ttPaymentData THEN
    FIND FIRST bank NO-LOCK
         WHERE bank.company   EQ ttPaymentData.company
           AND bank.bank-code EQ ttPaymentData.bankCode
         NO-ERROR.
IF AVAILABLE bank THEN
ASSIGN
    cBankIdentifierCode = STRING(bank.bk-act)
    .
    
FOR EACH ttPaymentData
    BREAK BY ttPaymentData.checkNo:
        IF FIRST-OF(ttPaymentData.checkNo) THEN
        iNumberOfTransactions = iNumberOfTransactions + 1.
        dControlSum = dControlSum +  ttPaymentData.amt .
        
END. 

ASSIGN  iMessageId                         = 0119
        iPaymentInformationIdentification  = 100001
        cPaymentMethod                     = "CHK"
        iInstructionIdentification         = 056502
        cRemittanceIdentification          = "/ADVCD///TMP01"
        cUnstructured                      = "/NAVC/PAT01U"
        cVendorCode                        = "CINV"
        cCode                              = "NURG"
        cProprietary                       = "CITI19"
        cTransactionId                     = "TXID"
        cMemberIdentification              = "031100209"
        cCreateDateTime                    = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") + " " + STRING(TIME, "HH:MM")
        cChequeMaturityDate                = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") 
        cTransactionDate                   = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") 
        .
        
FOR EACH ttPaymentData
    BREAK BY ttPaymentData.checkNo:
        
    IF FIRST-OF(ttPaymentData.checkNo) THEN
    dCheckAmount = 0.
    dCheckAmount = dCheckAmount + ttPaymentData.amt.
    
    IF LAST-OF(ttPaymentData.checkNo) THEN DO:
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ ttPaymentData.company
               AND vend.vend-no EQ ttPaymentData.vendNo
             NO-ERROR.
        IF AVAILABLE vend THEN
            ASSIGN
                lcVendorName                       = vend.name
                cCountryOfResidence                = vend.r-country
                cVendorPostCode                    = vend.r-zip
                cVendorTownName                    = vend.r-city
                cVendorCountry                     = vend.country
                lcVendorPostalAddress1             = vend.r-add1
                lcVendorPostalAddress2             = vend.r-add2
                cVendorCountrySubDivision          = vend.r-state
                cCompanyId                         = vend.bank-acct
                cCreditDebitIndicator              = vend.tax-gr
                cCountry                           = vend.country
                .
            
        FIND FIRST bank NO-LOCK
             WHERE bank.company   EQ ttPaymentData.company
               AND bank.bank-code EQ ttPaymentData.bankCode
             NO-ERROR.
        IF AVAILABLE bank THEN
            ASSIGN
                cCurrencyType  = bank.curr-code[1]
                .
        lcLineItemsData = STRING(bf-line-APIOutboundDetail.data).         
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "InstructionIdentification", STRING(iInstructionIdentification)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TransactionReferenceNumber", STRING(ttPaymentData.checkNo)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TransactionAmount", STRING(dCheckAmount)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ChequeNumber", STRING(ttPaymentData.checkNo)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ChequeMaturityDate", cChequeMaturityDate).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorName", lcVendorName).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorPostCode", cVendorPostCode).    
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorTownName", cVendorTownName).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorCountrySubDivision", cVendorCountrySubDivision).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorCountry", cVendorCountry).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorPostalAddress1", lcVendorPostalAddress1).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorPostalAddress2", lcVendorPostalAddress2).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "RemittanceIdentification", cRemittanceIdentification).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "Unstructured", cUnstructured).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorCode", cVendorCode).    
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "Code", cCode).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "InvoiceNumber", STRING(ttPaymentData.invNo)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "InvoiceDate", STRING(ttPaymentData.invDate)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "DuePayableAmount", STRING(dCheckAmount)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "Amount", STRING(dCheckAmount)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CreditDebitIndicator", cCreditDebitIndicator).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "RemittedAmount", STRING(dCheckAmount)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "AdditionalRemittanceInformation", cAdditionalRemittanceInformation).
        
        lcConcatLineItemsData = lcConcatLineItemsData + lcLineItemsData.        
        
    END.
    IF LAST(ttPaymentData.checkNo) THEN DO:  
    
        ioplcRequestData = REPLACE(ioplcRequestData, "$Detail$", lcConcatLineItemsData).
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "MessageId", STRING(iMessageId)).  
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CreateDateTime", cCreateDateTime).                   
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "NumberOfTransactions", STRING(iNumberOfTransactions)).        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ControlSum", STRING(dControlSum)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyName", cCompanyName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PaymentInformationIdentification", STRING(iPaymentInformationIdentification)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PaymentMethod", cPaymentMethod).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Code", cCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Proprietary", cProprietary).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TransactionDate",cTransactionDate). 
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Country",cCountry). 
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyId",cCompanyId).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TransactionId",cTransactionId).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CountryOfResidence", cCountryOfResidence).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrencyType",cCurrencyType).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankIdentifierCode",cBankIdentifierCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "MemberIdentification",cMemberIdentification).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorCountry",cVendorCountry).
    END.
END. /* FOR EACH ttPaymentData */  
   
ASSIGN 
    iSECount = NUM-ENTRIES(ioplcRequestData, "~n") - 1   
    /* Subtract lines before ST and after SE segments */
    iSECount = iSECount - 4
    .
    
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SECount", STRING(iSECount)).

RUN pUpdateDelimiter(
    INPUT-OUTPUT ioplcRequestData,
    INPUT        cRequestDataType
    ).                

/* To remove any unwanted blank lines */
ioplcRequestData = REPLACE(ioplcRequestData, "~n~n", "~n").

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .
     
