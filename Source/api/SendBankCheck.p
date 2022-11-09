/*------------------------------------------------------------------------
    File        : api/SendBankCheck.p
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

DEFINE VARIABLE iNumberOfTransactions            AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTotalSum                        AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE cCountry                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBankAccount                     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cVendorRemitToCountry            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrencyType                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBankIdentifierCode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorCountry                   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cVendorName                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorRemitToPostalCode         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorRemitToCity               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorRemitToState              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorRemitToStreetAddress1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorRemitToStreetAddress2     AS CHARACTER NO-UNDO.

DEFINE VARIABLE iInvoiceNumber                   AS INTEGER   NO-UNDO.
DEFINE VARIABLE dtInvoiceDate                    AS DATE      NO-UNDO.
DEFINE VARIABLE cCreditDebitIndicator            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAdditionalRemittanceInformation AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRequestDataType                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLineItemsData                  AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineItemsData            AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iPaymentID                       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lPostManual                      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCompany                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCheckAmount                     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iLineCount                       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFillZero                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProcessingCentre                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLineItemsDetailData            AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineDetailItemsData      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cCompCurr                        AS CHARACTER NO-UNDO.

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
    FIELD delMethod   AS CHARACTER
    .

DEFINE BUFFER bf-APIOutbound                FOR APIOutbound.
DEFINE BUFFER bf-line-APIOutboundDetail     FOR APIOutboundDetail.
DEFINE BUFFER bf-line-detail-APIOutboundDetail FOR APIOutboundDetail.

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
       AND ttArgs.argKey   EQ "PostManual"
    NO-ERROR.
    
lPostManual = IF AVAIL ttArgs THEN logical(ttArgs.argValue) ELSE NO. 

FIND FIRST ttArgs
     WHERE ttArgs.argType  EQ "ROWID"
       AND ttArgs.argKey   EQ "Company"
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
 
FIND FIRST bf-line-detail-APIOutboundDetail NO-LOCK
     WHERE bf-line-detail-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-line-detail-APIOutboundDetail.detailID      EQ "CheckDetail"
       AND bf-line-detail-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR. 
 
cRequestDataType = bf-APIOutbound.requestDataType. 
FIND FIRST company WHERE company.company EQ cCompany NO-LOCK NO-ERROR.
IF AVAILABLE company THEN cCompCurr = company.curr-code.

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
        ttPaymentData.delMethod   = IF ap-sel.deliveryMethod EQ "01" THEN "Overnight Delivery Payee"
                                    ELSE IF ap-sel.deliveryMethod EQ "03" THEN "Mail to Payor"
                                    ELSE "Mail Check Payee"
        .    
    IF cCompCurr NE ""  AND cCompCurr NE ap-inv.curr-code[1] THEN do:
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
        dTotalSum = dTotalSum +  ttPaymentData.amt .
        
END.  
iLineCount = 0.        
FOR EACH ttPaymentData
    BREAK BY ttPaymentData.checkNo:
        
    IF FIRST-OF(ttPaymentData.checkNo) THEN
    dCheckAmount = 0.
    dCheckAmount = ROUND(dCheckAmount + ttPaymentData.amt,2).
    cFillZero = FILL("0",1500).
    
    IF FIRST-OF(ttPaymentData.checkNo) THEN
    DO:
        lcConcatLineDetailItemsData = "".
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ ttPaymentData.company
               AND vend.vend-no EQ ttPaymentData.vendNo
             NO-ERROR.
        IF AVAILABLE vend THEN
            ASSIGN
                cVendorName                        = vend.name
                cVendorRemitToCountry              = vend.r-country
                cVendorRemitToPostalCode           = vend.r-zip
                cVendorRemitToCity                 = vend.r-city
                cVendorCountry                     = vend.country
                cVendorRemitToStreetAddress1       = vend.r-add1
                cVendorRemitToStreetAddress2       = vend.r-add2
                cVendorRemitToState                = vend.r-state
                cBankAccount                       = vend.bank-acct
                cCreditDebitIndicator              = vend.tax-gr
                cCountry                           = vend.country
                .
    END.
        
    lcLineItemsDetailData = STRING(bf-line-detail-APIOutboundDetail.data).          
    RUN updateRequestData(INPUT-OUTPUT lcLineItemsDetailData, "InvoiceNumber", STRING(ttPaymentData.invNo)).
    RUN updateRequestData(INPUT-OUTPUT lcLineItemsDetailData, "InvoiceDate", STRING(ttPaymentData.invDate)).
    RUN updateRequestData(INPUT-OUTPUT lcLineItemsDetailData, "DuePayableAmount", STRING(ttPaymentData.grossAmt)).
    RUN updateRequestData(INPUT-OUTPUT lcLineItemsDetailData, "Amount", STRING(ttPaymentData.adjAmt)).
    RUN updateRequestData(INPUT-OUTPUT lcLineItemsDetailData, "CreditDebitIndicator", cCreditDebitIndicator).
    RUN updateRequestData(INPUT-OUTPUT lcLineItemsDetailData, "RemittedAmount", STRING(ttPaymentData.netAmt)).
    
    lcConcatLineDetailItemsData = lcConcatLineDetailItemsData + lcLineItemsDetailData.       
                   
    IF LAST-OF(ttPaymentData.checkNo) THEN DO:
        
        cProcessingCentre = "".        
        IF cVendorRemitToCity EQ "Halifax" THEN
        cProcessingCentre = "00330".
        ELSE IF cVendorRemitToCity EQ "Montreal" THEN
        cProcessingCentre = "00310".
        ELSE IF cVendorRemitToCity EQ "Toronto" THEN
        cProcessingCentre = "00320".
        ELSE IF cVendorRemitToCity EQ "Regina" THEN
        cProcessingCentre = "00278".
        ELSE IF cVendorRemitToCity EQ "Winnipeg" THEN
        cProcessingCentre = "00370".
        ELSE IF cVendorRemitToCity EQ "Calgary" THEN
        cProcessingCentre = "00390".
        ELSE IF cVendorRemitToCity EQ "Vancouver" THEN
        cProcessingCentre = "00300".
                    
        FIND FIRST bank NO-LOCK
             WHERE bank.company   EQ ttPaymentData.company
               AND bank.bank-code EQ ttPaymentData.bankCode
             NO-ERROR.
        IF AVAILABLE bank THEN
            ASSIGN
                cCurrencyType  = bank.curr-code[1]
                .
        lcLineItemsData = STRING(bf-line-APIOutboundDetail.data).         
        iLineCount = iLineCount + 1.        
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TransactionAmount", STRING(dCheckAmount)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ChequeNumber", STRING(ttPaymentData.checkNo)).         
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorName", cVendorName).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorRemitToPostalCode", cVendorRemitToPostalCode).    
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorRemitToCity", cVendorRemitToCity).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorRemitToState", cVendorRemitToState).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorCountry", cVendorCountry).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorRemitToStreetAddress1", cVendorRemitToStreetAddress1).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "VendorRemitToStreetAddress2", cVendorRemitToStreetAddress2).                 
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "AdditionalRemittanceInformation", cAdditionalRemittanceInformation).
                                             
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CheckCounter", STRING(iLineCount)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "BankAccount",cBankAccount).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "SPACE", " ").          
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "FillZero", cFillZero).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "DeliveryMethod", ttPaymentData.delMethod).
        
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CheckDetail", lcConcatLineDetailItemsData).
        
        lcConcatLineItemsData = lcConcatLineItemsData + lcLineItemsData. 
                
    END.
    IF LAST(ttPaymentData.checkNo) THEN DO:  
    
        ioplcRequestData = REPLACE(ioplcRequestData, "$Detail$", lcConcatLineItemsData).                   
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "NumberOfTransactions", STRING(iNumberOfTransactions)).        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalSum", STRING(dTotalSum)).         
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Country",cCountry). 
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankAccount",cBankAccount).        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorRemitToCountry", cVendorRemitToCountry).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrencyType",cCurrencyType).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankIdentifierCode",cBankIdentifierCode).        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorCountry",cVendorCountry).
        
       RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ProcessingCentre",cProcessingCentre). 
       RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SPACE", " ").
       RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FillZero", cFillZero).
       RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CheckCounter","1").
    END.
END. /* FOR EACH ttPaymentData */     

RUN pUpdateDelimiter(
    INPUT-OUTPUT ioplcRequestData,
    INPUT        cRequestDataType
    ).                

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .
     
