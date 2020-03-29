/*------------------------------------------------------------------------
    File        : ap/APExportPayablesAdvFormat.p
    Purpose     : 

    Syntax      :

    Description : Creates a bank tranmittable file in Payables Advantage format

    Author(s)   : Mithun Porandla
    Created     : Tue Sep 24 18:31:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplManual       AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcFullFilePath AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcBankCode     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

DEFINE VARIABLE iPaymentID    AS INTEGER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE  NO-UNDO.

/* Header variables */
DEFINE VARIABLE cHeaderRecordIdentifier           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHeaderSenderIdentificationNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHeaderBusinessApplicationFormat  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHeaderCustomerName               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHeaderTransmissionDate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHeaderTransmissionTime           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHeaderClientFileID               AS CHARACTER NO-UNDO.

/* Trailer variables */
DEFINE VARIABLE cTrailerRecordIdentifier          AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTrailerTotalDollarAmountOfChecks AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iTrailerTotalCheckRecords         AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTrailerTotalDollarAmountOfACHs   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iTrailerTotalRecordsACHs          AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTrailerTotalPaymentDollarAmounts AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iTrailerTotalPaymentRecords       AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTrailerTotalFileDollarAmounts    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iTrailerTotalFileRecords          AS INTEGER   NO-UNDO.

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
    FIELD checkDate   AS DATE
    FIELD invDate     AS DATE
    .

DEFINE STREAM bankFileStr.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

FUNCTION formatString RETURNS CHARACTER PRIVATE
    (ipcString   AS CHARACTER,
     ipcDataType AS CHARACTER,
     ipiLength   AS INTEGER) FORWARD.

ASSIGN
    cHeaderRecordIdentifier           = "010"                  /* File Header Record Identifier */
    cHeaderBusinessApplicationFormat  = "AP0"                  /* Business Application format number */
    cHeaderTransmissionDate           = STRING(TODAY)
    cHeaderTransmissionTime           = STRING(TIME, "HH:MM")
    cTrailerRecordIdentifier          = "090"                  /* File Trailer record identifier */
    .

FIND FIRST company NO-LOCK
     WHERE company.company EQ ipcCompany
     NO-ERROR.
IF AVAILABLE company THEN
    cHeaderCustomerName = company.name.

FOR EACH ap-sel NO-LOCK
    WHERE ap-sel.company      EQ ipcCompany
      AND ap-sel.check-no     NE ?
      AND ap-sel.check-no     GT 0
      AND ap-sel.man-check    EQ iplManual  /* Manual */
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

IF NOT TEMP-TABLE ttPaymentData:HANDLE:HAS-RECORDS THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "No A/P Checks available for posting"
        .
    RETURN.
END.

FIND FIRST ttPaymentData NO-ERROR.
IF AVAILABLE ttPaymentData THEN
    FIND FIRST bank NO-LOCK
         WHERE bank.company   EQ ttPaymentData.company
           AND bank.bank-code EQ ttPaymentData.bankCode
         NO-ERROR.
IF NOT AVAILABLE bank THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "Bank Information not available"
        .
    RETURN. 
END.

IF bank.pay-type NE "PA" THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "Bank Code " + bank.bank-code
                   + " does not allow payment type 'PA'"
        .
    RETURN.
END.

ASSIGN
    cHeaderSenderIdentificationNumber = STRING(bank.bk-act)
    opcBankCode                       = bank.bank-code
    .

OUTPUT STREAM bankFileStr TO VALUE(ipcFullFilePath).
    
PUT STREAM bankFileStr UNFORMATTED
    formatString(cHeaderRecordIdentifier,"CHARACTER",3)            /* Len - 3,   Start - 1,  End - 3    */
    formatString(cHeaderSenderIdentificationNumber,"CHARACTER",15) /* Len - 15,  Start - 4,  End - 18   */
    formatString(cHeaderBusinessApplicationFormat,"CHARACTER",3)   /* Len - 3,   Start - 19, End - 21   */
    formatString(cHeaderCustomerName,"CHARACTER",16)               /* Len - 16,  Start - 22, End - 37   */
    formatString(cHeaderTransmissionDate,"DATE",8)                 /* Len - 8,   Start - 38, End - 45   */
    formatString(cHeaderTransmissionTime,"TIME",4)                 /* Len - 4,   Start - 46, End - 49   */
    formatString(cHeaderClientFileID,"CHARACTER",10)               /* Len - 10,  Start - 50, End - 59   */
    formatString("","CHARACTER",291)                               /* Len - 291, Start - 60, End - 350  */
    SKIP
    .

FOR EACH ttPaymentData
    BREAK BY ttPaymentData.checkNo:
    IF FIRST-OF(ttPaymentData.checkNo) THEN DO:
        IF ttPaymentData.paymentType EQ "Check" THEN DO:
            RUN pPrintCheckHeaderRecords (
                INPUT ttPaymentData.company,
                INPUT ttPaymentData.checkNo,
                INPUT ttPaymentData.paymentID
                ).
            
            ASSIGN
                iTrailerTotalCheckRecords = iTrailerTotalCheckRecords + 1
                iTrailerTotalFileRecords  = iTrailerTotalFileRecords + 2 /* Check Remittance Header and Check Header records */
                .
        END.
        ELSE IF ttPaymentData.paymentType EQ "ACH" THEN DO:
            RUN pPrintACHHeaderRecords (
                INPUT ttPaymentData.company,
                INPUT ttPaymentData.checkNo,
                INPUT ttPaymentData.paymentID
                ).
            
            ASSIGN
                iTrailerTotalRecordsACHs = iTrailerTotalRecordsACHs + 1
                iTrailerTotalFileRecords = iTrailerTotalFileRecords + 2 /* Check Remittance Header and Check Header records */
                .
        END.
    END.

    IF ttPaymentData.paymentType EQ "Check" THEN DO:
        RUN pPrintCheckDetailRecords (
            INPUT ttPaymentData.company,
            INPUT ttPaymentData.checkNo,
            INPUT ttPaymentData.paymentID
            ).

        ASSIGN
            dTrailerTotalDollarAmountOfChecks = dTrailerTotalDollarAmountOfChecks + ttPaymentData.netAmt
            iTrailerTotalFileRecords          = iTrailerTotalFileRecords + 1 /* Check Remittance detail records */
            .
    END.
    ELSE IF ttPaymentData.paymentType EQ "ACH" THEN DO:
        RUN pPrintACHDetailRecords (
            INPUT ttPaymentData.company,
            INPUT ttPaymentData.checkNo,
            INPUT ttPaymentData.paymentID
            ).

        ASSIGN
            dTrailerTotalDollarAmountOfACHs = dTrailerTotalDollarAmountOfACHs + ttPaymentData.netAmt
            iTrailerTotalFileRecords        = iTrailerTotalFileRecords + 1 /* Check Remittance detail records */
            .
    END.
    
    IF LAST-OF(ttPaymentData.checkNo) THEN DO:
        IF ttPaymentData.paymentType EQ "Check" THEN DO:
            RUN pPrintCheckTrailerRecords (
                INPUT ttPaymentData.company,
                INPUT ttPaymentData.checkNo,
                INPUT ttPaymentData.paymentID
                ).
            
            iTrailerTotalFileRecords  = iTrailerTotalFileRecords + 1. /* Check Remittance Trailer records */
        END.
        ELSE IF ttPaymentData.paymentType EQ "ACH" THEN DO:
            RUN pPrintACHTrailerRecords.
                        
            iTrailerTotalFileRecords  = iTrailerTotalFileRecords + 1. /* Check Remittance Trailer records */
        END.
    END.
    
    ASSIGN
        dTrailerTotalPaymentDollarAmounts = dTrailerTotalPaymentDollarAmounts + ttPaymentData.netAmt
        iTrailerTotalPaymentRecords       = iTrailerTotalPaymentRecords + 1
        dTrailerTotalFileDollarAmounts    = dTrailerTotalFileDollarAmounts + ttPaymentData.netAmt
        .
END.

iTrailerTotalFileRecords = iTrailerTotalFileRecords + 2.  /* File Header and Trailer Records count */
    
PUT STREAM bankFileStr UNFORMATTED
    formatString(cTrailerRecordIdentifier, "CHARACTER", 3)                 /* Len - 3,   Start - 1,   End - 3   */
    formatString(STRING(dTrailerTotalDollarAmountOfChecks), "DECIMAL", 13) /* Len - 13,  Start - 4,   End - 16  */
    formatString(STRING(iTrailerTotalCheckRecords), "INTEGER", 7)          /* Len - 7,   Start - 17,  End - 23  */
    formatString(STRING(dTrailerTotalDollarAmountOfACHs), "DECIMAL", 13)   /* Len - 13,  Start - 24,  End - 36  */
    formatString(STRING(iTrailerTotalRecordsACHs), "INTEGER", 7)           /* Len - 7,   Start - 37,  End - 43  */
    formatString("", "DECIMAL", 13)                                        /* Len - 13,  Start - 44,  End - 56  */
    formatString("", "INTEGER", 7)                                         /* Len - 7,   Start - 57,  End - 63  */
    formatString("", "DECIMAL", 13)                                        /* Len - 13,  Start - 64,  End - 76  */
    formatString("", "INTEGER", 7)                                         /* Len - 7,   Start - 77,  End - 83  */
    formatString(STRING(dTrailerTotalPaymentDollarAmounts), "DECIMAL", 13) /* Len - 13,  Start - 84,  End - 96  */
    formatString(STRING(iTrailerTotalPaymentRecords), "INTEGER", 7)        /* Len - 7,   Start - 97,  End - 103 */
    formatString(STRING(dTrailerTotalFileDollarAmounts), "DECIMAL", 13)    /* Len - 13,  Start - 104, End - 116 */
    formatString(STRING(iTrailerTotalFileRecords), "INTEGER", 7)           /* Len - 7,   Start - 117, End - 123 */
    formatString("","CHARACTER",227)                                       /* Len - 227, Start - 124, End - 350 */
    .

OUTPUT STREAM bankFileStr CLOSE.

DELETE OBJECT hdOutputProcs.

ASSIGN
    oplSuccess = TRUE
    opcMessage = "Sucess"
    .
    
PROCEDURE pPrintCheckHeaderRecords:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCheckNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPaymentID AS INTEGER   NO-UNDO.
    
    /* Check Record variables */
    DEFINE VARIABLE cCheckRecordIdentifier        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPaymentType             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckInstruction             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckVendorNumber            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckMailInstruction         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckCurrencyType            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckSerialNumber            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckEffectiveDateofCheck    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCheckPaymentAmount           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCheckPayeeName1              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeName2              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeAddress1           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeAddress2           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeCity               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeState              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeZipCode            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckPayeeCountry            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCheckNumberofRemittanceLines AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCheckPaymentdata             AS CHARACTER NO-UNDO.
    
    /* Check record remittance header variables */
    DEFINE VARIABLE cCheckHeaderRecordIdentifier    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckHeaderSubRecordIdentifier AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bfttPaymentData FOR ttPaymentData.
    
    FOR EACH bfttPaymentData
        WHERE bfttPaymentData.company EQ ipcCompany
          AND bfttPaymentData.checkNo EQ ipiCheckNo:
        ASSIGN
            dCheckPaymentAmount           = dCheckPaymentAmount + bfttPaymentData.netAmt
            iCheckNumberofRemittanceLines = iCheckNumberofRemittanceLines + 1
            .
    END.

    FIND FIRST bfttPaymentData
        WHERE bfttPaymentData.paymentID EQ ipiPaymentID 
        NO-ERROR.

    ASSIGN
        cCheckRecordIdentifier          = "060"       /* Check Header Record Identifier */
        cCheckPaymentType               = "CHK"       /* "CHK" - for check payment type */
        cCheckInstruction               = "X"         /* "X" = Print Check, "U" = Print Statement/Advice only, "I"  = Issue Info, "V" = Void, "T" = Vendor Advice */
        cCheckSerialNumber              = STRING(bfttPaymentData.checkNo)
        cCheckEffectiveDateofCheck      = STRING(bfttPaymentData.checkDate)
        cCheckHeaderRecordIdentifier    = "070"       /* Check Remittance Header record identifier */
        cCheckHeaderSubRecordIdentifier = "01"        /* Check Remittance Header sub-record identifier */
        .
    
    FIND FIRST vend NO-LOCK
         WHERE vend.company EQ bfttPaymentData.company
           AND vend.vend-no EQ bfttPaymentData.vendNo
         NO-ERROR.
    IF AVAILABLE vend THEN
        ASSIGN
            cCheckVendorNumber    = vend.vend-no
            cCheckPayeeName1      = vend.name
            cCheckPayeeName2      = vend.remit
            cCheckPayeeAddress1   = vend.r-add1
            cCheckPayeeAddress2   = vend.r-add2
            cCheckPayeeCity       = vend.r-city
            cCheckPayeeState      = vend.r-state
            cCheckPayeeZipCode    = vend.r-zip
            cCheckPayeeCountry    = vend.r-country
            cCheckPaymentData     = vend.check-memo
            cCheckMailInstruction = IF vend.country BEGINS "CA" THEN /* country validation for Canada */
                                        "CN"
                                    ELSE
                                        "UM"
            .
        
    FIND FIRST bank NO-LOCK
         WHERE bank.company   EQ bfttPaymentData.company
           AND bank.bank-code EQ bfttPaymentData.bankCode
         NO-ERROR.
    IF AVAILABLE bank THEN
        cCheckCurrencyType = bank.curr-code[1].

    PUT STREAM bankFileStr UNFORMATTED
        formatString(cCheckRecordIdentifier,"CHARACTER",3)              /* Len - 3,  Start - 1,   End - 3   */
        formatString(cCheckPaymentType,"CHARACTER",3)                   /* Len - 3,  Start - 4,   End - 6   */
        formatString(cCheckInstruction,"CHARACTER",1)                   /* Len - 1,  Start - 7,   End - 7   */
        formatString(cCheckVendorNumber,"CHARACTER",10)                 /* Len - 10, Start - 8,   End - 17  */
        formatString(cCheckMailInstruction,"CHARACTER",2)               /* Len - 2,  Start - 18,  End - 19  */
        formatString("","CHARACTER",2)                                  /* Len - 2,  Start - 20,  End - 21  */
        formatString(cCheckCurrencyType,"CHARACTER",3)                  /* Len - 3,  Start - 22,  End - 24  */
        formatString(cCheckSerialNumber,"INTEGER",10)                   /* Len - 10, Start - 25,  End - 34  */
        formatString(cCheckEffectiveDateofCheck,"DATE",8)               /* Len - 8,  Start - 35,  End - 42  */
        formatString(STRING(dCheckPaymentAmount),"DECIMAL",13)          /* Len - 13, Start - 43,  End - 55  */
        formatString(cCheckPayeeName1,"CHARACTER",35)                   /* Len - 35, Start - 56,  End - 90  */
        formatString(cCheckPayeeName2,"CHARACTER",35)                   /* Len - 35, Start - 91,  End - 125 */
        formatString(cCheckPayeeAddress1,"CHARACTER",35)                /* Len - 35, Start - 126, End - 160 */
        formatString(cCheckPayeeAddress2,"CHARACTER",35)                /* Len - 35, Start - 161, End - 195 */
        formatString("","CHARACTER",35)                                 /* Len - 35, Start - 196, End - 230 */
        formatString(cCheckPayeeCity,"CHARACTER",27)                    /* Len - 27, Start - 231, End - 257 */
        formatString(cCheckPayeeState,"CHARACTER",2)                    /* Len - 2,  Start - 258, End - 259 */
        formatString(cCheckPayeeZipCode,"CHARACTER",9)                  /* Len - 9,  Start - 260, End - 268 */
        formatString("","CHARACTER",1)                                  /* Len - 1,  Start - 269, End - 269 */
        formatString(cCheckPayeeCountry,"CHARACTER",3)                  /* Len - 3,  Start - 270, End - 272 */
        formatString(STRING(iCheckNumberofRemittanceLines),"INTEGER",5) /* Len - 5,  Start - 273, End - 277 */
        formatString("","CHARACTER",10)                                 /* Len - 10, Start - 278, End - 287 */
        formatString(cCheckPaymentdata,"CHARACTER",50)                  /* Len - 50, Start - 288, End - 337 */
        formatString("","CHARACTER",13)                                 /* Len - 13, Start - 338, End - 350 */

        SKIP
        .      
        
    PUT STREAM bankFileStr UNFORMATTED
        formatString(cCheckHeaderRecordIdentifier,"CHARACTER",3)    /* Len - 3,   Start - 1, End - 3   */
        formatString(cCheckHeaderSubRecordIdentifier,"CHARACTER",2) /* Len - 2,   Start - 4, End - 5   */
        formatString("","CHARACTER",345)                            /* Len - 345, Start - 6, End - 350 */
        SKIP
        .
        
END PROCEDURE.

PROCEDURE pPrintCheckDetailRecords:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCheckNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPaymentID AS INTEGER   NO-UNDO.

    /* Check record remittance detail variables */
    DEFINE VARIABLE cCheckDetailRecordIdentifier    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailSubRecordIdentifier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailInvoiceDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailInvoiceNumber       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailDescriptiveText     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailInvoiceGrossAmount  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailAdjustedAmount      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckDetailNetAmount           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bfttPaymentData FOR ttPaymentData.
    
    FIND FIRST bfttPaymentData
        WHERE bfttPaymentData.paymentID EQ ipiPaymentID 
        NO-ERROR.

    ASSIGN
        cCheckDetailRecordIdentifier    = "070"                            /* Check Remittance Detail record identifier */
        cCheckDetailSubRecordIdentifier = "06"                             /* Check Remittance Detail sub-record identifier */
        cCheckDetailInvoiceDate         = STRING(bfttPaymentData.invDate)
        cCheckDetailInvoiceNumber       = STRING(bfttPaymentData.invNo)
        cCheckDetailDescriptiveText     = STRING(bfttPaymentData.description)
        cCheckDetailInvoiceGrossAmount  = STRING(bfttPaymentData.grossAmt)
        cCheckDetailAdjustedAmount      = STRING(bfttPaymentData.adjAmt)
        cCheckDetailNetAmount           = STRING(bfttPaymentData.netAmt)
        .

    PUT STREAM bankFileStr UNFORMATTED
        formatString(cCheckDetailRecordIdentifier,"CHARACTER",3)    /* Len - 3,   Start - 1,   End - 3   */
        formatString(cCheckDetailSubRecordIdentifier,"CHARACTER",2) /* Len - 2,   Start - 4,   End - 5   */
        formatString(cCheckDetailInvoiceDate,"DATE",8)              /* Len - 8,   Start - 6,   End - 13  */
        formatString(cCheckDetailInvoiceNumber,"CHARACTER",20)      /* Len - 20,  Start - 14,  End - 33  */
        formatString(cCheckDetailDescriptiveText,"CHARACTER",30)    /* Len - 30,  Start - 34,  End - 63  */
        formatString(cCheckDetailInvoiceGrossAmount,"DECIMAL",13)   /* Len - 13,  Start - 64,  End - 76  */
        formatString(cCheckDetailAdjustedAmount,"DECIMAL",13)       /* Len - 13,  Start - 77,  End - 89  */
        formatString(cCheckDetailNetAmount,"DECIMAL",13)            /* Len - 13,  Start - 90,  End - 102 */
        formatString("","CHARACTER",248)                            /* Len - 248, Start - 103, End - 350 */
        SKIP
        .         
END PROCEDURE.

PROCEDURE pPrintCheckTrailerRecords:        
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCheckNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPaymentID AS INTEGER   NO-UNDO.

    /* Check record remittance trailer variables */
    DEFINE VARIABLE cCheckTrailerRecordIdentifier    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckTrailerSubRecordIdentifier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCheckTrailerInvoiceTotalAmount  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCheckTrailerAdjustedTotalamount AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bfttPaymentData FOR ttPaymentData.
    
    FOR EACH bfttPaymentData
        WHERE bfttPaymentData.company EQ ipcCompany
          AND bfttPaymentData.checkNo EQ ipiCheckNo:
        ASSIGN
            dCheckTrailerInvoiceTotalAmount  = dCheckTrailerInvoiceTotalAmount  + bfttPaymentData.grossAmt
            dCheckTrailerAdjustedTotalamount = dCheckTrailerAdjustedTotalamount + bfttPaymentData.adjAmt
            .
    END.
    
    ASSIGN
        cCheckTrailerRecordIdentifier    = "070" /* Check Remittance Trailer record identifier */
        cCheckTrailerSubRecordIdentifier = "09"  /* Check Remittance Trailer sub-record identifier */
        .

    PUT STREAM bankFileStr UNFORMATTED
        formatString(cCheckTrailerRecordIdentifier,"CHARACTER",3)           /* Len - 3,   Start - 1,  End - 3   */
        formatString(cCheckTrailerSubRecordIdentifier,"CHARACTER",2)        /* Len - 2,   Start - 4,  End - 5   */
        formatString(STRING(dCheckTrailerInvoiceTotalAmount),"DECIMAL",13)  /* Len - 13,  Start - 6,  End - 18  */
        formatString(STRING(dCheckTrailerAdjustedTotalamount),"DECIMAL",13) /* Len - 13,  Start - 19, End - 31  */
        formatString("","CHARACTER",319)                                    /* Len - 319, Start - 32, End - 350 */
        SKIP
        .        
END PROCEDURE.

PROCEDURE pPrintACHHeaderRecords:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCheckNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPaymentID AS INTEGER   NO-UNDO.

    /* ACH header record variables */
    DEFINE VARIABLE cACHRecordIdentifier        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHPaymentType             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHCanadianIndicator       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHVendorNumber            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHCurrencyType            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHTraceNumber             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHPaymentEffectiveDate    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dACHPaymentAmount           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cACHReceiverName            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHIndividualID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHReceiverAddress1        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHReceiverAddress2        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHReceiverCity            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHReceiverState           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHReceiverZip             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iACHNumberofRemittanceLines AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cACHReceiverABA             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHReceiverAccountNumber   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHTranCode                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHType                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDiscretionaryData       AS CHARACTER NO-UNDO.
    
    /* ACH Remittance header reacord variables */
    DEFINE VARIABLE cACHHeaderRecordIdentifier    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHHeaderSubRecordIdentifier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHHeaderVendorNumber        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bfttPaymentData FOR ttPaymentData.
    
    FOR EACH bfttPaymentData
        WHERE bfttPaymentData.company EQ ipcCompany
          AND bfttPaymentData.checkNo EQ ipiCheckNo:
        ASSIGN
            dACHPaymentAmount           = dACHPaymentAmount + bfttPaymentData.netAmt
            iACHNumberofRemittanceLines = iACHNumberofRemittanceLines + 1
            .
    END.

    FIND FIRST bfttPaymentData
        WHERE bfttPaymentData.paymentID EQ ipiPaymentID 
        NO-ERROR.

    ASSIGN
        cACHRecordIdentifier          = "060"                /* ACH Header record identifier */
        cACHPaymentType               = "ACH"                /* ACH - for ACH payment type */
        cACHCurrencyType              = "USD"                /* Default currency type */
        cACHTraceNumber               = STRING(bfttPaymentData.checkNo)
        cACHPaymentEffectiveDate      = STRING(bfttPaymentData.checkDate)
        cACHHeaderRecordIdentifier    = "070"                /* ACH Header record identifier */
        cACHHeaderSubRecordIdentifier = "01"                 /* ACH Header sub-record identifier */
        cACHHeaderVendorNumber        = bfttPaymentData.vendNo
        cACHTranCode                  = "22"                 /* "22" = Demand Deposit Credit, "23" = Prenote of Demand Credit, "24" = Zero dollar with Remittance, "32" = Automated Deposit,"33" = Prenote of savings acct authorization, "34" = Zero dollar with remittance data */
        cACHType                      = "CTX"                /* "CCD" = Disbursement - Zero to 1 remittance,"CTX"  = Disbursement - 1 to 9999 remittance,"PPD" = Disbursement to Consumer - Zero to 1 remittance */
        .
    
    FIND FIRST vend NO-LOCK
         WHERE vend.company EQ bfttPaymentData.company
           AND vend.vend-no EQ bfttPaymentData.vendNo
         NO-ERROR.
    IF AVAILABLE vend THEN
        ASSIGN
            cACHReceiverName          = vend.remit
            cACHIndividualID          = vend.vend-no
            cACHReceiverAddress1      = vend.r-add1
            cACHReceiverAddress2      = vend.r-add2
            cACHReceiverCity          = vend.r-city
            cACHReceiverState         = vend.r-state
            cACHReceiverZip           = vend.r-zip
            cACHReceiverABA           = STRING(vend.bank-rtn,"999999999")
            cACHReceiverAccountNumber = vend.bank-acct
            cACHDiscretionaryData     = vend.check-memo
            cACHCanadianIndicator     = IF vend.r-country BEGINS "CA" THEN
                                            "C"
                                        ELSE
                                            ""
            .

    PUT STREAM bankFileStr UNFORMATTED
        formatString(cACHRecordIdentifier,"CHARACTER",3)                /* Len - 3,  Start - 1,   End - 3   */
        formatString(cACHPaymentType,"CHARACTER",3)                     /* Len - 3,  Start - 4,   End - 6   */
        formatString(cACHCanadianIndicator,"CHARACTER",1)               /* Len - 1,  Start - 7,   End - 7   */
        formatString(cACHVendorNumber,"CHARACTER",10)                   /* Len - 10, Start - 8,   End - 17  */
        formatString("","CHARACTER",4)                                  /* Len - 4,  Start - 18,  End - 21  */
        formatString(cACHCurrencyType,"CHARACTER",3)                    /* Len - 3,  Start - 22,  End - 24  */
        formatString(cACHTraceNumber,"INTEGER",10)                      /* Len - 10, Start - 25,  End - 34  */
        formatString(cACHPaymentEffectiveDate,"DATE",8)                 /* Len - 8,  Start - 35,  End - 42  */
        formatString("","CHARACTER",3)                                  /* Len - 3,  Start - 43,  End - 45  */
        formatString(STRING(dACHPaymentAmount),"DECIMAL",10)            /* Len - 10, Start - 46,  End - 55  */
        formatString(cACHReceiverName,"CHARACTER",22)                   /* Len - 22, Start - 56,  End - 77  */
        formatString("","CHARACTER",13)                                 /* Len - 13, Start - 78,  End - 90  */
        formatString(cACHIndividualID,"CHARACTER",15)                   /* Len - 15, Start - 91,  End - 105 */
        formatString("","CHARACTER",20)                                 /* Len - 20, Start - 106, End - 125 */
        formatString(cACHReceiverAddress1,"CHARACTER",35)               /* Len - 35, Start - 126, End - 160 */
        formatString(cACHReceiverAddress2,"CHARACTER",35)               /* Len - 35, Start - 161, End - 195 */
        formatString("","CHARACTER",35)                                 /* Len - 35, Start - 196, End - 230 */
        formatString(cACHReceiverCity,"CHARACTER",27)                   /* Len - 27, Start - 231, End - 257 */
        formatString(cACHReceiverState,"CHARACTER",2)                   /* Len - 2,  Start - 258, End - 259 */
        formatString(cACHReceiverZip,"CHARACTER",9)                     /* Len - 9,  Start - 260, End - 268 */
        formatString("","CHARACTER",4)                                  /* Len - 4,  Start - 269, End - 272 */
        formatString(STRING(iACHNumberofRemittanceLines),"INTEGER",5)   /* Len - 5,  Start - 273, End - 277 */
        formatString("","CHARACTER",10)                                 /* Len - 10, Start - 278, End - 287 */
        formatString(cACHReceiverABA,"CHARACTER",9)                     /* Len - 9,  Start - 288, End - 296 */
        formatString("","CHARACTER",3)                                  /* Len - 3,  Start - 297, End - 299 */
        formatString(cACHReceiverAccountNumber,"CHARACTER",17)          /* Len - 17, Start - 300, End - 316 */
        formatString(cACHTranCode,"INTEGER",2)                          /* Len - 2,  Start - 317, End - 318 */
        formatString(cACHType,"CHARACTER",3)                            /* Len - 3,  Start - 319, End - 321 */
        formatString(cACHDiscretionaryData,"CHARACTER",20)              /* Len - 20, Start - 322, End - 341 */
        formatString("","CHARACTER",9)                                  /* Len - 9,  Start - 342, End - 350 */
        SKIP
        .      
        
    PUT STREAM bankFileStr UNFORMATTED
        formatString(cACHHeaderRecordIdentifier,"CHARACTER",3)      /* Len - 3,   Start - 1,  End - 3   */
        formatString(cACHHeaderSubRecordIdentifier,"CHARACTER",2)   /* Len - 2,   Start - 4,  End - 5   */
        formatString(cACHHeaderVendorNumber,"CHARACTER",10)         /* Len - 10,  Start - 6,  End - 15  */
        formatString("","CHARACTER",335)                            /* Len - 335, Start - 16, End - 350 */
        SKIP
        .
END PROCEDURE.

PROCEDURE pPrintACHDetailRecords:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCheckNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPaymentID AS INTEGER   NO-UNDO.

    /* ACH Remittance detail reacord variables */
    DEFINE VARIABLE cACHDetailRecordIdentifier    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailSubRecordIdentifier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailInvoiceDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailInvoiceNumber       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailDescriptiveText     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailInvoiceGrossAmount  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailAdjustedAmount      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHDetailNetAmount           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bfttPaymentData FOR ttPaymentData.
    
    FIND FIRST bfttPaymentData
        WHERE bfttPaymentData.paymentID EQ ipiPaymentID 
        NO-ERROR.

    ASSIGN
        cACHDetailRecordIdentifier    = "070"                            /* ACH Remittance Detail record identifier */
        cACHDetailSubRecordIdentifier = "06"                             /* ACH Remittance Detail sub-record identifier */
        cACHDetailInvoiceDate         = STRING(bfttPaymentData.invDate)
        cACHDetailInvoiceNumber       = STRING(bfttPaymentData.invNo)
        cACHDetailDescriptiveText     = STRING(bfttPaymentData.description)
        cACHDetailInvoiceGrossAmount  = STRING(bfttPaymentData.grossAmt)
        cACHDetailAdjustedAmount      = STRING(bfttPaymentData.adjAmt)
        cACHDetailNetAmount           = STRING(bfttPaymentData.netAmt)
        .

    PUT STREAM bankFileStr UNFORMATTED
        formatString(cACHDetailRecordIdentifier,"CHARACTER",3)      /* Len - 3,   Start - 1,   End - 3   */
        formatString(cACHDetailSubRecordIdentifier,"CHARACTER",2)   /* Len - 2,   Start - 4,   End - 5   */
        formatString(cACHDetailInvoiceDate,"DATE",8)                /* Len - 8,   Start - 6,   End - 13  */
        formatString(cACHDetailInvoiceNumber,"CHARACTER",20)        /* Len - 20,  Start - 14,  End - 33  */
        formatString(cACHDetailDescriptiveText,"CHARACTER",30)      /* Len - 30,  Start - 34,  End - 63  */
        formatString(cACHDetailInvoiceGrossAmount,"DECIMAL",13)     /* Len - 13,  Start - 64,  End - 76  */
        formatString(cACHDetailAdjustedAmount,"DECIMAL",13)         /* Len - 13,  Start - 77,  End - 89  */
        formatString(cACHDetailNetAmount,"DECIMAL",13)              /* Len - 13,  Start - 90,  End - 102 */
        formatString("","CHARACTER",248)                            /* Len - 248, Start - 103, End - 350 */
        SKIP
        .        
END PROCEDURE.

PROCEDURE pPrintACHTrailerRecords:
    /* ACH Remittance trailer reacord variables */
    DEFINE VARIABLE cACHTrailerRecordIdentifier    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cACHTrailerSubRecordIdentifier AS CHARACTER NO-UNDO.
    
    ASSIGN
        cACHTrailerRecordIdentifier    = "070"  /* ACH Remittance Trailer record identifier */
        cACHTrailerSubRecordIdentifier = "09"   /* ACH Remittance Trailer sub-record identifier */
        .

    PUT STREAM bankFileStr UNFORMATTED
        formatString(cACHTrailerRecordIdentifier,"CHARACTER",3)     /* Len - 3,   Start - 1,  End - 3   */
        formatString(cACHTrailerSubRecordIdentifier,"CHARACTER",2)  /* Len - 2,   Start - 4,  End - 5   */
        formatString("","INTEGER",26)                               /* Len - 26,  Start - 6,  End - 31  */
        formatString("","CHARACTER",319)                            /* Len - 319, Start - 32, End - 350 */
        SKIP
        .          
END PROCEDURE.

FUNCTION formatString RETURNS CHARACTER PRIVATE
    (ipcString    AS CHARACTER,
     ipcDataType  AS CHARACTER,
     ipiLength    AS INTEGER):

    RETURN DYNAMIC-FUNCTION (
                             'Output_FixedFormatString' IN hdOutputProcs,
                             INPUT ipcString,
                             INPUT ipcDataType,
                             INPUT ipiLength
                            ). 
END FUNCTION.
