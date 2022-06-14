/*------------------------------------------------------------------------
    File        : api/inbound/handlers/RemittanceAdviceRequestHandler.p
    Purpose     : Procedure to handle remittance advice

    Syntax      :

    Description : Procedure to handle remittance advice

    Author(s)   : DEVA$!
    Created     : Tue December 21 06:18:25 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy            AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserName               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

{XMLOutput/ttNodes.i NEW}

{ar/ttARCash.i}
{ar/ttARCashLine.i}
    
DEFINE VARIABLE cImportType            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImportTypeImporter    AS CHARACTER NO-UNDO INITIAL "Importer".
DEFINE VARIABLE cImportTypeEDI         AS CHARACTER NO-UNDO INITIAL "EDI".

IF ipcRequestDataType EQ "EDI" OR INDEX(iplcRequestData, "http://www.rssbus.com") GT 0 THEN
    cImportType = cImportTypeEDI.
ELSE IF ipcRequestDataType EQ "CSV" THEN
    cImportType = cImportTypeImporter.

DEFINE VARIABLE hdXMLProcs AS HANDLE NO-UNDO.
RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdXMLProcs).

RUN pPrepareInputs (
    INPUT cImportType,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

IF oplSuccess THEN
    RUN pProcessInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.


IF ERROR-STATUS:ERROR THEN
    opcMessage = ERROR-STATUS:GET-MESSAGE(1).

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdXMLProcs).
DELETE PROCEDURE hdXMLProcs.

PROCEDURE pPrepareInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Based on the type of import, this procedure routes the request import
          to other procedures 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcImportType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
    
    IF ipcImportType EQ cImportTypeEDI THEN DO:
        RUN pPrepareInputsEDI (
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.       
        IF NOT oplSuccess THEN
            RETURN. 
    END.
END PROCEDURE.

PROCEDURE pPrepareInputsEDI PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Reads the input EDI request data and save into temp-tables
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSequenceID  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE iOrderIDMeta            AS INTEGER   NO-UNDO. /* ISA */ 
    DEFINE VARIABLE iFunctionalGroupOrder   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTransactionOrderList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTransactionCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFromIdentity           AS CHARACTER NO-UNDO. /* ISA06 */
    DEFINE VARIABLE cToIdentity             AS CHARACTER NO-UNDO. /* ISA08 */    
    DEFINE VARIABLE cInterControlNumber     AS CHARACTER NO-UNDO. /* ISA12 */
    DEFINE VARIABLE iOrderID820             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDBPR             AS INTEGER   NO-UNDO. /* BPR Order ID */
    DEFINE VARIABLE cTransactionCode        AS CHARACTER NO-UNDO. /* BPR01 */
    DEFINE VARIABLE dCheckAmount            AS DECIMAL   NO-UNDO. /* BPR02 */
    DEFINE VARIABLE cCreditDebit            AS CHARACTER NO-UNDO. /* BPR03 */
    DEFINE VARIABLE cPaymentMethodCode      AS CHARACTER NO-UNDO. /* BPR04 */
    DEFINE VARIABLE iOrderIDCUR             AS INTEGER   NO-UNDO. /* CUR Order ID */        
    DEFINE VARIABLE cEntityIdentifierCode   AS CHARACTER NO-UNDO. /* CUR01 */
    DEFINE VARIABLE cCurrencyCode           AS CHARACTER NO-UNDO. /* CUR02 */
    DEFINE VARIABLE iOrderIDREF             AS INTEGER   NO-UNDO. /* REF Order ID */        
    DEFINE VARIABLE cRefIdentifierCode      AS CHARACTER NO-UNDO. /* REF01 */
    DEFINE VARIABLE cReferenceCode          AS CHARACTER NO-UNDO. /* REF02 */
    DEFINE VARIABLE iOrderIDDTM             AS INTEGER   NO-UNDO. /* DTM Order ID */        
    DEFINE VARIABLE dtCheckDate             AS DATE      NO-UNDO. /* DTM02 */    
    DEFINE VARIABLE cVendorID               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCheckNumber            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEFTNumber              AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cN1Loop1OrderList       AS CHARACTER NO-UNDO. /* N1Loop1 Order List */
    DEFINE VARIABLE iN1Loop1Count           AS INTEGER   NO-UNDO. /* N1Loop1 count */
    DEFINE VARIABLE iOrderIDN1              AS INTEGER   NO-UNDO. /* N1 Order ID */        
    DEFINE VARIABLE cN1EntityIdentifierCode AS CHARACTER NO-UNDO. /* N101 */
    DEFINE VARIABLE cN1IdentificationCode   AS CHARACTER NO-UNDO. /* N104 */
    DEFINE VARIABLE cPayorID                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cENTLoop1OrderList      AS CHARACTER NO-UNDO. /* ENTLoop1 Order List */
    DEFINE VARIABLE iENTLoop1Count          AS INTEGER   NO-UNDO. /* ENTLoop1 count */    
    DEFINE VARIABLE cRMRLoop1OrderList      AS CHARACTER NO-UNDO. /* RMR Loop1 order list */
    DEFINE VARIABLE iRMRLoop1Count          AS INTEGER   NO-UNDO. /* RMR loop1 count */
    DEFINE VARIABLE iOrderIDRMR             AS INTEGER   NO-UNDO. /* RMR Order ID */
    DEFINE VARIABLE cRMRRefIdQualifier      AS CHARACTER NO-UNDO. /* RMR01 */
    DEFINE VARIABLE cRMRReferenceID         AS CHARACTER NO-UNDO. /* RMR02 */
    DEFINE VARIABLE cInvoiceNumber          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dNetInovoiceAmount      AS DECIMAL   NO-UNDO. /* RMR04 */
    DEFINE VARIABLE dGrossInovoiceAmount    AS DECIMAL   NO-UNDO. /* RMR05 */   
    DEFINE VARIABLE dTermsDiscountAmount    AS DECIMAL   NO-UNDO. /* RMR06 */
    DEFINE VARIABLE cPONumber               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtInvoiceDate           AS DATE      NO-UNDO.
    
    /* Read XML from long char and store in temp-table */
    RUN XML_ReadToTT (
        INPUT iplcRequestData
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR OR NOT TEMP-TABLE ttNodes:HAS-RECORDS THEN DO:
        ASSIGN
            oplSuccess     = NO
            opcMessage = "Requested XML is not in valid format"
            .            
        RETURN.  
    END.

    RUN XML_GetFieldOrderByName (
        INPUT  "Meta",
        OUTPUT lRecFound,
        OUTPUT iOrderIDMeta
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing 'Meta' information "
            .
        RETURN.    
    END.

    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "ISA06",
        INPUT  iOrderIDMeta,
        OUTPUT lRecFound,
        OUTPUT cFromIdentity
        ).

    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "ISA06",
        INPUT  iOrderIDMeta,
        OUTPUT lRecFound,
        OUTPUT cToIdentity
        ).

    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "ISA13",
        INPUT  iOrderIDMeta,
        OUTPUT lRecFound,
        OUTPUT cInterControlNumber
        ).

    RUN XML_GetFieldOrderByName (
        INPUT  "FunctionalGroup",
        OUTPUT lRecFound,
        OUTPUT iFunctionalGroupOrder
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing xml element 'FunctionalGroup'"
            .
        RETURN.    
    END.

    RUN XML_GetFieldOrderListByNameAndParent (
        INPUT  "TransactionSet",
        INPUT  iFunctionalGroupOrder,
        OUTPUT cTransactionOrderList
        ).   
    IF cTransactionOrderList EQ "" THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "No Transactions are available to process"
            .
        RETURN.
    END.

    DO iTransactionCount = 1 TO NUM-ENTRIES(cTransactionOrderList):
        ASSIGN  
            iOrderID820             = 0   
            iOrderIDBPR             = 0   
            cTransactionCode        = ""      
            dCheckAmount            = 0 
            cCreditDebit            = ""
            cPaymentMethodCode      = ""      
            cN1Loop1OrderList       = "" 
            iN1Loop1Count           = 0   
            iOrderIDN1              = 0   
            cN1EntityIdentifierCode = "" 
            cN1IdentificationCode   = ""
            cPayorID                = "" 
            cENTLoop1OrderList      = "" 
            iENTLoop1Count          = 0   
            .

        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "TX-00401-820",
            INPUT  INTEGER(ENTRY(iTransactionCount, cTransactionOrderList)),
            OUTPUT lRecFound,
            OUTPUT iOrderID820
            ).
        IF NOT lRecFound THEN
            NEXT.

        /* Beginning Segment */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "BPR",
            INPUT  iOrderID820,
            OUTPUT lRecFound,
            OUTPUT iOrderIDBPR
            ).
        IF NOT lRecFound THEN
            NEXT.

        /* Transaction Handling Code */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BPR01",
            INPUT  iOrderIDBPR,
            OUTPUT lRecFound,
            OUTPUT cTransactionCode
            ).

        /* Monetary Amount */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BPR02",
            INPUT  iOrderIDBPR,
            OUTPUT lRecFound,
            OUTPUT cReturnValue
            ).
        IF lRecFound THEN
            dCheckAmount = DECIMAL(cReturnValue) NO-ERROR.
        
        /* Credit/Debit Flag Code */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BPR03",
            INPUT  iOrderIDBPR,
            OUTPUT lRecFound,
            OUTPUT cCreditDebit
            ).

        /* Payment Method Code */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BPR04",
            INPUT  iOrderIDBPR,
            OUTPUT lRecFound,
            OUTPUT cPaymentMethodCode
            ).

        /* Currency Segment */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "CUR",
            INPUT  iOrderID820,
            OUTPUT lRecFound,
            OUTPUT iOrderIDCUR
            ).
        IF lRecFound THEN DO:
            /* Entity Identifier Code */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "CUR01",
                INPUT  iOrderIDCUR,
                OUTPUT lRecFound,
                OUTPUT cEntityIdentifierCode
                ).
    
            /* Currency Code */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "CUR02",
                INPUT  iOrderIDCUR,
                OUTPUT lRecFound,
                OUTPUT cCurrencyCode
                ).
        END.

        /* Reference Identification */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "REF",
            INPUT  iOrderID820,
            OUTPUT lRecFound,
            OUTPUT iOrderIDREF
            ).
        IF lRecFound THEN DO:
            /* Reference Identification Qualifier */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "REF01",
                INPUT  iOrderIDREF,
                OUTPUT lRecFound,
                OUTPUT cRefIdentifierCode
                ).
    
            /* Reference Identification */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "REF02",
                INPUT  iOrderIDREF,
                OUTPUT lRecFound,
                OUTPUT cReferenceCode
                ).
            
            cRefIdentifierCode = TRIM(cRefIdentifierCode).
            
            IF cRefIdentifierCode EQ "CK" THEN
                cCheckNumber = cReferenceCode.
            ELSE IF cRefIdentifierCode EQ "EF" THEN
                cEFTNumber = cReferenceCode.
            ELSE IF cRefIdentifierCode EQ "VR" THEN
                cVendorID = cReferenceCode.
        END.
        
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "DTM",
            INPUT  iOrderID820,
            OUTPUT lRecFound,
            OUTPUT iOrderIDDTM
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "DTM02",
                INPUT  iOrderIDDTM,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            cReturnValue= TRIM(cReturnValue).
            
            IF lRecFound THEN
                dtCheckDate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                                   INTEGER(SUBSTRING(cReturnValue,7,2)), 
                                   INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.                       
        END.
        
        /* Entity identification information */
        RUN XML_GetFieldOrderListByNameAndParent (
            INPUT  "N1Loop1",
            INPUT  iOrderID820,
            OUTPUT cN1Loop1OrderList
            ).   
        IF cN1Loop1OrderList NE "" THEN DO:
            DO iN1Loop1Count = 1 TO NUM-ENTRIES(cN1Loop1OrderList):
                RUN XML_GetFieldOrderByNameAndParent (
                    INPUT  "N1",
                    INPUT  INTEGER(ENTRY(iN1Loop1Count, cN1Loop1OrderList)),
                    OUTPUT lRecFound,
                    OUTPUT iOrderIDN1
                    ).
                IF NOT lRecFound THEN
                    NEXT.

                /* Entity Identifier Code. FR - Message From, TO - Message To */
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "N101",
                    INPUT  iOrderIDN1,
                    OUTPUT lRecFound,
                    OUTPUT cN1EntityIdentifierCode
                    ).
                
                cN1EntityIdentifierCode = TRIM(cN1EntityIdentifierCode).

                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "N104",
                    INPUT  iOrderIDN1,
                    OUTPUT lRecFound,
                    OUTPUT cN1IdentificationCode
                    ).
                
                cN1IdentificationCode = TRIM(cN1IdentificationCode).
                
                IF cN1EntityIdentifierCode EQ "PE" OR cN1EntityIdentifierCode EQ "PR" THEN
                    cPayorID = cN1IdentificationCode.
            END.            
        END.

        CREATE ttARCash.
        ASSIGN
            iSequenceID            = iSequenceID + 1
            ttARCash.sequenceID    = iSequenceID
            ttARCash.checkNumber   = TRIM(cCheckNumber)
            ttARCash.checkDate     = dtCheckDate
            ttARCash.currency      = TRIM(cCurrencyCode)
            ttARCash.checkAmount   = dCheckAmount
            ttARCash.payorID       = TRIM(cPayorID)
            ttARCash.paymentMethod = TRIM(cPaymentMethodCode)
            ttARCash.creditDebit   = TRIM(cCreditDebit)
            ttARCash.importType    = cImportTypeEDI
            .
            
        /* Entity */
        RUN XML_GetFieldOrderListByNameAndParent (
            INPUT  "ENTLoop1",
            INPUT  iOrderID820,
            OUTPUT cENTLoop1OrderList
            ).   
        IF cENTLoop1OrderList NE "" THEN DO:
            DO iENTLoop1Count = 1 TO NUM-ENTRIES(cENTLoop1OrderList):
                RUN XML_GetFieldOrderListByNameAndParent (
                    INPUT  "RMRLoop1",
                    INPUT  INTEGER(ENTRY(iENTLoop1Count, cENTLoop1OrderList)),
                    OUTPUT cRMRLoop1OrderList
                    ).   
                IF cRMRLoop1OrderList NE "" THEN DO:
                    ASSIGN
                        iRMRLoop1Count      = 0
                        iOrderIDRMR         = 0
                        .
                    DO iRMRLoop1Count = 1 TO NUM-ENTRIES (cRMRLoop1OrderList):
                        RUN XML_GetFieldOrderByNameAndParent (
                            INPUT  "RMR",
                            INPUT  INTEGER(ENTRY(iRMRLoop1Count, cRMRLoop1OrderList)),
                            OUTPUT lRecFound,
                            OUTPUT iOrderIDRMR
                            ).
                        IF lRecFound THEN DO:
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "RMR01",
                                INPUT  iOrderIDRMR,
                                OUTPUT lRecFound,
                                OUTPUT cRMRRefIdQualifier
                                ).
                                
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "RMR02",
                                INPUT  iOrderIDRMR,
                                OUTPUT lRecFound,
                                OUTPUT cRMRReferenceID
                                ).

                            IF TRIM(cRMRRefIdQualifier) EQ "IV" THEN
                                cInvoiceNumber = TRIM(cRMRReferenceID).
        
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "RMR04",
                                INPUT  iOrderIDRMR,
                                OUTPUT lRecFound,
                                OUTPUT cReturnValue
                                ).
                            IF lRecFound THEN
                                dNetInovoiceAmount = DECIMAL(cReturnValue) NO-ERROR.
                                
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "RMR05",
                                INPUT  iOrderIDRMR,
                                OUTPUT lRecFound,
                                OUTPUT cReturnValue
                                ).
                            IF lRecFound THEN
                                dGrossInovoiceAmount = DECIMAL(cReturnValue) NO-ERROR.            

                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "RMR06",
                                INPUT  iOrderIDRMR,
                                OUTPUT lRecFound,
                                OUTPUT cReturnValue
                                ).
                            IF lRecFound THEN
                                dTermsDiscountAmount = DECIMAL(cReturnValue) NO-ERROR.            
        
                            system.SharedConfig:Instance:SetValue("APIInboundEvent_UserField1", cInvoiceNumber).
                        END.

                        /* Reference Identification */
                        RUN XML_GetFieldOrderByNameAndParent (
                            INPUT  "REF",
                            INPUT  INTEGER(ENTRY(iRMRLoop1Count, cRMRLoop1OrderList)),
                            OUTPUT lRecFound,
                            OUTPUT iOrderIDREF
                            ).
                        IF lRecFound THEN DO:
                            /* Reference Identification Qualifier */
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "REF01",
                                INPUT  iOrderIDREF,
                                OUTPUT lRecFound,
                                OUTPUT cRefIdentifierCode
                                ).
                    
                            /* Reference Identification */
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "REF02",
                                INPUT  iOrderIDREF,
                                OUTPUT lRecFound,
                                OUTPUT cReferenceCode
                                ).
                            
                            cRefIdentifierCode = TRIM(cRefIdentifierCode).
                            
                            IF cRefIdentifierCode EQ "PO" THEN
                                cPONumber = cReferenceCode.

                            system.SharedConfig:Instance:SetValue("APIInboundEvent_UserField2", cPONumber).
                        END.  

                        RUN XML_GetFieldOrderByNameAndParent (
                            INPUT  "DTM",
                            INPUT  INTEGER(ENTRY(iRMRLoop1Count, cRMRLoop1OrderList)),
                            OUTPUT lRecFound,
                            OUTPUT iOrderIDDTM
                            ).
                        IF lRecFound THEN DO:
                            RUN XML_GetFieldValueByNameAndParent (
                                INPUT  "DTM02",
                                INPUT  iOrderIDDTM,
                                OUTPUT lRecFound,
                                OUTPUT cReturnValue
                                ).
                            cReturnValue= TRIM(cReturnValue).
                            
                            IF lRecFound THEN
                                dtInvoiceDate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                                                     INTEGER(SUBSTRING(cReturnValue,7,2)), 
                                                     INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.                       
                        END.
        
                        CREATE ttARCashLine.
                        ASSIGN
                            ttARCashLine.sequenceID       = ttARCash.sequenceID
                            ttARCashLine.invoiceID        = cInvoiceNumber
                            ttARCashLine.invoiceDate      = dtInvoiceDate
                            ttARCashLine.checkNetAmount   = dNetInovoiceAmount
                            ttARCashLine.checkGrossAmount = dGrossInovoiceAmount
                            ttARCashLine.discountAmount   = dTermsDiscountAmount
                            .                                                                      
                    END.
                END.
            END.
        END.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to process input data
 Notes:
------------------------------------------------------------------------------*/         
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    RUN api/inbound/CreateCashReceipt.p (
        INPUT-OUTPUT TABLE ttARCash,
        INPUT-OUTPUT TABLE ttARCashLine,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END PROCEDURE.
