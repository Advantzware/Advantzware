
/*------------------------------------------------------------------------
    File        : api\inbound\handlers\ApplicationAdviceRequestHandler.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Mon Dec 20 13:09:00 IST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

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

DEFINE VARIABLE cImportType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdXMLProcs       AS HANDLE    NO-UNDO.

DEFINE VARIABLE cImportTypeImporter    AS CHARACTER NO-UNDO INITIAL "Importer".
DEFINE VARIABLE cImportTypeEDI         AS CHARACTER NO-UNDO INITIAL "EDI".

IF ipcRequestDataType EQ "EDI" OR INDEX(iplcRequestData, "http://www.rssbus.com") GT 0 THEN
    cImportType = cImportTypeEDI.
ELSE IF ipcRequestDataType EQ "CSV" THEN
    cImportType = cImportTypeImporter.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdXMLProcs).

RUN pPrepareInputs (
    INPUT cImportType,
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
    DEFINE VARIABLE iOrderSeqID  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE iOrderIDMeta           AS INTEGER   NO-UNDO. /* ISA */ 
    DEFINE VARIABLE iFunctionalGroupOrder  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTransactionOrderList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTransactionCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFromIdentity          AS CHARACTER NO-UNDO. /* ISA06 */
    DEFINE VARIABLE cToIdentity            AS CHARACTER NO-UNDO. /* ISA08 */    
    DEFINE VARIABLE cInterControlNumber    AS CHARACTER NO-UNDO. /* ISA12 */
    DEFINE VARIABLE iOrderID824            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDBGN            AS INTEGER   NO-UNDO. /* BGN Order ID */
    DEFINE VARIABLE cPurposeCode           AS CHARACTER NO-UNDO. /* BGN01 */
    DEFINE VARIABLE cReferenceID           AS CHARACTER NO-UNDO. /* BGN02 */
    DEFINE VARIABLE dtTransactionDate      AS DATE      NO-UNDO. /* BGN03 */
    DEFINE VARIABLE cN1Loop1OrderList      AS CHARACTER NO-UNDO. /* N1Loop1 Order List */
    DEFINE VARIABLE iN1Loop1Count          AS INTEGER   NO-UNDO. /* N1Loop1 count */
    DEFINE VARIABLE iOrderIDN1             AS INTEGER   NO-UNDO. /* N1 Order ID */        
    DEFINE VARIABLE cEntityIdentityCode    AS CHARACTER NO-UNDO. /* N101 */
    DEFINE VARIABLE cShipToName            AS CHARACTER NO-UNDO. /* N102 */
    DEFINE VARIABLE cShipToID              AS CHARACTER NO-UNDO. /* N104 */
    DEFINE VARIABLE cOTILoop1OrderList     AS CHARACTER NO-UNDO. /* OTI Loop1 order list */
    DEFINE VARIABLE iOTILoop1Count         AS INTEGER   NO-UNDO. /* OTI loop1 count */
    DEFINE VARIABLE iOrderIDOTI            AS INTEGER   NO-UNDO. /* OTI Order ID */
    DEFINE VARIABLE cAppAckCode            AS CHARACTER NO-UNDO. /* OTI01 */
    DEFINE VARIABLE cReferenceIDQualifier  AS CHARACTER NO-UNDO. /* OTI02 */
    DEFINE VARIABLE cOTIReferenceID        AS CHARACTER NO-UNDO. /* OTI03 */
    DEFINE VARIABLE cOTITransactionSetID   AS CHARACTER NO-UNDO. /* OTI10 */   
    DEFINE VARIABLE cInvoiceNumber         AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE iOrderIDTEDLoop1       AS INTEGER   NO-UNDO. /* TEDLoop1 Order ID */
    DEFINE VARIABLE iOrderIDTED            AS INTEGER   NO-UNDO. /* TED Order ID */
    DEFINE VARIABLE cAppErrorCode          AS CHARACTER NO-UNDO. /* TED01 */
    DEFINE VARIABLE cErrorMessage          AS CHARACTER NO-UNDO. /* TED02 */

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
            iOrderID824            = 0   
            iOrderIDBGN            = 0   
            cPurposeCode           = ""      
            cReferenceID           = "" 
            dtTransactionDate      = ?      
            cN1Loop1OrderList      = "" 
            iN1Loop1Count          = 0   
            iOrderIDN1             = 0   
            cEntityIdentityCode    = "" 
            cShipToName            = ""
            cShipToID              = "" 
            iOrderIDOTI            = 0   
            cAppAckCode            = "" 
            cReferenceIDQualifier  = ""
            cOTIReferenceID        = ""
            cOTITransactionSetID   = "" 
            cOTILoop1OrderList     = "" 
            iOTILoop1Count         = 0   
            .

        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "TX-00401-824",
            INPUT  INTEGER(ENTRY(iTransactionCount, cTransactionOrderList)),
            OUTPUT lRecFound,
            OUTPUT iOrderID824
            ).
        IF NOT lRecFound THEN
            NEXT.

        /* Beginning Segment */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "BGN",
            INPUT  iOrderID824,
            OUTPUT lRecFound,
            OUTPUT iOrderIDBGN
            ).
        IF NOT lRecFound THEN
            NEXT.

        /* Invoice Date */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BGN01",
            INPUT  iOrderIDBGN,
            OUTPUT lRecFound,
            OUTPUT cPurposeCode
            ).

        /* Invoice # */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BGN02",
            INPUT  iOrderIDBGN,
            OUTPUT lRecFound,
            OUTPUT cReferenceID
            ).

        /* Transaction Date */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BGN03",
            INPUT  iOrderIDBGN,
            OUTPUT lRecFound,
            OUTPUT cReturnValue
            ).
        IF lRecFound AND LENGTH(cReturnValue) GE 8 THEN
            dtTransactionDate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                                INTEGER(SUBSTRING(cReturnValue,7,2)), 
                                INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.

        /* Entity identification information */
        RUN XML_GetFieldOrderListByNameAndParent (
            INPUT  "N1Loop1",
            INPUT  iOrderID824,
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
                    OUTPUT cEntityIdentityCode
                    ).
                
                IF cEntityIdentityCode EQ "FR" OR cEntityIdentityCode EQ "ST" THEN DO:
                    /* ShipTo Name */
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "N102",
                        INPUT  iOrderIDN1,
                        OUTPUT lRecFound,
                        OUTPUT cShipToName
                        ).
    
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "N104",
                        INPUT  iOrderIDN1,
                        OUTPUT lRecFound,
                        OUTPUT cShipToID
                        ).
                END.                
            END.            
        END.

        /* Entity identification information */
        RUN XML_GetFieldOrderListByNameAndParent (
            INPUT  "OTILoop1",
            INPUT  iOrderID824,
            OUTPUT cOTILoop1OrderList
            ).   
        IF cOTILoop1OrderList NE "" THEN DO:
            ASSIGN
                iOTILoop1Count      = 0
                iOrderIDOTI         = 0
                .

            DO iOTILoop1Count = 1 TO NUM-ENTRIES(cOTILoop1OrderList):
                /* Original Transaction Identification */
                RUN XML_GetFieldOrderByNameAndParent (
                    INPUT  "OTI",
                    INPUT  INTEGER(ENTRY(iOTILoop1Count, cOTILoop1OrderList)),
                    OUTPUT lRecFound,
                    OUTPUT iOrderIDOTI
                    ).
                IF lRecFound THEN DO:
                    /* Application Acknowledgment Code */
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "OTI01",
                        INPUT  iOrderIDOTI,
                        OUTPUT lRecFound,
                        OUTPUT cAppAckCode
                        ).
        
                    /* Reference Identification Qualifier */
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "OTI02",
                        INPUT  iOrderIDOTI,
                        OUTPUT lRecFound,
                        OUTPUT cReferenceIDQualifier
                        ).
        
                    /* Reference Identification */
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "OTI03",
                        INPUT  iOrderIDOTI,
                        OUTPUT lRecFound,
                        OUTPUT cOTIReferenceID
                        ).
        
                    /* Transaction Set Identifier Code */
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "OTI10",
                        INPUT  iOrderIDOTI,
                        OUTPUT lRecFound,
                        OUTPUT cOTITransactionSetID
                        ).                

                    IF TRIM(cOTITransactionSetID) EQ "810" THEN DO:
                        cInvoiceNumber = TRIM(cOTIReferenceID).
                        
                        system.SharedConfig:Instance:SetValue("APIInboundEvent_UserField1", cInvoiceNumber).
                    END.
                END.
                
                /* Technical Error Description */
                RUN XML_GetFieldOrderByNameAndParent (
                    INPUT  "TEDLoop1",
                    INPUT  INTEGER(ENTRY(iOTILoop1Count, cOTILoop1OrderList)),
                    OUTPUT lRecFound,
                    OUTPUT iOrderIDTEDLoop1
                    ).
                IF lRecFound THEN DO:
                    /* Charge segment */
                    RUN XML_GetFieldOrderByNameAndParent (
                        INPUT  "TED",
                        INPUT  iOrderIDTEDLoop1,
                        OUTPUT lRecFound,
                        OUTPUT iOrderIDTED
                        ).
                    IF lRecFound THEN DO:
                        /* Application Error Condition Code */
                        RUN XML_GetFieldValueByNameAndParent (
                            INPUT  "TED01",
                            INPUT  iOrderIDTED,
                            OUTPUT lRecFound,
                            OUTPUT cAppErrorCode
                            ).  
                        
                        /* Free Form Message */                       
                        RUN XML_GetFieldValueByNameAndParent (
                            INPUT  "TED02",
                            INPUT  iOrderIDTED,
                            OUTPUT lRecFound,
                            OUTPUT cErrorMessage
                            ).                                               
                    END.
                END.
            END.
        END.
    END.
    
    IF TRIM(cAppAckCode) EQ "TR" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invoice '" + TRIM(cInvoiceNumber) + "' failed to process. " + "Reason: '" + TRIM(cErrorMessage) + "'"
            .    
    END.
    ELSE
        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
END PROCEDURE.
