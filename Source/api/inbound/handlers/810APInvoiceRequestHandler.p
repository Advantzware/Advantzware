
/*------------------------------------------------------------------------
    File        : api/inbound/810APInvoiceRequestHandler.p.p
    Purpose     : 

    Syntax      :

    Description : Creates an invoice from a x12 810 file

    Author(s)   : Mithun Porandla

    Created     : Tue Jun 09 06:22:19 EDT 2020
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
DEFINE INPUT  PARAMETER ipcUsername               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdXMLProcs     AS HANDLE   NO-UNDO.
DEFINE VARIABLE iResponseCode  AS INTEGER  NO-UNDO.
DEFINE VARIABLE hdJSONProcs    AS HANDLE   NO-UNDO.
DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

{XMLOutput/ttNodes.i NEW}

DEFINE TEMP-TABLE ttInvoice
    FIELD Company           AS CHARACTER 
    FIELD Location          AS CHARACTER 
    FIELD InvoiceNo         AS CHARACTER 
    FIELD VendorID          AS CHARACTER 
    FIELD InvoiceDate       AS DATE      
    FIELD DueDate           AS DATE 
    FIELD TotalAmount       AS DECIMAL     
    FIELD LineAmount        AS DECIMAL   
    FIELD LineQuantity      AS DECIMAL   
    FIELD LineQuantityUom   AS CHARACTER 
    FIELD LinePrice         AS DECIMAL   
    FIELD LinePriceUom      AS CHARACTER 
    FIELD LineTax           AS LOGICAL   
    FIELD LineAccount       AS CHARACTER 
    FIELD TaxGroup          AS CHARACTER
    FIELD VendorTerms       AS CHARACTER
    FIELD Discount          AS DECIMAL
    FIELD DiscountDueDate   AS DATE
    FIELD DiscountDays      AS INTEGER
    FIELD DiscountPercent   AS DECIMAL   
    FIELD LinePONumber      AS INTEGER   
    FIELD LinePOLine        AS INTEGER   
    .

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdXMLProcs).

RUN XML_ReadToTT IN hdXMLProcs (
    INPUT iplcRequestData
    ) NO-ERROR.

RUN pPrepareInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

IF oplSuccess THEN
    RUN pProcessInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).

IF oplSuccess THEN
    ASSIGN
        iResponseCode = 200
        opcMessage    = "Success"
        .    
ELSE
    iResponseCode = 400.

lcResponseData = iplcResponseDataStructure.

lcResponseData = REPLACE(lcResponseData, "$response$", opcMessage).

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.

/* Generate the response data in fixed format of response_code, response_message and response_data */
RUN JSON_GetResponseData IN hdJSONProcs (
    INPUT  iResponseCode,      /* Value which goes into response_code tag */
    INPUT  lcResponseData,     /* Value which goes into response_message tag */
    INPUT  oplcResponseData,   /* Value which goes into response_data tag */
    OUTPUT oplcResponseData
    ) NO-ERROR.

DELETE PROCEDURE hdJSONProcs.

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdXMLProcs).
DELETE PROCEDURE hdXMLProcs.
/* **********************  Internal Procedures  *********************** */

PROCEDURE pPrepareInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReturnValue           AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lRecFound              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iFunctionalGroupOrder  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTransactionOrderList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTransactionCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderID810            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDBIG            AS INTEGER   NO-UNDO. /* BIG Order ID */
    DEFINE VARIABLE dtInvoiceDate          AS DATE      NO-UNDO. /* BIG01 */
    DEFINE VARIABLE cInvoiceNo             AS CHARACTER NO-UNDO. /* BIG02 */
    DEFINE VARIABLE dtPODate               AS DATE      NO-UNDO. /* BIG03 */
    DEFINE VARIABLE iPONo                  AS INTEGER   NO-UNDO. /* BIG04 */
    DEFINE VARIABLE cN1Loop1OrderList      AS CHARACTER NO-UNDO. /* N1Loop1 Order List */
    DEFINE VARIABLE iN1Loop1Count          AS INTEGER   NO-UNDO. /* N1Loop1 count */
    DEFINE VARIABLE iOrderIDN1             AS INTEGER   NO-UNDO. /* N1 Order ID */        
    DEFINE VARIABLE cEntityIdentityCode    AS CHARACTER NO-UNDO. /* N101 */
    DEFINE VARIABLE cVendorID              AS CHARACTER NO-UNDO. /* N104 */
    DEFINE VARIABLE iOrderIDITD            AS INTEGER   NO-UNDO. /* ITD Order ID */
    DEFINE VARIABLE cTermsTypeCode         AS CHARACTER NO-UNDO. /* ITD01 */
    DEFINE VARIABLE cTermsBasisDateCode    AS CHARACTER NO-UNDO. /* ITD02 */
    DEFINE VARIABLE dTermsDiscountPercent  AS DECIMAL   NO-UNDO. /* ITD03 */
    DEFINE VARIABLE dtTermsDiscountDueDate AS DATE      NO-UNDO. /* ITD04 */
    DEFINE VARIABLE dtTermsNetDueDate      AS DATE      NO-UNDO. /* ITD06 */
    DEFINE VARIABLE iTermsDiscountDaysDue  AS INTEGER   NO-UNDO. /* ITD07 */    
    DEFINE VARIABLE dTermsDiscountAmount   AS DECIMAL   NO-UNDO. /* ITD08 */        
    DEFINE VARIABLE cIT1Loop1OrderList     AS CHARACTER NO-UNDO. /* IT1Loop1 Order List */
    DEFINE VARIABLE iIT1Loop1Count         AS INTEGER   NO-UNDO. /* IT1Loop1 count */
    DEFINE VARIABLE iOrderIDIT1            AS INTEGER   NO-UNDO. /* IT1 Order ID */
    DEFINE VARIABLE dQuantityInvoiced      AS DECIMAL   NO-UNDO. /* IT102 */
    DEFINE VARIABLE cQuantityUOM           AS CHARACTER NO-UNDO. /* IT103 */
    DEFINE VARIABLE dUnitPrice             AS DECIMAL   NO-UNDO. /* IT104 */
    DEFINE VARIABLE cUnitPriceUOM          AS CHARACTER NO-UNDO. /* IT105 */
    DEFINE VARIABLE cPIDLoop1OrderList     AS CHARACTER NO-UNDO. /* PIDLoop1 Order List */
    DEFINE VARIABLE iPIDLoop1Count         AS INTEGER   NO-UNDO. /* PIDLoop1 count */
    DEFINE VARIABLE iOrderIDTDS            AS INTEGER   NO-UNDO. /* TDS Order ID */
    DEFINE VARIABLE dTotalInvoiceAmount    AS DECIMAL   NO-UNDO. /* TDS01 */
    DEFINE VARIABLE iOrderIDTXI            AS INTEGER   NO-UNDO. /* TXI Order ID */
    DEFINE VARIABLE cTaxCode               AS CHARACTER NO-UNDO. /* TXI01 */
    DEFINE VARIABLE dTaxAmount             AS DECIMAL   NO-UNDO. /* TXI02 */
    DEFINE VARIABLE dTaxPercentage         AS DECIMAL   NO-UNDO. /* TXI03 */
    DEFINE VARIABLE iOrderIDCTT            AS INTEGER   NO-UNDO. /* CTT Order ID */
    DEFINE VARIABLE iTotalLineItems        AS INTEGER   NO-UNDO. /* CTT01 */
    
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
            iOrderID810            = 0   
            iOrderIDBIG            = 0   
            dtInvoiceDate          = ?      
            cInvoiceNo             = "" 
            dtPODate               = ?      
            iPONo                  = 0
            cN1Loop1OrderList      = "" 
            iN1Loop1Count          = 0   
            iOrderIDN1             = 0   
            cEntityIdentityCode    = "" 
            cVendorID              = "" 
            iOrderIDITD            = 0   
            cTermsTypeCode         = "" 
            cTermsBasisDateCode    = "" 
            dTermsDiscountPercent  = 0   
            dtTermsDiscountDueDate = ?      
            dtTermsNetDueDate      = ?      
            dTermsDiscountAmount   = 0   
            cIT1Loop1OrderList     = "" 
            iIT1Loop1Count         = 0   
            iOrderIDIT1            = 0   
            cPIDLoop1OrderList     = "" 
            iPIDLoop1Count         = 0   
            iOrderIDTDS            = 0   
            dTotalInvoiceAmount    = 0   
            iOrderIDTXI            = 0   
            cTaxCode               = "" 
            dTaxAmount             = 0   
            dTaxPercentage         = 0   
            iOrderIDCTT            = 0   
            iTotalLineItems        = 0
            .
    
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "TX-00401-810",
            INPUT  INTEGER(ENTRY(iTransactionCount, cTransactionOrderList)),
            OUTPUT lRecFound,
            OUTPUT iOrderID810
            ).
        IF NOT lRecFound THEN
            NEXT.

        /* Beginning Segment for Invoice */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "BIG",
            INPUT  iOrderID810,
            OUTPUT lRecFound,
            OUTPUT iOrderIDBIG
            ).
        IF NOT lRecFound THEN
            NEXT.
        
        /* Invoice Date */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BIG01",
            INPUT  iOrderIDBIG,
            OUTPUT lRecFound,
            OUTPUT cReturnValue
            ).
        IF lRecFound AND LENGTH(cReturnValue) GE 8 THEN
            dtInvoiceDate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                                 INTEGER(SUBSTRING(cReturnValue,7,2)), 
                                 INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.
        
        /* Invoice # */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BIG02",
            INPUT  iOrderIDBIG,
            OUTPUT lRecFound,
            OUTPUT cInvoiceNo
            ).

        /* Purchase Order Date */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BIG03",
            INPUT  iOrderIDBIG,
            OUTPUT lRecFound,
            OUTPUT cReturnValue
            ).
        IF lRecFound AND LENGTH(cReturnValue) GE 8 THEN
            dtPODate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                            INTEGER(SUBSTRING(cReturnValue,7,2)), 
                            INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.
        
        /* Purchase Order Number */
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "BIG04",
            INPUT  iOrderIDBIG,
            OUTPUT lRecFound,
            OUTPUT cReturnValue
            ).
        IF lRecFound THEN
            iPONo = INTEGER(cReturnValue) NO-ERROR.

        /* Entity identification information */
        RUN XML_GetFieldOrderListByNameAndParent (
            INPUT  "N1Loop1",
            INPUT  iOrderID810,
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
                
                /* Entity Identifier Code. RE - Remit Address, BT - Bill-to-Party, ST - Ship To, VN - Vendor Number */
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "N101",
                    INPUT  iOrderIDN1,
                    OUTPUT lRecFound,
                    OUTPUT cEntityIdentityCode
                    ).

                /* Skip any other enitity code other than vendor */                    
                IF cEntityIdentityCode NE "VN" THEN                
                    NEXT.                

                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "N104",
                    INPUT  iOrderIDN1,
                    OUTPUT lRecFound,
                    OUTPUT cVendorID
                    ).

                LEAVE.
            END.            
        END.
        
        /* Terms of Sale/Deferred Terms of Sale */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "ITD",
            INPUT  iOrderID810,
            OUTPUT lRecFound,
            OUTPUT iOrderIDITD
            ).
        IF lRecFound THEN DO:
            /* Terms Type Code */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD01",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cTermsTypeCode
                ).

            /* Terms Basis Date Code */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD02",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cTermsBasisDateCode
                ).
            
            /* Terms Discount Percent */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD03",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound THEN
                dTermsDiscountPercent = DECIMAL(cReturnValue) NO-ERROR.

            /* Terms Discount Due Date */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD04",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound AND LENGTH(cReturnValue) GE 8 THEN
                dtTermsDiscountDueDate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                                              INTEGER(SUBSTRING(cReturnValue,7,2)), 
                                              INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.
            
            /* Terms Net Due Date */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD06",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound AND LENGTH(cReturnValue) GE 8 THEN
                dtTermsNetDueDate = DATE(INTEGER(SUBSTRING(cReturnValue,5,2)), 
                                         INTEGER(SUBSTRING(cReturnValue,7,2)), 
                                         INTEGER(SUBSTRING(cReturnValue,1,4))) NO-ERROR.

            /* Terms Discount Days Due */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD07",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound THEN
                iTermsDiscountDaysDue = INTEGER(cReturnValue) NO-ERROR.

            /* Terms Discount Amount */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ITD08",
                INPUT  iOrderIDITD,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound THEN
                dTermsDiscountAmount = DECIMAL(cReturnValue) NO-ERROR.
        END.

        /* Total Monetary Value Summary */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "TDS",
            INPUT  iOrderID810,
            OUTPUT lRecFound,
            OUTPUT iOrderIDTDS
            ).
        IF lRecFound THEN DO:
            /* Monetary amount */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "TDS01",
                INPUT  iOrderIDTDS,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound AND cReturnValue NE "" THEN
                dTotalInvoiceAmount = DECIMAL(cReturnValue) NO-ERROR.            
        END.

        /* Total Information */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "TXI",
            INPUT  iOrderID810,
            OUTPUT lRecFound,
            OUTPUT iOrderIDTXI
            ).
        IF lRecFound THEN DO:
            /* Tax Code */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "TXI01",
                INPUT  iOrderIDTXI,
                OUTPUT lRecFound,
                OUTPUT cTaxCode
                ).

            /* Tax Amount */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "TXI02",
                INPUT  iOrderIDTXI,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).    
            IF lRecFound AND cReturnValue NE "" THEN
                dTaxAmount = DECIMAL(cReturnValue) NO-ERROR.

            /* Tax Amount */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "TXI03",
                INPUT  iOrderIDTXI,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).    
            IF lRecFound AND cReturnValue NE "" THEN
                dTaxPercentage = DECIMAL(cReturnValue) NO-ERROR.                
        END.

        /* Transaction Totals */
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "CTT",
            INPUT  iOrderID810,
            OUTPUT lRecFound,
            OUTPUT iOrderIDCTT
            ).
        IF lRecFound THEN DO:
            /* Total number of line items in the transaction set */
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "CTT01",
                INPUT  iOrderIDCTT,
                OUTPUT lRecFound,
                OUTPUT cReturnValue
                ).
            IF lRecFound AND cReturnValue NE "" THEN
                iTotalLineItems = INTEGER(cReturnValue).
        END.       

        /* Baseline Item Data (Invoice) */
        RUN XML_GetFieldOrderListByNameAndParent (
            INPUT  "IT1Loop1",
            INPUT  iOrderID810,
            OUTPUT cIT1Loop1OrderList
            ).   

        IF cIT1Loop1OrderList NE "" THEN DO:
            DO iIT1Loop1Count = 1 TO NUM-ENTRIES(cIT1Loop1OrderList):                
                ASSIGN
                    dQuantityInvoiced = 0
                    cQuantityUOM      = ""
                    dUnitPrice        = 0
                    cUnitPriceUOM     = ""
                    .

                RUN XML_GetFieldOrderByNameAndParent (
                    INPUT  "IT1",
                    INPUT  INTEGER(ENTRY(iIT1Loop1Count, cIT1Loop1OrderList)),
                    OUTPUT lRecFound,
                    OUTPUT iOrderIDIT1
                    ).

                /* Quantity Invoiced */
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "IT102",
                    INPUT  iOrderIDIT1,
                    OUTPUT lRecFound,
                    OUTPUT cReturnValue
                    ).                
                IF lRecFound AND cReturnValue NE "" THEN
                    dQuantityInvoiced = DECIMAL(cReturnValue) NO-ERROR.

                /* Quantity UOM */
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "IT103",
                    INPUT  iOrderIDIT1,
                    OUTPUT lRecFound,
                    OUTPUT cQuantityUOM
                    ).                

                /* Unit Price */
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "IT104",
                    INPUT  iOrderIDIT1,
                    OUTPUT lRecFound,
                    OUTPUT cReturnValue
                    ).                
                IF lRecFound AND cReturnValue NE "" THEN
                    dUnitPrice = DECIMAL(cReturnValue) NO-ERROR.
                
                /* Unit Price UOM */
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "IT105",
                    INPUT  iOrderIDIT1,
                    OUTPUT lRecFound,
                    OUTPUT cQuantityUOM
                    ).                

                /* Product/Item Description. Not required to populate */
                RUN XML_GetFieldOrderListByNameAndParent (
                    INPUT  "PIDLoop1",
                    INPUT  INTEGER(ENTRY(iIT1Loop1Count, cIT1Loop1OrderList)),
                    OUTPUT cPIDLoop1OrderList
                    ).   
                IF cPIDLoop1OrderList NE "" THEN DO:
                    DO iPIDLoop1Count = 1 TO NUM-ENTRIES(cPIDLoop1OrderList):
                    END.
                END.

                CREATE ttInvoice.
                ASSIGN
                    ttInvoice.Company         = cocode
                    ttInvoice.Location        = locode
                    ttInvoice.InvoiceNo       = cInvoiceNo
                    ttInvoice.VendorID        = cVendorID
                    ttInvoice.InvoiceDate     = dtInvoiceDate
                    ttInvoice.DueDate         = ?
                    ttInvoice.TotalAmount     = dTotalInvoiceAmount
                    ttInvoice.LineAmount      = dQuantityInvoiced * dUnitPrice
                    ttInvoice.LineQuantity    = dQuantityInvoiced
                    ttInvoice.LineQuantityUom = cQuantityUOM
                    ttInvoice.LinePrice       = dUnitPrice
                    ttInvoice.LinePriceUom    = cUnitPriceUOM
                    ttInvoice.LineTax         = FALSE            /* Need clarification */ 
                    ttInvoice.LineAccount     = ""
                    ttInvoice.TaxGroup        = cTaxCode
                    ttInvoice.VendorTerms     = cTermsTypeCode
                    ttInvoice.Discount        = dTermsDiscountAmount
                    ttInvoice.DiscountDueDate = dtTermsDiscountDueDate
                    ttInvoice.DiscountDays    = iTermsDiscountDaysDue
                    ttInvoice.DiscountPercent = dTermsDiscountPercent
                    ttInvoice.LinePONumber    = iPONo
                    .
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
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DO TRANSACTION ON ERROR UNDO, LEAVE:
        FOR EACH ttInvoice:
            RUN api\inbound\CreateVendorInvoice.p (
                INPUT  ttInvoice.Company, 
                INPUT  ttInvoice.VendorID,
                INPUT  ttInvoice.InvoiceNo,
                INPUT  STRING(ttInvoice.InvoiceDate),
                INPUT  STRING(ttInvoice.DueDate),
                INPUT  ttInvoice.DiscountPercent,
                INPUT  ttInvoice.TotalAmount,                  
                INPUT  ttInvoice.LinePONumber,                 
                INPUT  ttInvoice.LinePOLine,     
                INPUT  ttInvoice.LineAccount,
                INPUT  ttInvoice.LineQuantity,            
                INPUT  ttInvoice.LineQuantityUOM, 
                INPUT  ttInvoice.LinePrice,            
                INPUT  ttInvoice.LinePriceUOM,
                INPUT  0,                            /* Square Feet */
                INPUT  ttInvoice.LineAmount,
                INPUT  "",                           /* Line Description */
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                )NO-ERROR.

            IF ERROR-STATUS:ERROR THEN
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                    .

            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                UNDO, LEAVE.
        END.
    END.
END PROCEDURE.

