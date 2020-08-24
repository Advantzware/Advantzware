/*------------------------------------------------------------------------
    File        : oe/OrderImportInputHandler.p
    Purpose     : Procedure to handle order imports

    Syntax      :

    Description : Procedure to handle order imports

    Author(s)   : Mithun Porandla
    Created     : Wed July 15 07:33:22 EDT 2019
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

DEFINE VARIABLE cOrderImportType AS CHARACTER NO-UNDO.

{oe/ttOrder.i}
{XMLOutput/ttNodes.i NEW}

{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

IF ipcRequestDataType EQ "XML" THEN
    cOrderImportType = cOrderImportTypeCXMLMonitor.
ELSE IF ipcRequestDataType EQ "EDI" THEN
    cOrderImportType = cOrderImportTypeEDI.
ELSE IF ipcRequestDataType EQ "CSV" THEN
    cOrderImportType = cOrderImportTypeImporter.

DEFINE VARIABLE hdXMLProcs AS HANDLE NO-UNDO.
RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdXMLProcs).

RUN pPrepareInputs (
    INPUT cOrderImportType,
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
    DEFINE INPUT  PARAMETER ipcOrderImportType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    IF ipcOrderImportType EQ cOrderImportTypeCXMLMonitor THEN DO:
        RUN pPrepareInputsCXMLMonitor (
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.       
        IF NOT oplSuccess THEN
            RETURN. 
    END.
END PROCEDURE.

PROCEDURE pPrepareInputsCXMLMonitor PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Reads the input cXML request data and save into temp-tables
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderSeqID  AS INTEGER   NO-UNDO.
    
    /* Order header */
    DEFINE VARIABLE iOrderIDcXML               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDHeader             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPayLoadID                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDFrom               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDFromCred           AS INTEGER   NO-UNDO.        
    DEFINE VARIABLE cFromDomain                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFromIdentity              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDTo                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDToCred             AS INTEGER   NO-UNDO.        
    DEFINE VARIABLE cToDomain                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cToIdentity                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDSender             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDSenderCred         AS INTEGER   NO-UNDO.        
    DEFINE VARIABLE cSenderDomain              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSenderIdentity            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSenderSharedSecret        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSenderUserAgent           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDRequest            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDOrderRequest       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDOrderRequestHeader AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOrderID                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderDate                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDOrderRequestTotal  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOrderTotalCost            AS CHARACTER NO-UNDO.
    
    /* ShipTo address */
    DEFINE VARIABLE iOrderIDShipTo            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDShipToAddress     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cShipToAddressID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToName               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDShipToPostAddress AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cShipToDeliverTo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddressOrderList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress            AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE cShipToCity               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToState              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToZip                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDShipToCountry     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cShipToCountryCode        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToEmail              AS CHARACTER NO-UNDO.
        
    /* BillTo address */
    DEFINE VARIABLE iOrderIDBillTo            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDBillToAddress     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBillToAddressID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBillToName               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDBillToPostAddress AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBillToDeliverTo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBillToAddressOrderList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBillToAddress            AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE cBillToCity               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBillToState              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBillToZip                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDBillToCountry     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBillToCountryCode        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBillToEmail              AS CHARACTER NO-UNDO.

    /* Shipping details */
    DEFINE VARIABLE iOrderIDShipping AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cShippingCost    AS CHARACTER NO-UNDO.
    
    /* Payment Details */
    DEFINE VARIABLE iOrderIDPayment AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDPcard   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCardNo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCardExpiryDate AS CHARACTER NO-UNDO.
    
    /* Contact details */
    DEFINE VARIABLE iOrderIDContact AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cContactName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cContactEmail   AS CHARACTER NO-UNDO.
    
    /* Order comments */
    DEFINE VARIABLE cOrderComments AS CHARACTER NO-UNDO.

    /* Item details */
    DEFINE VARIABLE cItemOutList             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemQuantity            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLineNumber              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLineDueDate             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDItemID           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSupplierPartID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSupplierPartAuxiliaryID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDItemDetail       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderIDUnitPrice        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemUnitPrice           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDescription         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemUOM                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemManufacturerPartID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderIDDistribution     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDistributionCharge      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLineTotalCost           AS CHARACTER NO-UNDO.
    
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
        INPUT  "cXML",
        OUTPUT lRecFound,
        OUTPUT iOrderIDcXML
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing xml element 'cXML'"
            .
        RETURN.    
    END.

    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "payloadID",
        INPUT  iOrderIDcXML,
        OUTPUT lRecFound,
        OUTPUT cPayLoadID
        ).

    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Header",
        INPUT  iOrderIDcXML,
        OUTPUT lRecFound,
        OUTPUT iOrderIDHeader
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing xml element 'Header'"
            .
        RETURN.    
    END.
        
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "From",
        INPUT  iOrderIDHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDFrom
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "Credential",
            INPUT  iOrderIDFrom,
            OUTPUT lRecFound,
            OUTPUT iOrderIDFromCred
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "domain",
                INPUT  iOrderIDFromCred,
                OUTPUT lRecFound,
                OUTPUT cFromDomain
                ).
        
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Identity",
                INPUT  iOrderIDFromCred,
                OUTPUT lRecFound,
                OUTPUT cFromIdentity
                ).
        END.
    END.
    
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "To",
        INPUT  iOrderIDHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDTo
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "Credential",
            INPUT  iOrderIDTo,
            OUTPUT lRecFound,
            OUTPUT iOrderIDToCred
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "domain",
                INPUT  iOrderIDToCred,
                OUTPUT lRecFound,
                OUTPUT cToDomain
                ).
        
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Identity",
                INPUT  iOrderIDToCred,
                OUTPUT lRecFound,
                OUTPUT cToIdentity
                ).
        END.
    END.
    
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Sender",
        INPUT  iOrderIDHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDSender
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "Credential",
            INPUT  iOrderIDSender,
            OUTPUT lRecFound,
            OUTPUT iOrderIDSenderCred
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "domain",
                INPUT  iOrderIDSenderCred,
                OUTPUT lRecFound,
                OUTPUT cSenderDomain
                ).
        
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Identity",
                INPUT  iOrderIDSenderCred,
                OUTPUT lRecFound,
                OUTPUT cSenderIdentity
                ).
            
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "SharedSecret",
                INPUT  iOrderIDSenderCred,
                OUTPUT lRecFound,
                OUTPUT cSenderSharedSecret
                ).
        END.
        
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "UserAgent",
            INPUT  iOrderIDSender,
            OUTPUT lRecFound,
            OUTPUT cSenderUserAgent
            ).
    END.

    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Request",
        INPUT  iOrderIDcXML,
        OUTPUT lRecFound,
        OUTPUT iOrderIDRequest
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing xml element 'Request'"
            .
        RETURN.    
    END.
       
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "OrderRequest",
        INPUT  iOrderIDRequest,
        OUTPUT lRecFound,
        OUTPUT iOrderIDOrderRequest
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing xml element 'OrderRequest'"
            .
        RETURN.    
    END.
    
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "OrderRequestHeader",
        INPUT  iOrderIDOrderRequest,
        OUTPUT lRecFound,
        OUTPUT iOrderIDOrderRequestHeader
        ).
    IF NOT lRecFound THEN DO:
        ASSIGN 
            oplSuccess = FALSE
            opcMessage = "Missing xml element 'OrderRequestHeader'"
            .
        RETURN.    
    END.
    
    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "orderID",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT cOrderID
        ).

    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "orderDate",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT cOrderDate
        ).

    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Total",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDOrderRequestTotal
        ).
    IF lRecFound THEN
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "Money",
            INPUT  iOrderIDOrderRequestTotal,
            OUTPUT lRecFound,
            OUTPUT cOrderTotalCost
            ).

    /* ShipTo address information */
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "ShipTo",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDShipTo
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "Address",
            INPUT  iOrderIDShipTo,
            OUTPUT lRecFound,
            OUTPUT iOrderIDShipToAddress
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "addressID",
                INPUT  iOrderIDShipToAddress,
                OUTPUT lRecFound,
                OUTPUT cShipToAddressID
                ).            

            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Name",
                INPUT  iOrderIDShipToAddress,
                OUTPUT lRecFound,
                OUTPUT cShipToName
                ).  
            
            RUN XML_GetFieldOrderByNameAndParent (
                INPUT  "PostalAddress",
                INPUT  iOrderIDShipToAddress,
                OUTPUT lRecFound,
                OUTPUT iOrderIDShipToPostAddress
                ).        
            IF lRecFound THEN DO:
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "DeliverTo",
                    INPUT  iOrderIDShipToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cShipToDeliverTo
                    ).  
                
                RUN XML_GetFieldOrderListByNameAndParent (
                    INPUT  "Street",
                    INPUT  iOrderIDShipToPostAddress,
                    OUTPUT cShipToAddressOrderList
                    ).
                DO iIndex = 1 TO NUM-ENTRIES(cShipToAddressOrderList):
                    IF iIndex GT 2 THEN
                        LEAVE.
                        
                    RUN XML_GetFieldValueByFieldOrder (
                        INPUT  INTEGER(ENTRY(iIndex, cShipToAddressOrderList)),
                        OUTPUT lRecFound,
                        OUTPUT cShipToAddress[iIndex]
                        ).                      
                END.
                
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "City",
                    INPUT  iOrderIDShipToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cShipToCity
                    ).  

                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "State",
                    INPUT  iOrderIDShipToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cShipToState
                    ).  

                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "PostalCode",
                    INPUT  iOrderIDShipToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cShipToZip
                    ).  
                
                RUN XML_GetFieldOrderByNameAndParent (
                    INPUT  "Country",
                    INPUT  iOrderIDShipToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT iOrderIDShipToCountry
                    ).         
                IF lRecFound THEN
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "isoCountryCode",
                        INPUT  iOrderIDShipToCountry,
                        OUTPUT lRecFound,
                        OUTPUT cShipToCountryCode
                        ).                        
            END.   
            
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Email",
                INPUT  iOrderIDShipToAddress,
                OUTPUT lRecFound,
                OUTPUT cShipToEmail
                ).                       
        END.
    END.
    
    /* BillTo address information */
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "BillTo",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDBillTo
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "Address",
            INPUT  iOrderIDBillTo,
            OUTPUT lRecFound,
            OUTPUT iOrderIDBillToAddress
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "addressID",
                INPUT  iOrderIDBillToAddress,
                OUTPUT lRecFound,
                OUTPUT cBillToAddressID
                ).            

            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Name",
                INPUT  iOrderIDBillToAddress,
                OUTPUT lRecFound,
                OUTPUT cBillToName
                ).  
            
            RUN XML_GetFieldOrderByNameAndParent (
                INPUT  "PostalAddress",
                INPUT  iOrderIDBillToAddress,
                OUTPUT lRecFound,
                OUTPUT iOrderIDBillToPostAddress
                ).        
            IF lRecFound THEN DO:
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "DeliverTo",
                    INPUT  iOrderIDBillToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cBillToDeliverTo
                    ).  
                
                RUN XML_GetFieldOrderListByNameAndParent (
                    INPUT  "Street",
                    INPUT  iOrderIDBillToPostAddress,
                    OUTPUT cBillToAddressOrderList
                    ).
                DO iIndex = 1 TO NUM-ENTRIES(cBillToAddressOrderList):
                    IF iIndex GT 2 THEN
                        LEAVE.
                    
                    RUN XML_GetFieldValueByFieldOrder (
                        INPUT  INTEGER(ENTRY(iIndex, cBillToAddressOrderList)),
                        OUTPUT lRecFound,
                        OUTPUT cBillToAddress[iIndex]
                        ).
                END.
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "City",
                    INPUT  iOrderIDBillToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cBillToCity
                    ).  

                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "State",
                    INPUT  iOrderIDBillToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cBillToState
                    ).  

                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "PostalCode",
                    INPUT  iOrderIDBillToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT cBillToZip
                    ).  
                
                RUN XML_GetFieldOrderByNameAndParent (
                    INPUT  "Country",
                    INPUT  iOrderIDBillToPostAddress,
                    OUTPUT lRecFound,
                    OUTPUT iOrderIDBillToCountry
                    ).         
                IF lRecFound THEN
                    RUN XML_GetFieldValueByNameAndParent (
                        INPUT  "isoCountryCode",
                        INPUT  iOrderIDBillToCountry,
                        OUTPUT lRecFound,
                        OUTPUT cBillToCountryCode
                        ).                        
            END.   
            
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Email",
                INPUT  iOrderIDBillToAddress,
                OUTPUT lRecFound,
                OUTPUT cBillToEmail
                ).                       
        END.
    END.

    /* Shipping information */
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Shipping",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDShipping
        ).
    IF lRecFound THEN
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "Money",
            INPUT  iOrderIDShipping,
            OUTPUT lRecFound,
            OUTPUT cShippingCost
            ).     
    
    /* Card details */
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Payment",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDPayment
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "PCard",
            INPUT  iOrderIDPayment,
            OUTPUT lRecFound,
            OUTPUT iOrderIDPCard
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "number",
                INPUT  iOrderIDPCard,
                OUTPUT lRecFound,
                OUTPUT cCardNo
                ).            

            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "expiration",
                INPUT  iOrderIDPCard,
                OUTPUT lRecFound,
                OUTPUT cCardExpiryDate
                ).            
        END.
    END.    
    
    /* Contact information */
    RUN XML_GetFieldOrderByNameAndParent (
        INPUT  "Contact",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT iOrderIDContact
        ).
    IF lRecFound THEN DO:
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "Name",
            INPUT  iOrderIDContact,
            OUTPUT lRecFound,
            OUTPUT cContactName
            ).     

        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "Email",
            INPUT  iOrderIDContact,
            OUTPUT lRecFound,
            OUTPUT cContactEmail
            ).     
    END.

    /* Order comments */
    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "Comments",
        INPUT  iOrderIDOrderRequestHeader,
        OUTPUT lRecFound,
        OUTPUT cOrderComments
        ).
    
    iOrderSeqID = iOrderSeqID + 1.
    
    CREATE ttOrder.
    ASSIGN    
        ttOrder.orderSeqID      = iOrderSeqID
        ttOrder.company         = cocode
        ttOrder.warehouseID     = locode
        ttOrder.payLoadID       = cPayLoadID
        ttOrder.fromDomain      = cFromDomain
        ttOrder.fromIdentity    = cFromIdentity
        ttOrder.toDomain        = cToDomain
        ttOrder.toIdentity      = cToIdentity
        ttOrder.senderDomain    = cSenderDomain
        ttOrder.senderIdentity  = cSenderIdentity
        ttOrder.sharedSecret    = cSenderSharedSecret
        ttOrder.userAgent       = cSenderUserAgent
        ttOrder.poID            = cOrderID
        ttOrder.orderDate       = DATE(INTEGER(SUBSTRING(cOrderDate, 6, 2)), 
                                       INTEGER(SUBSTRING(cOrderDate, 9, 2)), 
                                       INTEGER(SUBSTRING(cOrderDate, 1, 4)))
        ttOrder.totalCost       = DECIMAL(cOrderTotalCost)
        ttOrder.shipToID        = cShipToAddressID
        ttOrder.shipToName      = cShipToName
        ttOrder.shipToDeliverTo = cShipToDeliverTo
        ttOrder.shipToAddress1  = cShipToAddress[1]
        ttOrder.shipToAddress2  = cShipToAddress[2]
        ttOrder.shipToCity      = cShipToCity
        ttOrder.shipToState     = cShipToState
        ttOrder.shipToZip       = cShipToZip
        ttOrder.shipToCountry   = cShipToCountryCode
        ttOrder.shipToEmail     = cShipToEmail
        ttOrder.billToID        = cBillToAddressID
        ttOrder.billToName      = cBillToName
        ttOrder.billToDeliverTo = cBillToDeliverTo
        ttOrder.billToAddress1  = cBillToAddress[1]
        ttOrder.billToAddress2  = cBillToAddress[2]
        ttOrder.billToCity      = cBillToCity
        ttOrder.billToState     = cBillToState
        ttOrder.billToZip       = cBillToZip
        ttOrder.billToCountry   = cBillToCountryCode
        ttOrder.billToEmail     = cBillToEmail
        ttOrder.freightCost     = DECIMAL(cShippingCost)
        ttOrder.cardNo          = cCardNo
        ttOrder.cardExpiryDate  = DATE(INTEGER(SUBSTRING(cCardExpiryDate, 6, 2)), 
                                       INTEGER(SUBSTRING(cCardExpiryDate, 9, 2)), 
                                       INTEGER(SUBSTRING(cCardExpiryDate, 1, 4)))
        ttOrder.cardType        = IF cCardNo BEGINS '3' THEN 
                                      'AMEX'
                                  ELSE IF cCardNo BEGINS '4' THEN 
                                      'VISA'
                                  ELSE IF cCardNo BEGINS '5' THEN 
                                      'MC'
                                  ELSE IF cCardNo BEGINS '6' THEN 
                                      'DISCOVER'
                                  ELSE ''
        ttOrder.contactName     = cContactName
        ttOrder.contactEmail    = cContactEmail
        ttOrder.comments        = cOrderComments        
        ttOrder.action          = cOrderActionCreate
        ttOrder.importType      = cOrderImportType
        ttOrder.stat            = "W"
        NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.    
    END.
    
    /* Item details */
    RUN XML_GetFieldOrderListByNameAndParent (
        INPUT  "ItemOut",
        INPUT  iOrderIDOrderRequest,
        OUTPUT cItemOutList
        ).
    DO iIndex = 1 TO NUM-ENTRIES(cItemOutList):
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "quantity",
            INPUT  INTEGER(ENTRY(iIndex, cItemOutList)),
            OUTPUT lRecFound,
            OUTPUT cItemQuantity
            ).
        
        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "lineNumber",
            INPUT  INTEGER(ENTRY(iIndex, cItemOutList)),
            OUTPUT lRecFound,
            OUTPUT cLineNumber
            ).

        RUN XML_GetFieldValueByNameAndParent (
            INPUT  "requestedDeliveryDate",
            INPUT  INTEGER(ENTRY(iIndex, cItemOutList)),
            OUTPUT lRecFound,
            OUTPUT cLineDueDate
            ).

        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "ItemID",
            INPUT  INTEGER(ENTRY(iIndex, cItemOutList)),
            OUTPUT lRecFound,
            OUTPUT iOrderIDItemID
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "SupplierPartID",
                INPUT  iOrderIDItemID,
                OUTPUT lRecFound,
                OUTPUT cSupplierPartID
                ).

            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "SupplierPartAuxiliaryID",
                INPUT  iOrderIDItemID,
                OUTPUT lRecFound,
                OUTPUT cSupplierPartAuxiliaryID
                ).            
        END.

        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "ItemDetail",
            INPUT  INTEGER(ENTRY(iIndex, cItemOutList)),
            OUTPUT lRecFound,
            OUTPUT iOrderIDItemDetail
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldOrderByNameAndParent (
                INPUT  "UnitPrice",
                INPUT  iOrderIDItemDetail,
                OUTPUT lRecFound,
                OUTPUT iOrderIDUnitPrice
                ).
            IF lRecFound THEN
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "Money",
                    INPUT  iOrderIDUnitPrice,
                    OUTPUT lRecFound,
                    OUTPUT cItemUnitPrice
                    ).
                
            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "Description",
                INPUT  iOrderIDItemDetail,
                OUTPUT lRecFound,
                OUTPUT cItemDescription
                ).

            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "UnitOfMeasure",
                INPUT  iOrderIDItemDetail,
                OUTPUT lRecFound,
                OUTPUT cItemUOM
                ).            

            RUN XML_GetFieldValueByNameAndParent (
                INPUT  "ManufacturerPartID",
                INPUT  iOrderIDItemDetail,
                OUTPUT lRecFound,
                OUTPUT cItemManufacturerPartID
                ).            
        END.

        RUN XML_GetFieldOrderByNameAndParent (
            INPUT  "Distribution",
            INPUT  INTEGER(ENTRY(iIndex, cItemOutList)),
            OUTPUT lRecFound,
            OUTPUT iOrderIDDistribution
            ).
        IF lRecFound THEN DO:
            RUN XML_GetFieldOrderByNameAndParent (
                INPUT  "Charge",
                INPUT  iOrderIDDistribution,
                OUTPUT lRecFound,
                OUTPUT iDistributionCharge
                ).
            IF lRecFound THEN
                RUN XML_GetFieldValueByNameAndParent (
                    INPUT  "Money",
                    INPUT  iDistributionCharge,
                    OUTPUT lRecFound,
                    OUTPUT cLineTotalCost
                    ).
        END.

        CREATE ttOrderLine.
        ASSIGN
            ttOrderLine.orderSeqID              = ttOrder.orderSeqID
            ttOrderLine.company                 = cocode
            ttOrderLine.payLoadID               = cPayLoadID
            ttOrderLine.poID                    = cOrderID
            ttOrderLine.lineNo                  = INTEGER(cLineNumber)
            ttOrderLine.quantity                = DECIMAL(cItemQuantity)
            ttOrderLine.supplierPartID          = cSupplierPartID
            ttOrderLine.manufacturerPartID      = cItemManufacturerPartID
            ttOrderLine.supplierPartAuxiliaryID = cSupplierPartAuxiliaryID
            ttOrderLine.unitPrice               = DECIMAL(cItemUnitPrice) 
            ttOrderLine.itemName                = cItemDescription
            ttOrderLine.uom                     = cItemUOM
            ttOrderLine.dueDate                 = DATE(INTEGER(SUBSTRING(cLineDueDate, 6, 2)), 
                                                        INTEGER(SUBSTRING(cLineDueDate, 9, 2)), 
                                                        INTEGER(SUBSTRING(cLineDueDate, 1, 4)))
            ttOrderLine.lineCost                = DECIMAL(cLineTotalCost)
            ttOrderLine.action                  = cOrderActionCreate
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                .
            RETURN.    
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

    RUN oe/OrderImport.p (
        INPUT-OUTPUT TABLE ttOrder,
        INPUT-OUTPUT TABLE ttOrderLine,
        OUTPUT       oplSuccess,
        OUTPUT       opcMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE (1)
            .    
END PROCEDURE.
