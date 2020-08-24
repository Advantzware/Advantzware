/*------------------------------------------------------------------------
    File        : oe/OrderImport.p
    Purpose     : Process the order creation

    Syntax      :

    Description : Process the order creation

    Author(s)   : Porandla Mithun
    Created     : Tue Jul 21 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{oe/ttOrder.i}

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrder.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrderLine.
DEFINE OUTPUT       PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE cValidateListImporter    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateListCXML        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateListCXMLMonitor AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateListEDI         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateProcedureList   AS CHARACTER NO-UNDO.

/* Saves the list of internal preocedures */
cValidateProcedureList = THIS-PROCEDURE:INTERNAL-ENTRIES.

ASSIGN
    cValidateListImporter    = "pValidateCustomer,pValidateOrder,pValidateOrderDate,pValidateOrderShipTo,"
                             + "pValidateLineItems"
    cValidateListCXMLMonitor = "pValidateCustomer,pValidateOrderDate,pValidateOrderShipTo,"
                             + "pValidateOrderWithPayload,pValidatePayloadID,pValidateLineItems"
    cValidateListEDI         = ""
    .

/* Input pre-processing */
RUN pPreProcessInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.
  
/* Input validation */
IF oplSuccess THEN
    RUN pValidateInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

/* Input processing */
IF oplSuccess THEN
    RUN pProcessInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    opcMessage = ERROR-STATUS:GET-MESSAGE(1).

FUNCTION fGetCustNoForcXMLMonitor RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcIdentity AS CHARACTER, ipcShipToID AS CHARACTER) FORWARD.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pPreProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Pre-processes the inputs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    FOR EACH ttOrder:
        IF ttOrder.importType EQ cOrderImportTypeCXMLMonitor THEN DO:
            RUN pPreProcessOrderCXMLMonitor (
                BUFFER ttOrder,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
            IF NOT oplSuccess THEN
                RETURN.
        END.
    END.    
END PROCEDURE.

PROCEDURE pPreProcessOrderCXMLMonitor PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Pre-processes the inputs for cXML orders
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPreProcessBy AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-itemfg FOR itemfg.
       
    IF AVAILABLE ipbf-ttOrder THEN DO:        
        IF ipbf-ttOrder.action EQ cOrderActionCreate THEN DO:
            /* Validation of fromIdentity */
            IF ipbf-ttOrder.fromIdentity EQ "" THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = 'FromIdentity is empty for Order# ' + STRING(ipbf-ttOrder.orderID)
                    .
            END. 
            ELSE DO:
                ipbf-ttOrder.customerID = fGetCustNoForcXMLMonitor(ipbf-ttOrder.company, ipbf-ttOrder.fromIdentity, ipbf-ttOrder.shipToID).
                
                IF ipbf-ttOrder.customerID NE "" THEN
                    cPreProcessBy = "Identity".
            END.
            
            IF cPreProcessBy EQ "" THEN DO:
                /* This procedure validates company code,shipToID and location code, 
                   and returns valid company code,location code,shipToID and customer number.
                   and additionally it returns the shipto table buffer to access any other data 
                   from shipto table */ 
                RUN cXML/getCustDetails.p (
                    INPUT        ipbf-ttOrder.fromIdentity,
                    INPUT-OUTPUT ipbf-ttOrder.shipToID,   /* Shipto SiteID */
                    INPUT-OUTPUT ipbf-ttOrder.company,
                    INPUT-OUTPUT ipbf-ttOrder.warehouseID,
                    INPUT        ipbf-ttOrder.orderID,
                    OUTPUT       ipbf-ttOrder.customerID,
                    OUTPUT       oplSuccess,
                    OUTPUT       opcMessage,
                    BUFFER       bf-shipto
                    ).
                IF NOT oplSuccess THEN
                    RETURN.
                
                cPreProcessBy = "SiteID".
            END.
        END.
        
        IF cPreprocessBy EQ "" THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Unable to validate the customer/shipto for Order#: " + ipbf-ttOrder.poID
                .
            RETURN.
        END.
        
        IF cPreprocessBy EQ "SiteID" THEN
            RUN pPreProcessOrderCXMLMonitorBySiteID (
                BUFFER ttOrder,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        ELSE IF cPreprocessBy EQ "Identity" THEN
            RUN pPreProcessOrderCXMLMonitorByIdentity (
                BUFFER ttOrder,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        
        IF NOT oplSuccess THEN
            RETURN.

        IF NUM-ENTRIES(ipbf-ttOrder.billToID, ":") GE 2 AND TRIM(ENTRY(2, ipbf-ttOrder.billToID, ":")) NE "" THEN 
            ipbf-ttOrder.billToID = ENTRY(2, ipbf-ttOrder.billToID, ":").
    END.
END PROCEDURE.

PROCEDURE pPreProcessOrderCXMLMonitorBySiteID PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Pre-processes the inputs for cXML orders
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    IF AVAILABLE ipbf-ttOrder THEN DO:        
        IF ipbf-ttOrder.action EQ cOrderActionUpdate OR ipbf-ttOrder.action EQ cOrderActionDelete THEN DO:
            FIND FIRST bf-oe-ord NO-LOCK
                 WHERE bf-oe-ord.company      EQ ipbf-ttOrder.company
                   AND bf-oe-ord.cust-no      EQ ipbf-ttOrder.customerID
                   AND bf-oe-ord.po-no        EQ ipbf-ttOrder.poID
                   AND bf-oe-ord.spare-char-3 EQ ipbf-ttOrder.payLoadID
                 NO-ERROR.
            IF AVAILABLE bf-oe-ord THEN
                ipbf-ttOrder.orderID = bf-oe-ord.ord-no.
        END.
            
        FOR EACH ttOrderLine
            WHERE ttOrderLine.orderSeqID EQ ipbf-ttorder.OrderSeqID:
            ttOrderLine.company = ipbf-ttOrder.company.
            
            /* If supplier part id is blank, use first 8 characters of description */
            IF ttOrderLine.supplierPartID EQ "" THEN
                ttOrderLine.supplierPartID = SUBSTRING(ttOrderLine.itemName, 1, 8).

            IF ttOrderLine.supplierPartID EQ "" THEN DO:
                ASSIGN
                   oplSuccess = NO
                   opcMessage = "SupplierPartID is empty for line (" + STRING(ttOrderLine.lineNo) + ") : Order#" + ipbf-ttOrder.poID
                   .    
                RETURN.
            END.
        
            FIND FIRST bf-itemfg NO-LOCK 
                 WHERE bf-itemfg.company EQ ttOrderLine.company
                   AND bf-itemfg.def-loc EQ ipbf-ttOrder.wareHouseID
                   AND bf-itemfg.part-no EQ ttOrderLine.supplierPartID
                   AND bf-itemfg.stat    EQ "A"
                 NO-ERROR.
            IF NOT AVAILABLE bf-itemfg THEN DO:                
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "SupplierPartID '" + ttOrderLine.supplierPartID + "' not found for Order#: " + ipbf-ttOrder.poID
                    .     
                RETURN.
            END.
            
            ttOrderLine.manufacturerPartID = bf-itemfg.i-no.
        END.
    END.
END PROCEDURE.

PROCEDURE pPreProcessOrderCXMLMonitorByIdentity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Pre-processes the inputs for cXML orders
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-itemfg FOR itemfg.
        
    IF AVAILABLE ipbf-ttOrder THEN DO:        
        FOR EACH ttOrderLine
            WHERE ttOrderLine.orderSeqID EQ ipbf-ttOrder.orderSeqID:
            
            ttOrderLine.company = ipbf-ttOrder.company.
                            
            FIND FIRST bf-itemfg NO-LOCK
                 WHERE bf-itemfg.company EQ ttOrderLine.company
                   AND bf-itemfg.i-no    EQ TRIM(ttOrderLine.manufacturerPartID) 
                 NO-ERROR.
    
            IF NOT AVAIL bf-itemfg THEN
                 FIND FIRST bf-itemfg NO-LOCK
                      WHERE bf-itemfg.company EQ ttOrderLine.company
                        AND bf-itemfg.cust-no EQ ipbf-ttOrder.customerID
                        AND bf-itemfg.part-no EQ TRIM(ttOrderLine.supplierPartID) 
                     NO-ERROR.

            IF NOT AVAILABLE bf-itemfg THEN DO:
                ASSIGN 
                    oplSuccess = FALSE
                    opcMessage = "Item not found with manufactureID: " + TRIM(ttOrderLine.manufacturerPartID) + ' and supplierID: ' + ttOrderLine.supplierPartID
                    .
                RETURN.
            END.
 
            ttOrderLine.manufacturerPartID = bf-itemfg.i-no.            
        END.
    END.
END PROCEDURE.

PROCEDURE pValidateCustomer PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate customer 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust   FOR cust.

    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ ipbf-ttOrder.company
           AND bf-cust.cust-no EQ ipbf-ttOrder.customerID
         NO-ERROR.
    IF NOT AVAILABLE bf-cust THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'Customer: ' + ipbf-ttOrder.customerID + ' not found for Order#: ' + ipbf-ttOrder.poID + ' for payload: ' + ipbf-ttOrder.payLoadID 
            .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE pValidateInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to validate inputs
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iEntry           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEntryProcedure  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValidateList    AS CHARACTER NO-UNDO.
    
    FOR EACH ttOrder:
        IF ttOrder.importType EQ cOrderImportTypeCXMLMonitor THEN
            cValidateList = cValidateListCXMLMonitor.
        ELSE IF ttOrder.importType EQ cOrderImportTypeImporter THEN
            cValidateList = cValidateListImporter.
        ELSE IF ttOrder.importType EQ cOrderImportTypeEDI THEN
            cValidateList = cValidateListEDI.
        
        IF cValidateList EQ "" THEN
            NEXT.
        
        DO iEntry = 1 TO NUM-ENTRIES(cValidateList):
            /* Verify if the validate procedure is available in this procedure. */
            IF LOOKUP(ENTRY(iEntry, cValidateList), cValidateProcedureList) GT 0 THEN DO:
                RUN VALUE(ENTRY(iEntry, cValidateList)) (
                    BUFFER ttOrder,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ).
                IF NOT oplSuccess THEN
                    RETURN.
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Process the inputs to get the result
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOrderProcs AS HANDLE NO-UNDO.
    RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.
    
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        RUN ProcessOrdersFromImport IN hdOrderProcs (
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
        
        IF NOT oplSuccess THEN
            UNDO, LEAVE.
    END.
    
    DELETE PROCEDURE hdOrderProcs.
END PROCEDURE.

PROCEDURE pValidateLineItems PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate line items
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    FOR EACH ttOrderLine
        WHERE ttOrderLine.orderSeqID EQ ipbf-ttOrder.orderSeqID:
        IF ttOrderLine.supplierPartID EQ "" AND ttOrderLine.manufacturerPartID EQ "" THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = 'Part Number is missing from XML file for Order# ' + ipbf-ttOrder.poID
                .
            RETURN.
        END. 
    END.        
END PROCEDURE.

PROCEDURE pValidateOrderDate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate Order date
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    IF ipbf-ttOrder.orderDate EQ ? THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'OrderDate is empty for Order# ' + STRING(ipbf-ttOrder.orderID)
            .                
        RETURN.
    END. 
END PROCEDURE.

PROCEDURE pValidateOrderShipTo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate Order date
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    IF ipbf-ttOrder.shipToID EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "ShipToID is empty for Order# " + STRING(ipbf-ttOrder.orderID)
            .                
        RETURN. 
    END.
END PROCEDURE.

PROCEDURE pValidateOrder PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validates order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    IF (ipbf-ttOrder.action EQ cOrderActionUpdate OR ipbf-ttOrder.action EQ cOrderActionDelete) AND ipbf-ttOrder.orderID EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'OrderID is empty'
            .                
        RETURN.
    END.

    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipbf-ttOrder.company
           AND bf-oe-ord.ord-no  EQ ipbf-ttOrder.orderID
         NO-ERROR.
    IF AVAILABLE bf-oe-ord AND ipbf-ttOrder.action EQ cOrderActionCreate THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'Order already exists with Order#: ' + STRING(ipbf-ttOrder.orderID)
            .                
        RETURN.
    END.
    ELSE IF NOT AVAILABLE bf-oe-ord AND (ipbf-ttOrder.action EQ cOrderActionUpdate OR ipbf-ttOrder.action EQ cOrderActionDelete) THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'Order does not exists with Order#: ' + STRING(ipbf-ttOrder.orderID)
            .                
        RETURN.
    END.

END PROCEDURE.

PROCEDURE pValidateOrderWithPayload PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate the order with payload id
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord FOR oe-ord.
        
    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company      EQ ipbf-ttOrder.company
           AND bf-oe-ord.cust-no      EQ ipbf-ttOrder.customerID
           AND bf-oe-ord.po-no        EQ ipbf-ttOrder.poID
           AND bf-oe-ord.spare-char-3 EQ ipbf-ttOrder.payloadID
         NO-ERROR.
    IF AVAILABLE bf-oe-ord AND ipbf-ttOrder.action EQ cOrderActionCreate THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'Order already exists with Order#: ' + ipbf-ttOrder.poID + ' for payload: ' + ipbf-ttOrder.payloadID
            .                
        RETURN.
    END.
    ELSE IF NOT AVAILABLE bf-oe-ord AND (ipbf-ttOrder.action EQ cOrderActionUpdate OR ipbf-ttOrder.action EQ cOrderActionDelete) THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'Order does not exists with Order#: ' + ipbf-ttOrder.poID + ' for payload: ' + ipbf-ttOrder.payloadID
            .                
        RETURN.
    END.

END PROCEDURE.

PROCEDURE pValidatePayloadID PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate payload id
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    /* Validation of PayloadID */
    IF ipbf-ttOrder.payloadID EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = 'PayloadID is empty' 
            .                
        RETURN.
    END. 
END PROCEDURE.

FUNCTION fGetCustNoForcXMLMonitor RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcIdentity AS CHARACTER, ipcShipToID AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the customer number for cXML Import using monitor
 Notes:
------------------------------------------------------------------------------*/            
    DEFINE VARIABLE cReturnCust AS CHARACTER NO-UNDO.
      
    DEFINE BUFFER bf-sys-ctrl-shipto FOR sys-ctrl-shipto.
    DEFINE BUFFER bf-edMast          FOR edMast.
    DEFINE BUFFER bf-edShipTo        FOR edShipTo.
    DEFINE BUFFER bf-shipto          FOR shipto.
    
    FIND FIRST bf-sys-ctrl-shipto NO-LOCK
         WHERE bf-sys-ctrl-shipto.company   EQ ipcCompany
           AND bf-sys-ctrl-shipto.name      EQ 'cXMLOrder'
           AND bf-sys-ctrl-shipto.cust-vend EQ YES
           AND bf-sys-ctrl-shipto.char-fld  EQ ipcIdentity
           AND bf-sys-ctrl-shipto.log-fld   EQ YES 
         NO-ERROR.
    IF AVAILABLE bf-sys-ctrl-shipto THEN DO:
        cReturnCust = bf-sys-ctrl-shipto.cust-vend-no.
        
        /* Option to find cust# by cust-no + ship-id */
        IF bf-sys-ctrl-shipto.int-fld EQ 1 AND cReturnCust GT "" AND ipcShipToID GT "" THEN DO:
            FOR EACH  bf-edMast NO-LOCK 
                WHERE bf-edMast.cust EQ cReturnCust
                ,
                EACH  bf-edShipTo NO-LOCK 
                WHERE bf-edShipTo.partner EQ bf-edMast.partner
                  AND bf-edShipTo.siteID  EQ ipcIdentity
                ,
                FIRST bf-shipto NO-LOCK
                WHERE bf-shipto.company EQ ipcCompany
                  AND bf-shipto.cust-no EQ bf-edShipto.cust
                  AND bf-shipto.ship-id EQ ipcShipToID
                :
               cReturnCust = bf-edShipTo.cust.
           END.
        END.
    END.

    RETURN cReturnCust.
END FUNCTION.

