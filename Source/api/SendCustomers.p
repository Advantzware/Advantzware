/*------------------------------------------------------------------------
    File        : api/SendCustomers.p
    Purpose     : Returns the request data for customer addition

    Syntax      :

    Description : Returns the request data for customer addition

    Author(s)   : 
    Created     : Wed May 11 01:55:32 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    {api/ttCustomer.i}
        
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdTTHandle     AS HANDLE NO-UNDO.
    DEFINE VARIABLE mptrTTCustomer AS MEMPTR NO-UNDO.
    
    DEFINE VARIABLE lcCustomer       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatCustomer AS LONGCHAR NO-UNDO.
    
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
        
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE  ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "TTCustomerHandle" NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "Invalid temp-table handle"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        hdTTHandle = HANDLE(ttArgs.argValue).
        
        IF NOT VALID-HANDLE (hdTTHandle) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid temp-table handle"
                .
            
            RETURN.        
        END.

        /* Code to send data from dynamic temp-table handle to static temp-table */
        hdTTHandle:WRITE-XML("MEMPTR", mptrTTCustomer).
        
        TEMP-TABLE ttCustomer:READ-XML("MEMPTR", mptrTTCustomer, "EMPTY", ?, FALSE).
        
        SET-SIZE(mptrTTCustomer) = 0.                  
        
        FIND FIRST APIOutboundDetail NO-LOCK    
             WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND APIOutboundDetail.parentID      EQ "SendCustomers"
               AND APIOutboundDetail.detailID      EQ "Customer"
             NO-ERROR.
        IF AVAILABLE APIOutboundDetail THEN DO:
            FOR EACH ttCustomer:
                lcCustomer = APIOutboundDetail.data.
                
                RUN updateRequestData(INPUT-OUTPUT lcCustomer, "CustomerCRMID", ttCustomer.crmCustomerID).
                RUN updateRequestData(INPUT-OUTPUT lcCustomer, "CustomerID", ttCustomer.customerID).

                lcConcatCustomer = lcConcatCustomer + lcCustomer.    
            END.
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatCustomer, INPUT gcRequestDataType).
        END.
        
        ioplcRequestData = REPLACE(ioplcRequestData, "$Customers$", lcConcatCustomer).
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.        
        
