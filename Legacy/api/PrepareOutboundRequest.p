/*------------------------------------------------------------------------
    File        : api/PrepareOutboundRequest.p
    Purpose     : Prepares outbound request

    Syntax      :

    Description : Prepares outbound request

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api/ttArgs.i}

DEFINE INPUT  PARAMETER TABLE           FOR ttArgs.
DEFINE INPUT  PARAMETER ipcAPIID        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.  
DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE cSysCtrlName AS CHARACTER NO-UNDO INITIAL "APIConfig".
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.

FIND FIRST APIOutbound NO-LOCK
     WHERE APIOutbound.APIID = ipcAPIID 
       AND APIOutbound.isActive
     NO-ERROR.

IF AVAILABLE APIOutbound THEN DO:
    RUN sys/ref/nk1look.p (
        cCompany,             /* Company Code */
        cSysCtrlName,         /* sys-ctrl name */
        "I",                  /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        FALSE,                /* Use ship-to */
        FALSE,                /* ship-to vendor */
        "",                   /* ship-to vendor value */
        "",                   /* shi-id value */
        OUTPUT cReturnValue,
        OUTPUT lRecFound
        ).
        
    IF NOT lRecFound THEN DO:
       ASSIGN
           opcMessage = "No API Configuration available in sys-ctrl table"
           oplSuccess = FALSE
           .
       RETURN.
    END.
    
    IF APIOutbound.requestHandler NE "" THEN DO:
        IF INDEX(APIOutbound.requestHandler,STRING(INTEGER(cReturnValue),"9999")) EQ 0 THEN DO:
            ASSIGN
                opcMessage = "Mismatch in APIOutbound and sys-ctrl request handler code for " + APIOutbound.clientID
                oplSuccess = FALSE
                .
            RETURN.
        END.  
    END.

    IF APIOutbound.responseHandler NE "" THEN DO:
        IF INDEX(APIOutbound.responseHandler,STRING(INTEGER(cReturnValue),"9999")) EQ 0 THEN DO:
            ASSIGN
                opcMessage = "Mismatch in APIOutbound and sys-ctrl response handler code for " + APIOutbound.clientID
                oplSuccess = FALSE
                .
            RETURN.
        END.  
    END.
        
    oplcRequestData = APIOutbound.requestData.
    /* Transform Request Data */
    RUN pPrepareRequest (
        ipcAPIID,
        INPUT APIOutbound.requestHandler,
        INPUT-OUTPUT oplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END.

PROCEDURE pPrepareRequest PRIVATE:
   /*------------------------------------------------------------------------------
   Purpose: Prepares request for the given API ID
   Notes:
   ------------------------------------------------------------------------------*/
   
   DEFINE INPUT         PARAMETER ipcAPIID          AS CHARACTER NO-UNDO.
   DEFINE INPUT         PARAMETER ipcRequestHandler AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER oplcRequestData   AS LONGCHAR  NO-UNDO.
   DEFINE OUTPUT        PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
   DEFINE OUTPUT        PARAMETER opcMessage        AS CHARACTER NO-UNDO.

   CASE ipcAPIID:
       WHEN "AddCustomer" THEN
           RUN api/AddCustomer.p (
               INPUT TABLE ttArgs,
               INPUT ipcRequestHandler,
               INPUT-OUTPUT oplcRequestData,
               OUTPUT oplSuccess,
               OUTPUT opcMessage
               ).
        WHEN "AddProduct" THEN
            RUN api/AddProduct.p (
                INPUT TABLE ttArgs,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "AddVendor" THEN
            RUN api/AddVendor.p (
                INPUT TABLE ttArgs,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "AddPurchaseOrder" THEN
            RUN api/AddPurchaseOrder.p (
                INPUT TABLE ttArgs,
                INPUT ipcAPIID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "AddPicklist" THEN
            RUN api/AddPicklist.p (
                INPUT TABLE ttArgs,
                INPUT ipcAPIID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
   END CASE.
END PROCEDURE.
