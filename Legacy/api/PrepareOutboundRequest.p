/*------------------------------------------------------------------------
    File        : api/PrepareOutboundRequest.p
    Purpose     : Prepares outbound request

    Syntax      :

    Description : Prepares outbound request

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcAPIID        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.  
DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.

FIND FIRST APIOutbound NO-LOCK
     WHERE APIOutbound.APIID = ipcAPIID 
       AND APIOutbound.isActive
     NO-ERROR.

IF AVAILABLE APIOutbound THEN DO:    
    oplcRequestData = APIOutbound.requestData.
    /* Transform Request Data */
    RUN pPrepareRequest (
        ipcAPIID,
        INPUT APIOutbound.requestHandler,
        INPUT-OUTPUT oplcRequestData,
        OUTPUT opcMessage,
        OUTPUT oplSuccess
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
   DEFINE OUTPUT        PARAMETER opcMessage        AS CHARACTER NO-UNDO.
   DEFINE OUTPUT        PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.

   CASE ipcAPIID:
       WHEN "AddCustomer" THEN
           RUN api/AddCustomer.p (
               INPUT ipcRequestHandler,
               INPUT-OUTPUT oplcRequestData,
               OUTPUT opcMessage,
               OUTPUT oplSuccess
               ).
        WHEN "AddProduct" THEN
            RUN api/AddProduct.p (
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT opcMessage,
                OUTPUT oplSuccess
                ).
        WHEN "AddVendor" THEN
            RUN api/AddVendor.p (
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT opcMessage,
                OUTPUT oplSuccess
                ).
        WHEN "AddPurchaseOrder" THEN
            RUN api/AddPurchaseOrder.p (
                INPUT ipcAPIID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT opcMessage,
                OUTPUT oplSuccess
                ).
        WHEN "AddPicklist" THEN
            RUN api/AddPicklist.p (
                INPUT ipcAPIID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT opcMessage,
                OUTPUT oplSuccess
                ).
   END CASE.
END PROCEDURE.
