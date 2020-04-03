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

DEFINE INPUT  PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT  PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplcRequestData         AS LONGCHAR  NO-UNDO.  
DEFINE OUTPUT PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.

FIND FIRST APIOutbound NO-LOCK
     WHERE APIOutbound.apiOutboundID EQ ipiAPIOutboundID
     NO-ERROR.
IF AVAILABLE APIOutbound AND 
   APIOutbound.isActive THEN DO:           
    oplcRequestData = APIOutbound.requestData.
    /* Transform Request Data */
    RUN pPrepareRequest (
        INPUT  APIOutbound.apiID,
        INPUT  APIOutbound.apiOutboundID,
        INPUT  ipiAPIOutboundTriggerID,        
        INPUT  APIOutbound.requestHandler,
        INPUT-OUTPUT oplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END.
ELSE
    ASSIGN
        opcMessage = "API Outbound configuration for Outbound Sequence ID [" 
                   + STRING(ipiAPIOutboundID)
                   + "], is not available or inactive"
        oplSuccess = FALSE
        .
        
PROCEDURE pPrepareRequest PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Prepares request for the given API ID
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcAPIID                AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER oplcRequestData         AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
 
    CASE ipcAPIID:
        WHEN "SendCustomer" THEN
            RUN api/SendCustomer.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendFinishedGood" THEN
            RUN api/SendFinishedGood.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendVendor" THEN
            RUN api/SendVendor.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendPurchaseOrder" THEN
            RUN api/SendPurchaseOrder.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendPurchaseOrderStatus" THEN
            RUN api/SendPurchaseOrderStatus.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendPurchaseOrderLineStatus" THEN
            RUN api/SendPurchaseOrderLineStatus.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendInvoice" THEN
            RUN api/SendInvoice.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendInvoiceStatus" THEN
            RUN api/SendInvoiceStatus.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendInvoiceLineStatus" THEN
            RUN api/SendInvoiceLineStatus.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).                
        WHEN "SendRelease" THEN
            RUN api/SendRelease.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "CheckTransfer" THEN
            RUN api/CheckTransfer.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).        
        WHEN "SendAdvancedShipNotice" THEN
            RUN api/SendAdvancedShipNotice.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
        WHEN "SendJob" THEN
            RUN api/SendJob.p (
                INPUT TABLE ttArgs,
                INPUT ipiAPIOutboundID,
                INPUT ipiAPIOutboundTriggerID,
                INPUT ipcRequestHandler,
                INPUT-OUTPUT oplcRequestData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
                
    END CASE.   
END PROCEDURE.
