/*------------------------------------------------------------------------
    File        : api\OutboundProcs.p
    Purpose     : Procedures related to Outbound API

    Syntax      :

    Description : Procedures related to Outbound API

    Author(s)   : Porandla Mithun
    Created     : Fri September 06 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE GetAPIOutboundID:
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAPIOutboundID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
        
    FIND FIRST APIOutbound NO-LOCK
          WHERE APIOutbound.company  EQ ipcCompany
            AND APIOutbound.apiID    EQ ipcAPIID
            AND APIOutbound.clientID EQ ipcClientID
          NO-ERROR.
    IF AVAILABLE APIOutbound AND
        APIOutbound.isActive THEN
        ASSIGN
            oplValid         = TRUE
            opcMessage       = "Success"
            opiAPIOutboundID = APIOutbound.apiOutboundID
            .
    ELSE
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Outbound configuration for API ID ["
                       + ipcAPIID + "] is not available or inactive" 
            .            
END PROCEDURE.

PROCEDURE GetAPIOutboundTriggerID:
    DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID                AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.

    FIND FIRST APIOutboundTrigger NO-LOCK
         WHERE APIOutboundTrigger.company   EQ ipcCompany
           AND APIOutboundTrigger.apiID     EQ ipcAPIID
           AND APIOutboundTrigger.clientID  EQ ipcClientID
           AND APIOutboundTrigger.triggerID EQ ipcTriggerID
         NO-ERROR.
    IF AVAILABLE APIOutboundTrigger AND
        APIOutboundTrigger.isActive THEN
        ASSIGN
            oplValid                = TRUE
            opcMessage              = "Success"
            opiAPIOutboundTriggerID = APIOutbound.apiOutboundID
            .
    ELSE
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Outbound Trigger configuration for Trigger ID ["
                       + ipcTriggerID + "] is not available or inactive" 
            .
END PROCEDURE.
