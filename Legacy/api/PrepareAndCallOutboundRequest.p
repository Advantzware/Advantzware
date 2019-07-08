{api\ttArgs.i}

DEFINE INPUT  PARAMETER TABLE         FOR ttArgs.
DEFINE INPUT  PARAMETER ipcAPIID          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcParentProgram  AS CHARACTER NO-UNDO.
    
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.

RUN api/PrepareOutboundRequest.p (
    INPUT TABLE ttArgs,
    ipcAPIID,    
    OUTPUT lcRequestData,
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).            
 
IF NOT lSuccess THEN DO:
   RUN api/CreateAPIOutboundEvent.p (
       INPUT ipcAPIID,
       INPUT lcRequestData,
       INPUT "",
       INPUT ipcParentProgram,
       INPUT lSuccess,
       INPUT cMessage,
       INPUT NOW
       ).    
END.
ELSE    
    RUN api/CallOutBoundAPI.p (
        ipcAPIID,
        lcRequestData,
        ipcParentProgram,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

