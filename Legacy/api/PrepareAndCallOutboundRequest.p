{api\ttArgs.i}

DEFINE INPUT  PARAMETER ipcAPIID     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableList AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcROWIDList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess   AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cParentProgram AS CHARACTER NO-UNDO.   
DEFINE VARIABLE iCount         AS INTEGER   NO-UNDO.
    
EMPTY TEMP-TABLE ttArgs.

IF ipcAPIID EQ "" THEN DO:
    ASSIGN
        opcMessage = "API ID cannot be empty"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF ipcTableList EQ "" THEN DO:
    ASSIGN
        opcMessage = "Empty value passed in table list"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF ipcROWIDList EQ "" THEN DO:
    ASSIGN
        opcMessage = "Empty value passed in ROW ID list"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF NUM-ENTRIES(ipcTableList) NE NUM-ENTRIES(ipcROWIDList) THEN DO:
    ASSIGN
        opcMessage = "Mismatch of number of entries in table and rowid list"
        oplSuccess = FALSE
        .
    RETURN.
END.

DO iCount = 1 TO NUM-ENTRIES(ipcTableList):
    CREATE ttArgs.
    ASSIGN
        ttArgs.argType  = "ROWID"
        ttArgs.argKey   = ENTRY(iCount,ipcTableList)
        ttArgs.argValue = ENTRY(iCount,ipcROWIDList)
        .
END.

cParentProgram = PROGRAM-NAME(2).

RUN api/PrepareOutboundRequest.p (
    INPUT TABLE ttArgs,
    ipcAPIID,    
    OUTPUT lcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).            
 
IF NOT oplSuccess THEN DO:
   RUN api/CreateAPIOutboundEvent.p (
       INPUT ipcAPIID,
       INPUT lcRequestData,
       INPUT "",
       INPUT cParentProgram,
       INPUT oplSuccess,
       INPUT opcMessage,
       INPUT NOW
       ).    
END.
ELSE    
    RUN api/CallOutBoundAPI.p (
        ipcAPIID,
        lcRequestData,
        cParentProgram,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).

