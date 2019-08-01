{api\ttArgs.i}

DEFINE INPUT  PARAMETER ipcAPIID     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableList AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcROWIDList AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplReTrigger AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess   AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcNotes        AS LONGCHAR  NO-UNDO.
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

/* Fetch request data from APIOutboundEvent record 
   in case the API is re-triggerred */
IF iplReTrigger THEN DO:
    FOR FIRST ttArgs 
        WHERE ttArgs.argType EQ "ROWID" 
          AND ttArgs.argKey  EQ "APIOutboundEvent"
          , 
          FIRST APIOutboundEvent EXCLUSIVE-LOCK
          WHERE ROWID(APIOutboundEvent) = TO-ROWID(ttArgs.argValue):
        ASSIGN 
            lcRequestData = APIOutboundEvent.RequestData
            oplSuccess    = TRUE
            .
    END.
    
    IF NOT oplSuccess THEN DO:
        opcMessage = "Re-trigger failed!".
        RETURN.
    END.
END.
ELSE
    /* Prepare and fetch request data from PrepareOutboundRequest.p 
       when the API is called for the first time */
    RUN api/PrepareOutboundRequest.p (
        INPUT TABLE ttArgs,
        ipcAPIID,    
        OUTPUT lcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).

IF NOT oplSuccess THEN DO:
   /* If the API event is not re-triggerred then
      only create a log else the log will be updated */
   IF NOT iplReTrigger THEN
       RUN api/CreateAPIOutboundEvent.p (
           INPUT ipcAPIID,
           INPUT lcRequestData,
           INPUT "", /* Response Data */
           INPUT cParentProgram,
           INPUT oplSuccess,
           INPUT opcMessage,
           INPUT NOW
         ). 
         
   RETURN.
END.

/* If all good then make the API call */
RUN api/CallOutBoundAPI.p (
    ipcAPIID,
    lcRequestData,
    cParentProgram,
    OUTPUT lcResponseData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

/* Add a record in APIOutboundEvent table here,
   when API Event is not re-triggered */
IF iplReTrigger THEN DO:
    ASSIGN
        lcNotes                         = APIOutboundEvent.notes
        APIOutboundEvent.responseData   = lcResponseData
        APIOutboundEvent.success        = oplSuccess
        APIOutboundEvent.errorMessage   = opcMessage
        APIOutboundEvent.requestDate    = NOW
        APIOutboundEvent.callingProgram = IF NUM-ENTRIES(cParentProgram," ") EQ 2 THEN 
                                              ENTRY(2, cParentProgram, " ")
                                          ELSE
                                              cParentProgram                                                                                  
        APIOutboundEvent.notes          = lcNotes
                                        + "~n"
                                        + STRING(NOW, "99/99/9999 HH:MM:SS.SSS")
                                        + " - "
                                        + USERID("ASI")
                                        + " - "
                                        + "Subsequent Trigger"
                                        + " - "
                                        + STRING(oplSuccess, "SUCCESS/FAILURE")
                                        + " - "
                                        + opcMessage
        .
END.
ELSE
    RUN api\CreateAPIOutboundEvent.p (
        INPUT ipcAPIID,
        INPUT lcRequestData,
        INPUT lcResponseData,
        INPUT cParentProgram,
        INPUT oplSuccess,
        INPUT opcMessage,
        INPUT NOW
        ).
