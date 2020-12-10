/*------------------------------------------------------------------------
    File        : api\CreateAPIInboundEvent.p
    Purpose     : Creates APIInboundEvent Table Log

    Syntax      :

    Description : Creates APIInboundEvent Table Log

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 21 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
USING System.SharedConfig.

DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipiInboundEventID   AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRoute            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplcReponseData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplSuccess          AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcMessage          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDateTime         AS DATETIME  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPayloadID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDelayedProcStat  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent  AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcNotes       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcRequestData AS LONGCHAR NO-UNDO.

FIND FIRST APIInboundEvent EXCLUSIVE-LOCK
     WHERE APIInboundEvent.apiInboundEventID EQ ipiInboundEventID
     NO-ERROR.
IF NOT AVAILABLE APIInboundEvent AND iplReTrigger THEN
    RETURN. /* Do not create APIInboundEvent in case of invalid re-trigger event */
    
IF AVAILABLE APIInboundEvent THEN DO:
    ASSIGN
       lcNotes                         = APIInboundEvent.errorMessage
       lcRequestData                   = APIInboundEvent.requestData
       APIInboundEvent.errorMessage    = STRING(ipcDateTime, "99/99/9999 HH:MM:SS.SSS")
                                         + " - "
                                         + USERID("ASI")
                                         + " - "
                                         + "Subsequent Trigger"
                                         + " - "
                                         + STRING(iplSuccess, "SUCCESS/FAILURE")
                                         + " - "
                                         + ipcMessage
                                         + "@@@"
                                         + lcNotes
       .
    
    IF lcRequestData NE iplcRequestData THEN
        APIInboundEvent.requestData = iplcRequestData.
END.     
ELSE DO:
    CREATE APIInboundEvent.
    ASSIGN
        APIInboundEvent.company         = ipcCompany
        APIInboundEvent.apiRoute        = ipcRoute
        APIInboundEvent.requestData     = iplcRequestData
        APIInboundEvent.errorMessage    = STRING(ipcDateTime) 
                                          + " - " 
                                          + USERID("ASI") 
                                          + " - "
                                          + "Initial Trigger"
                                          + " - "
                                          + STRING(iplSuccess, "SUCCESS/FAILURE")
                                          + " - "
                                          + ipcMessage
        APIInboundEvent.requestedBy     = ipcRequestedBy
        APIInboundEvent.recordSource    = ipcRecordSource
        APIInboundEvent.notes           = ipcNotes
        APIInboundEvent.externalID      = ipcPayloadID
        .
END.

ASSIGN 
    APIInboundEvent.delayedProcessingStatus = ipcDelayedProcStat
    APIInboundEvent.responseData            = iplcReponseData
    APIInboundEvent.success                 = iplSuccess
    APIInboundEvent.requestDateTime         = ipcDateTime
    APIInboundEvent.userField1              = SharedConfig:Instance:ConsumeValue("APIInboundEvent_UserField1")
    APIInboundEvent.userField2              = SharedConfig:Instance:ConsumeValue("APIInboundEvent_UserField2")
    APIInboundEvent.userField3              = SharedConfig:Instance:ConsumeValue("APIInboundEvent_UserField3")
    .
 
opcAPIInboundEvent = STRING(ROWID(APIInboundEvent))
