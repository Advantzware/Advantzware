/*------------------------------------------------------------------------
    File        : api\inbound\handlers\ConsumeInventoryViaBOLHandler.p
    Purpose     : Consume Inventory Via Bill Of Lading 

    Syntax      :

    Description : Consume Inventory Via Bill Of Lading 

    Author(s)   : Mithun Porandla
    Created     : Thu October 24 05:16:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRequestDataType        AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRequestedBy            AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRecordSource           AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcNotes                  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUsername               AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.
    
    /* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's
       create trigger, only if session.p is running persistently, else will be populated with empty value.
       ( refer methods/triggers/create.i ) */
    
    /* This will eventually move to setsession approach */
    &SCOPED-DEFINE NEW NEW
    {methods/defines/globdefs.i}
    {methods/defines/hndldefs.i}
    
    DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
    DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.
        
    RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
    
    RUN system/session.p  PERSISTENT SET hdSession.
    SESSION:ADD-SUPER-PROCEDURE (hdSession).
    RUN system/TagProcs.p PERSISTENT SET hdTags.
    SESSION:ADD-SUPER-PROCEDURE (hdTags).
    {sys/inc/var.i "new shared"}
    {sys/inc/varasgn.i}    
 
    /* Consume Inventory Via BOL */
    RUN api\inbound\ConsumeInventoryViaBOL (
        INPUT  ipcRoute,
        INPUT  ipcVerb,
        INPUT  iplcRequestData,
        OUTPUT ipcRequestedBy,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
            
    IF NOT oplSuccess THEN
    DO:
       oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.   
       
       /* Log the request to APIInboundEvent */
       RUN api\CreateAPIInboundEvent.p (
           INPUT ipcRoute,
           INPUT iplcRequestData,
           INPUT oplcResponseData,
           INPUT oplSuccess,
           INPUT opcMessage,
           INPUT NOW,
           INPUT ipcRequestedBy,
           INPUT ipcRecordSource,
           INPUT ipcNotes,
           INPUT  "", /* PayloadID */
           OUTPUT opcAPIInboundEvent
           ).
       
       RETURN.  
    END.
     
    ASSIGN
        oplcResponseData = '~{"response_code":200,"response_message":"Inventory Consume is successful"}'
        opcMessage = "Success"
        .
    
    /* Log the request to APIInboundEvent */
    RUN api\CreateAPIInboundEvent.p (
        INPUT ipcRoute,
        INPUT iplcRequestData,
        INPUT oplcResponseData,
        INPUT oplSuccess,
        INPUT opcMessage,
        INPUT NOW,
        INPUT ipcRequestedBy,
        INPUT ipcRecordSource,
        INPUT ipcNotes,
        INPUT  "", /* PayloadID */
        OUTPUT opcAPIInboundEvent
        ).
    
    
    SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
    DELETE PROCEDURE hdSession.
    SESSION:REMOVE-SUPER-PROCEDURE (hdTags).
    DELETE PROCEDURE hdTags.
    
