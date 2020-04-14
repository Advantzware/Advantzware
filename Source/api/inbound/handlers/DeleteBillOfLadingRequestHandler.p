/*------------------------------------------------------------------------
    File        : api\inbound\handlers\DeleteBillOfLadingRequestHandler.p
    Purpose     : Process request data for delete BOL 

    Syntax      :

    Description : Process request data for delete BOL

    Author(s)   : Vishnu Vellanki
    Created     : Thu Apr 09 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttRequest.i}
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

DEFINE VARIABLE hdJSONProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBOLID      AS INTEGER   NO-UNDO.

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs). 

/* Get request data fields in a temp-table */
RUN ReadRequestData IN hdJSONProcs (
    INPUT  iplcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttRequest
    ).

IF NOT oplSuccess THEN DO:
    RUN JSON_GetResponseData (
        INPUT 400,        /* response code    */
        INPUT opcMessage, /* response message */
        INPUT "",         /* response date    */
        OUTPUT oplcResponseData
        ).

    RETURN.
END.

RUN pProcessInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

RUN JSON_EscapeExceptionalCharacters (
    INPUT-OUTPUT opcMessage
    ) NO-ERROR.
    
IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
    RUN JSON_GetResponseData (
        INPUT 400,        /* response code    */
        INPUT opcMessage, /* response message */
        INPUT "",         /* response data    */
        OUTPUT oplcResponseData
        ).      
ELSE DO:
    opcMessage = "Success".
    RUN JSON_GetResponseData (
        INPUT 200,        /* response code    */
        INPUT opcMessage, /* response message */
        INPUT "",         /* response data    */
        OUTPUT oplcResponseData
        ).
END.
        
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

PROCEDURE pProcessInputs:
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE lRecFound      AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.
    
    /* Fetches Requestor */          
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.  
        
    /* Fetches Company */
    RUN JSON_GetFieldValueByName (
        INPUT  "Company", 
        OUTPUT lRecFound, 
        OUTPUT cCompany
        ) NO-ERROR.

    /* Fetches BOLID */
    RUN JSON_GetFieldValueByName (
        INPUT  "BOLID", 
        OUTPUT lRecFound, 
        OUTPUT cBOLID
        ) NO-ERROR.
        

    /* This is to fetch response data*/
    RUN api\inbound\DeleteBillOfLading.p (
        INPUT  cCompany,
        INPUT  cBOLID,
        INPUT  ipcUserName,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) .
     
    IF NOT oplSuccess THEN
        RETURN.
        
END PROCEDURE. 

 

