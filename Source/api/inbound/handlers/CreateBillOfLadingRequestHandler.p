/*------------------------------------------------------------------------
    File        : api\inbound\handlers\CreateBillOfLadingRequestHandler.p
    Purpose     : Process request data for create BOL 

    Syntax      :

    Description : Process request data for create BOL

    Author(s)   : Vishnu Vellanki
    Created     : Tue Sep 19 07:33:22 EDT 2019
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

DEFINE VARIABLE hdttRequestData AS HANDLE    NO-UNDO. 
DEFINE VARIABLE hdJSONProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdttBuffer      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdttQuery       AS HANDLE    NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReleaseID      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBOLID          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iResponseCode   AS INTEGER   NO-UNDO.

{api/inbound/ttRequest.i}

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs. 

/* Get request data fields in a temp-table */
RUN ReadRequestData IN hdJSONProcs (
    INPUT  iplcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttRequest
    ).
        
IF NOT oplSuccess THEN DO:   
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
        
/* This will fetch fields from request data */
FOR EACH ttRequest:
    CASE ttRequest.FieldName:
        WHEN "Company" THEN
           cCompany = ttRequest.FieldValue.
        WHEN "ReleaseID" THEN
           cReleaseID = INT(ttRequest.FieldValue).
        WHEN "Requester" THEN
           ipcRequestedBy = ttRequest.FieldValue.
        WHEN "RequesterNotes" THEN
           ipcNotes = ttRequest.FieldValue.
    END CASE.
END.

/* This is to fetch response data*/ 
RUN api\inbound\CreateBillOfLading.p (
    INPUT  cCompany, 
    INPUT  cReleaseID,
    INPUT  ipcUserName,
    OUTPUT cBOLID,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

ASSIGN
    oplcResponseData = iplcResponseDataStructure
    oplcResponseData = IF oplSuccess THEN 
                           REPLACE(oplcResponseData, "&1",STRING(cCompany))
                       ELSE
                          REPLACE(oplcResponseData, "&1","")
    oplcResponseData = IF oplSuccess THEN 
                           REPLACE(oplcResponseData, "&2",STRING(cBOLID))
                       ELSE
                          REPLACE(oplcResponseData, "&2","0")
    opcMessage       = IF oplSuccess THEN 
                           "Success"
                       ELSE
                           opcMessage
    iResponseCode    = IF oplSuccess THEN
                           200
                       Else
                           400
    oplcResponseData = '~{"response_code":' + STRING(iResponseCode) + ',"response_message":"' + opcMessage + '","response_data":[' + oplcResponseData + ']}'. 
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
    
DELETE PROCEDURE hdJSONProcs.

                                                      
