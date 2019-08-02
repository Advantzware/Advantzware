/*------------------------------------------------------------------------
    File        : api/inbound/handlers/cXMLOrderRequestHandler.p
    Purpose     : To create cXML based purchase orders

    Syntax      :

    Description : To create cXML based purchase orders

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 25 07:33:22 EDT 2019
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
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cPayLoadID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponse        AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cResponseCode     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE riAPIInboundEvent AS ROWID     NO-UNDO. 

FUNCTION GetDateTimeTZStringForResponse RETURNS CHARACTER() FORWARD.

RUN cXML/gencXMLOrder.p (
    iplcRequestData,
    NO, /* Add records to temp-table only */
    OUTPUT cPayLoadID,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) 
    NO-ERROR.

/* Prepares response data */
ASSIGN
    opcMessage       = IF oplSuccess THEN "OK" ELSE opcMessage
    cResponseCode    = IF oplSuccess THEN "200" ELSE "400"
    lcResponse       = '~<Status code="' + cResponseCode + '" text="' + opcMessage  + '"/>'
    oplcResponseData = iplcResponseDataStructure
    oplcResponseData = REPLACE(oplcResponseData,"$payloadID",cPayLoadID)
    oplcResponseData = REPLACE(oplcResponseData,"$response",lcResponse)
    oplcResponseData = REPLACE(oplcResponseData,"$timestamp",GetDateTimeTZStringForResponse())
    oplcResponseData = REPLACE(oplcResponseData,'"','\"')
    oplcResponseData = '~{"response_code": ' +  cResponseCode + ',"response_message":"' + oplcResponseData + '"}'
    . 

/* Log the request to APIInboundEvent */
RUN api\CreateAPIInboundEvent.p (
    INPUT  ipcRoute,
    INPUT  iplcRequestData,
    INPUT  oplcResponseData,
    INPUT  oplSuccess,
    INPUT  opcMessage,
    INPUT  NOW,
    INPUT  ipcRequestedBy,
    INPUT  ipcRecordSource,
    INPUT  ipcNotes,
    INPUT  cPayLoadID,
    OUTPUT riAPIInboundEvent
    ).



/* Return time */                            
FUNCTION GetDateTimeTZStringForResponse RETURNS CHARACTER():
    DEFINE VARIABLE cDateString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimString  AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDateString = ENTRY(1,STRING(NOW)," ")
        cTimString  = ENTRY(2,STRING(NOW)," ")
        .
           
    RETURN ENTRY(3,cDateString,"/") + "-" + 
           ENTRY(2,cDateString,"/") + "-" + 
           ENTRY(1,cDateString,"/") + "T" + 
           cTimString  . 
END.

                                                      
