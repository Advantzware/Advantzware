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
DEFINE INPUT  PARAMETER ipcUserName               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cPayLoadID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponse        AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cResponseCode     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderID          AS CHARACTER NO-UNDO.

/* Currenly cCompany and cWarehouseID are not assigned.Once session 
   manager related work is completed then cCompany and cWarehouseID 
   will be assigned */
DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouseID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInternalException AS CHARACTER NO-UNDO.
DEFINE VARIABLE cText              AS CHARACTER NO-UNDO.

FUNCTION GetDateTimeTZStringForResponse RETURNS CHARACTER() FORWARD.

RUN cXML/gencXMLOrder.p (
    INPUT  iplcRequestData,
    INPUT  NO, /* Add records to temp-table only */
    INPUT  cCompany, 
    INPUT  cWarehouseID,
    OUTPUT cPayLoadID,
    OUTPUT cOrderID,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT cInternalException
    ) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    ASSIGN
        cResponseCode      = "500"
        cInternalException = ERROR-STATUS:GET-MESSAGE(1)
        opcMessage         = "Internal Server Error at AppServer (#10)"
        oplcResponseData   = '~{"response_code": ' +  cResponseCode + ',"response_message":"' + opcMessage + '"}'
        opcMessage         = cInternalException
        .
ELSE
    /* Prepares response data */
    ASSIGN
        cResponseCode    = "200"
        cText            = IF cOrderID NE "" THEN
                               "OK : PO # " + cOrderID
                           ELSE
                               "OK"  
        lcResponse       = '~<Status code="' + cResponseCode + '" text="'+ cText + '"/>'
        oplcResponseData = iplcResponseDataStructure
        oplcResponseData = REPLACE(oplcResponseData,"$payloadID$",cPayLoadID)
        oplcResponseData = REPLACE(oplcResponseData,"$response$",lcResponse)
        oplcResponseData = REPLACE(oplcResponseData,"$timestamp$",GetDateTimeTZStringForResponse())
        oplcResponseData = REPLACE(oplcResponseData,'"','\"')
        oplcResponseData = '~{"response_code": ' +  cResponseCode + ',"response_message":"' + oplcResponseData + '"}'
        opcMessage       = IF cInternalException NE "" THEN
                               cInternalException
                           ELSE
                               opcMessage
        . 

/* Return time */                            
FUNCTION GetDateTimeTZStringForResponse RETURNS CHARACTER():
    DEFINE VARIABLE cDateString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimString  AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDateString = ENTRY(1,STRING(NOW)," ")
        cTimString  = ENTRY(2,STRING(NOW)," ")
        .
           
    RETURN ENTRY(3,cDateString,"/") + "-" + 
           ENTRY(1,cDateString,"/") + "-" + 
           ENTRY(2,cDateString,"/") + "T" + 
           cTimString  . 
END.

                                                      
