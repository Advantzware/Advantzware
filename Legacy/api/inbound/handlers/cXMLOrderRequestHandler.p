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

RUN cXML/gencXMLOrder.p (
    iplcRequestData,
    NO, /* Add records to temp-table only */
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

ASSIGN
    oplcResponseData  = '~{"response_code": 200,"response_message":"' + opcMessage + '"}'. 
    .





                                                      
