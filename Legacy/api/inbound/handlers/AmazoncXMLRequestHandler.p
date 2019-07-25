/*------------------------------------------------------------------------
    File        : api/inbound/handlers/AmazoncXMLRequestHandler.p
    Purpose     : This accepts Amazon cXML

    Syntax      :

    Description : This accepts Amazon cXML

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

            
/* Put Request Data from a variable into a Temporary file */
COPY-LOB iplcRequestData TO FILE "C:\tmp\cXML.xml".

/* More code to be added for processing the cXML */

ASSIGN
    opcMessage = "Success"
    oplcResponseData  = '~{"response_code": 200,"response_message":"' + opcMessage + '"}'. 
    .





                                                      
