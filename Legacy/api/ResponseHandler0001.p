/*------------------------------------------------------------------------
    File        : api/premier/ResponseHandler.p
    Purpose     : Returns the response

    Syntax      :

    Description : Returns the response

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 13 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hdttJSON   AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
       
DEFINE VARIABLE hdttBuffer AS HANDLE   NO-UNDO.
DEFINE VARIABLE hdttQuery  AS HANDLE   NO-UNDO.
DEFINE VARIABLE iFldCount  AS INTEGER  NO-UNDO.

CREATE QUERY hdttQuery.
hdttBuffer = hdttJSON:DEFAULT-BUFFER-HANDLE.
hdttQuery:SET-BUFFERS(hdttBuffer).
hdttQuery:QUERY-PREPARE("FOR EACH NewTable").
hdttQuery:QUERY-OPEN.
hdttQuery:GET-FIRST.

IF hdttBuffer:AVAILABLE THEN DO:
    DO iFldCount = 1 TO hdttBuffer:NUM-FIELDS:
        IF hdttBuffer:BUFFER-FIELD(iFldCount):DATA-TYPE EQ "DECIMAL" THEN
            oplSuccess = (hdttBuffer:BUFFER-FIELD(iFldCount):BUFFER-VALUE = 1). /* 1 = success, 0 = failure */    
        ELSE
            opcMessage = hdttBuffer:BUFFER-FIELD(iFldCount):BUFFER-VALUE.
    END.
END.        
ELSE
    ASSIGN
        oplSuccess = NO
        opcMessage = "Unrecognised Response Data - please verify in the APIOutboundEventsViewer"
        .

hdttQuery:QUERY-CLOSE.

IF VALID-HANDLE(hdttQuery) THEN
   DELETE OBJECT hdttQuery.
