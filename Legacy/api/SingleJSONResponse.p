DEFINE INPUT PARAMETER httJSON   AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER httBuffer AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER httQuery  AS HANDLE    NO-UNDO.
       
CREATE QUERY httQuery.
httBuffer = httJSON:DEFAULT-BUFFER-HANDLE.
httQuery:SET-BUFFERS(httBuffer).
httQuery:QUERY-PREPARE("FOR EACH NewTable").
httQuery:QUERY-OPEN.
httQuery:GET-FIRST.
IF httBuffer:AVAILABLE THEN
    ASSIGN
        oplSuccess = (httBuffer:BUFFER-FIELD(1):BUFFER-VALUE = 1) /* 1 = success, 0 = failure */
        opcMessage = httBuffer:BUFFER-FIELD(2):BUFFER-VALUE
        .
ELSE
      ASSIGN
             oplSuccess = NO
             opcMessage = "Unrecognised Response Data - please verify in the APIOutboundEventsViewer"
             .
END.

httQuery:QUERY-CLOSE.

IF VALID-HANDLE(httQuery) THEN
   DELETE OBJECT httQuery.
