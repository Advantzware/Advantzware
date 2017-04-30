/* select_ONote.i */

DEFINE VARIABLE cHeaderValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE hONote       AS HANDLE    NO-UNDO.

{methods/run_link.i "RECORD-SOURCE" "get-job-header" "(OUTPUT cHeaderValue)"}
IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
    RUN windows/opnotes.w PERSISTENT SET hONote ({1},{2},{3},{4}).
    RUN dispatch IN hONote ("initialize") .
END.
ELSE
RUN windows/opnotes.w ({1},{2},{3},{4}).
