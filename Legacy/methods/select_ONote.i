/* select_ONote.i */


DEFINE VARIABLE cHeaderValue AS CHARACTER NO-UNDO.

{methods/run_link.i "RECORD-SOURCE" "get-job-header" "(OUTPUT cHeaderValue)"}
RUN windows/opnotes.w ({1},{2},{3},{4}).
