/* select_ONote.i */

  DEFINE VARIABLE cHeaderValue AS CHARACTER NO-UNDO.

  {methods/run_link.i "RECORD-SOURCE" "get-job-header" "(OUTPUT cHeaderValue)"}
  RUN windows/opnotes.w (rec_key_value,cHeaderValue,"",1).
