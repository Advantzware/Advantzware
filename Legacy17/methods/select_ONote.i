/* select_ONote.i */

  DEFINE VARIABLE cHeaderValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hONote       AS HANDLE    NO-UNDO.

  {methods/run_link.i "record-SOURCE" "get-job-header" "(output cHeaderValue)"}
  IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
      RUN windows/opnotes.w PERSISTENT SET hONote (rec_key_value,cHeaderValue,"",1).
      RUN dispatch IN hONote ("initialize") .
  END.
  ELSE
  RUN windows/opnotes.w (rec_key_value,cHeaderValue,"",1).
