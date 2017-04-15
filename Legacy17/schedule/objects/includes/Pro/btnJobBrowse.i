/* btnJobBrowse.i - trigger for btnJobBrowse in board.w */

  IF NOT VALID-HANDLE(jobBrowseHandle) THEN
  DO:
    jobBrowse = findProgram('{&objects}','','/jobBrowse.w').
    RUN VALUE(jobBrowse) PERSISTENT SET jobBrowseHandle.
    RUN adm-initialize IN jobBrowseHandle.
  END.
  RUN setPopup IN containerHandle (2,jobBrowseHandle).
  RUN passHandle IN jobBrowseHandle (THIS-PROCEDURE,'{&Board}',{1}).
