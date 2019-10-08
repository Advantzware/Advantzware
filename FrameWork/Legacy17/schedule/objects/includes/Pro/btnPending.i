/* btnPending.i - used in trigger for btnPending in board.w */

  IF NOT VALID-HANDLE(pendingHandle) THEN
  DO:
    pending = findProgram('{&objects}','','/pending.w').
    RUN VALUE(pending) PERSISTENT SET pendingHandle.
    RUN adm-initialize IN pendingHandle.
    RUN setPopup IN containerHandle (6,pendingHandle).
    /*RUN passHandle IN pendingHandle (THIS-PROCEDURE,'{&Board}').*/
  END.
  RUN passHandle IN pendingHandle (THIS-PROCEDURE,'{&Board}').
