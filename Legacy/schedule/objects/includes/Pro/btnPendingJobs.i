/* btnPendingJobs.i - used in trigger for btnPendingJobs in board.w */

  IF NOT VALID-HANDLE(pendingJobsHandle) THEN
  DO:
    pending = findProgram('{&objects}','','/pendingJobs.w').
    RUN VALUE(pending) PERSISTENT SET pendingJobsHandle.
    RUN adm-initialize IN pendingJobsHandle.
    RUN setPopup IN containerHandle (10,pendingJobsHandle).
    /*RUN passHandle IN pendingJobsHandle (THIS-PROCEDURE,'{&Board}').*/
  END.
  RUN passHandle IN pendingJobsHandle (THIS-PROCEDURE,'{&Board}').
