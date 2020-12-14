/* smartrun.i */

DO:
  RUN Running_Procedures 
  &IF "{&IAMPERSIST}" NE "yes" &THEN
  IN PERSISTENT-HANDLE
  &ENDIF
  (run-proc,OUTPUT is-running).
  IF NOT is-running THEN DO:
    phandle = ?.
    RUN VALUE(run-proc) PERSISTENT SET phandle {1} NO-ERROR. /* YSK to stop if there's error*/
    IF ERROR-STATUS:ERROR THEN RETURN.
    IF VALID-HANDLE(phandle) THEN DO:  /* YSK not to get error 3137,3135,3140 when close window */
       IF phandle:PRIVATE-DATA = "persist." OR
          INDEX(PROGRAM-NAME(1),"applhelp") NE 0 THEN phandle:PRIVATE-DATA = "mainmenu.".       
       /* Set the option frame size and colour to give blue background to icons and 
          add the handle of scope define object to temptable for resizizng */
       RUN beforeinitialize IN phandle NO-ERROR. 
       RUN dispatch IN phandle ('initialize':U) NO-ERROR.
       
       /* Add the handle of all smart object to be resized/shifted on resize to the temptable and 
          Shift all the icons towards right */
       RUN afterinitialize IN phandle NO-ERROR.
       RUN Set-Focus IN phandle NO-ERROR.
    END. /* if valid-handle */
  END. /* if not running */
END. /* do: */
