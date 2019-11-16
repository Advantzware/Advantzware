/* smartrun.i */

DO:
  RUN Running_Procedures 
  &IF "{&IAMPERSIST}" NE "yes" &THEN
  IN PERSISTENT-HANDLE
  &ENDIF
  (run-proc,OUTPUT is-running).
  IF NOT is-running THEN
  DO: /* took persistent out, per joe 6.3.2005 (res), put back 7.1.2005 */
    phandle = ?.
    RUN VALUE(run-proc) PERSISTENT SET phandle {1} NO-ERROR. /* YSK to stop if there's error*/
    IF ERROR-STATUS:ERROR THEN RETURN.

    IF VALID-HANDLE(phandle) THEN DO:  /* YSK not to get error 3137,3135,3140 when close window */
       IF phandle:PRIVATE-DATA = "persist." OR
          INDEX(PROGRAM-NAME(1),"applhelp") NE 0 THEN phandle:PRIVATE-DATA = "mainmenu.".       
       RUN dispatch IN phandle ('initialize':U) NO-ERROR.
       RUN Set-Focus IN phandle NO-ERROR.
    END.
  END.
END.
