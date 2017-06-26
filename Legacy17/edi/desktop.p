/* desktop.p */
/* desktop.p */
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES 
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE nonPersistProgram edi/monitor.w
&SCOPED-DEFINE getCompanyProc CUSTOM/getcomp.p


{nosweat.i}
/* Original Code 
{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id LIKE NOSWEAT._user._userid NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

SESSION:SET-WAIT-STATE("GENERAL").

g_version = "2.1A-8.2A".

m_id = OS-GETENV("opsysid").
IF m_id = ? THEN m_id = "".


IF NOT SETUSERID(m_id,"","NOSWEAT") OR m_id EQ "" THEN
RUN nosweat/login.w.

IF USERID("NOSWEAT") EQ "" OR quit_login THEN DO:
  SESSION:SET-WAIT-STATE("").
  QUIT.
END.

FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK NO-ERROR.
IF NOT AVAILABLE users THEN DO:     
  SESSION:SET-WAIT-STATE("").
  MESSAGE "User Login Does Not Exist in Users File" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
  QUIT.
END.
g_track_usage = users.track_usage.

FOR EACH parmfile NO-LOCK:
  CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX ERROR.
  END.
END.

RUN chkdate.p.
IF CONNECTED("NOSWEAT") THEN DO:
  {methods/setdevid.i}
  RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
  RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
  RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,YES). 
  g_groups = "".
  
  FOR EACH usergrps NO-LOCK:
    IF CAN-DO(usergrps.users,USERID("NOSWEAT")) THEN
    g_groups = g_groups + usergrps.usergrps + ",".
  END.
  RUN custom/getcomp.p.
  RUN edi/monitor.w.
END.
ELSE DO:
  SESSION:SET-WAIT-STATE("").
  MESSAGE "CONNECT to NOSWEAT'S Database Failed" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 
SESSION:SET-WAIT-STATE("").
QUIT.
*/