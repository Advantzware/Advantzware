/* sharpsh.p  from nosweat.p for sharp shooter*/
PROPATH = "..\,.\" + PROPATH.
&SCOPED-DEFINE loginProcedure addon/nosweat/loginss.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE execProgram sssetups.    
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES
&SCOPED-DEFINE sharpshooterFlag YES
&SCOPED-DEFINE getCompanyProc sshoot/comp-loc.p

{nosweat.i}

/* Original Code */
/*
{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}
DEF NEW GLOBAL SHARED VAR g_lookup-var AS cha NO-UNDO.

DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-sharpshooter AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
g-sharpshooter = YES.
ASSIGN
  ldummy = SESSION:SET-WAIT-STATE("GENERAL")
  g_version = "2.1A-8.2A".

m_id = OS-GETENV("opsysid").
IF m_id = ? THEN
m_id = "".

IF NOT SETUSERID(m_id,"",ldbname(1)) THEN
RUN ./nosweat/loginss.w.

IF USERID(ldbname(1)) = "" OR quit_login THEN
DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  QUIT.
END.

FIND users WHERE users.user_id = USERID(ldbname(1)) NO-LOCK NO-ERROR.
IF NOT AVAILABLE users THEN
DO:     
  ldummy = SESSION:SET-WAIT-STATE("").
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
/*
/* ======= 
  Load program & lookup data 
  =========*/
IF userid(ldbname(1)) = "ASI" OR USERID(ldbname(1)) = "NOSWEAT" THEN RUN asiload.p.
*/
RUN chkdate.p.

IF CONNECTED(ldbname(1)) THEN
DO:
  {methods/setdevid.i}
  RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
  RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
  RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,yes).
  g_groups = "". /* YSK need to reset */
  FOR EACH usergrps NO-LOCK:
    IF CAN-DO(usergrps.users,USERID(ldbname(1))) THEN
    g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
  END.
  /*RUN Get_Procedure IN Persistent-Handle ("comp_loc.",OUTPUT run-proc,yes). */
  RUN sshoot/comp-loc.p.
  
  init_menu = yes.
  DO WHILE init_menu:
    init_menu = no.
    RUN Get_Procedure IN Persistent-Handle ("sssetups.",OUTPUT run-proc,yes). 
    
    
  END.
END.
ELSE
DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  MESSAGE "CONNECT to NOSWEAT'S Database Failed" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 

ldummy = SESSION:SET-WAIT-STATE("").
QUIT.
*/