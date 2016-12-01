/* nosweat.p */

&IF DEFINED(execProgram) EQ 0 &THEN
&SCOPED-DEFINE execProgram mainmenu.
&ENDIF

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id LIKE ASI._user._userid NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-sharpshooter AS LOG NO-UNDO.  /* no, it's yes only from sharpsh.p */

g-sharpshooter = NO.

ASSIGN
  ldummy = SESSION:SET-WAIT-STATE("GENERAL")
  g_version = "2.1A-8.2A".

m_id = OS-GETENV("opsysid").

IF m_id = ? THEN m_id = "".

IF NOT SETUSERID(m_id,"","ASI") OR m_id EQ "" THEN
RUN nosweat/login-gil.w.


IF USERID("ASI") = "" OR quit_login  THEN
DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  QUIT.
END.

FIND users WHERE users.user_id = USERID("ASI") NO-LOCK NO-ERROR.
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
/* ======= 
  Load program & lookup data 
  =========*/

IF USERID("ASI") = "ASI" OR USERID("ASI") = "ASI" THEN RUN asiload.p.

RUN chkdate.p.
IF CONNECTED("ASI") THEN
DO:
  RUN createSingleUserPFs.
  {methods/setdevid.i}
  RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
  RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
  RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,yes).

  g_groups = "". /* YSK need to reset */
  FOR EACH usergrps NO-LOCK:
    IF CAN-DO(usergrps.users,USERID("ASI")) THEN
    g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
  END.
  
  init_menu = yes.
  DO WHILE init_menu:
    init_menu = no.
    RUN Get_Procedure IN Persistent-Handle ("{&execProgram}",OUTPUT run-proc,yes).
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

PROCEDURE createSingleUserPFs:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO NUM-DBS:
    OUTPUT TO VALUE(LC(LDBNAME(i)) + '-1.pf').
    PUT UNFORMATTED '-db ' PDBNAME(i) ' -1' SKIP.
    OUTPUT CLOSE.
  END. /* do i */
END PROCEDURE.
