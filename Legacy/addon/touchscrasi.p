/* touchscrasi.p */

propath = "p:\asi10test\patch\rco1010,p:\asi10test\patch\rco1010\addon,p:\asi10test\rco1010,p:\asi10test\rco1010\addon," + PROPATH.
{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id LIKE NOSWEAT._user._userid NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-sharpshooter AS LOG NO-UNDO.  /* no, it's yes only from sharpsh.p */

ldummy = SESSION:SET-WAIT-STATE("GENERAL").

FOR EACH parmfile NO-LOCK:
  CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX ERROR.
  END.
END.

RUN touch/touchscr2.p NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO: 
   ldummy = SESSION:SET-WAIT-STATE("").
   QUIT.
END.

/* moved to touch/touchscr2.p 
{sys/inc/tslogin.i}
IF tslogin-log THEN DO:
  m_id = OS-GETENV("OPSYSID").
  IF m_id = ? THEN m_id = "".
  IF NOT SETUSERID(m_id,"","NOSWEAT") THEN RUN nosweat/login.w.

  IF USERID("NOSWEAT") = "" OR quit_login THEN
  DO:
    ldummy = SESSION:SET-WAIT-STATE("").
    QUIT.
  END.

  FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK NO-ERROR.
  IF NOT AVAILABLE users THEN
  DO:     
    ldummy = SESSION:SET-WAIT-STATE("").
    MESSAGE "User Login Does Not Exist in Users File" SKIP(1)
      "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
    QUIT.
  END.
  ASSIGN
  g_company = ""
  g_loc = "".
  FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND usercomp.company_default 
       NO-LOCK NO-ERROR.
  IF AVAIL usercomp THEN do:
    g_company = usercomp.company.
    FIND FIRST ASI.usercomp WHERE usercomp.user_id = USERID("NOSWEAT") AND
            usercomp.company = g_company AND
            usercomp.loc NE "" AND usercomp.loc_default = yes
            NO-LOCK NO-ERROR.
    IF AVAIL usercomp THEN g_loc = usercomp.loc.
  END.
  IF g_company = "" THEN DO:
      MESSAGE
      "No Default Company or Location Exists. Please contact system administrator for help."
        VIEW-AS ALERT-BOX ERROR.
      QUIT.
  END.
  g_track_usage = users.track_usage.
END.  /* prompt login */

RUN custom/gettime.p.   /* time-source */
*/

IF CONNECTED("NOSWEAT") THEN DO:
    RUN ./nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN touch/touchscr.w.
END.
ELSE
DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  MESSAGE "CONNECT to NOSWEAT'S Database Failed" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 

ldummy = SESSION:SET-WAIT-STATE("").
QUIT.
