/* touchscr.p */
/* WFK - 15626 */
/* PROPATH = "..\," + PROPATH. */
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE nonPersistProgram touch/touchscr.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES
&SCOPED-DEFINE getCompanyProc sshoot/comp-loc.p
&SCOPED-DEFINE appName touchscreen
/* {nosweat.i} */

/* Original Code */

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
DEFINE VARIABLE hSession AS HANDLE NO-UNDO.

ldummy = SESSION:SET-WAIT-STATE("GENERAL").

FOR EACH parmfile NO-LOCK:
  CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX ERROR.
  END.
END.

if connected(ldbname(1))
and ldbname(1) = "ASI" then do:
    create alias nosweat for database value(ldbname(1)).
    create alias emptrack for database value(ldbname(1)).
    create alias jobs for database value(ldbname(1)).
    create alias rfq for database value(ldbname(1)).
    create alias asihelp for database value(ldbname(1)).
    create alias asihlp for database value(ldbname(1)).
    create alias asinos for database value(ldbname(1)).
end.

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
  IF NOT SETUSERID(m_id,"",ldbname(1)) THEN RUN nosweat/login.w.

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
  ASSIGN
  g_company = ""
  g_loc = "".
  FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND usercomp.company_default 
       NO-LOCK NO-ERROR.
  IF AVAIL usercomp THEN do:
    g_company = usercomp.company.
    FIND FIRST ASI.usercomp WHERE usercomp.user_id = USERID(ldbname(1)) AND
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
IF CONNECTED(ldbname(1)) THEN DO:
    /* WFK - 15626 - Made path relative */
    RUN addon/nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN system/session.p  PERSISTENT SET hSession.
    SESSION:ADD-SUPER-PROCEDURE (hSession).
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

