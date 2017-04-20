/* touchscr.p */
PROPATH = "..\," + PROPATH.
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE nonPersistProgram touch/touchscr.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE createSingleUserPFs YES
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES
&SCOPED-DEFINE overrideUserID landrum
&SCOPED-DEFINE overrideUserPasswd landrum
&SCOPED-DEFINE getCompanyProc sshoot/comp-loc.p

{nosweat.i}


/* Original Code */
/*
{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
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

RUN touch/touchscr2landrum.p NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO: 
   ldummy = SESSION:SET-WAIT-STATE("").
   QUIT.
END.

IF CONNECTED(ldbname(1)) THEN DO:
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
*/