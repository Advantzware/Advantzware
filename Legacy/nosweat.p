/* nosweat.p */

    &SCOPED-DEFINE loginProcedure nosweat/login.w
    &SCOPED-DEFINE checkUserRecord YES
    &SCOPED-DEFINE connectDatabases YES
    &SCOPED-DEFINE runAsiLoad YES
    &SCOPED-DEFINE createSingleUserPFs YES
    &SCOPED-DEFINE execProgram mainmenu.    
    &SCOPED-DEFINE checkExpiredLicense YES
    &GLOBAL-DEFINE checkUserCount YES
   
{nosweat.i}


/*
&IF DEFINED(execProgram) EQ 0 &THEN
&SCOPED-DEFINE execProgram mainmenu.
&ENDIF

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED        VARIABLE quit_login      AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE m_id            AS CHAR NO-UNDO.
DEFINE                   VARIABLE ldummy          AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE i               AS INTEGER   NO-UNDO.
DEFINE                   VARIABLE cEulaFile       AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE cEulaVersion    AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE lEulaAccepted   AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE lExit           AS LOGICAL   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG       NO-UNDO.  /* no, it's yes only from sharpsh.p */

g-sharpshooter = NO.

ASSIGN
    ldummy    = SESSION:SET-WAIT-STATE("GENERAL")
    g_version = "2.1A-8.2A".

m_id = OS-GETENV("opsysid").

IF m_id = ? THEN m_id = "".

IF NOT SETUSERID(m_id,"",ldbname(1)) OR m_id EQ "" THEN
    RUN nosweat/login.w.

IF USERID(ldbname(1)) = "" OR quit_login  THEN
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

    IF SEARCH (parmfile.parmfile) > "" THEN 
        CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
    ELSE 
        IF SEARCH (REPLACE (parmfile.parmfile, ".~\", "")) > "" THEN   
            CONNECT -pf VALUE(SEARCH(REPLACE (parmfile.parmfile, ".~\", ""))) NO-ERROR.
        ELSE 
        DO:
            MESSAGE "Cannot find .pf file: " parmfile.parmfile SKIP 
                REPLACE (parmfile.parmfile, ".~\", "")
                VIEW-AS ALERT-BOX ERROR .
          
            RETURN .     
        END.
  
    IF ERROR-STATUS:ERROR THEN
    DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
            VIEW-AS ALERT-BOX ERROR.
    END.
END.
/* ======= 
  Load program & lookup data 
  =========*/

IF USERID(ldbname(1)) = "ASI" OR USERID(ldbname(1)) = "NOSWEAT" THEN RUN asiload.p.

RUN chkdate.p.
cEulaFile = SEARCH("Eula.txt").
  
IF CONNECTED(ldbname(1)) THEN
DO:
    /*
    lEulaAccepted = NO.
    IF cEulaFile NE ? THEN 
    DO:
        RUN system/checkEula.p (INPUT cEulaFile, OUTPUT lEulaAccepted, OUTPUT cEulaVersion).
        IF NOT lEulaAccepted THEN 
            RUN windows/wUserEula.w (INPUT cEulaFile, OUTPUT lEulaAccepted).
    END. 
    ELSE 
        MESSAGE "User Agreement File Not Found! Exiting."
            VIEW-AS ALERT-BOX.
          
    IF NOT lEulaAccepted THEN 
        QUIT.
    */

            
    RUN createSingleUserPFs.
    {methods/setdevid.i}
    RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
    /*RUN system/userLogin.p (OUTPUT lExit).
    IF lExit THEN 
        QUIT. */
    RUN system/checkExpiredLicense.p.    
    RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,YES).
    g_groups = "". /* YSK need to reset */
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID(ldbname(1))) THEN
            g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
    END.
  
    init_menu = YES.
    DO WHILE init_menu:
        init_menu = NO.
        RUN Get_Procedure IN Persistent-Handle ("{&execProgram}",OUTPUT run-proc,YES).
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
        PUT UNFORMATTED 
            '-db ' PDBNAME(i) ' -1' SKIP.
        OUTPUT CLOSE.
    END. /* do i */
END PROCEDURE.
*/