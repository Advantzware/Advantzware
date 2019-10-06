/* nosweat.p use addmain.w instead mainmenu.w for help.
             addmain.w and mainmenu.w are exactly same*/
    &SCOPED-DEFINE loginProcedure nosweat/login.w
    &SCOPED-DEFINE checkUserRecord YES
    &SCOPED-DEFINE connectDatabases YES
    &SCOPED-DEFINE runAsiLoad YES
    &SCOPED-DEFINE createSingleUserPFs YES
    &SCOPED-DEFINE execProgram addmain.    
    &SCOPED-DEFINE checkExpiredLicense YES
    &GLOBAL-DEFINE checkUserCount YES
    &SCOPED-DEFINE addonPersist YES
IF INDEX(PROPATH,'viper') = 0 THEN
DO:
    IF OS-GETENV('VIPER') NE ? THEN
        PROPATH = PROPATH + ',' + OS-GETENV('VIPER').
    IF OS-GETENV('VIPERVFL') NE ? THEN
        PROPATH = PROPATH + ',' + OS-GETENV('VIPERVFL').
END.
{nosweat.i}

/* Original Code */
/*
{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED        VARIABLE quit_login      AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE m_id            AS CHAR NO-UNDO.
DEFINE                   VARIABLE ldummy          AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE i               AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG       NO-UNDO.  /* no, it's yes only from sharpsh.p */
DEFINE                   VARIABLE cEulaFile       AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE cEulaVersion    AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE lEulaAccepted   AS LOGICAL   NO-UNDO.

g-sharpshooter = NO.

ASSIGN
    ldummy    = SESSION:SET-WAIT-STATE("GENERAL")
    g_version = "6.0-8.3B".

IF INDEX(PROPATH,'viper') = 0 THEN
DO:
    IF OS-GETENV('VIPER') NE ? THEN
        PROPATH = PROPATH + ',' + OS-GETENV('VIPER').
    IF OS-GETENV('VIPERVFL') NE ? THEN
        PROPATH = PROPATH + ',' + OS-GETENV('VIPERVFL').
END.

m_id = OS-GETENV("OPSYSID").
IF m_id = ? THEN
    m_id = "".

IF NOT SETUSERID(m_id,"",ldbname(1)) THEN
    RUN nosweat/login.w.

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

/* ======= 
  Load program & lookup data 
  =========*/
IF USERID(ldbname(1)) = "ASI" OR USERID(ldbname(1)) = "NOSWEAT" THEN RUN asiload.p.

cEulaFile = SEARCH("Eula.txt").
IF CONNECTED(ldbname(1)) THEN
DO: 
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

    RUN system/checkExpiredLicense.p.
    RUN createSingleUserPFs.
    {methods/setdevid.i}
    RUN addon/nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
    RUN Get_Procedure IN Persistent-Handle ("user_dir.",OUTPUT run-proc,YES).
    g_groups = "".
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID(ldbname(1))) THEN
            g_groups = g_groups + usergrps.usergrps + ",".
    END.
    init_menu = YES.
    DO WHILE init_menu:
        init_menu = NO.
        RUN Get_Procedure IN Persistent-Handle ("addmain.",OUTPUT run-proc,YES).
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