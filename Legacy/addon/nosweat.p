/* nosweat.p use addmain.w instead mainmenu.w for help.
             addmain.w and mainmenu.w are exactly same*/

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED        VARIABLE quit_login     AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE m_id           LIKE NOSWEAT._user._userid NO-UNDO.
DEFINE                   VARIABLE ldummy         AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE i              AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter AS LOG       NO-UNDO.  /* no, it's yes only from sharpsh.p */
DEFINE                   VARIABLE cEulaFile      AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE cEulaVersion   AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE lEulaAccepted  AS LOGICAL   NO-UNDO.

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

IF NOT SETUSERID(m_id,"","NOSWEAT") THEN
    RUN nosweat/login.w.

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
IF USERID("NOSWEAT") = "ASI" OR USERID("NOSWEAT") = "NOSWEAT" THEN RUN asiload.p.
cEulaFile = SEARCH("Eula.txt").
IF CONNECTED("NOSWEAT") THEN
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
    DEFINE VARIABLE iExpiredDays    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lModulesExpired AS LOG     NO-UNDO.
    FOR EACH module NO-LOCK:
        IF module.expire-date LE TODAY + 7 THEN
            ASSIGN lModulesExpired = TRUE
                iExpiredDays    = module.expire-date - TODAY.
    END.
    
    IF lModulesExpired EQ TRUE THEN 
    DO:
        MESSAGE "Warning: One or more of your Advantzware licenses will expire in " iExpiredDays
            VIEW-AS ALERT-BOX.
        
        DEFINE VARIABLE cNewLicense AS CHARACTER NO-UNDO.
        IF USERID("nosweat") EQ 'nosweat' THEN 
        DO:
            MESSAGE "Do you have a new license code to enter?"
                VIEW-AS ALERT-BOX QUESTION UPDATE lNewLicense AS LOG.
        END.
        IF lNewLicense THEN 
        DO:
            UPDATE cNewLicense.
            DEFINE VARIABLE iCnt AS INTEGER.
            DO iCnt = 1 TO LENGTH(cNewLicense):
                SUBSTRING(cNewLicense, i, 1) = STRING(9 - INTEGER(SUBSTRING(cNewlicense, i, 1))).
            END.
        END.
    
        FIND FIRST sys-ctrl WHERE sys-ctrl.name = "site number" NO-LOCK NO-ERROR.
        IF AVAILABLE sys-ctrl AND lNewLicense THEN 
        DO:
            IF INTEGER(SUBSTRING(cNewLicense, 1, 5)) EQ 
                sys-ctrl.int-fld THEN 
                FOR EACH module:
                    module.expire-date = TODAY + 365.
                END.
        END.        
    END.


    RUN createSingleUserPFs.
    {methods/setdevid.i}
    RUN addon/nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
    RUN Get_Procedure IN Persistent-Handle ("user_dir.",OUTPUT run-proc,YES).
    g_groups = "".
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID("NOSWEAT")) THEN
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
