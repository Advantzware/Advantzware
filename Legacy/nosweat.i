/* nosweat.i */

/* Preprocessor options:   
    &SCOPED-DEFINE loginProcedure login.w
    &SCOPED-DEFINE checkUserRecord YES 
    &SCOPED-DEFINE connectDatabases YES
    &SCOPED-DEFINE runAsiLoad YES
    &SCOPED-DEFINE createSingleUserPFs YES
    &SCOPED-DEFINE execProgram mainmenu.
    &SCOPED-DEFINE nontProgram oerep/r-loadtg.w
    &SCOPED-DEFINE checkExpiredLicense YES
    &SCOPED-DEFINE checkUserCount YES
    &SCOPED-DEFINE overrideUserID ASI /* See addon/touch/touchscr2landrum */
    &SCOPED-DEFINE overrideUserPasswd passwd
    &SCOPED-DEFINE overrideCompany 10
    &SCOPED-DEFINE overrideLoc MAIN
    &SCOPED-DEFINE getCompanyProc CUSTOM/getcomp.p
    &SCOPED-DEFINE sharpshooterFlag YES
    &SCOPED-DEFINE checkBlankCompany YES
    &SCOPED-DEFINE addonPersist YES
    &SCOPED-DEFINE appName(touchscreen, advantzware, sharpshooter)
        
    Note: gettime.p used in addon/touchscr.p
*/

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}
{ed/sharedv.i "NEW GLOBAL"}

DEFINE NEW SHARED        VARIABLE quit_login      AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE m_id            AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE ldummy          AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE i               AS INTEGER   NO-UNDO.
DEFINE                   VARIABLE lExit           AS LOGICAL   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG       NO-UNDO.  /* no, it's yes only from sharpsh.p */
DEFINE                   VARIABLE tslogin-log     AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE lFound          AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE cTsLogin        AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cIniLoc         AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cUsrLoc         AS CHARACTER NO-UNDO.

&IF "{&sharpshooterFlag}" EQ "YES"  &THEN
  g-sharpshooter = YES.
&ELSE
  g-sharpshooter = NO.
&ENDIF

ASSIGN
    ldummy    = SESSION:SET-WAIT-STATE("GENERAL")
    g_version = "16.7-11.6"
    .
/* Need databases connect to find nk1 values */
&IF "{&connectDatabases}" EQ "YES"  &THEN
RUN connectDatabases.
&ENDIF

IF CONNECTED(LDBNAME(1))
AND ldbname(1) = "ASI" THEN DO:
    CREATE ALIAS nosweat FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS jobs FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS rfq FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asihelp FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asihlp FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asinos FOR DATABASE VALUE(LDBNAME(1)).
END.

/* Need to obtain the nk1 value without a company # */
IF "{&appName}" EQ "Touchscreen" THEN
  RUN sys/inc/tslogin.p (OUTPUT tslogin-log).

/* If a login procedure is specified */
&IF DEFINED(loginProcedure) NE 0  &THEN
RUN setUserProc.
&ENDIF

/* If a particular user is specified */
&IF DEFINED(overrideUserID) NE 0  &THEN
RUN setDefaultUser.
&ENDIF

/* Must have a user id at this point */
IF USERID(LDBNAME(1)) = "" OR quit_login  THEN
DO:
    ldummy = SESSION:SET-WAIT-STATE("").
    QUIT.
END.

&IF "{&checkUserRecord}" EQ "YES"  &THEN
RUN userRecordCheck.
&ENDIF


&IF "{&runAsiLoad}" EQ "YES"  &THEN
RUN asiload.p.
&ENDIF

RUN chkdate.p.
  
IF CONNECTED(LDBNAME(1)) THEN DO:
    &IF "{&createSingleUserPFs}" EQ "YES" &THEN          
      /* Decided this was not needed */
      /* RUN createSingleUserPFs. */
    &ENDIF

    {methods/setdevid.i}

    &IF DEFINED(getCompanyProc) NE 0  &THEN
    RUN getCompanyProc.
    &ENDIF

    &IF "{&addonPersist}" EQ "YES"  &THEN
      RUN addon/nosweat/persist.p PERSISTENT SET Persistent-Handle.
    &ELSE
      RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
    &ENDIF
    
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
    
    &IF "{&checkUserCount}" EQ "YES"  &THEN

    /* IF NOT (LDBNAME(1) EQ "ASI" OR LDBNAME(2) EQ "ASI" OR LDBNAME(3) EQ "ASI") THEN  */
    DO TRANSACTION:            
        /* Check EULA and number of sessions here using combined db or in mainmenu if ASI is physically connected */
        RUN system/userLogin.p (OUTPUT lExit).
        IF lExit THEN 
            QUIT. 
    END.
    &ENDIF
    
    &IF "{&checkExpiredLicense}" EQ "YES"  &THEN
    RUN system/checkExpiredLicense.p.    
    &ENDIF
    
    /* This should be discussed...if it should always be run */
    RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,YES).
    
    RUN setUserGroups.
    
    &IF DEFINED(overrideCompany) NE 0  &THEN
    RUN setCompanyOverride (INPUT "{&overrideCompany}").
    &ENDIF
    
    &IF DEFINED(overrideLoc) NE 0  &THEN
    RUN setLocOverride (INPUT "{&overrideLoc}").    
    &ENDIF
    
    &IF "{&checkBlankCompany}" EQ "YES"  &THEN
    RUN checkBlankCompany.
    IF ERROR-STATUS:ERROR THEN DO: 
        ldummy = SESSION:SET-WAIT-STATE("").
        QUIT.
    END.
    &ENDIF
    
    &IF DEFINED(execProgram) NE 0  &THEN
    RUN startPersistentMenu.
    &ENDIF
    
    &IF DEFINED(nonPersistProgram) NE 0  &THEN
    RUN startNonPersistentMenu.
    &ENDIF
END.
ELSE DO:
    ldummy = SESSION:SET-WAIT-STATE("").
    MESSAGE "CONNECT to NOSWEAT'S Database Failed" SKIP(1)
        "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 
    
DO TRANSACTION:
  RUN system/userLogOut.p.
END.
SESSION:SET-WAIT-STATE("").
QUIT.
/* End Main Block */

PROCEDURE checkBlankCompany:
    IF g_company EQ "" THEN DO:
        MESSAGE
            "No Default Company or Location Exists. Please contact system administrator for help."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.    
END PROCEDURE.

PROCEDURE connectDatabases:
    FOR EACH parmfile NO-LOCK:
        IF SEARCH (parmfile.parmfile) > "" THEN 
            CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
        ELSE 
            IF SEARCH (REPLACE (parmfile.parmfile, ".~\", "")) > "" THEN   
                CONNECT -pf VALUE(SEARCH(REPLACE (parmfile.parmfile, ".~\", ""))) NO-ERROR.
            ELSE DO:
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
    IF CONNECTED("asihelp") AND NOT CONNECTED("asihlp") THEN CREATE ALIAS asihlp FOR DATABASE asihelp.
END PROCEDURE.

PROCEDURE createSingleUserPFs:
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    DO i = 1 TO NUM-DBS:
        OUTPUT TO VALUE(LC(LDBNAME(i)) + '-1.pf').
        PUT UNFORMATTED 
            '-db ' PDBNAME(i) ' -1' SKIP.
        OUTPUT CLOSE.
    END. /* do i */
END PROCEDURE.

PROCEDURE getCompanyProc:
    /* Replaces RUN CUSTOM/getcomp.p */
    RUN VALUE("{&getCompanyProc}").    
END PROCEDURE.

PROCEDURE setUserGroups:
    g_groups = "". /* YSK need to reset */
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID(LDBNAME(1))) THEN
            g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
    END.
END PROCEDURE.

PROCEDURE setCompanyOverride:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    /* Replaces g_company = "012" */
    g_company = ipcCompany.
END PROCEDURE.

PROCEDURE setDefaultUser:
    /* {&defaultUser} */
    IF SETUSERID("{&overrideUserID}","{&overrideUserPasswd}",LDBNAME(1)) THEN.
END PROCEDURE. 

PROCEDURE setLocOverride:
  DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
  /* Replaces g_loc     = "MAIN". */
  g_loc = ipcLoc.
END PROCEDURE.
    
PROCEDURE setUserProc:
    /* Touchscreen may not prompt so check for tslogin for touch programs */
    IF "{&appName}" NE "touchscreen" OR
      ("{&appName}" EQ "touchscreen" AND tslogin-log ) THEN DO:
        m_id = OS-GETENV("opsysid").

        IF m_id = ? THEN m_id = "".

        IF NOT SETUSERID(m_id,"",LDBNAME(1)) OR m_id EQ "" THEN
            RUN VALUE("{&loginProcedure}").
    END.              
END PROCEDURE.

PROCEDURE startNonPersistentMenu:
      IF "{&appName}" EQ "touchscreen" THEN
        RUN custom/gettime.p.
      RUN VALUE("{&nonPersistProgram}").
END PROCEDURE.

PROCEDURE startPersistentMenu:
    init_menu = YES.
    DO WHILE init_menu:
        init_menu = NO.
        RUN Get_Procedure IN Persistent-Handle ("{&execProgram}",OUTPUT run-proc,YES).
    END.
END PROCEDURE.

PROCEDURE userRecordCheck:
    FIND users WHERE users.user_id = USERID(LDBNAME(1)) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE users THEN 
        FIND users WHERE users.user_id = USERID(LDBNAME(2)) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE users THEN DO:     
        ldummy = SESSION:SET-WAIT-STATE("").
        MESSAGE "User Login Does Not Exist in Users File" SKIP(1)
            "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
        QUIT.
    END.
    g_track_usage = users.track_usage.
END PROCEDURE.
