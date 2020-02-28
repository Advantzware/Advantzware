/*------------------------------------------------------------------------
    File        : userLogIn.p
    Purpose     : 
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : Mon Apr 10 11:04:51 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcMode  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER oplExit AS LOGICAL       NO-UNDO.

DEFINE VARIABLE lAnswer         AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cEulaFile       AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lEulaAccepted   AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cEulaVersion    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcNk1Value      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE llRecFound      AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lEnforceUserCount   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPromptMultiSession AS LOGICAL NO-UNDO.
DEFINE VARIABLE iAllUserCount   AS INTEGER NO-UNDO.
DEFINE VARIABLE iThisUserCount  AS INTEGER NO-UNDO.
DEFINE VARIABLE cUserName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentUserID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponse       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lLogMeIn        AS LOG NO-UNDO.
DEFINE VARIABLE iLoginCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE ppid AS INTEGER NO-UNDO.
DEFINE VARIABLE lStrongDisconnect AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{system/sysconst.i}

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE GetCurrentProcessId EXTERNAL "kernel32.dll":
/*------------------------------------------------------------------------------
 Purpose: Gets the current process ID of this session on the user's machine
 Notes:   Required by logout when user chooses "Log out other sessions"
------------------------------------------------------------------------------*/
    DEFINE RETURN PARAMETER ppid AS LONG NO-UNDO.

END PROCEDURE.

ASSIGN
    cocode = gcompany
    locode = gloc
    igsSessionID = NEXT-VALUE(session_seq)
    cCurrentUserID = USERID(LDBNAME(1))
    oplExit = FALSE.

/* Find related sys-ctrl records */
    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.name EQ "enforceUserCount"
        NO-ERROR.
    IF AVAILABLE sys-ctrl THEN ASSIGN 
        lEnforceUserCount = sys-ctrl.log-fld 
        lStrongDisconnect = sys-ctrl.int-fld EQ 1
        NO-ERROR.
    ELSE ASSIGN  
        lEnforceUserCount = TRUE
        lStrongDisconnect = FALSE.
    
    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.name EQ "promptMultiSession"
        NO-ERROR.
    IF AVAILABLE sys-ctrl THEN ASSIGN 
        lPromptMultiSession = sys-ctrl.log-fld NO-ERROR.
    ELSE ASSIGN 
        lPromptMultiSession = YES.

/* Find the userControl record.  If not avail, create it. */
    FIND FIRST userControl NO-LOCK NO-ERROR.
    IF NOT AVAILABLE userControl THEN DO:
        RUN util/createUserControl.p.
        FIND FIRST userControl NO-LOCK NO-ERROR.
        IF NOT AVAILABLE userControl THEN DO:
            MESSAGE 
                "User Control Record was not found.  Exiting application."
                VIEW-AS ALERT-BOX.
            ASSIGN 
                oplExit = TRUE.
            RETURN.
        END. 
    END.

/* Find this user in users table */
    FIND FIRST users NO-LOCK WHERE 
        users.user_id EQ cCurrentUserID 
        NO-ERROR.
    IF NOT AVAIL users THEN DO:
        MESSAGE 
            "Unable to locate this user in the users table." SKIP 
            "Aborting login..."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            oplExit = TRUE.
        RETURN.
    END.
    ELSE IF users.isLocked THEN DO:
        MESSAGE
            "Your Advantzware account has been locked." SKIP
            "Please contact a Systems Administrator."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            oplExit = TRUE.
        RETURN.
    END.
/*    NOT IN USE AT THIS TIME                                      */
/*    ELSE IF NOT users.isActive THEN DO:                          */
/*        MESSAGE                                                  */
/*            "Your Advantzware account has been inactivated." SKIP*/
/*            "Please contact a Systems Administrator."            */
/*            VIEW-AS ALERT-BOX ERROR.                             */
/*        ASSIGN                                                   */
/*            oplExit = TRUE.                                      */
/*        RETURN.                                                  */
/*    END.                                                         */
    ELSE ASSIGN  
        cUserName = users.user_name.

/* Verify user has "signed" the EULA agreement */
    cEulaFile = SEARCH("{&EulaFile}").
    IF cEulaFile = ""
        OR cEulaFile = ? THEN ASSIGN
            cEulaFile = SEARCH("eula.txt").
    
    RUN system/checkEula.p (INPUT cEulaFile, OUTPUT lEulaAccepted, OUTPUT cEulaVersion).
    
    IF NOT lEulaAccepted THEN 
        RUN windows/wUserEula.w (INPUT cEulaFile, OUTPUT lEulaAccepted).
    IF NOT lEulaAccepted THEN 
        oplExit = TRUE. 
  
/* Get DB connection info for this user */
    FIND FIRST asi._myconnection 
        NO-LOCK NO-ERROR.  
    FIND FIRST audit._myconnection 
        NO-LOCK NO-ERROR.  
    IF NOT AVAIL asi._myconnection 
    OR NOT AVAIL audit._myconnection THEN DO:
        MESSAGE 
            "Unable to locate database connection info." SKIP 
            "Cannot continue.  Please contact Advantzware" SKIP 
            "Support for assistance.  Exiting..."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            oplExit = TRUE.
        RETURN.
    END.
    FIND FIRST asi._connect NO-LOCK WHERE 
        asi._connect._connect-usr = asi._myconnection._myconn-userid
        NO-ERROR. 
    FIND FIRST audit._connect NO-LOCK WHERE 
        audit._connect._connect-usr = audit._myconnection._myconn-userid
        NO-ERROR. 
    
/* If user has other sessions open, prompt to add another or close them */
    iLoginCnt = 0.
    FOR EACH userLog NO-LOCK WHERE 
        userLog.userStatus EQ "Logged In" AND  
        userLog.user_id EQ cCurrentUserID:
        ASSIGN 
            iLoginCnt = iLoginCnt + 1.
    END.
    IF iLoginCnt GT 0 
    AND lPromptMultiSession THEN DO:
        /* user 'monitor' gets a free pass for multiple connections; everybody else counts */
        IF cCurrentUserID NE "monitor" THEN 
            RUN system/wSession.w (INPUT iLoginCnt, INPUT userControl.maxSessionsPerUser, OUTPUT cResponse).
        ELSE ASSIGN 
            cResponse = "".
        CASE cResponse:
            WHEN "" THEN ASSIGN     /* First time, or user wants multiple sessions */ 
                lLogMeIn = TRUE.
            WHEN "Exit Application" THEN DO:
                ASSIGN     /* User bails out */
                    lLogMeIn = FALSE 
                    oplExit = TRUE.
                RETURN.
            END.          
            WHEN "Log Out Other Sessions" THEN DO TRANSACTION:  /* User wants other sessions logged out */
                ASSIGN 
                    lLogMeIn = TRUE.
                FOR EACH userLog EXCLUSIVE-LOCK WHERE 
                    userLog.userStatus EQ "Logged In" AND  
                    userLog.user_id EQ cCurrentUserID AND 
                    userLog.sessionID NE igsSessionID:
                    /* Check for record locks */
                    IF CAN-FIND (FIRST asi._lock WHERE asi._lock._lock-usr EQ userlog.asiUsrNo)
                    /* Crappy auditing has a lock when main menu is up */
                    /* OR CAN-FIND (FIRST audit._lock WHERE audit._lock._lock-usr EQ userlog.audUsrNo) */
                    THEN DO:
                        MESSAGE 
                            "You have records locked in another session. Please" SKIP 
                            "resolve these prior to logging in again."
                            VIEW-AS ALERT-BOX ERROR.
                        ASSIGN 
                            lLogMeIn = FALSE 
                            oplExit = TRUE.
                        RETURN.    
                    END.      
                    RUN system/userLogout.p (YES, userLog.sessionID).
                    /* This is still experimental.  If NK1 "enforceUserCount" integer value is set to 1, this line will 
                    "kill" any open sessions on user's workstation.  Risk is if run on server with RDP, AND multiple users have same
                    user-id, will/may also kill other users' sessions.  Unknown without further testing, so do not recommend this be set
                    in the field */
                    IF lStrongDisconnect THEN 
                        OS-COMMAND SILENT VALUE("taskkill /f /pid " + STRING(userlog.processID)).         
                END.   
            END.
        END CASE.
    END.

/* If adding, test for user count equaling or exceeding max available users in NK5 */
    IF NOT oplExit 
    AND lEnforceUserCount THEN DO:
        iAllUserCount = 0.
        FOR EACH userLog NO-LOCK WHERE 
            userLog.userStatus EQ "Logged In" AND 
            userLog.user_id NE "Monitor" AND 
            userLog.user_id NE "ASI": 
            ASSIGN 
                iAllUserCount = iAllUserCount + 1.
        END.            
        /* ASI and Monitor can log in no matter what count is; otherwise exceeding the grace count prevents login */
        IF iAllUserCount + 1 GT userControl.maxAllowedUsers + userControl.numUsersOverLimit 
        AND cCurrentUserID NE "Monitor" 
        AND cCurrentUserID NE "ASI" THEN DO:            
            MESSAGE 
                "The maximum number of connections has been reached.  Exiting the application."
                VIEW-AS ALERT-BOX WARNING .
            ASSIGN             
                oplExit = TRUE.                                       
        END.        
        ELSE IF iAllUserCount + 1 GT userControl.maxAllowedUsers THEN DO:            
            MESSAGE 
                "The maximum number of connections has been reached." SKIP 
                "Please notify your administrator to add more licences."
                VIEW-AS ALERT-BOX WARNING .
        END.
    END. 

IF NOT oplExit THEN DO TRANSACTION:
    RUN GetCurrentProcessID (OUTPUT ppid).
    CREATE userLog.
    ASSIGN 
        userLog.user_id         = cCurrentUserID       
        userLog.sessionID       = igsSessionID
        userlog.mode            = ipcMode 
        userlog.processID       = ppid
        userLog.userName        = cUserName
        userLog.IpAddress       = OS-GETENV("userDomain")
        userLog.deviceName      = OS-GETENV("computerName")  + (IF AVAIL(asi._myconnection) THEN "-" + STRING(asi._myconnection._myconn-userid) ELSE "")
        userLog.EulaVersion     = DECIMAL(cEulaVersion)
        userLog.logoutDateTime  = ?
        userLog.userStatus      = "Logged In"
        userLog.loginDateTime   = DATETIME(TODAY, MTIME)
        userLog.rec_key         = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99") + 
                                  STRING(DAY(TODAY),"99") + 
                                  STRING(TIME,"99999") + 
                                  STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
        userLog.dbDisconnect    = FALSE
        userLog.userDisconnect  = FALSE 
        userLog.notifyLocked    = ?
        userLog.notifyAutoLogout = ?
        userLog.notifyDisable   = ?
        userLog.asiUsrNo        = asi._connect._Connect-Usr     /* Displays the user number. */
        userLog.asiPID          = asi._connect._Connect-Pid     /* Displays the process ID of the user session. */
        userLog.asiLoginDtTm    = DATETIME(TODAY, MTIME)
        userLog.asiUserID       = asi._connect._connect-name    /* Displays the user name used for the connection. */
        userLog.asiType         = asi._connect._Connect-type    /* SELF, REMC, BROK, SERV, or BTCH */
        userLog.asiTTY          = asi._connect._Connect-Device  /* Device from which the user has connected to the database. */
        userLog.asiLimbo        = ?
        userLog.audUsrNo        = audit._connect._Connect-Usr     /* Displays the user number. */
        userLog.audPID          = audit._connect._Connect-Pid     /* Displays the process ID of the user session. */
        userLog.audLoginDtTm    = DATETIME(TODAY, MTIME)
        userLog.audUserID       = audit._connect._connect-name    /* Displays the user name used for the connection. */
        userLog.audType         = audit._connect._Connect-type    /* SELF, REMC, BROK, SERV, or BTCH */
        userLog.audTTY          = audit._connect._Connect-Device  /* Device from which the user has connected to the database. */
        userLog.audLimbo        = ?
        .
    FIND CURRENT userLog NO-LOCK NO-ERROR.
END.  /* If not exiting */
