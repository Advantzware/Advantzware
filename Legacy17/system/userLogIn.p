
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
DEFINE OUTPUT PARAMETER oplExit AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lAnswer         AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cEulaFile       AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lEulaAccepted   AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cEulaVersion    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcNk1Value      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE llRecFound      AS LOGICAL       NO-UNDO.
DEFINE VARIABLE enforceUserCount-log  AS LOGICAL NO-UNDO.
DEFINE VARIABLE promptMultiSession-log AS LOGICAL NO-UNDO.
DEFINE VARIABLE iAllUserCount   AS INTEGER NO-UNDO.
DEFINE VARIABLE iThisUserCount  AS INTEGER NO-UNDO.
DEFINE VARIABLE cUserName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMaxSessionsPerUser AS INTEGER NO-UNDO.
DEFINE VARIABLE cCurrentUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponse AS CHARACTER NO-UNDO.
{methods/defines/hndldefs.i}
{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc
    igsSessionID = NEXT-VALUE(session_seq)
    cCurrentUserID = USERID(LDBNAME(1)).

/* System Constant Values  contains user eula file*/
{system/sysconst.i}

/* Note: cocode may not be available at this point */
FIND FIRST sys-ctrl NO-LOCK 
    WHERE /* sys-ctrl.company EQ cocode
    AND */ sys-ctrl.name    EQ "enforceUserCount"
    NO-ERROR.
IF AVAILABLE sys-ctrl THEN DO:
    enforceUserCount-log = sys-ctrl.log-fld NO-ERROR.
END. 
ELSE 
    enforceUserCount-log = TRUE.
FIND FIRST sys-ctrl NO-LOCK 
    WHERE /* sys-ctrl.company EQ cocode
    AND */ sys-ctrl.name    EQ "promptMultiSession"
    NO-ERROR.
IF AVAILABLE sys-ctrl THEN DO:
    promptMultiSession-log = sys-ctrl.log-fld NO-ERROR.
END. 
ELSE
    promptMultiSession-log = YES.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* CHECK IF USER has more than one login record */
/* IF so, ask IF this IS replacing OR adding */
/* You have n session currently open.  Are you replacing one or adding a new one?" */
DEFINE VARIABLE iLoginCnt AS INTEGER NO-UNDO.

FIND FIRST userControl NO-LOCK NO-ERROR.

IF NOT AVAILABLE userControl THEN DO:
    RUN util/createUserControl.p.
    FIND FIRST userControl NO-LOCK NO-ERROR.
    IF NOT AVAILABLE userControl THEN DO:
        MESSAGE "User Control Record was not found.  Exiting application."
        VIEW-AS ALERT-BOX.
        QUIT.
    END. 
END.
  
iMaxSessionsPerUser = userControl.maxSessionsPerUser.

FIND FIRST asi._myconnection NO-LOCK NO-ERROR.  

FIND FIRST asi._connect NO-LOCK WHERE _connect._connect-usr = _myconnection._myconn-userid
    NO-ERROR. 
        
cEulaFile = SEARCH("{&EulaFile}").
IF cEulaFile = ""
OR cEulaFile = ? THEN ASSIGN
    cEulaFile = SEARCH("eula.txt").

RUN system/checkEula.p (INPUT cEulaFile, OUTPUT lEulaAccepted, OUTPUT cEulaVersion).

IF NOT lEulaAccepted THEN 
    RUN windows/wUserEula.w (INPUT cEulaFile, OUTPUT lEulaAccepted).
IF NOT lEulaAccepted THEN 
    oplExit = TRUE. 
    
iLoginCnt = 0.
FOR EACH userLog NO-LOCK WHERE userLog.userStatus EQ "Logged In" 
  AND  userLog.user_id EQ cCurrentUserID:
  iLoginCnt = iLoginCnt + 1.
END.

/* If user has other sessions open, prompt to add another or close them */
IF iLoginCnt GT 0 AND promptMultiSession-log THEN DO:
    RUN system/wSession.w (INPUT iLoginCnt, INPUT iMaxSessionsPerUser, OUTPUT cResponse).

    CASE cResponse:
        WHEN "" THEN 
          oplExit = TRUE.
        WHEN "Exit Application" THEN 
            oplExit = TRUE.          
        WHEN "Log Out Other Sessions" THEN DO:
            FOR EACH userLog EXCLUSIVE-LOCK WHERE userLog.userStatus EQ "Logged In" 
                AND  userLog.user_id EQ cCurrentUserID:
                ASSIGN 
                    userLog.logoutDateTime = DATETIME(TODAY, MTIME)
                    userLog.userStatus     = "User Logged Out".
            END.            
        END.
    END CASE.
                     
END.


/* IF adding, CHECK nk1 VALUE AND determine IF the USER COUNT IS over the limit */
IF NOT oplExit AND enforceUserCount-log THEN DO:
    
    iAllUserCount = 0.
    FOR EACH userLog NO-LOCK WHERE userLog.userStatus EQ "Logged In" 
        BREAK BY userLog.IpAddress 
              BY userLog.deviceName 
              BY userLog.user_id:
        IF FIRST-OF(userLog.user_id) THEN 
          iAllUserCount = iAllUserCount + 1.
    END.            
    

    FIND FIRST userControl NO-LOCK NO-ERROR.
    IF AVAILABLE userControl THEN DO:        
        /* +1 represents the additional session for the current new login */

        IF iAllUserCount + 1 GT userControl.maxAllowedUsers + userControl.numUsersOverLimit THEN 
        DO:            
            MESSAGE "The maximum number of connections has been reached.  Exiting the application."
                VIEW-AS ALERT-BOX WARNING .
            oplExit = TRUE.                                       
        END.        
        ELSE IF iAllUserCount + 1 GT userControl.maxAllowedUsers THEN 
            DO:            
                MESSAGE "The maximum number of connections has been reached." SKIP 
                    "Please notify your administrator to add more licences."
                    VIEW-AS ALERT-BOX WARNING .
            
            END.
            
    END. /* Avail user control */
    
END. /* If not exit was chosen */


/* Reject TO accept the ADD - The system will exceed the maximum user count of n */
FIND FIRST users NO-LOCK WHERE users.user_id EQ cCurrentUserID NO-ERROR.
IF AVAILABLE users THEN 
    cUserName = users.user_name.
IF NOT oplExit THEN DO TRANSACTION:
    
    CREATE userLog.
    ASSIGN 
        userLog.user_id       = cCurrentUserID       
        userLog.sessionID     = igsSessionID 
        userLog.userName      = cUserName
        userLog.IpAddress     = OS-GETENV("userDomain")
        userLog.deviceName    = OS-GETENV("computerName")    
        userLog.EulaVersion   = DECIMAL(cEulaVersion)
        userLog.userStatus    = "Logged In"
        userLog.loginDateTime = DATETIME(TODAY, MTIME)
        .
    FIND CURRENT userLog NO-LOCK NO-ERROR.
            
END.  /* If not exiting */
