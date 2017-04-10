
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
DEFINE VARIABLE iAllUserCount         AS INTEGER NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

/* System Constant Values  contains user eula file*/
{system/sysconst.i}

RUN sys/ref/nk1look.p (INPUT cocode, "enforceUserCount", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT lcNk1Value, OUTPUT llRecFound).
IF llRecFound THEN
    enforceUserCount-log = LOGICAL(lcNk1Value) NO-ERROR.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* CHECK IF USER has more than one login record */
/* IF so, ask IF this IS replacing OR adding */
/* You have n session currently open.  Are you replacing one or adding a new one?" */
DEFINE VARIABLE iLoginCnt AS INTEGER NO-UNDO.

FIND FIRST asi._myconnection NO-LOCK NO-ERROR.  

FIND FIRST asi._connect NO-LOCK WHERE _connect._connect-usr = _myconnection._myconn-userid
    NO-ERROR. 
    
cEulaFile = SEARCH("{&EulaFile}").
    
RUN system/checkEula.p (INPUT cEulaFile, OUTPUT lEulaAccepted, OUTPUT cEulaVersion).
IF NOT lEulaAccepted THEN 
    oplExit = TRUE. 
    
iLoginCnt = 0.
FOR EACH userLog NO-LOCK WHERE userLog.userStatus EQ "Logged In" 
  AND  userLog.user_id EQ USERID("nosweat"):
  iLoginCnt = iLoginCnt + 1.
END.

IF iLoginCnt GT 0 THEN DO:
    
    MESSAGE "You have " + string(iLoginCnt) + " other open sessions and can replace them or add a new one." SKIP 
      "Choose 'YES' to add an additional session." SKIP
      "Choose 'NO' to replace the existing sessions with this new one." SKIP
      "Choose 'CANCEL' to Exit"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lAnswer.
      
    CASE lAnswer:
        WHEN ? THEN 
          oplExit = TRUE.
        WHEN NO THEN DO:
            FOR EACH userLog EXCLUSIVE-LOCK WHERE userLog.userStatus EQ "Logged In" 
                AND  userLog.user_id EQ USERID("nosweat"):
                userLog.userStatus = "User Logged Out".
            END.            
        END.
    END CASE.
                     
END.


/* IF adding, CHECK nk1 VALUE AND determine IF the USER COUNT IS over the limit */
IF NOT oplExit THEN DO:
    
    iAllUserCount = 0.
    FOR EACH userLog NO-LOCK WHERE userLog.userStatus EQ "Logged In" 
        :
        
        iAllUserCount = iAllUserCount + 1.
    END.            
    
    FIND FIRST userControl NO-LOCK NO-ERROR.
    IF AVAILABLE userControl THEN DO:        
        /* +1 represents the additional session for the current new login */
        IF iAllUserCount + 1 GT userControl.maxAllowedUsers + userControl.numUsersOverLimit THEN DO:            
           MESSAGE "The maximum number of sessions has been reached.  Exiting the application."
                   VIEW-AS ALERT-BOX WARNING .
           oplExit = TRUE.                                       
        END.
    END. /* Avail user control */
    
END. /* If not exit was chosen */

/* Reject TO accept the ADD - The system will exceed the maximum user count of n */

IF NOT oplExit THEN DO TRANSACTION:
    CREATE userLog.
    ASSIGN 
        userLog.user_id       = USERID("NOSWEAT")       
        userLog.sessionID     = asi._myconnection._myconn-pid 
        userLog.userName      = asi._connect._connect-name
        userLog.IpAddress     = asi._connect._connect-ip
        userLog.deviceName    = asi._connect._connect-device    
        userLog.EulaVersion   = DECIMAL(cEulaVersion)
        userLog.userStatus    = "Logged In"
        userLog.loginDateTime = DATETIME(TODAY, MTIME).
END.  /* If not exiting */
FIND CURRENT userLog NO-LOCK NO-ERROR.