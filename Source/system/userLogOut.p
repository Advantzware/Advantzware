/*------------------------------------------------------------------------
    File        : userLogOut.p
    Purpose     : 
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : Mon Apr 10 13:02:49 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iplDisconnect AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER ipiUserNum    AS INTEGER   NO-UNDO.

DEFINE VARIABLE iAsiConnectPid       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iUserToDisconnect    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSessionToDisconnect AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCurrentUserID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponse            AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDBUserNum           AS INTEGER   NO-UNDO.

DEFINE BUFFER bUserLog FOR userLog.

{methods/defines/hndldefs.i}
{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}

/* ***************************  Main Block  *************************** */
ASSIGN   
    cocode               = gcompany
    locode               = gloc
    cCurrentUserID       = USERID(LDBNAME(1))
    iSessionToDisconnect = ipiUserNum
    .

/* This occurs when the logout is initiated anywhere in the system for THIS user */
IF ipiUserNum EQ 0 THEN DO:
    FIND FIRST asi._myconnection 
        NO-LOCK NO-ERROR.
    IF AVAILABLE asi._myconnection THEN DO: 
        FOR EACH userlog NO-LOCK WHERE 
            userlog.userStatus EQ "Logged In" AND  
            userLog.asiUsrNo EQ  asi._myconnection._myconn-userId AND 
            userLog.asiPID EQ asi._myconnection._myconn-PID:
            DO TRANSACTION:
                FIND bUserLog EXCLUSIVE WHERE 
                    ROWID(bUserLog) EQ ROWID(userLog) 
                    NO-ERROR.
                IF AVAIL bUserLog THEN ASSIGN 
                    buserLog.logoutDateTime = DATETIME(TODAY, MTIME)
                    buserLog.userStatus     = "User Logged Out".
                IF iplDisconnect THEN DO:
                    FIND FIRST asi._connect NO-LOCK WHERE 
                        asi._connect._connect-Usr EQ buserLog.asiUsrNo AND 
                        asi._connect._connect-PID EQ buserLog.asiPID
                        NO-ERROR.
                    IF AVAIL asi._connect THEN ASSIGN                   
                        buserLog.dbDisconnect   = TRUE.
                END.
            END.
        END.                
    END.
END. 
/* and this occurs when a process needs to log out a specified user session */
ELSE DO:
    FOR EACH userlog NO-LOCK WHERE 
        userlog.userStatus EQ "Logged In" AND  
        userLog.sessionID EQ  ipiUserNum:
        DO TRANSACTION:
            FIND bUserLog EXCLUSIVE WHERE 
                    ROWID(bUserLog) EQ ROWID(userLog) 
                    NO-ERROR.
            IF AVAIL bUserLog THEN ASSIGN 
                    buserLog.logoutDateTime = DATETIME(TODAY, MTIME)
                    buserLog.userStatus     = "Disconnected".
            IF iplDisconnect THEN DO:
                FIND FIRST asi._connect NO-LOCK WHERE 
                    asi._connect._connect-Usr EQ buserLog.asiUsrNo AND 
                    asi._connect._connect-PID EQ buserLog.asiPID
                    NO-ERROR.
                IF AVAIL asi._connect THEN ASSIGN                   
                    buserLog.dbDisconnect   = TRUE.
            END.
        END.
    END.                
END.       
