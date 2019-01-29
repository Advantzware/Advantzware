
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
DEFINE INPUT  PARAMETER iplDisconnect AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER ipiUserNum    AS INTEGER NO-UNDO.
DEFINE VARIABLE iAsiConnectPid AS INTEGER   NO-UNDO.
DEFINE VARIABLE iUserToDisconnect    AS INTEGER NO-UNDO.
DEFINE VARIABLE iSessionToDisconnect AS INTEGER NO-UNDO.
DEFINE VARIABLE cCurrentUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponse      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDBUserNum     AS INTEGER   NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN   
    cocode         = gcompany
    locode         = gloc
    cCurrentUserID = USERID(LDBNAME(1))
    iSessionToDisconnect = ipiUserNum
    .
    

IF ipiUserNum EQ 0 THEN 
DO:
    FIND FIRST asi._myconnection NO-LOCK NO-ERROR.
    IF AVAILABLE asi._myconnection THEN DO: 
        iUserToDisconnect = asi._myconnection._myconn-userId.
        FOR EACH userlog NO-LOCK 
             WHERE userlog.userStatus EQ "Logged In":
            iDBUserNum = INTEGER(SUBSTRING(userLog.deviceName, R-INDEX(userLog.deviceName,"-") + 1)) NO-ERROR.
            IF iDBUserNum EQ iUserToDisconnect THEN 
              iSessionToDisconnect = userLog.sessionID.
        END.
                
    END.
END. 

       
FIND FIRST userLog NO-LOCK 
           WHERE /*userLog.user_id     = cCurrentUserID       
           AND */ userLog.sessionID     = iSessionToDisconnect 
    AND userLog.userStatus EQ "Logged In" 
    USE-INDEX sessionID
    NO-ERROR.
        
  
IF AVAILABLE  userLog THEN 
DO TRANSACTION:
    FIND CURRENT userLog EXCLUSIVE-LOCK.  
    iDBUserNum = INTEGER(SUBSTRING(userLog.deviceName, R-INDEX(userLog.deviceName,"-") + 1)) NO-ERROR.
    ASSIGN 
        userLog.logoutDateTime = DATETIME(TODAY, MTIME)
        userLog.userStatus     = "User Logged Out".

    FIND CURRENT userLog NO-LOCK.

END.

/* Disconnect from database also */
IF iplDisconnect THEN DO:
    /* _connect id is one more than the database user number shown in _myconnection */
    FIND FIRST asi._connect NO-LOCK WHERE asi._connect._connect-id EQ iDbUserNum  NO-ERROR.
                  
    IF AVAILABLE asi._connect AND AVAILABLE userLog THEN 
    DO:
        FIND CURRENT userLog EXCLUSIVE-LOCK.
        userLog.dbDisconnect = TRUE.
                                      
    END. /* If avail _connect */
END. /* If disconnecting */


 
