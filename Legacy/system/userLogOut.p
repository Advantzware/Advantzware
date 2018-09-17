
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
DEFINE VARIABLE cUserKillFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAsiConnectPid AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCurrentUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponse      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDBUserNum     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cReturnChar   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cLogoutFolder AS CHARACTER NO-UNDO.
DEFINE STREAM sLogOut.
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
    cCurrentUserID = USERID(LDBNAME(1)).


RUN sys/ref/nk1look.p (INPUT gcompany, "UserControl", "C" /* Character*/, 
    INPUT NO /* check by cust */, 
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT cReturnChar, 
    OUTPUT lRecFound).
IF lRecFound THEN 
    cLogoutFolder = cReturnChar  .
   
IF SEARCH( cLogoutFolder) EQ ? THEN 
    OS-CREATE-DIR VALUE( cLogoutFolder).  
        
FIND FIRST userLog NO-LOCK 
           WHERE /*userLog.user_id     = cCurrentUserID       
           AND */ userLog.sessionID     = ipiUserNum 
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
        iAsiConnectPid = asi._connect._connect-pid.        
    
        cUserKillFile = asi._connect._connect-name 
            + STRING(iDbUserNum)
            + STRING(TODAY, "99999999") + STRING(TIME) + ".TXT".
        OUTPUT STREAM sLogOut TO VALUE(cLogoutFolder + "\" + cUserkillFile).
        EXPORT STREAM sLogOut "ASI" userlog.user_id iDbUserNum iAsiConnectPid.
        OUTPUT STREAM sLogOut CLOSE.
                                      
    END. /* If avail _connect */
END. /* If disconnecting */


 
