
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

{methods/defines/hndldefs.i}
{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i NEW SHARED}

DEFINE VARIABLE cCurrentUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponse      AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN
    cocode         = gcompany
    locode         = gloc
    cCurrentUserID = USERID(LDBNAME(1))
    .
FIND FIRST userLog NO-LOCK 
     WHERE userLog.user_id    EQ cCurrentUserID       
       AND userLog.sessionID  EQ igsSessionID 
       AND userLog.userStatus EQ "Logged In" 
     USE-INDEX sessionID
     NO-ERROR.
IF AVAILABLE  userLog THEN DO TRANSACTION:
    FIND CURRENT userLog EXCLUSIVE-LOCK.  
    ASSIGN 
        userLog.logoutDateTime = DATETIME(TODAY, MTIME)
        userLog.userStatus     = "User Logged Out"
        .
    FIND CURRENT userLog NO-LOCK.
END. 
