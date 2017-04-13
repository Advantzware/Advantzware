
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


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST asi._myconnection NO-LOCK.  
FIND FIRST asi._connect NO-LOCK WHERE _connect._connect-id = _myconnection._myconn-id
    NO-ERROR. 
  
FIND FIRST userLog NO-LOCK 
    WHERE userLog.user_id       = USERID("NOSWEAT")       
    AND userLog.sessionID     = asi._myconnection._myconn-pid 
    AND userLog.userStatus EQ "Logged In" 
    NO-ERROR.
        
  
IF AVAILABLE  userLog THEN 
DO TRANSACTION:
    FIND CURRENT userLog EXCLUSIVE-LOCK.  
    ASSIGN 
        userLog.logoutDateTime = DATETIME(TODAY, MTIME)
        userLog.userStatus     = "User Logged Out".

    FIND CURRENT userLog NO-LOCK.


END.


 
