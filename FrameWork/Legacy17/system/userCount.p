DEFINE INPUT  PARAMETER  ipcDatabase       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  opiLicensedUsers  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER  opiSessionCount      AS INTEGER INITIAL 0 NO-UNDO.
DEFINE OUTPUT PARAMETER  opiUniqueUsers    AS INTEGER INITIAL 0 NO-UNDO.

DEFINE VARIABLE iAllUserCount AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttUsers
    FIELD usrName AS CHARACTER 
    INDEX i1 usrName.

opiSessionCount = 0.
iAllUserCount = 0.
FOR EACH userLog NO-LOCK WHERE userLog.userStatus EQ "Logged In" 
    BREAK BY userLog.user_id:
    IF FIRST-OF(userLog.user_id) THEN 
        iAllUserCount = iAllUserCount + 1.
    opiSessionCount = opiSessionCount + 1.
END.
opiUniqueUsers = iAllUserCount.

FIND FIRST userControl NO-LOCK NO-ERROR.
IF AVAILABLE userControl THEN 
DO:
    opiLicensedUsers = userControl.numLicensedUsers.  
            
END. /* Avail user control */
 
