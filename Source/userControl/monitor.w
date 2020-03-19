/*------------------------------------------------------------------------
    File        : userControl/Monitor.w
    Purpose     : Standard monitor for user autologout function
    Syntax      :  
    Description :   
    Author(s)   : WK
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
    /* Check for module licensing */
    DEF VAR lAccess AS LOG NO-UNDO.
    RUN util/CheckModule.p (INPUT "ASI", INPUT "AutoLogout", INPUT NO /*prompt if no access*/, OUTPUT lAccess).
    IF NOT lAccess THEN RETURN.
    
    DEFINE BUFFER bUserLog FOR userlog.

    {custom/monitor.w "userControl" "userControl"}

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fGetDtTmFromConnectTime RETURNS DATETIME 
	( INPUT cConnectTime AS CHAR ) FORWARD.

FUNCTION fnGetDLC RETURNS CHARACTER 
    (  ) FORWARD.

FUNCTION fnGetPhysicalDb RETURNS CHARACTER 
    (ipcDbName AS CHARACTER) FORWARD.
DEFINE VARIABLE lIsAnAdmin AS LOGICAL NO-UNDO.


/* **********************  Internal Procedures  *********************** */
PROCEDURE ipDisconnectUserLog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprUserLogRow AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcLogoutMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoginUserNum  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAsiConnectPid AS INTEGER NO-UNDO.    
    
    FIND FIRST userLog NO-LOCK
        WHERE ROWID(userLog) EQ iprUserLogRow
        NO-ERROR.
    IF NOT AVAILABLE userLog THEN 
        RETURN.
              
    FIND FIRST asi._connect NO-LOCK WHERE  
        asi._connect._Connect-Usr EQ userLog.asiUsrNo AND 
        asi._connect._Connect-Pid EQ userLog.asiPID          
        NO-ERROR.
    IF AVAILABLE asi._connect THEN DO:
        RUN monitorActivity (" User  " + userLog.user_id + " (" + STRING(userLog.asiUserID) + ") " +  ipcLogoutMessage,YES,'').
        ASSIGN 
            cDb = fnGetPhysicalDb("ASI").
        RUN disconnectUser (cDb, userLog.asiUsrNo, userLog.user_id).
    END.
        
    FIND FIRST audit._connect NO-LOCK WHERE 
        audit._connect._Connect-Usr EQ userLog.audUsrNo AND 
        audit._connect._Connect-Pid EQ userLog.audPID          
        NO-ERROR.
    IF AVAILABLE audit._connect THEN DO:
        RUN monitorActivity (" User  " + userLog.user_id + " (" + STRING(userLog.audUserID) + ") " +  ipcLogoutMessage,YES,'').
        ASSIGN 
            cDb = fnGetPhysicalDb("AUDIT").
        RUN disconnectUser (cDb, userLog.audUsrNo, userlog.user_id).
    END.
                                             
    RUN ipLogUserOut (INPUT ROWID(userLog)).
                 
END PROCEDURE.

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored files, process files, post files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE monitorFile               AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE attrList                  AS CHARACTER FORMAT 'X(4)' NO-UNDO.
    DEFINE VARIABLE errorStatus               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE saveMonitor               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturn                   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cXMLError                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLFile                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLProcessed             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLResponse              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE returnValue               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hPDS                      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE nextRNo                   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nextRelease               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nextRelNo                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPathIn                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPathout                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRtnChar                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE PrePressHotFolderIn-char  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE PrePressHotFolderOut-char AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullFilePath             AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE dtNextLogoutTime          AS DATETIME  NO-UNDO.
    DEFINE VARIABLE iLoginUserNum             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAsiConnectPid            AS INTEGER   NO-UNDO.  

    /* For users tagged for disconnect via login-choose-logout-other-sessions or via NK5 */
    /* Note that securityLevel not tested here */
    FOR EACH userLog EXCLUSIVE-LOCK WHERE 
        userLog.dbDisconnect EQ TRUE:
        RUN ipDisconnectUserLog (INPUT ROWID(userLog), 
                                 INPUT " tagged for disconnect ").
        ASSIGN 
            userLog.dbDisconnect   = FALSE
            userLog.logoutDateTime = DATETIME(TODAY, MTIME)
            userLog.userStatus     = "Logged Out".
    END.
    
    ASSIGN 
        iUserCheckCountdown = iUserCheckCountdown - 1.
        
    /* Run every sixty timer-click cycles */
    IF iUserCheckCountdown LE 0 
    OR lRunNow THEN 
    DO:
        ASSIGN 
            iUserCheckCountdown = 60.
        /* NEW FUNCTIONALITY */
        /* Purge userLog records if logged out and over 30 days old */
        FOR EACH userLog WHERE 
            userLog.logoutDateTime NE ? AND 
            userLog.logoutDateTime LT ADD-INTERVAL(DATETIME(TODAY), -30, "Days"):
            DELETE userLog.
        END.

        /* Check for users logged in too long */
        FIND FIRST userControl NO-LOCK.
        ASSIGN 
            iAutoLogoutHours = userControl.autoLogoutTime.
        IF iAutoLogoutHours GT 0 THEN 
        DO:
            RUN monitorActivity ('Check for users logged in too long ' ,YES,'').
            ASSIGN 
                cdb = fnGetPhysicalDb("ASI").
            FOR EACH userLog NO-LOCK WHERE 
                userLog.userStatus EQ "Logged In":
                FIND FIRST users NO-LOCK WHERE 	
                    users.user_id EQ  userLog.user_id 	
                    NO-ERROR.	
                IF NOT AVAILABLE users THEN /* This should never happen, but bypass if it does */ 	
                    NEXT.                /* Don't autoLogout admin-level users */
                IF users.securityLevel GE 900 THEN NEXT.
                /* Add logout hours to the users login time to get time when they should get logged out */ 
                ASSIGN 
                    dtNextLogoutTime =  ADD-INTERVAL (userLog.loginDateTime,  iAutoLogoutHours , "Hours") .
                /* If NOW is later than the scheduled logout time, log him out */
                IF DATETIME(TODAY, MTIME) GT dtNextLogoutTime THEN
                    RUN ipDisconnectUserLog (INPUT ROWID(userLog), INPUT " connected more than  " + STRING(iAutoLogoutHours) +  " hours ").
            END. /* Each userlog */
        END.
        
        /* NEW FUNCTIONALITY */
        /* Read list of userlogs and compare to connections; if not present, mark as disconnected */
        RUN monitorActivity ('Check for Userlogs without DB connections ' ,YES,'').
        FOR EACH userlog EXCLUSIVE WHERE 
            userLog.userStatus = "Logged In":
            FIND FIRST asi._connect NO-LOCK WHERE     
                asi._connect._Connect-Usr EQ userLog.asiUsrNo AND  
                asi._connect._Connect-Pid EQ userLog.asiPID
                NO-ERROR. 
            FIND FIRST audit._connect NO-LOCK WHERE     
                audit._connect._Connect-Usr EQ userLog.audUsrNo AND  
                audit._connect._Connect-Pid EQ userLog.audPID
                NO-ERROR. 
            IF NOT AVAIL asi._connect 
            OR NOT AVAIL audit._connect THEN ASSIGN 
                userLog.dbDisconnect   = FALSE
                userLog.logoutDateTime = DATETIME(TODAY, MTIME)
                userLog.userStatus     = "Disconnected".
        END.
                
        FOR EACH asi._connect NO-LOCK WHERE 
            CAN-DO("REMC,SELF",asi._connect._connect-type) AND
            asi._connect._connect-clienttype NE "SQLC" AND
            asi._connect._connect-clienttype NE "APSV":
            IF INTERVAL(DATETIME(TODAY, MTIME),fGetDtTmFromConnectTime(asi._connect._connect-time),"seconds") LE 30 THEN 
                NEXT.                
            IF CAN-FIND (FIRST userLog WHERE 
                            userLog.asiUsrNo = asi._connect._Connect-Usr AND 
                            userLog.asiPID   = asi._connect._Connect-Pid AND 
                            userLog.userStatus = "Logged In") THEN NEXT.
            ASSIGN 
                cdb = fnGetPhysicalDb("ASI").
            RUN disconnectUser (cDb, asi._connect._Connect-Usr, asi._connect._connect-name).
        END.

        /* Read list of connections and compare to userLogins; if not present, log him out of DB */
        RUN monitorActivity ('Check for DB users without userLogs ' ,YES,'').
        testasi:
        FOR EACH asi._connect NO-LOCK WHERE 
            CAN-DO("REMC,SELF",asi._connect._connect-type) AND
            asi._connect._connect-clienttype NE "SQLC" AND 
            asi._connect._connect-clienttype NE "APSV":
            IF INTERVAL(DATETIME(TODAY, MTIME),fGetDtTmFromConnectTime(asi._connect._connect-time),"seconds") LE 30 THEN 
                NEXT.                
            IF CAN-FIND (FIRST userLog WHERE 
                            userLog.asiUsrNo = asi._connect._Connect-Usr AND 
                            userLog.asiPID   = asi._connect._Connect-Pid AND 
                            userLog.userStatus = "Logged In") THEN NEXT testasi.
            ASSIGN 
                cdb = fnGetPhysicalDb("ASI").
            RUN disconnectUser (cDb, asi._connect._Connect-Usr, asi._connect._connect-name).
        END.
        testaudit:
        FOR EACH audit._connect NO-LOCK WHERE 
            CAN-DO("REMC,SELF",audit._connect._connect-type) AND
            audit._connect._connect-clienttype NE "SQLC" AND 
            audit._connect._connect-clienttype NE "APSV": 
            IF INTERVAL(DATETIME(TODAY, MTIME),fGetDtTmFromConnectTime(audit._connect._connect-time),"seconds") LE 30 THEN 
                NEXT.                
            IF CAN-FIND (FIRST userLog WHERE 
                            userLog.audUsrNo = audit._connect._Connect-Usr AND 
                            userLog.audPID   = audit._connect._Connect-Pid AND 
                            userLog.userStatus = "Logged In") THEN NEXT testaudit.
            ASSIGN 
                cdb = fnGetPhysicalDb("audit").
            RUN disconnectUser (cDb, audit._connect._Connect-Usr, audit._connect._connect-name).
        END.
    END.
    
END PROCEDURE.

&SCOPED-DEFINE monitorActivity
PROCEDURE disconnectUser:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDb AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserNum AS INTEGER.
    DEFINE INPUT PARAMETER ipcUserName AS CHAR NO-UNDO.
    DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDLC = fnGetDLC().
    RUN monitorActivity (" Requesting Disconnect for user # " + STRING(ipcUserNum) + " (" + STRING(ipcUserName) + ") from DB " + ipcDB + " ",YES,'').
    OS-COMMAND SILENT VALUE(cDLC + "\bin\proshut " + ipcDb + " -C disconnect " + TRIM(STRING(ipcUserNum,">>>9"))).
    RETURN.

END PROCEDURE.

PROCEDURE ipLogUserOut:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprUserlog AS ROWID NO-UNDO.

    FIND bf-userLog EXCLUSIVE-LOCK WHERE 
        ROWID(bf-userlog) EQ iprUserLog 
        NO-ERROR.
    IF AVAILABLE bf-UserLog THEN ASSIGN 
        bf-userLog.logoutDateTime = DATETIME(TODAY, MTIME)
        bf-userLog.userStatus     = "Logged Out".
    FIND CURRENT bf-userLog NO-LOCK NO-ERROR.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetDtTmFromConnectTime RETURNS DATETIME 
	( INPUT cConnectTime AS CHAR  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE result AS DATETIME NO-UNDO.
    DEF VAR iYear AS INT NO-UNDO.
    DEF VAR iMonth AS INT NO-UNDO.
    DEF VAR iDay AS INT NO-UNDO.
    DEF VAR iHour AS INT NO-UNDO.
    DEF VAR iMin AS INT NO-UNDO.
    DEF VAR iSec AS INT NO-UNDO.
    DEF VAR cTime AS CHAR NO-UNDO.
    
    ASSIGN 
        iMonth = LOOKUP(ENTRY(2,cConnectTime," "),"Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec")
        iDay   = INTEGER(ENTRY(3,cConnectTime," "))
        iYear  = INTEGER(ENTRY(5,cConnectTime," "))
        cTime  = ENTRY(4,cConnectTime," ")
        iHour  = INTEGER(ENTRY(1,cTime,":"))
        iMin   = INTEGER(ENTRY(2,cTime,":"))
        iSec   = INTEGER(ENTRY(3,cTime,":"))
        RESULT = DATETIME(iMonth,iDay,iYear,iHour,iMin,iSec).
    
    RETURN result.

END FUNCTION.

FUNCTION fnGetDLC RETURNS CHARACTER 
    (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDLC    AS CHARACTER NO-UNDO.

    GET-KEY-VALUE SECTION 'Startup' KEY 'DLC' VALUE cDLC.
    cResult = cDLC.
    RETURN cResult.
		
END FUNCTION.

FUNCTION fnGetPhysicalDb RETURNS CHARACTER 
    (ipcDbName AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cPhysDb         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hFileListBuffer AS HANDLE    NO-UNDO.

    DO iCount = 1 TO NUM-DBS:
        CREATE BUFFER hFileListBuffer FOR TABLE LDBNAME(iCount) + "._FileList".
        hFileListBuffer:FIND-FIRST("WHERE " + LDBNAME(iCount) + "._fileList._fileList-Name MATCHES '*.db'" , NO-LOCK).    
        IF LDBNAME(iCount) EQ ipcDbName THEN ASSIGN 
            cPhysDb = hFileListBuffer::_fileList-Name.
        DELETE OBJECT hFileListBuffer.
    END.
    
    RETURN cPhysDb.
		
END FUNCTION.

