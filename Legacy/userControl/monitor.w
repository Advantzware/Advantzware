/* monitor.w */

{custom/monitor.w "userControl" "userControl"}

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnGetDLC RETURNS CHARACTER 
	(  ) FORWARD.

FUNCTION fnGetPhysicalDb RETURNS CHARACTER 
    (ipcDbName AS CHARACTER) FORWARD.
DEFINE VARIABLE lIsAnAdmin AS LOGICAL NO-UNDO.

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored files, process files, post files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
  DEFINE VARIABLE errorStatus AS INTEGER NO-UNDO.
  DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cXMLError AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXMLFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXMLProcessed AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXMLResponse AS CHARACTER NO-UNDO.
  DEFINE VARIABLE returnValue AS CHARACTER NO-UNDO.
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

DEFINE VARIABLE cMonitorFolder   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProcessedFolder AS CHARACTER NO-UNDO.    
ASSIGN 
             cMonitorFolder=  monitorIMportDir
            cProcessedFolder = cMonitorFolder + "\Processed"           
            .
     IF SEARCH(cMonitorFolder) EQ ? THEN 
       OS-CREATE-DIR VALUE(cMonitorFolder).
    IF SEARCH(cProcessedFolder) EQ ? THEN 
        OS-CREATE-DIR VALUE(cProcessedFolder).       
     INPUT FROM OS-DIR(cMonitorFolder) NO-ECHO.
    REPEAT:

        IMPORT monitorFile ^ attrList.
        IF attrList NE 'f' OR monitorFile BEGINS '.' OR
       INDEX(monitorFile,'.txt') EQ 0  THEN NEXT.
    
        INPUT STREAM s1 FROM VALUE(monitorImportDir + "\" + monitorFile).
        IMPORT STREAM s1 cDb cUserID cUserNum cPID.
        INPUT STREAM s1 CLOSE. 
        
        cDb = fnGetPhysicalDb(cDb).
        FIND FIRST asi._connect NO-LOCK WHERE asi._connect._connect-usr EQ integer(cUserNum)  NO-ERROR.
        IF AVAILABLE asi._connect AND asi._connect._connect-name EQ cUserID THEN 
        DO:
            iAsiConnectPid= asi._connect._connect-pid.
            RUN disconnectUser (INPUT cDb, INPUT cUserNum).
            /* Try to find the same user connected to the audit database */
            FIND FIRST audit._connect NO-LOCK 
                WHERE audit._connect._connect-pid EQ iAsiConnectPid
                NO-ERROR. 
            IF AVAILABLE audit._connect AND LDBNAME(2) EQ "Audit" THEN 
            DO:
                cDb = fnGetPhysicalDb("Audit").
                /* _connect id is one more than the actual database user number */
                RUN disconnectUser (INPUT cDb, INPUT audit._connect._connect-usr).
            END.
            
            /* Try to find user to log out */
            FOR EACH userLog NO-LOCK WHERE logoutDateTime EQ ? :

                    IF INDEX(userLog.deviceName, "-") GT 0 THEN 
                    DO:
                        iLoginUserNum = INTEGER(SUBSTRING(userLog.deviceName, R-INDEX(userLog.deviceName,"-") + 1)) NO-ERROR.
                        IF NOT ERROR-STATUS:ERROR THEN 
                        DO:
                            IF iLoginUserNum EQ asi._connect._connect-usr                            
                               AND asi._connect._connect-name EQ cUserID THEN DO:
                                RUN ipLogUserOut (INPUT   ROWID(userLog)).
                             END. /* If user id matches */
                        END. /* If not error-status */
                    END. /* If device name contains dash */
                
             END.   /* Each userlog */      
        END. /* If available matching _connect */
        
       
        cPathout = monitorImportDir + "\" + "Processed".
        cFullFilePath = monitorImportDir + "\" + monitorFile.
        OS-COPY VALUE(cFullFilePath) VALUE(cPathOut).    

        IF INTEGER(OS-ERROR) EQ 0 THEN 
            OS-DELETE VALUE(cFullFilePath).
                          
    END. /* os-dir repeat */
    INPUT CLOSE.

    /* Check for users logged in too long */
    FIND FIRST userControl NO-LOCK.
    
    /* Check this each time in case it changes */
    iAutoLogoutHours = userControl.autoLogoutTime.
    iUserCheckCountdown = iUserCheckCountdown - 1.
    IF iAutoLogoutHours GT 0 AND iUserCheckCountdown LE 0 THEN 
    DO:
      iUserCheckCountdown = 60. /* Minimum 60 seconds */
      RUN monitorActivity ('Check for users logged in too long ' ,YES,'').
 
        cdb = fnGetPhysicalDb("ASI").
        FOR EACH userLog NO-LOCK WHERE userLog.logoutDateTime EQ ? :
            FIND FIRST users NO-LOCK WHERE users.user_id EQ  userLog.user_id NO-ERROR.
            IF NOT AVAILABLE users THEN 
               NEXT.
            RUN epCanAccessUser IN hPgmSecurity ("browsers/userlog.w", "", userLog.user_id, OUTPUT lIsAnAdmin).               
                      
            /* Don't log someone out who is an admin */
            IF lIsAnAdmin OR users.user_id EQ "ASI" THEN 
               NEXT.
               
            /* Add logout hours to the users login time to get time when they should get logged out */ 
            dtNextLogoutTime =  ADD-INTERVAL (userLog.loginDateTime,  iAutoLogoutHours , "Hours") .
            IF DATETIME(TODAY, MTIME) GT dtNextLogoutTime THEN 
            DO:

                IF INDEX(userLog.deviceName, "-") GT 0 THEN 
                DO:
                    iLoginUserNum = INTEGER(SUBSTRING(userLog.deviceName, R-INDEX(userLog.deviceName,"-") + 1)) NO-ERROR.
                    IF NOT ERROR-STATUS:ERROR THEN 
                    DO:
                                        
                        FIND FIRST asi._connect NO-LOCK WHERE asi._connect._connect-usr EQ iLoginUserNum  NO-ERROR.
                        IF AVAILABLE asi._connect AND asi._connect._connect-name EQ userLog.user_id THEN 
                        DO:
                        RUN monitorActivity (STRING(TODAY) + " " + STRING(mtime, "hh:mm") + " User  " + userLog.user_id + "(" + STRING(iLoginUserNum) + ")" +  " connected more than  " + STRING(iAutoLogoutHours) 
                                +  " hours ",YES,'').

                            iAsiConnectPid= asi._connect._connect-pid.
                            RUN disconnectUser (INPUT cDb, INPUT iLoginUserNum).
                            /* Try to find the same user connected to the audit database */
                            FIND FIRST audit._connect NO-LOCK 
                                WHERE audit._connect._connect-pid EQ iAsiConnectPid
                                NO-ERROR. 
                            IF AVAILABLE audit._connect AND LDBNAME(2) EQ "Audit" THEN 
                            DO:
                                cDb = fnGetPhysicalDb("Audit").
                                /* _connect id is one more than the actual database user number */
                                RUN disconnectUser (INPUT cDb, INPUT audit._connect._connect-usr).
                            END.  /* If audit _connect found */                           
                        END. /* If _connect found */                                                               
                    END. /* If deviceName contains a user number */
                                             
                   RUN ipLogUserOut (INPUT   ROWID(userLog)).

                END. /* If deviceName contains a dash */             
            END. /* If max login time reached */
        END. /* Each userlog */
    END. /* If logouttime gt 0 */

END PROCEDURE.

&SCOPED-DEFINE monitorActivity
PROCEDURE disconnectUser:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcDb AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserNum AS INTEGER.
    DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.
    cDLC = fnGetDLC().
    
    RUN monitorActivity (STRING(TODAY) + " " + STRING(mtime, "hh:mm") + " Disconnect user # " + STRING(ipcUserNum) + " " + " from DB " + ipcDB + " ",YES,'').
    OS-COMMAND SILENT VALUE(cDLC + "\bin\proshut " + ipcDb + " -C disconnect " + TRIM(STRING(ipcUserNum,">>>9"))).
    RETURN.

END PROCEDURE.

PROCEDURE ipLogUserOut:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iprUserlog AS ROWID NO-UNDO.

    FIND bf-userLog EXCLUSIVE-LOCK WHERE ROWID(bf-userlog) EQ 
        iprUserLog NO-ERROR.
    IF AVAILABLE bf-UserLog THEN 
        ASSIGN 
            bf-userLog.logoutDateTime = DATETIME(TODAY, MTIME)
            bf-userLog.userStatus     = "User Logged Out"
            .
    FIND CURRENT bf-userLog NO-LOCK NO-ERROR.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fnGetDLC RETURNS CHARACTER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.

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
        IF LDBNAME(iCount) EQ ipcDbName THEN 
            cPhysDb = hFileListBuffer::_fileList-Name.

   
        DELETE OBJECT hFileListBuffer.
    END.
    
    RETURN cPhysDb.
		
END FUNCTION.

