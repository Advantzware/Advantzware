
/*------------------------------------------------------------------------
    File        : autoLogout.p
    Purpose     : 

    Syntax      :

    Description : Disconnect users from database

    Author(s)   : Wade Kaldawi
    Created     : Thu Jun 07 10:44:04 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE cMonitorFolder   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProcessedFolder AS CHARACTER NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE monitorImportDir   AS CHARACTER FORMAT "X(256)":U INITIAL "./cXML" 
    LABEL "Monitoring Directory" 
    VIEW-AS FILL-IN 
    SIZE 89 BY 1
    BGCOLOR 14 NO-UNDO.
    
    
DEFINE VARIABLE cProdDbName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTestDbName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbDir             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMapDir            AS CHARACTER NO-UNDO.    
DEFINE VARIABLE cDbDrive           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDlcDir            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTopDir            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDb                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserId            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserNum           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPid               AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt               AS INTEGER.
DEFINE VARIABLE iAutoLogoutHours   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cReturnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cLogFile           AS CHARACTER NO-UNDO.
DEFINE BUFFER bf-userLog FOR userLog.
DEFINE STREAM s1.
DEFINE STREAM sLog.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnGetDLC RETURNS CHARACTER 
	(  ) FORWARD.

FUNCTION fnGetPhysicalDb RETURNS CHARACTER 
    (ipcDbName AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

/* Don't have a specific company so use first one set up for NK1 */
FOR EACH company NO-LOCK:

    RUN sys/ref/nk1look.p (INPUT company.company, "AutoLogout", "C" /* Character*/, 
        INPUT NO /* check by cust */, 
        INPUT YES /* use cust not vendor */,
        INPUT "" /* cust */, 
        INPUT "" /* ship-to*/,
        OUTPUT cReturnChar, 
        OUTPUT lRecFound).
    IF lRecFound THEN 
    DO:
        ASSIGN 
            cMonitorFolder   = cReturnChar 
            cProcessedFolder = cMonitorFolder + "\Processed"
            monitorIMportDir   = cMonitorFolder
            cLogFile = cMonitorFolder + "\" + "autologout.log"
            .
    
        FILE-INFO:FILE-NAME = cMonitorFolder.
        IF FILE-INFO:FILE-TYPE EQ ?  THEN        
            OS-CREATE-DIR  value(cMonitorFolder).  
          
        FILE-INFO:FILE-NAME = cProcessedFolder.
        IF FILE-INFO:FILE-TYPE EQ ?  THEN        
            OS-CREATE-DIR VALUE(cProcessedFolder).    
        OUTPUT STREAM sLog TO VALUE(cLogFile) APPEND.  
       PUT STREAM sLog unformatted "Starting autoLogoutProcess" + STRING(TODAY) + " " + STRING(mtime, "hh:mm") SKIP.
       IF NOT OPSYS BEGINS "WIN" THEN DO:
           PUT STREAM sLog  "Not a windows system exiting autoLogoutProcess" + STRING(TODAY) + " " + STRING(mtime, "hh:mm").
           LEAVE. 
        END.
        REPEAT:
            /* Default wait time between execution until db field is added for this */
            PAUSE 10.
            RUN postMonitor.
        END. 
    END. /* If autoLogout NK1 defined */
END. /* each company */    
OUTPUT STREAM sLog CLOSE.
/* Nothing setup, just exit */
RETURN. 
 
/* **********************  Internal Procedures  *********************** */

PROCEDURE disconnectUser:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcDb AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserNum AS INTEGER.
    DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.
    cDLC = fnGetDLC().
    PUT STREAM sLog   UNFORMATTED STRING(TODAY) + " " + STRING(mtime, "hh:mm") + " Disconnect user # " + STRING(ipcUserNum) + " " + " from DB " + ipcDB + " "  SKIP.
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

PROCEDURE loadDbLocations:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

    INPUT FROM n:\admin\advantzware.ini.
    REPEAT:
        IMPORT DELIMITER "=" 
            cName 
            cValue
            .
     
        CASE cName:
            WHEN "ProdDbName" THEN 
                cProdDbName = cValue.
            WHEN "TestDbName" THEN 
                cTestDbName = cValue.
            WHEN "dbDir" THEN 
                cDbDir = cValue.
            WHEN "mapDir" THEN 
                cMapDir = cValue.
            WHEN "DbDrive" THEN 
                cDbDrive = cValue.
            WHEN "DlcDir" THEN 
                cDlcDir = cValue.
            WHEN "topdir" THEN 
                cTopDir = cValue.
                  
        END CASE.
    END.
    INPUT CLOSE.

END PROCEDURE.

PROCEDURE postMonitor:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE monitorFile               AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE attrList                  AS CHARACTER FORMAT 'X(4)' NO-UNDO.
    DEFINE VARIABLE errStatus                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE saveMonitor               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturn                   AS LOGICAL   NO-UNDO.
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
    
    INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
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
    
    IF iAutoLogoutHours GT 0 THEN 
    DO:
        cdb = fnGetPhysicalDb("ASI").
        FOR EACH userLog NO-LOCK WHERE userLog.logoutDateTime EQ ? :
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
                            PUT STREAM sLog   UNFORMATTED STRING(TODAY) + " " + STRING(mtime, "hh:mm") + " User  " + userLog.user_id + "(" + STRING(iLoginUserNum) + ")" +  " connected more than  " + STRING(iAutoLogoutHours) 
                                +  " hours " DATETIME(TODAY,mtime ) dtNextLogoutTime  SKIP.
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

