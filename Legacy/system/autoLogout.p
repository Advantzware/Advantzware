
/*------------------------------------------------------------------------
    File        : autoLogout.p
    Purpose     : 

    Syntax      :

    Description : Disconnect users from database

    Author(s)   : Wade Kaldawi
    Created     : Thu Jun 07 10:44:04 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE var  iphCallingProc AS HANDLE NO-UNDO.
DEFINE var ipcMonitorFolder AS CHARACTER NO-UNDO.
DEFINE VARIABLE  ipcProcessedFolder AS CHARACTER NO-UNDO.
ipcMonitorFolder = ".\users\usercontrol".
ipcProcessedFolder = ".\users\usercontrol\processed".
/* ***************************  Definitions  ************************** */
DEFINE VARIABLE monitorImportDir AS CHARACTER FORMAT "X(256)":U INITIAL "./cXML" 
    LABEL "Monitoring Directory" 
    VIEW-AS FILL-IN 
    SIZE 89 BY 1
    BGCOLOR 14 NO-UNDO.
    monitorIMportDir = ipcMonitorFolder.
    
DEFINE VARIABLE cProdDbName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTestDbName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMapDir     AS CHARACTER NO-UNDO.    
DEFINE VARIABLE cDbDrive AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDlcDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTopDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDb      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserId  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserNum AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPid     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt AS INTEGER.
DEFINE BUFFER bf-userLog FOR userLog.
DEFINE STREAM s1.
/*
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}

ASSIGN
    cocode = g_company
    locode = g_loc.
    */
/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
DEFINE FRAME DEFAULT-FRAME

  
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 168.4 BY 27.05.
/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame   AS WIDGET-HANDLE    NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.    
CREATE CONTROL-FRAME CtrlFrame ASSIGN
    FRAME           = FRAME DEFAULT-FRAME:HANDLE
    ROW             = 1
    COLUMN          = 90
    HEIGHT          = 4.76
    WIDTH           = 20
    WIDGET-ID       = 4
    HIDDEN          = yes
    SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
/*CtrlFrame:MOVE-AFTER(monitorImportDir:HANDLE IN FRAME DEFAULT-FRAME).*/
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnGetPhysicalDb RETURNS CHARACTER 
    (ipcDbName AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
/* RUN loadDbLocations. */
RUN control_load.
WAIT-FOR CLOSE OF THIS-PROCEDURE.
/* **********************  Internal Procedures  *********************** */

PROCEDURE disconnectUser:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcDb AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserNum AS INTEGER.
   /* MESSAGE "disconnecting db" ipcDb
   "user" ipcUserNum skip
   "dlc" cDlcDir
   VIEW-AS ALERT-BOX. */
    OS-COMMAND SILENT VALUE("C:\Progress\OpenEdge116_64\bin\proshut " + ipcDb + " -C disconnect " + TRIM(STRING(ipcUserNum,">>>9"))).
    RETURN.

END PROCEDURE.

PROCEDURE loadDbLocations:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

INPUT FROM n:\admin\advantzware.ini.
REPEAT:
     IMPORT DELIMITER "=" 
       cName 
       cValue
     .
     
     CASE cName:
         WHEN "ProdDbName" THEN cProdDbName = cValue.
         WHEN "TestDbName" THEN cTestDbName = cValue.
         WHEN "dbDir" THEN cDbDir = cValue.
         WHEN "mapDir" THEN cMapDir = cValue.
         WHEN "DbDrive" THEN cDbDrive = cValue.
         WHEN "DlcDir" THEN cDlcDir = cValue.
         WHEN "topdir" THEN cTopDir = cValue.
                  
     END CASE.
 END.
 INPUT CLOSE.

END PROCEDURE.

PROCEDURE postMonitor:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE monitorFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE attrList    AS CHARACTER FORMAT 'X(4)' NO-UNDO.
    DEFINE VARIABLE errStatus   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturn     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hPDS        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE nextRNo     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nextRelease AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nextRelNo   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPathIn     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPathout    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRtnChar                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE PrePressHotFolderIn-char  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE PrePressHotFolderOut-char AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullFilePath             AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE iAutoLogoutHours AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtNextLogoutTime AS DATETIME NO-UNDO.
    DEFINE VARIABLE iLoginUserNum     AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iAsiConnectPid      AS INTEGER NO-UNDO.
    
   /*  RUN monitorActivity IN iphCallingProc ('Check dir ' + monitorImportDir,YES,'') . */

    INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
    REPEAT:

        IMPORT monitorFile ^ attrList.
        IF attrList NE 'f' OR monitorFile BEGINS '.' /*OR
       INDEX(monitorFile,'.xml') EQ 0 */ THEN NEXT.
       INPUT STREAM s1 FROM VALUE(monitorImportDir + "\" + monitorFile).
       IMPORT STREAM s1 cDb cUserID cUserNum cPID.
       INPUT STREAM s1 CLOSE. 

       /* RUN monitorActivity IN iphCallingProc ('Requested Disconnnect ' + cUserID + " " + cUserNum,YES,'') . */
       RUN disconnectUser (INPUT cDb, INPUT cUserNum).
       
       cPathout = monitorImportDir + "\" + "Processed".
        cFullFilePath = monitorImportDir + "\" + monitorFile.
        OS-COPY VALUE(cFullFilePath) VALUE(cPathOut).    

        IF INTEGER(OS-ERROR) EQ 0 THEN 
            OS-DELETE VALUE(cFullFilePath).
                          
    END. /* os-dir repeat */
    INPUT CLOSE.

    /* Check for users logged in too long */
    FIND FIRST userControl  NO-LOCK.
    /* RUN monitorActivity IN iphCallingProc ('Check overdue users ' + monitorImportDir,YES,'') . */
    iAutoLogoutHours = userControl.autoLogoutTime.
    cdb = fnGetPhysicalDb("Audit").
    FOR EACH userLog NO-LOCK WHERE logoutDateTime EQ ? :
         
         dtNextLogoutTime =  ADD-INTERVAL (userLog.loginDateTime,  iAutoLogoutHours , "Hours") .
         
         IF DATETIME(today, time) GT dtNextLogoutTime THEN DO:
             /* RUN monitorActivity IN iphCallingProc ('Disconnect ' + userLog.userName,YES,'') . */
             IF INDEX(userLog.deviceName, "-") GT 0 THEN DO:
                 iLoginUserNum = INTEGER(SUBSTRING(userLog.deviceName, R-INDEX(userLog.deviceName,"-") + 1)) NO-ERROR.
                 IF NOT ERROR-STATUS:ERROR THEN DO:
                     /* RUN monitorActivity IN iphCallingProc ('Disconnect from database ASI user ' + STRING(iLoginUserNum) + " " + userLog.userName,YES,'') . */
                     /* _connect id is one more than the database user number shown in _myconnection */
                     FIND FIRST asi._connect NO-LOCK WHERE asi._connect._connect-id EQ iLoginUserNum + 1 NO-ERROR.
                     IF AVAILABLE asi._connect AND asi._connect._connect-name EQ userLog.userName THEN DO:
                         iAsiConnectPid= asi._connect._connect-pid.
                       RUN disconnectUser (INPUT cDb, INPUT iLoginUserNum).
                         /* Try to find the same user connected to the audit database */
                         FIND FIRST audit._connect NO-LOCK 
                                     WHERE audit._connect._connect-name EQ userLog.userName 
                                          AND audit._connect._connect-pid EQ iAsiConnectPid
                                     NO-ERROR. 
                         IF AVAILABLE audit._connect AND LDBNAME(2) EQ "Audit" THEN DO:
                             cDb = fnGetPhysicalDb("Audit").
                             /* _connect id is one more than the actual database user number */
                             RUN disconnectUser (INPUT cDb, INPUT audit._connect._connect-id - 1).
                         END.  /* If audit _connect found */                           
                     END. /* If _connect found */                                                               
                 END. /* If deviceName contains a user number */
                 ELSE 
                    /* RUN monitorActivity IN iphCallingProc ('Disconnect from database failed for ' + userLog.userName,YES,'') . */
                            
               FIND CURRENT bf-userLog EXCLUSIVE-LOCK.  
               ASSIGN 
                   bf-userLog.logoutDateTime = DATETIME(TODAY, MTIME)
                   bf-userLog.userStatus     = "User Logged Out"
                   .
               FIND CURRENT bf-userLog NO-LOCK.
             END. /* If deviceName contains a dash */             
         END. /* If max login time reached */
    END. /* Each userlog */
END PROCEDURE.

PROCEDURE CtrlFrame.PSTimer.Tick .
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  None required for OCX.
      Notes:       
    ------------------------------------------------------------------------------*/
    MESSAGE "tick" VIEW-AS ALERT-BOX.
    RUN postMonitor.
    iCnt = iCnt + 1.
    IF iCnt GT 3 THEN QUIT.


END PROCEDURE.

PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/


    DEFINE VARIABLE UIB_S   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE OCXFile AS CHARACTER NO-UNDO.

    OCXFile = SEARCH( "windows/monitor.wrx":U ).
    IF OCXFile = ? THEN
        OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
            R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

    IF OCXFile <> ? THEN
    DO:
        ASSIGN
            chCtrlFrame    = CtrlFrame:COM-HANDLE
            UIB_S          = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
            CtrlFrame:NAME = "CtrlFrame":U
            .
        RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
    END.
    ELSE MESSAGE "monitor.wrx":U SKIP(1)
            "The binary control file could not be found. The controls cannot be loaded."
            VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

END PROCEDURE.
/* ************************  Function Implementations ***************** */

FUNCTION fnGetPhysicalDb RETURNS CHARACTER 
	(ipcDbName AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cPhysDb AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount          AS INTEGER NO-UNDO.
    DEFINE VARIABLE hFileListBuffer AS HANDLE  NO-UNDO.

    DO iCount = 1 TO NUM-DBS:
        CREATE BUFFER hFileListBuffer FOR TABLE LDBNAME(iCount) + "._FileList".
        hFileListBuffer:FIND-FIRST("WHERE " + LDBNAME(iCount) + "._fileList._fileList-Name MATCHES '*.db'" , NO-LOCK).    
        IF LDBNAME(iCount) EQ ipcDbName THEN 
          cPhysDb = hFileListBuffer::_fileList-Name.

   
        /* For older releases that do not support the shorthand syntax used above: */
        /* MESSAGE hFileListBuffer:BUFFER-FIELD("_fileList-Name"):BUFFER-VALUE VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        DELETE OBJECT hFileListBuffer.
    END.
    
	RETURN cPhysDb.


		
END FUNCTION.

