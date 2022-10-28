&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: asiInstaller.w
  Description: utility to install ASI patches and releases
  Input Parameters:  <none> 
  Output Parameters: <none>
  Author: MYT
  Created: 10/1/2017 and highly modified/adapted over next several months
  Change History:
    12/19/2017 - MYT -  updated to handle 16.6.9 data fixes
                        added Clean Before Install to suppress deletion
                        of existing programs/resources directories prior
                        to install of new (lets customers stay live) 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ipcAsiDbName AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcAsiDbLongName AS CHAR NO-UNDO. 
DEF INPUT PARAMETER ipiAsiDbPort AS INT NO-UNDO.
DEF INPUT PARAMETER ipcAsiDbDir AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcAudDbName AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcAudDbLongName AS CHAR NO-UNDO. 
DEF INPUT PARAMETER ipiAudDbPort AS INT NO-UNDO.
DEF INPUT PARAMETER ipcAudDbDir AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiCurrDbVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiPatchDbVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiCurrAudVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiPatchAudVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiUserLevel AS INT NO-UNDO.
DEF INPUT PARAMETER iplBackupASI AS LOG NO-UNDO.
DEF INPUT PARAMETER iplBackupAudit AS LOG NO-UNDO.
DEF INPUT PARAMETER ipcLogFile AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopiStatus AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE SV SCREEN-VALUE IN FRAME DEFAULT-FRAME

{updateFileVars.i}

DEF STREAM s1.
DEF STREAM outStream.
DEF STREAM logStream.
DEF STREAM iniStream.

DEF SHARED TEMP-TABLE ttUpdateHist
    FIELD fromVersion AS CHAR 
    FIELD toVersion AS CHAR 
    FIELD applyDate AS DATE 
    FIELD startTimeInt AS INT
    FIELD startTime AS CHAR 
    FIELD endTimeInt AS INT 
    FIELD endTime AS CHAR 
    FIELD user_id AS CHAR 
    FIELD success AS LOG INITIAL NO 
    FIELD updLog AS CHAR.     

DEF VAR cDbDirAlone AS CHAR NO-UNDO.
DEF VAR cCurrDir AS CHAR NO-UNDO.
DEF VAR cPortNo AS CHAR NO-UNDO.
DEF VAR cPatchList AS CHARACTER NO-UNDO.
DEF VAR cAsiDbVer AS CHARACTER NO-UNDO.
DEF VAR cAudDbVer AS CHARACTER NO-UNDO.
DEF VAR iPatchAudVer AS INTEGER NO-UNDO.
DEF VAR iPatchDbVer AS INTEGER NO-UNDO.
DEF VAR iPatchEnvVer AS INTEGER NO-UNDO.
DEF VAR iAsiEntry AS INTEGER NO-UNDO.
DEF VAR iAudEntry AS INTEGER NO-UNDO.
DEF VAR ptrToString      AS MEMPTR    NO-UNDO.
DEF VAR intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
DEF VAR intResult        AS INTEGER   NO-UNDO.
DEF VAR iDbCtr AS INT NO-UNDO.
DEF VAR delCtr AS INT NO-UNDO.
DEF VAR dupCtr AS INT NO-UNDO.
DEF VAR cTemp AS CHAR NO-UNDO.
DEF VAR cMsgStr AS CHAR FORMAT "x(80)" EXTENT 500 NO-UNDO.
DEF VAR timestring AS CHAR NO-UNDO.
DEF VAR iUserCount AS INT NO-UNDO.
DEF VAR iMsgCtr AS INT NO-UNDO.
DEF VAR iExtra AS INT NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR v1 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v2 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR cBadDirList AS CHAR NO-UNDO.
DEF VAR cPatchNo AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR cMapDrive AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR iCurrVerExt AS INT NO-UNDO.
DEF VAR iNumberChecked AS INT NO-UNDO.
DEF VAR iLastIni AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR iListEntry AS INT NO-UNDO.
DEF VAR cIniLoc AS CHAR NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR lAuditLicensed AS LOG NO-UNDO INITIAL TRUE.
DEF VAR lConnectAudit AS LOG NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lHasAllFiles AS LOG NO-UNDO.
DEF VAR lSuccess AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR lValidDB AS LOG NO-UNDO.
DEF VAR origPropath AS CHAR NO-UNDO.
DEF VAR connectStatement AS CHAR NO-UNDO.
DEF VAR cRunPgm AS CHAR NO-UNDO.
DEF VAR tslogin-cha AS CHAR NO-UNDO.
DEF VAR hPreRun AS HANDLE.
DEF VAR xDbDir AS CHAR NO-UNDO.
DEF VAR cUsrList AS CHAR NO-UNDO.
DEF VAR lUpdUsr AS LOG NO-UNDO.
DEF VAR iLockoutTries AS INT NO-UNDO.
DEF VAR lNeedUsercontrol AS LOG NO-UNDO.
DEF VAR cTestDir AS CHAR NO-UNDO.
DEF VAR lAllOK AS LOG NO-UNDO.
DEF VAR cUserID AS CHAR FORMAT "x(8)" LABEL "User ID" NO-UNDO.
DEF VAR cPassword AS CHAR FORMAT "x(24)" LABEL "Password" NO-UNDO.
DEF VAR cThisPatch AS CHAR NO-UNDO.
DEF VAR cfrom AS CHAR.
DEF VAR cTo AS CHAR.
DEF VAR iDBCurrVer AS INT NO-UNDO.
DEF VAR iDBTgtVer AS INT NO-UNDO.
DEF VAR iInstance AS INT NO-UNDO.
DEF VAR cEnvVer AS CHAR NO-UNDO.
DEF VAR iFromDelta AS INT NO-UNDO.
DEF VAR iToDelta AS INT NO-UNDO.
DEF VAR cDeltaFile AS CHAR NO-UNDO.

PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.

PROCEDURE SetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
   DEFINE INPUT  PARAMETER chrCurDir AS CHARACTER.
   DEFINE RETURN PARAMETER iResult AS LONG.
END PROCEDURE.

PROCEDURE GetLastError EXTERNAL "kernel32.dll":
    DEFINE RETURN PARAMETER iReturnValue AS LONG.
END.


PROCEDURE ShellExecuteA EXTERNAL "shell32" :
    define input parameter hwnd as long.
    define input parameter lpOperation as char.
    define input parameter lpFile as char.
    define input parameter lpParameters as char.
    define input parameter lpDirectory as char.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 eStatus 
&Scoped-Define DISPLAYED-OBJECTS fiSiteName fiHostname fiDbName fiDbDir ~
fiPortNo fiFromVer fiToVer fiDeltaFilename eStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 77 BY 11.67 NO-UNDO.

DEFINE VARIABLE fiDbDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "in directory" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Upgrading Database" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiDeltaFilename AS CHARACTER FORMAT "X(256)":U 
     LABEL "using delta file" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromVer AS CHARACTER FORMAT "X(256)":U 
     LABEL "from ASI version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiHostname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Server Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiPortNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "running on port" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSiteName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Site Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiToVer AS CHARACTER FORMAT "X(256)":U 
     LABEL "to version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1
     FGCOLOR 3 .

DEFINE RECTANGLE rStatusBar
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 10 BY 1
     BGCOLOR 3 FGCOLOR 3 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSiteName AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 68
     fiHostname AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 36
     fiDbName AT ROW 3.86 COL 29 COLON-ALIGNED
     fiDbDir AT ROW 4.81 COL 29 COLON-ALIGNED
     fiPortNo AT ROW 5.76 COL 29 COLON-ALIGNED
     fiFromVer AT ROW 6.71 COL 29 COLON-ALIGNED
     fiToVer AT ROW 7.67 COL 29 COLON-ALIGNED WIDGET-ID 46
     fiDeltaFilename AT ROW 8.62 COL 29 COLON-ALIGNED WIDGET-ID 50
     eStatus AT ROW 10.52 COL 4 NO-LABEL WIDGET-ID 52
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.81 COL 4 WIDGET-ID 54
     rStatusBar AT ROW 22.43 COL 5
     RECT-6 AT ROW 22.43 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84 BY 22.91 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Advantzware Update - Upgrade Database"
         COLUMN             = 5
         ROW                = 2
         HEIGHT             = 22.91
         WIDTH              = 84
         MAX-HEIGHT         = 39.29
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 39.29
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiDbDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDeltaFilename IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFromVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHostname IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPortNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSiteName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rStatusBar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Advantzware Update - Upgrade Database */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    IF USERID(LDBNAME(1)) EQ "" THEN QUIT.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Advantzware Update - Upgrade Database */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE
   ON END-KEY UNDO MAIN-BLOCK, LEAVE:
    RUN enable_UI.

    IF ipiUserLevel LT 5 THEN DO:
        MESSAGE
            "You do not have sufficient permissions to run this" SKIP
            "procedure.  Please contact your System Administrator."
            VIEW-AS ALERT-BOX ERROR.
        QUIT.
    END.
    
    /* Get the initial directory for later resetting */
    SET-SIZE(ptrToString) = 256.
    RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                              INPUT-OUTPUT ptrToString,
                              OUTPUT       intResult).
    ASSIGN 
        cCurrDir = GET-STRING(ptrToString,1).    
    
    RUN ipFindIniFile ("..\advantzware.ini",
                       OUTPUT cIniLoc).
    IF cIniLoc NE "" THEN DO:
        ASSIGN
            FILE-INFO:FILE-NAME = cIniLoc
            cIniLoc = FILE-INFO:FULL-PATHNAME.
        RUN ipReadIniFile.
    END.
    RUN ipExpandVarNames.
    RUN ipSetDispVars.

    ASSIGN 
        fiHostName:{&SV} = cHostName
        fiDbName:{&SV} = ipcAsiDbName
        fiPortNo:{&SV} = STRING(ipiAsiDbPort,"9999")
        fiDbDir:{&SV} = ipcAsiDbDir
        fiFromVer:{&SV} = STRING(ipiCurrDbVer,"99999999")
        fiToVer:{&SV} = STRING(ipiPatchDbVer,"99999999")
        .
        
    RUN ipProcessRequest.
    
END.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiSiteName fiHostname fiDbName fiDbDir fiPortNo fiFromVer fiToVer 
          fiDeltaFilename eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-6 eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBackupDBs C-Win 
PROCEDURE ipBackupDBs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLocItem AS CHAR NO-UNDO.
    DEF VAR cLocDir AS CHAR NO-UNDO.
    DEF VAR cLocName AS CHAR NO-UNDO.
    DEF VAR cLocPort AS CHAR NO-UNDO.
    DEF VAR cBackupName AS CHAR NO-UNDO.
    DEF VAR cLockFile AS CHAR NO-UNDO.  
    DEF VAR cPrefix  AS CHAR NO-UNDO.
    DEF VAR cThisDir AS CHAR NO-UNDO.
      
    IF iplBackupASI THEN DO:
        ASSIGN
            cLocDir  = ipcAsiDbDir
            cLocName = ipcAsiDbName
            cLocPort = STRING(ipiAsiDbPort,"9999")
            cBackupName = cDbBackup + "\" + cLocName + "_" +
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + "_" +
                       STRING(TIME) + ".bak" 
            cCmdLine = cDLCDir + "\bin\probkup online " + 
                       cDbDir + "\" +
                       cLocDir + "\" +
                       cLocName + " " + 
                       cBackupName
            cLockFile = cDbDir + "\" + 
                       cLocDir + "\" +
                       ipcAsiDbName + ".lk".
            .
    
        IF SEARCH(cLockFile) EQ ? THEN ASSIGN 
            cCmdLine = REPLACE(cCmdLine," online","").
        
        ASSIGN
            iopiStatus = 6.
        RUN ipStatus ("  Backing Up database " + ipcAsiDbName).
        
        OS-COMMAND SILENT VALUE(cCmdLine).
        
        IF SEARCH(cBackupName) NE ? THEN DO:
            ASSIGN
                iopiStatus = 8
                lSuccess = TRUE.
            RUN ipStatus ("    " + ipcAsiDbName + " Backup successful").
        END.
        ELSE DO:
            ASSIGN
                iopiStatus = 8
                lSuccess = FALSE.
            RUN ipStatus ("    " + ipcAsiDbName + " Backup FAILED").
        END.
    END.

    IF iplBackupAudit THEN DO:
        ASSIGN
            cLocDir  = ipcAudDbDir
            cLocName = ipcAudDbName
            cLocPort = STRING(ipiAudDbPort,"9999")
            cBackupName = cDbBackup + "\" + cLocName + "_" +
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + "_" +
                       STRING(TIME) + ".bak" 
            cCmdLine = cDLCDir + "\bin\probkup online " + 
                       cDbDir + "\" +
                       cLocDir + "\" +
                       cLocName + " " + 
                       cBackupName
            cLockFile = cDbDir + "\" + 
                       cLocDir + "\" +
                       ipcAudDbName + ".lk".
            .
    
        IF SEARCH(cLockFile) EQ ? THEN ASSIGN 
            cCmdLine = REPLACE(cCmdLine," online","").
        
        ASSIGN
            iopiStatus = 9.
        RUN ipStatus ("  Backing Up database " + ipcAudDbName).
        
        OS-COMMAND SILENT VALUE(cCmdLine).
        
        IF SEARCH(cBackupName) NE ? THEN DO:
            ASSIGN
                iopiStatus = 11
                lSuccess = TRUE.
            RUN ipStatus ("    " + ipcAudDbName + " Backup successful").
        END.
        ELSE DO:
            ASSIGN
                iopiStatus = 11
                lSuccess = FALSE.
            RUN ipStatus ("    " + ipcAudDbName + " Backup FAILED").
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateAudit C-Win 
PROCEDURE ipCreateAudit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDBIdent AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiListIndex AS INT NO-UNDO.
    
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLocItem AS CHAR NO-UNDO.
    DEF VAR cLocDir AS CHAR NO-UNDO.
    DEF VAR cLocName AS CHAR NO-UNDO.
    DEF VAR cLocPort AS CHAR NO-UNDO.
    DEF VAR cStructureST AS CHAR NO-UNDO.    
    DEF VAR cAuditDF AS CHAR NO-UNDO.
    DEF VAR cStatement AS CHAR NO-UNDO.
    DEF VAR cCmd AS CHAR NO-UNDO.
    DEF VAR cAudName AS CHAR NO-UNDO.
    DEF VAR cAudPort AS CHAR NO-UNDO.
    DEF VAR cStartString AS CHAR NO-UNDO.
        
    RUN ipStatus ("Creating Audit database").
    
    RUN ipSetCurrentDir (cDbDrive + "\" + cTopDir + "\databases\audit"). 
    ASSIGN
        cLocItem = ipcDbIdent
        cLocDir  = ENTRY(1,cLocItem,"-")
        cLocName = ENTRY(2,cLocItem,"-")
        cLocPort = ENTRY(3,cLocItem,"-")
        cStructureST = cDrive + "\" + cTopDir + "\" +
                   cUpdatesDir + "\" +
                   "Structure\STFiles\audit.st"
        cAuditDf = cDrive + "\" + cTopDir + "\" +
                   cUpdatesDir + "\" +
                   "Structure\DFFiles\asiAudit.df"
        cAudName = "aud" + SUBSTRING(cLocName,4,8)
        cAudPort = STRING(INT(cLocPort) + 10,"9999").

    IF cDbDrive EQ cDrive THEN ASSIGN
        cCmdLine = cDlcDir + '\bin\prostrct create ' + 
                   cAudName + ' ' + 
                   cStructureST.
    ELSE ASSIGN
        cCmdLine = cDlcDir + '\bin\prostrct create ' + 
                   cAudName + ' ' + 
                   cStructureST.

    OS-COMMAND SILENT VALUE(cCmdLine).

    
    IF cDbDrive EQ cDrive THEN ASSIGN
        cCmdLine = cDlcDir + "\bin\procopy " + 
                   cDlcDir + "\empty4 " + 
                   cAudName.
    ELSE ASSIGN
        cCmdLine = cDlcDir + "\bin\procopy " + 
                   cDlcDir + "\empty4 " + 
                   cAudName.

    RUN ipStatus ("  Copying metaschema data...").
    OS-COMMAND SILENT VALUE(cCmdLine).

        ASSIGN
            /* Single user connect statment */
            cStatement = "-db " + 
                         cAudName +
                         " -1 -ld " + cAudName.
        /* Connect to the database single user */
        CONNECT VALUE(cStatement).
        CREATE ALIAS DICTDB FOR DATABASE VALUE(cAudName).
        /* Load the delta */
        RUN ipStatus ("  Loading schema...").
        RUN prodict/load_df.p (cAuditDf). 
                
        /* Disconnect it */
        RUN ipStatus ("  Disconnecting.").
        DISCONNECT VALUE(cAudName).
        
    /* Create/Write a txt file that can be loaded into conmgr.properties */
    RUN ipStatus ("  Building conmgr file.").
    OUTPUT TO c:\tmp\conmgrdelta.txt.
    PUT UNFORMATTED "[configuration." + cAudName + ".defaultconfiguration]" + CHR(10).
    PUT UNFORMATTED "    afterimageprocess=false" + CHR(10).
    PUT UNFORMATTED "    asynchronouspagewriters=1" + CHR(10).
    PUT UNFORMATTED "    beforeimageprocess=true" + CHR(10).
    PUT UNFORMATTED "    blocksindatabasebuffers=32768" + CHR(10).
    PUT UNFORMATTED "    database=" + cAudName + CHR(10).
    PUT UNFORMATTED "    displayname=defaultConfiguration" + CHR(10).
    PUT UNFORMATTED "    locktableentries=96000" + CHR(10).
    PUT UNFORMATTED "    monitored=true" + CHR(10).
    PUT UNFORMATTED "    otherargs=" + CHR(10).
    PUT UNFORMATTED "    servergroups=" + cAudName + ".defaultconfiguration.defaultservergroup" + CHR(10).
    PUT UNFORMATTED "    watchdogprocess=true" + CHR(10).
    PUT UNFORMATTED "" + CHR(10).
    PUT UNFORMATTED "[database." + cAudName + "]" + CHR(10).
    PUT UNFORMATTED "    autostart=true" + CHR(10).
    PUT UNFORMATTED "    configurations=" + cAudName + ".defaultconfiguration" + CHR(10).
    PUT UNFORMATTED "    databasename=" + cDbDrive + "\" + cTopDir + "\Databases\Audit\" + cAudName + CHR(10).
    PUT UNFORMATTED "    defaultconfiguration=" + cAudName + ".defaultconfiguration" + CHR(10).
    PUT UNFORMATTED "    displayname=" + cAudName + CHR(10).
    PUT UNFORMATTED "    monitorlicensed=true" + CHR(10).
    PUT UNFORMATTED "" + CHR(10).
    PUT UNFORMATTED "[servergroup." + cAudName + ".defaultconfiguration.defaultservergroup]" + CHR(10).
    PUT UNFORMATTED "    configuration=" + cAudName + ".defaultconfiguration" + CHR(10).
    PUT UNFORMATTED "    displayname=defaultServerGroup" + CHR(10).
    PUT UNFORMATTED "    port=" + cAudPort + CHR(10).
    PUT UNFORMATTED "    type=both" + CHR(10).
    OUTPUT CLOSE.
    
    ASSIGN
        cCmdLine     = cDlcDir + "\bin\mergeprop -type database -action create -delta " + 
                       "c:\tmp\conmgrdelta.txt -silent"
        cStartString = cDlcDir + "\bin\dbman" + 
                       " -host " + cHostName + 
                       " -port " + cPortNo + 
                       " -database " + cAudName + 
                       " -start".

    RUN ipStatus ("  Merging conmgr info.").
    OS-COMMAND SILENT VALUE(cCmdLine).
    RUN ipStatus ("  Waiting for connection manager.").
    PAUSE 10 NO-MESSAGE.
    
    RUN ipStatus ("  Serving " + cAudName).

    OS-COMMAND VALUE(cStartString).
    PAUSE 10 NO-MESSAGE.
    
    ASSIGN
        ENTRY(iListEntry,cAudDbList) = cAudName
        ENTRY(iListEntry,cAudPortList) = cAudPort.
    RUN ipSetCurrentDir (cCurrDir). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpandVarNames C-Win 
PROCEDURE ipExpandVarNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Modify variables for ease of use */
    ASSIGN
        cMapDir = cDrive + "\" + cTopDir
        cAdminDir = cMapDir + "\" + cAdminDir
        cBackupDir = cMapDir + "\" + cBackupDir
        cDbDirAlone = cDbDir
        cDBDir = cDbDrive + "\" + cTopDir + "\" + cDbDir 
        cDocDir = cMapDir + "\" + cDocDir
        cDeskDir = cMapDir + "\" + cDeskDir
        cEnvDir = cMapDir + "\" + cEnvDir
        cInstallDir = cMapDir + "\" + cInstallDir
        cUpdatesDir = cMapDir + "\" + cUpdatesDir

        cDbAdmin = cAdminDir + "\" + cDbAdmin
        cEnvAdmin = cAdminDir + "\" + cEnvAdmin
        cDbBackup = cBackupDir + "\" + cDbBackup
        cPgmBackup = cBackupDir + "\" + cPgmBackup
        cResBackup = cBackupDir + "\" + cResBackup
        cDbAuditDir = cDbDir + "\" + cDbAuditDir
        cDbDataDir = cDbDir + "\" + cDbDataDir
        cDbProdDir = cDbDir + "\" + cDbProdDir
        cDbShipDir = cDbDir + "\" + cDbShipDir
        cDbStructDir = cDbDir + "\" + cDbStructDir
        cDbTestDir = cDbDir + "\" + cDbTestDir
        cEnvProdDir = cEnvDir + "\" + cEnvProdDir
        cEnvTestDir = cEnvDir + "\" + cEnvTestDir
        cUpdAdminDir = cUpdatesDir + "\" + cUpdAdminDir
        cUpdCompressDir = cUpdatesDir + "\" + cUpdCompressDir
        cUpdDataDir = cUpdatesDir + "\" + cUpdDataDir
        cUpdDeskDir = cUpdatesDir + "\" + cUpdDeskDir
        cUpdMenuDir = cUpdatesDir + "\" + cUpdMenuDir
        cUpdProgramDir = cUpdatesDir + "\" + cUpdProgramDir
        cUpdRelNotesDir = cUpdatesDir + "\" + cUpdRelNotesDir
        cUpdStructureDir = cUpdatesDir + "\" + cUpdStructureDir
        cLockoutTries = SUBSTRING(cLockoutTries,1,1)
        iLockoutTries = IF cLockoutTries NE "" 
                        AND ASC(cLockoutTries) GE 48
                        AND ASC(cLockoutTries) LE 57 THEN INT(cLockoutTries) ELSE 0
        wDbVerList = cDbVerList
        wAudVerList = cAudVerList
        iAsiEntry = LOOKUP(ipcAsiDbName,cDbList)
        iAudEntry = LOOKUP(ipcAudDbName,cAudDbList)
        .
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetPatchList C-Win 
PROCEDURE ipGetPatchList :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLine AS CHARACTER.
    
    ASSIGN
        cPatchList = "".

    INPUT FROM VALUE (cUpdatesDir + "\patch.mft").
    REPEAT:
        IMPORT 
            cLine.

        CASE ENTRY(1,cLine,"="):
            WHEN "patchVer" THEN ASSIGN cPatchList = ENTRY(2,cLine,"=").
            WHEN "asiDbVer" THEN ASSIGN cAsiDbVer = ENTRY(2,cLine,"=").
            WHEN "audDbVer" THEN ASSIGN cAudDbVer = ENTRY(2,cLine,"=").
        END CASE.
    END.

    ASSIGN
        iPatchEnvVer = fIntVer(cPatchList)
        iPatchDbVer  = fIntVer(cAsiDbVer)
        iPatchAudVer  = fIntVer(cAudDbVer)
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcessRequest C-Win 
PROCEDURE ipProcessRequest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iLookup AS INT NO-UNDO.
    
    RUN ipStatus ("Beginning Database Schema Update").
        RUN ipReadAdminSvcProps.    

    /* Process "regular" database (asixxxx.db OR XXXXXXd.db) */
    IF iplBackupASI
    OR iplBackupAudit THEN DO:
        RUN ipBackupDBs.
        IF NOT lSuccess THEN 
        DO:
            ASSIGN 
                oplSuccess = FALSE.
            RUN ipStatus ("  Upgrade failed in ipBackupDBs").
            RETURN.
        END.
    END.
    
    IF ipiPatchDbVer GT ipiCurrDbVer
    OR ipiPatchAudVer GT ipiCurrAudVer THEN 
        RUN ipUpgradeDBs.
    IF NOT lSuccess THEN 
    DO:
        ASSIGN 
            oplSuccess = FALSE.
        RUN ipStatus ("  Upgrade failed in ipUpgradeDBs").
        RETURN.
    END.
    
    RUN ipStatus ("Database Schema Update Complete").
    RUN ipWriteIniFile.
    PAUSE 2 NO-MESSAGE.

    ASSIGN
        fiFromVer:{&SV} = fiToVer:{&SV}
        oplSuccess = lSuccess.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadAdminSvcProps C-Win 
PROCEDURE ipReadAdminSvcProps :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cLine AS CHAR NO-UNDO.
    
    RUN ipStatus ("  Reading admin service props file " + fiDbName:{&SV}).

    INPUT FROM VALUE (cDLCDir + "\properties\AdminServerPlugins.properties").
    REPEAT:
        IMPORT cLine.
        ASSIGN 
            cLine = TRIM(cLine).
        IF ENTRY(1,cLine,"=") EQ "port" THEN DO:
            ASSIGN 
                cPortNo = ENTRY(2,cLine,"=").
            LEAVE.
        END.
    END.
    INPUT CLOSE.
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetCurrentDir C-Win 
PROCEDURE ipSetCurrentDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipTgtDir AS CHAR NO-UNDO.
    DEF VAR iResult AS INT NO-UNDO.
    DEF VAR iReturnValue AS INT NO-UNDO.
    
    RUN SetCurrentDirectoryA (ipTgtDir, OUTPUT iResult).

    IF iResult NE 1 THEN DO:
        RUN GetLastError (output iReturnValue).
        MESSAGE 
            "Unable to set working directory." SKIP
            "Error code:" iReturnValue 
            VIEW-AS ALERT-BOX.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetDispVars C-Win 
PROCEDURE ipSetDispVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO:
        FOR EACH ttIniFile:
            CASE ttIniFile.cVarName:
                WHEN "siteName" THEN ASSIGN fiSiteName:{&SV} = ttIniFile.cVarValue.
                WHEN "hostname" THEN ASSIGN cHostName = ttIniFile.cVarValue.
            END CASE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipStatus C-Win 
PROCEDURE ipStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcStatus AS CHAR NO-UNDO.
                
    IF INDEX(ipcStatus,"duplicate") EQ 0 THEN DO:
        ASSIGN
            eStatus:{&SV}       = eStatus:{&SV} + ipcStatus + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        ASSIGN
            iMsgCtr = iMsgCtr + 1
            cMsgStr[iMsgCtr] = ipcStatus.
        FIND FIRST ttUpdateHist NO-LOCK NO-ERROR.
        IF AVAIL ttUpdateHist THEN ASSIGN 
                ttUpdateHist.updLog = ttUpdateHist.updLog + STRING(TODAY,"99/99/99") + "  " + STRING(TIME,"HH:MM:SS") + "  " + cMsgStr[iMsgCtr] + CHR(10)
                ttUpdateHist.endTimeInt = INT(TIME)
                ttUpdateHist.endTime = STRING(time,"HH:MM:SS AM")        
                ttUpdateHist.success = lSuccess.        
        OUTPUT STREAM logStream TO VALUE(ipcLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr[iMsgCtr] FORMAT "x(160)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
    END.

    ASSIGN 
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).        
    
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpgradeDBs C-Win 
PROCEDURE ipUpgradeDBs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cStopString AS CHAR NO-UNDO.
    DEF VAR cStartString AS CHAR NO-UNDO.
    DEF VAR cStatement AS CHAR NO-UNDO.
    DEF VAR cDeltaDf AS CHAR NO-UNDO.
    DEF VAR cLockFile AS CHAR NO-UNDO.
    DEF VAR cDbLogFile AS CHAR NO-UNDO.
    DEF VAR cNewList AS CHAR NO-UNDO.
    DEF VAR cNewSel AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cReplEntry AS CHAR NO-UNDO.
    DEF VAR cDelta AS CHAR NO-UNDO.
    DEF VAR cPrefix AS CHAR NO-UNDO.
    DEF VAR cPrefix2 AS CHAR NO-UNDO.
    DEF VAR cThisDb AS CHAR NO-UNDO. 
    DEF VAR cThisDir AS CHAR NO-UNDO.
    DEF VAR iWaitCount AS INT NO-UNDO.
    DEF VAR lAlternate AS LOG NO-UNDO.
    DEF VAR cFullDelta AS CHAR NO-UNDO.
    DEF VAR cThisPort AS CHAR NO-UNDO.
    DEF VAR cMissingFilesDelta AS CHAR NO-UNDO.

    /* Upgrade asi database */
    IF ipiPatchDbVer GT ipiCurrDbVer THEN DO:
        RUN ipStatus ("  Upgrading database " + ipcAsiDbName).
        
        ASSIGN 
            cDelta = "asi" + STRING(ipiCurrDbVer,"99999999") + "-" + STRING(ipiPatchDbVer,"99999999") + ".df"
            cFullDelta = cMapDir + "\Updates\Structure\DFFiles\" + cDelta
            cStopString = cDlcDir + "\bin\dbman" + 
                         " -host " + cHostName + 
                         " -port " + cPortNo + 
                         " -database " + ipcAsiDbName + 
                         " -stop"
            cStartString = cDlcDir + "\bin\dbman" + 
                         " -host " + cHostName + 
                         " -port " + cPortNo + 
                         " -database " + ipcAsiDbName + 
                         " -start"
            /* Single user connect statment */
            cStatement = "-db " + cDbDrive + "\" +
                         cTopDir + "\Databases\" + 
                         ipcAsiDbDir + "\" +
                         ipcAsiDbName + 
                         " -1 -ld updDB1"
            cLockFile = cDbDrive + "\" +
                         cTopDir + "\Databases\" + 
                         ipcAsiDbDir + "\" +
                         ipcAsiDbName + ".lk"
            cDbLogFile = cDbDrive + "\" +
                         cTopDir + "\Databases\" + 
                         ipcAsiDbDir + "\" +
                         ipcAsiDbName + ".lg"
            .

        IF SEARCH(cFullDelta) EQ ? THEN DO:
            RUN ipStatus ("    Unable to locate delta file " + cDelta + ".  Cancelling.").
            RUN ipStatus ("      (" + cFullDelta + ")").
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.

        
        /* Unserve the database */
        RUN ipStatus ("    Stopping database service").
        ASSIGN  
            iWaitCount = 0.
        OS-COMMAND SILENT VALUE(cStopString).
        /* May have to wait for DB to shut down */
       
        WaitBlock:
        DO WHILE SEARCH(cLockFile) NE ?:
            IF iWaitCount MOD 3 EQ 0 THEN 
                RUN ipStatus ("    Waiting for removal of lock file").
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            IF iWaitCount EQ 15 
            OR SEARCH(cLockFile) EQ ? THEN DO:
                LEAVE waitblock.
            END.
        END.
        
        IF SEARCH(cLockFile) NE ? THEN DO:
            /* dbman failed - try proshut */
            ASSIGN  
                iWaitCount = 0
                cStopString = cDlcDir + "\bin\proshut -by" + ipcAsiDbLongName
                .
            OS-COMMAND SILENT VALUE(cStopString).
            /* May have to wait for DB to shut down */
            WaitBlock2:
            DO WHILE SEARCH(cLockFile) NE ?:
                IF iWaitCount MOD 3 EQ 0 THEN 
                    RUN ipStatus ("    Waiting for removal of lock file").
                ASSIGN 
                    iWaitCount = iWaitCount + 1.
                PAUSE 2 NO-MESSAGE.
                IF iWaitCount EQ 15 
                OR SEARCH(cLockFile) EQ ? THEN DO:
                    LEAVE waitblock2.
                END.
            END.
        END.
        
        IF SEARCH(cLockFile) NE ? THEN DO:
            RUN ipStatus (    "Unable to shut down server for " + ipcAsiDbName + ".  Cancelling.").
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
        
        /* Remove the log file */
        OS-DELETE VALUE(REPLACE(ipcAsiDbLongName,".db",".lg")).
        
        /* Connect to the database single user */
        ASSIGN 
            iopiStatus = 13.
        RUN ipStatus ("    Connecting single-user mode").
        CONNECT VALUE(cStatement).
    
        /* Test ASI DB to see if audit DB is licensed */
        IF CONNECTED("updDB1") THEN DO:
            RUN ipStatus ("    Creating DICTDB alias").
            CREATE ALIAS DICTDB FOR DATABASE VALUE("updDB1").
            CREATE ALIAS asi FOR DATABASE updDB1.
            
            RUN asiAuditTest.r (OUTPUT lAuditLicensed, OUTPUT lHasAllFiles).
            DELETE ALIAS asi.
        END.

        /* Load the delta */
        RUN ipStatus ("    Loading delta " + STRING(cFullDelta)).
        RUN prodict/load_df.p (cFullDelta). 
            
        /* Disconnect it */
        RUN ipStatus ("    Disconnecting").
        DISCONNECT VALUE("updDB1").
        
        /* Re-Serve it */
        RUN ipStatus ("    Serving " + ipcAsiDbName).
        ASSIGN
            iWaitCount = 0.
                
        OS-COMMAND SILENT VALUE(cStartString).
        
        Waitblock3:
        DO WHILE SEARCH(cLockFile) EQ ?:
            IF iWaitCount MOD 3 EQ 0 THEN 
                RUN ipStatus ("    Waiting for restore of lock file").
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            IF iWaitCount EQ 15 THEN 
            DO:
                LEAVE waitblock3.
            END.
        END.
        
        IF SEARCH(cLockFile) EQ ? THEN DO:
            MESSAGE 
                "Unable to restart the ASI database after upgrade." SKIP 
                "This may be a symptom of several problems.  You should contact" SKIP 
                "Advantzware support before continuing the update process."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
        
    END.
    
    /* Now do the same thing for the AUDIT DB */
    IF ipiPatchAudVer GT ipiCurrAudVer THEN DO:
        ASSIGN 
            iopiStatus = 15.
        RUN ipStatus ("  Upgrading database " + ipcAudDbName).
        
        ASSIGN 
            cDelta = "aud" + STRING(ipiCurrAudVer,"99999999") + "-" + STRING(ipiPatchAudVer,"99999999") + ".df"
            cFullDelta = cMapDir + "\Updates\Structure\DFFiles\" + cDelta
            cStopString = cDlcDir + "\bin\dbman" + 
                         " -host " + cHostName + 
                         " -port " + cPortNo + 
                         " -database " + ipcAudDbName + 
                         " -stop"
            cStartString = cDlcDir + "\bin\dbman" + 
                         " -host " + cHostName + 
                         " -port " + cPortNo + 
                         " -database " + ipcAudDbName + 
                         " -start"
            /* Single user connect statment */
            cStatement = "-db " + cDbDrive + "\" +
                         cTopDir + "\Databases\" + 
                         ipcAudDbDir + "\" +
                         ipcAudDbName + 
                         " -1 -ld updDB2"
            cLockFile = cDbDrive + "\" +
                         cTopDir + "\Databases\" + 
                         ipcAudDbDir + "\" +
                         ipcAudDbName + ".lk"
            cDbLogFile = cDbDrive + "\" +
                         cTopDir + "\Databases\" + 
                         ipcAudDbDir + "\" +
                         ipcAudDbName + ".lg"
            .

        IF SEARCH(cFullDelta) EQ ? THEN DO:
            RUN ipStatus ("    Unable to locate delta file " + cDelta + ".  Cancelling.").
            RUN ipStatus ("      (" + cFullDelta + ")").
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.

        /* Unserve the database */
        RUN ipStatus ("    Stopping database service").
        ASSIGN  
            iWaitCount = 0.
        OS-COMMAND SILENT VALUE(cStopString).
        /* May have to wait for DB to shut down */
        WaitBlock4:
        DO WHILE SEARCH(cLockFile) NE ?:
            IF iWaitCount MOD 3 EQ 0 THEN 
                RUN ipStatus ("    Waiting for removal of lock file").
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            IF iWaitCount EQ 15 
            OR SEARCH(cLockFile) EQ ? THEN DO:
                LEAVE waitblock4.
            END.
        END.
        
        IF SEARCH(cLockFile) NE ? THEN DO:
            /* dbman failed - try proshut */
            ASSIGN  
                iWaitCount = 0
                cStopString = cDlcDir + "\bin\proshut -by" + ipcAudDbLongName
                .
            OS-COMMAND SILENT VALUE(cStopString).
            /* May have to wait for DB to shut down */
            WaitBlock5:
            DO WHILE SEARCH(cLockFile) NE ?:
                IF iWaitCount MOD 3 EQ 0 THEN 
                    RUN ipStatus ("    Waiting for removal of lock file").
                ASSIGN 
                    iWaitCount = iWaitCount + 1.
                PAUSE 2 NO-MESSAGE.
                IF iWaitCount EQ 15 
                OR SEARCH(cLockFile) EQ ? THEN DO:
                    LEAVE waitblock5.
                END.
            END.
        END.
        
        IF SEARCH(cLockFile) NE ? THEN DO:
            RUN ipStatus (    "Unable to shut down server for " + ipcAudDbName + ".  Cancelling.").
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
    
        /* Remove the log file */
        OS-DELETE VALUE(REPLACE(ipcAudDbLongName,".db",".lg")).
        
        /* Connect to the database single user */
        ASSIGN 
            iopiStatus = 17.
        RUN ipStatus ("    Connecting single-user mode").
        CONNECT VALUE(cStatement).
    
        /* If audit DB not licensed, drop and reload all tables empty */
        IF CONNECTED("updDB2") 
        AND lAuditLicensed EQ FALSE THEN DO:
            IF ipiCurrDbVer LT 20011000 THEN ASSIGN 
                cFullDelta = REPLACE(cFullDelta,cDelta,"audEmptyOld.df").
            ELSE ASSIGN
                cFullDelta = REPLACE(cFullDelta,cDelta,"audEmpty.df").
        END.

        /* Load the delta */
        RUN ipStatus ("    Loading delta " + STRING(cFullDelta)).
        RUN prodict/load_df.p (cFullDelta). 
            
        /* Disconnect it */
        RUN ipStatus ("    Disconnecting").
        DISCONNECT VALUE("updDB2").
        
        /* Re-Serve it */
        RUN ipStatus ("    Serving " + ipcAudDbName).
        ASSIGN
            iWaitCount = 0.
                
        OS-COMMAND SILENT VALUE(cStartString).
        
        Waitblock6:
        DO WHILE SEARCH(cLockFile) EQ ?:
            IF iWaitCount MOD 3 EQ 0 THEN 
                RUN ipStatus ("    Waiting for restore of lock file").
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            IF iWaitCount EQ 15 THEN 
            DO:
                LEAVE waitblock6.
            END.
        END.
        
        IF SEARCH(cLockFile) EQ ? THEN DO:
            MESSAGE 
                "Unable to restart the AUDIT database after upgrade." SKIP 
                "This may be a symptom of several problems.  You should contact" SKIP 
                "Advantzware support before continuing the update process."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
    END. 

    ASSIGN 
        ENTRY(iAsiEntry,wDbVerList) =  SUBSTRING(STRING(ipiPatchDbVer),1,2) + "." +
                                      SUBSTRING(STRING(ipiPatchDbVer),3,2) + "." +
                                      SUBSTRING(STRING(ipiPatchDbVer),5,2)
        ENTRY(iAudEntry,wAudVerList) =  SUBSTRING(STRING(ipiPatchDbVer),1,2) + "." +
                                      SUBSTRING(STRING(ipiPatchDbVer),3,2) + "." +
                                      SUBSTRING(STRING(ipiPatchDbVer),5,2).
    RUN ipWriteIniFile.
    
    ASSIGN
        iopiStatus = 19
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cStrVal AS CHAR EXTENT 4 NO-UNDO.
    DEF VAR iIntVal AS INT  EXTENT 4 NO-UNDO.
    DEF VAR iIntVer AS INT  NO-UNDO.
    ASSIGN
        cStrVal[1] = ENTRY(1,cVerString,".")
        cStrVal[2] = ENTRY(2,cVerString,".")
        cStrVal[3] = ENTRY(3,cVerString,".")
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GT 3 THEN ENTRY(4,cVerString,".") ELSE "0"
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer    = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

