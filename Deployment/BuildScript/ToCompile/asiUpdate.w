&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File:                 asiUpdate1.w
  Description:          Update launch program 
  Input Parameters:     <none>
  Output Parameters:    <none>
  Author:               MYT 
  Created:              02/01/2019 
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

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE FRAME-NAME DEFAULT-FRAME
&SCOPED-DEFINE IN IN FRAME DEFAULT-FRAME
&SCOPED-DEFINE SV SCREEN-VALUE {&IN}
&SCOPED-DEFINE WOn SESSION:set-wait-state ("general").
&SCOPED-DEFINE WOff SESSION:set-wait-state ("").
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE createSingleUserPFs YES
&SCOPED-DEFINE execProgram mainmenu.    
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES
&SCOPED-DEFINE WTRUE 1
&SCOPED-DEFINE WFALSE 0

{updateFileVars.i}

DEFINE STREAM s1.
DEFINE STREAM sInstr.
DEFINE STREAM outFile.
DEFINE STREAM logFile.
DEFINE STREAM outStream.
DEFINE STREAM logStream.
DEFINE STREAM iniStream.
DEFINE STREAM sKeep.

DEFINE TEMP-TABLE ttDatabases
    FIELD cName AS CHARACTER
    FIELD cDir AS CHARACTER
    FIELD cPort AS CHARACTER
    FIELD cVer AS CHARACTER
    FIELD cAudName AS CHARACTER
    FIELD cAudPort AS CHARACTER
    FIELD cAudVer AS CHARACTER.

DEF TEMP-TABLE ttUpdateLog
    FIELD cLine AS CHAR. 
    
    
DEF NEW SHARED TEMP-TABLE ttUpdateHist
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
    
DEF TEMP-TABLE ttBackupSize
    FIELD ttType AS CHAR 
    FIELD ttID AS CHAR 
    FIELD ttSize AS INT.

DEFINE VARIABLE c7ZErrFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE c7ZOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsiDbDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsiDbLongName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsiDbName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsiDbVer AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAudDbDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAudDbLongName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAudDbName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAudDbVer AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBadDirList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCfgCfg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCmdLine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDevCfg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDLList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfrom AS CHARACTER.
DEFINE VARIABLE cFtpErrFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFtpInstrFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFtpOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFtpPassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFtpUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIniLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInstallerFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIpAddress AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMapDrive AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE cMsgStr AS CHARACTER FORMAT "x(80)" EXTENT 500 NO-UNDO.
DEFINE VARIABLE cOrigPropath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNewPropath  AS CHARACTER NO-UNDO.
DEFINE VARIABLE connectStatement AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER FORMAT "x(24)" LABEL "Password" NO-UNDO.
DEFINE VARIABLE cPatchList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPatchNo AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE cRunCfg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunPgm AS CHARACTER NO-UNDO.
DEFINE VARIABLE cState  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTemp AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTestDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cThisPatch AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTo AS CHARACTER.
DEFINE VARIABLE cUserID AS CHARACTER FORMAT "x(8)" LABEL "User ID" NO-UNDO.
DEFINE VARIABLE cUsrLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUsrList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUsrLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarName AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE delCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE deMinLevel AS DECIMAL NO-UNDO INITIAL 16.7.
DEFINE VARIABLE dupCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE hPreRun AS HANDLE.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iAsiDbPort AS INTEGER NO-UNDO.
DEFINE VARIABLE iAsiDbVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iAudDbPort AS INTEGER NO-UNDO.
DEFINE VARIABLE iAudDbVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iStatus AS INTEGER NO-UNDO INITIAL 1.
DEFINE VARIABLE iCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrAudVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrDbVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrEnvVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrVerExt AS INTEGER NO-UNDO.
DEFINE VARIABLE iDBCurrVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iDBTgtVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iExtra AS INTEGER NO-UNDO.
DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
DEFINE VARIABLE iLastIni AS INTEGER NO-UNDO.
DEFINE VARIABLE iListEntry AS INTEGER NO-UNDO.
DEFINE VARIABLE iLockoutTries AS INTEGER NO-UNDO.
DEFINE VARIABLE iMsgCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumberChecked AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumUsers AS INTEGER NO-UNDO.
DEFINE VARIABLE iPatchAudVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iPatchDbVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iPatchEnvVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iUserCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iUserLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE jCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllOK AS LOG NO-UNDO.
DEFINE VARIABLE lBackupASI AS LOG NO-UNDO.
DEFINE VARIABLE lBackupAudit AS LOG NO-UNDO.
DEFINE VARIABLE lPurgeAudit AS LOG NO-UNDO.
DEFINE VARIABLE lKeepFiles AS LOG NO-UNDO.
DEFINE VARIABLE lConnectAudit AS LOG NO-UNDO.
DEFINE VARIABLE lCorrupt AS LOG NO-UNDO.
DEFINE VARIABLE lFoundIni AS LOG NO-UNDO.
DEFINE VARIABLE lFoundUsr AS LOG NO-UNDO.
DEFINE VARIABLE lHeader AS LOG NO-UNDO.
DEFINE VARIABLE ll-ans AS LOG NO-UNDO.
DEFINE VARIABLE lNeedUsercontrol AS LOG NO-UNDO.
DEFINE VARIABLE lOKtoProceed AS LOG  NO-UNDO.
DEFINE VARIABLE lSuccess AS LOG NO-UNDO.
DEFINE VARIABLE lSysError AS LOG NO-UNDO.
DEFINE VARIABLE lUpdUsr AS LOG NO-UNDO.
DEFINE VARIABLE lValidDB AS LOG NO-UNDO.
DEFINE VARIABLE origPropath AS CHARACTER NO-UNDO.
DEFINE VARIABLE timestring AS CHARACTER NO-UNDO.
DEFINE VARIABLE tslogin-cha AS CHARACTER NO-UNDO.
DEFINE VARIABLE v1 AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v2 AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE xDbDir AS CHARACTER NO-UNDO.

PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.

PROCEDURE SetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT  PARAMETER chrCurDir AS CHARACTER.
    DEFINE RETURN PARAMETER iResult AS LONG.
END PROCEDURE.

PROCEDURE BringWindowToTop EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intHwnd   AS LONG.
    DEFINE RETURN PARAMETER intResult AS LONG.
END PROCEDURE.

PROCEDURE GetLastError EXTERNAL "kernel32.dll":
    DEFINE RETURN PARAMETER iReturnValue AS LONG.
END.

PROCEDURE GetDiskFreeSpaceExA EXTERNAL "kernel32.dll" :
    DEFINE  INPUT  PARAMETER  lpDirectoryName        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT  PARAMETER  FreeBytesAvailable     AS MEMPTR    NO-UNDO.
    DEFINE OUTPUT  PARAMETER  TotalNumberOfBytes     AS MEMPTR    NO-UNDO.
    DEFINE OUTPUT  PARAMETER  TotalNumberOfFreeBytes AS MEMPTR    NO-UNDO.
    DEFINE RETURN  PARAMETER  iReturnVal                 AS LONG      NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-4 RECT-6 RECT-7 fiUserID bCancel ~
eStatus 
&Scoped-Define DISPLAYED-OBJECTS fiCredentials fiUserID fiPassword ~
fiTargetEnv fiTargetDB slEnvList slDBList fiOptions tbBackupASI tbBackupAUD ~
tbPurgeAudit fiPurgeLimit fiMins tbKeepFiles fiFromVersion fiToVersion ~
fiStatus eStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
    ( INPUT cVerString AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get64BitValue C-Win 
FUNCTION get64BitValue RETURNS DECIMAL
  ( INPUT m64 AS MEMPTR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD mTime C-Win
FUNCTION mTime RETURNS LOGICAL 
  (INPUT iNumSecs AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCancel AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 11 BY 1.91.

DEFINE BUTTON bUpdate 
     LABEL "Start Update" 
     SIZE 40 BY 1.91.

DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 77 BY 3.81 NO-UNDO.

DEFINE VARIABLE fiCredentials AS CHARACTER FORMAT "X(256)":U INITIAL " Credentials:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromVersion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Process will Upgrade From Version" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiMins AS CHARACTER FORMAT "X(256)":U INITIAL "mins.)" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "Options:" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "ASI-supplied Password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiPurgeLimit AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "(Limit purge to" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .95 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U INITIAL "Status/Help:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE fiTargetDB AS CHARACTER FORMAT "X(256)":U INITIAL " Target Database:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiTargetEnv AS CHARACTER FORMAT "X(256)":U INITIAL " Target Environment:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiToVersion AS CHARACTER FORMAT "X(256)":U 
     LABEL "To Version" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enter your name" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 3.33.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 3.57.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1
     FGCOLOR 3 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 4.52.

DEFINE RECTANGLE rStatusBar
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 10 BY 1
     BGCOLOR 3 FGCOLOR 3 .

DEFINE VARIABLE slDBList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 26 BY 2.38 NO-UNDO.

DEFINE VARIABLE slEnvList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 26 BY 2.38 NO-UNDO.

DEFINE VARIABLE tbBackupASI AS LOGICAL INITIAL yes 
     LABEL "Backup ASI database (before upgrade)" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE tbBackupAUD AS LOGICAL INITIAL yes 
     LABEL "Backup AUDIT database (before upgrade)" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE tbKeepFiles AS LOGICAL INITIAL no 
     LABEL "Keep files in Updates directory to re-apply to another Environment" 
     VIEW-AS TOGGLE-BOX
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE tbPurgeAudit AS LOGICAL INITIAL no 
     LABEL "Purge Audit records" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCredentials AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 70 NO-TAB-STOP 
     fiUserID AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 18
     bCancel AT ROW 2.19 COL 70 WIDGET-ID 16 NO-TAB-STOP 
     fiPassword AT ROW 3.38 COL 25 COLON-ALIGNED WIDGET-ID 20 PASSWORD-FIELD 
     fiTargetEnv AT ROW 5.05 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 72 NO-TAB-STOP 
     fiTargetDB AT ROW 5.05 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 74 NO-TAB-STOP 
     slEnvList AT ROW 6.24 COL 6 NO-LABEL WIDGET-ID 58
     slDBList AT ROW 6.24 COL 46 NO-LABEL WIDGET-ID 76
     fiOptions AT ROW 9.1 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 60 NO-TAB-STOP 
     tbBackupASI AT ROW 10.05 COL 14 WIDGET-ID 62
     tbBackupAUD AT ROW 11 COL 14 WIDGET-ID 64
     tbPurgeAudit AT ROW 11.95 COL 14 WIDGET-ID 66
     fiPurgeLimit AT ROW 11.95 COL 53 COLON-ALIGNED WIDGET-ID 80
     fiMins AT ROW 11.95 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 82 NO-TAB-STOP 
     tbKeepFiles AT ROW 12.91 COL 14 WIDGET-ID 68
     fiFromVersion AT ROW 14.57 COL 36 COLON-ALIGNED WIDGET-ID 38
     fiToVersion AT ROW 14.57 COL 61 COLON-ALIGNED
     bUpdate AT ROW 16 COL 23 WIDGET-ID 14
     fiStatus AT ROW 17.43 COL 2 COLON-ALIGNED NO-LABEL
     eStatus AT ROW 18.38 COL 4 NO-LABEL WIDGET-ID 52
     RECT-2 AT ROW 1.48 COL 2 WIDGET-ID 44
     RECT-4 AT ROW 5.52 COL 2 WIDGET-ID 48
     rStatusBar AT ROW 22.43 COL 5
     RECT-6 AT ROW 22.43 COL 5
     RECT-7 AT ROW 9.57 COL 2 WIDGET-ID 78
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
         TITLE              = "Advantzware Update"
         COLUMN             = 3
         ROW                = 1.48
         HEIGHT             = 22.91
         WIDTH              = 84
         MAX-HEIGHT         = 26.67
         MAX-WIDTH          = 84
         VIRTUAL-HEIGHT     = 26.67
         VIRTUAL-WIDTH      = 84
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
/* SETTINGS FOR BUTTON bUpdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCredentials IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiCredentials:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiFromVersion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMins IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiMins:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiOptions IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiOptions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiPassword IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPurgeLimit IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiStatus:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiTargetDB IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTargetDB:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiTargetEnv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTargetEnv:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiToVersion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rStatusBar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slDBList IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slEnvList IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbBackupASI IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbBackupAUD IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbKeepFiles IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbPurgeAudit IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Advantzware Update */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
        QUIT.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Advantzware Update */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        QUIT.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancel C-Win
ON CHOOSE OF bCancel IN FRAME DEFAULT-FRAME /* Exit */
OR CHOOSE OF bUpdate
    DO:
        DEFINE VARIABLE cFTPxmit AS CHARACTER NO-UNDO.
        
        ASSIGN 
            lBackupASI = tbBackupASI:CHECKED IN FRAME default-frame
            lBackupAudit = tbBackupAUD:CHECKED 
            lPurgeAudit = tbPurgeAudit:CHECKED 
            lKeepFiles = tbKeepFiles:CHECKED
            cFTPxmit = cEnvAdmin + "\FTPout.txt".
    
        CASE SELF:NAME:
            WHEN "bCancel" THEN 
                DO:
                    RUN ipStatus("User chose Exit button").
                    RUN ipStatus("  Cleaning work files").
            
                    OS-DELETE VALUE(cFTPInstrFile).
                    OS-DELETE VALUE(cFTPOutputFile).
                    OS-DELETE VALUE(cFTPErrFile).
                    OS-DELETE VALUE(c7zOutputFile).
                    OS-DELETE VALUE(c7zErrFile).
                    OS-DELETE VALUE(cEnvAdmin + "\" + cOutFile).
                    OS-DELETE VALUE(cFTPxmit).
                    OS-DELETE VALUE(cEnvAdmin + "\cOutputFile").
            
                    RUN ipStatus("Upgrade Complete.").
                    RUN ipStatus(" ").

                    APPLY 'close' TO THIS-PROCEDURE.
                    QUIT.
                END.
            WHEN "bUpdate" THEN 
                DO:
                    FIND FIRST ttUpdateHist NO-ERROR.
                    IF AVAIL ttUpdateHist THEN ASSIGN 
                        ttUpdateHist.toVersion = fiToVersion:{&SV}
                        ttUpdateHist.user_id = fiUserID:{&SV}.
                        
                    RUN ipStatus("User chose Start Update button").
            
                    RUN ipValidateChoices (OUTPUT lOKtoProceed).
                    IF NOT lOKtoProceed THEN 
                    DO:
                        RUN ipStatus("User made invalid choices for application").
                        RETURN.
                    END.
                    
                    RUN ipCheckDiskSpace (OUTPUT lOKtoProceed).
                    IF NOT lOKtoProceed THEN 
                    DO:
                        RUN ipStatus("Insufficient disk space for backup files").
                        APPLY 'close' TO THIS-PROCEDURE.
                        QUIT.
                    END.
                    
                    RUN ipStatus(" ").
                    RUN ipStatus("UPGRADING ENVIRONMENT " + slEnvList:{&SV}).
                    RUN ipStatus("  from version " + fiFromVersion:{&SV} + " to version: " + fiToVersion:{&SV}).
                    RUN ipStatus(" ").
    
                    RUN ipProcess.
            
                    IF lSuccess THEN DO:
                        ASSIGN 
                            cOutFile = cOutDir + "-SUCCESS.txt".
                        RUN ipSendVerification.
                        ASSIGN  
                            iStatus = 100.
                        RUN ipStatus("Upgrade Complete.").
                        MESSAGE 
                            "Congratulations! Your Advantzware update completed successfully."
                            VIEW-AS ALERT-BOX.
                        APPLY 'close' TO THIS-PROCEDURE.
                        QUIT.
                    END.
                    ELSE DO:
                        ASSIGN 
                            cOutFile = cOutDir + "-FAILED.txt".
                        RUN ipSendVerification.
                        RUN ipStatus("Upgrade Failed.").
                        MESSAGE 
                            "There was an issue with update processing." SKIP 
                            "Please contact Advantzware Support."
                            VIEW-AS ALERT-BOX.
                        APPLY 'close' TO THIS-PROCEDURE.
                        QUIT.
                    END.
                END.
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword C-Win
ON LEAVE OF fiPassword IN FRAME DEFAULT-FRAME /* ASI-supplied Password */
OR RETURN OF fiPassword
DO:
        IF SELF:{&SV} EQ "Boxco2020"
        OR SELF:{&SV} EQ "Boxco2020!" THEN ASSIGN 
            iUserLevel = 10.
        ELSE IF SELF:{&SV} EQ "installme" THEN ASSIGN 
            iUserLevel = 6.
        ELSE DO:
            MESSAGE 
                "You entered an invalid Password." SKIP 
                "Please re-enter or cancel."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                SELF:{&SV} = "".
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
        END. 
        
        ASSIGN 
            slEnvList:SENSITIVE = TRUE 
            slDbList:SENSITIVE = TRUE 
            tbBackupASI:SENSITIVE = TRUE
            tbBackupAUD:SENSITIVE = TRUE
            tbKeepFiles:SENSITIVE = TRUE
            tbPurgeAudit:SENSITIVE = TRUE
            bUpdate:SENSITIVE = TRUE.
            
        APPLY 'value-changed' TO slEnvList.
                    
        IF NUM-ENTRIES(slEnvList:LIST-ITEMS) GT 1 THEN 
            APPLY 'entry' TO slEnvList.
        ELSE IF NUM-ENTRIES(slDbList:LIST-ITEMS) GT 1 THEN
            APPLY 'entry' TO slDbList.
        ELSE 
            APPLY 'entry' TO bUpdate.
    
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON ENTRY OF fiUserID IN FRAME DEFAULT-FRAME /* Enter your name */
OR ENTRY OF fiPassword
OR ENTRY OF slEnvList
OR ENTRY OF bUpdate
OR ENTRY OF slDbList
DO:
    CASE SELF:NAME:
        WHEN "fiUserID" THEN 
            ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "HELP: Enter your name (not a UserID)" + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "fiPassword" THEN 
            ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "HELP: Enter a valid upgrade password" + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "slEnvList" THEN 
            ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "HELP: Select the environment to update" + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "slDbList" THEN 
            ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "HELP: Select the database to update" + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "bUpdate" THEN 
            ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "HELP: Click to start the update process" + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* Enter your name */
DO:
    IF SELF:{&SV} = "" THEN 
    DO:
        RUN ipStatus("  Blank UserID - ENTRY to Exit button").
        APPLY 'entry' TO bCancel.
        RETURN NO-APPLY.
    END.

    IF SELF:{&SV} EQ "asi" THEN ASSIGN 
        iUserLevel = 10.
    ELSE ASSIGN 
        iUserLevel = 6.
        
    RUN ipStatus("  Entered UserID " + SELF:{&SV}).

    ASSIGN 
        fiPassword:SENSITIVE = TRUE.
    APPLY 'entry' TO fiPassword.
    RETURN NO-APPLY.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slEnvList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slEnvList C-Win
ON VALUE-CHANGED OF slEnvList IN FRAME DEFAULT-FRAME
OR VALUE-CHANGED OF slDbList
DO:
    CASE SELF:NAME:
        WHEN "slEnvList" THEN DO:
            ASSIGN
                iIndex = LOOKUP(SELF:{&SV},SELF:LIST-ITEMS)
                fiFromVersion:{&SV} = ENTRY(iIndex,cEnvVerList)
                iCurrEnvVer = fIntVer(fiFromVersion:{&SV})
                ENTRY(3,cOutDir,"-") = SELF:{&SV}
                slDbList:LIST-ITEMS = "".
            DO ictr = 1 TO NUM-ENTRIES(cDbVerList):
                IF ENTRY(iCtr,cDbVerList) EQ fiFromVersion:{&SV} THEN DO: 
                    slDbList:ADD-LAST(ENTRY(iCtr,cDbList)).
                END.                
            END.
            IF NUM-ENTRIES(slDbList:LIST-ITEMS) EQ 1 THEN DO:
                ASSIGN 
                    slDbList:{&SV} = ENTRY(1,slDbList:LIST-ITEMS).
                APPLY 'value-changed' TO slDbList.
            END.
        END.
        WHEN "slDbList" THEN DO:
            ASSIGN 
                iIndex = LOOKUP(slDbList:{&SV},cDbList)
                cAsiDbName = ENTRY(iIndex,cDBList)
                cAsiDbLongName = cDbDir + "\" + ENTRY(iIndex,cDbDirList) + "\" + cAsiDbName + ".db"
                iAsiDbPort = INTEGER(ENTRY(iIndex,cDBPortList))
                cAsiDbDir = ENTRY(iIndex,cDBDirList)
                cAudDbName = ENTRY(iIndex,cAudDbList)
                cAudDbLongName = cDbDir + "\" + ENTRY(iIndex,cAudDirList) + "\" + cAudDbName + ".db"
                iAudDbPort = INTEGER(ENTRY(iIndex,cAudPortList))
                cAudDbDir = ENTRY(iIndex,cAudDirList)
                .
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbPurgeAudit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbPurgeAudit C-Win
ON VALUE-CHANGED OF tbPurgeAudit IN FRAME DEFAULT-FRAME /* Purge Audit records */
DO:
    IF SELF:CHECKED THEN ASSIGN 
        fiPurgeLimit:SENSITIVE = TRUE 
        fiPurgeLimit:{&SV} = IF fiPurgeLimit:{&SV} EQ "0" THEN "30" ELSE fiPurgeLimit:{&SV}.
    ELSE ASSIGN 
        fiPurgeLimit:SENSITIVE = FALSE
        fiPurgeLimit:{&SV} = "0".
     
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
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    
    DEFINE VARIABLE deEnvVer AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lNeedDBWork AS LOG NO-UNDO.
    DEFINE VARIABLE lGoodNos AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lGoodList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rc AS INTEGER NO-UNDO.
    
    /* This window will ALWAYS be on top of other windows */
    RUN BringWindowToTop (c-Win:HWND, OUTPUT rc).
    
    /* There should not be any connected DBs at this point.  If there are, disconnect */
    DO ictr = 1 TO NUM-DBS:
        DISCONNECT VALUE (LDBNAME(ictr)).
    END. 
    
    RUN ipFindIniFile ("..\advantzware.ini",
                       OUTPUT cIniLoc).
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    ELSE DO:
        ASSIGN 
            lKeepFiles = TRUE.        
        RUN ipSetKeepFlag.
        RETURN.    
    END.
    
    RUN ipExpandVarNames.
        
    ASSIGN
        slEnvList:LIST-ITEMS = cEnvList
        slEnvList:{&SV} = ENTRY(1,slEnvList:LIST-ITEMS)
        iIndex              = 1
        fiFromVersion:{&SV} = ENTRY(iIndex,cEnvVerList)
        iCurrEnvVer         = fIntVer(fiFromVersion:{&SV})
        iCurrDbVer          = fIntVer(ENTRY(iIndex,cDBVerList))
        iCurrAudVer         = fIntVer(ENTRY(iIndex,cAudVerList))
        cAsiDbName          = ENTRY(iIndex,cDBList)
        cAudDbName          = ENTRY(iIndex,cAudDbList)
        cAsiDbLongName      = cDbDir + "\" + ENTRY(iIndex,cDbDirList) + "\" + cAsiDbName
        cAudDbLongName      = cDbDir + "\" + ENTRY(iIndex,cAudDirList) + "\" + cAudDbName
        iStatus             = 2
        lSuccess            = TRUE 
        .

    RUN ipGetPatchList.
    IF NOT lSuccess THEN DO:
        ASSIGN 
            lKeepFiles = TRUE.        
        RUN ipSetKeepFlag.
        RETURN.    
    END.    

    ASSIGN
        bUpdate:SENSITIVE = FALSE
        cIpAddress     = "34.203.15.64"
        cFtpUser       = "ftptest"
        cFtpPassword   = "TestFTP1!"
        cInstallerFile = "*.7z"
        cFtpOutputFile = cEnvAdmin + "\ftpOutput.txt"
        cFtpErrFile    = cEnvAdmin + "\ftpErrs.txt"
        c7ZOutputFile  = cEnvAdmin + "\7zOutput.txt"
        c7ZErrFile     = cEnvAdmin + "\7zErrs.txt"
        cFtpInstrFile  = cEnvAdmin + "\ftpInstr.txt"
        cOutDir = cSiteName + "-" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "-" + slEnvList:SCREEN-VALUE
        iStatus = 3
        .    
    
    EMPTY TEMP-TABLE ttUpdateHist.
    EMPTY TEMP-TABLE ttUpdateLog. 
    CREATE ttUpdateHist.
    ASSIGN 
        ttUpdateHist.fromVersion = fiFromVersion:{&SV}
        ttUpdateHist.applyDate = TODAY 
        ttUpdateHist.startTimeInt = INT(TIME)
        ttUpdateHist.startTime = STRING(TIME,"HH:MM:SS AM")
        ttUpdateHist.success = FALSE.

    ASSIGN 
        iStatus = 4.
    RUN ipStatus("Initialize").
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
    QUIT.
END.

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
  DISPLAY fiCredentials fiUserID fiPassword fiTargetEnv fiTargetDB slEnvList 
          slDBList fiOptions tbBackupASI tbBackupAUD tbPurgeAudit fiPurgeLimit 
          fiMins tbKeepFiles fiFromVersion fiToVersion fiStatus eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-4 RECT-6 RECT-7 fiUserID bCancel eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCheckDiskSpace C-Win 
PROCEDURE ipCheckDiskSpace :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER oplOkToProceed AS LOG.
    
    DEF VAR iDiskFreeSpace AS INT NO-UNDO.
    DEF VAR iDiskTotalSpace AS INT NO-UNDO.
    DEF VAR iAsiBakSize AS INT NO-UNDO.
    DEF VAR iAudBakSize AS INT NO-UNDO.
    
    RUN ipGetDiskSpace (cDrive,
                       "MB",
                       OUTPUT iDiskFreeSpace,
                       OUTPUT iDiskTotalSpace).
                       
    IF NOT lBackupASI 
    AND NOT lBackupAudit THEN DO:
        ASSIGN 
            oplOKToProceed = TRUE.
        RETURN.
    END. 
    
/*    RUN ipStatus("  Calculating estimated DB Backup size").                                                        */
/*    RUN ipGetDbBakSize (cAsiDbName, cAsiDbLongName, OUTPUT iAsiBakSize).                                           */
/*    RUN ipGetDbBakSize (cAudDbName, cAudDbLongName, OUTPUT iAudBakSize).                                           */
/*    RUN ipStatus("  Estimated DB Backup size is " + STRING(iAsiBakSize + iAudBakSize,">>>,>>9.9<" ) + " MB").      */
/*                                                                                                                   */
/*    IF iDiskFreeSpace - iAsiBakSize - iAudBakSize - 2000 LT 0 THEN DO:                                             */
/*        MESSAGE                                                                                                    */
/*            "There is a problem with disk space on this server.  " +                                               */
/*            "Please provide a minimum of " + TRIM(STRING((2000 + iAsiBakSize + iAudBakSize) / 1024,">>>,>>9.9<")) +*/
/*            " GB of free space before continuing."                                                                 */
/*            VIEW-AS ALERT-BOX ERROR.                                                                               */
/*        ASSIGN                                                                                                     */
/*            oplOKToProceed = FALSE.                                                                                */
/*    END.                                                                                                           */
/*    ELSE                                                                                                           */
    ASSIGN
        oplOKToProceed = TRUE.
                           
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
        lConnectAudit = IF INDEX(cConnectAudit,"Y") NE 0 OR INDEX(cConnectAudit,"T") NE 0 THEN TRUE ELSE FALSE
        cLockoutTries = SUBSTRING(cLockoutTries,1,1)
        iLockoutTries = IF cLockoutTries NE "" 
                        AND ASC(cLockoutTries) GE 48
                        AND ASC(cLockoutTries) LE 57 THEN INT(cLockoutTries) ELSE 0
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetDbBakSize C-Win 
PROCEDURE ipGetDbBakSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDbName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcDbLongName AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opiBakSize AS INT NO-UNDO.
    
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLine AS CHAR NO-UNDO.
    DEF VAR iSize AS DECI NO-UNDO.
    DEF VAR cUnit AS CHAR NO-UNDO.
    DEF VAR lServed AS LOG NO-UNDO.
    DEF VAR iSeed AS INT NO-UNDO.
    DEF VAR cFile1 AS CHAR NO-UNDO.
    DEF VAR cFile2 AS CHAR NO-UNDO.
    
    ASSIGN 
        iSize = 0
        lServed = FALSE 
        iSeed = (TIME * 1000) + RANDOM(1,998)
        cFile1 = "c:\tmp\bak" + STRING(iSeed,"99999999") + ".txt"
        cFile2 = "c:\tmp\bak" + STRING ((iSeed + 1),"99999999") + ".txt".        

    RUN ipStatus("    Calculating estimated " + ipcDbName + " Backup size").

    ASSIGN 
        cCmdLine = cDLCDir + "\bin\_dbutil probkup " + ipcDbLongName + " c:\tmp\dbname.bak -estimate > " + cFile1 + " && exit".

    OS-COMMAND SILENT VALUE(cCmdLine).

    {&Won}
    DO WHILE iSize = 0 AND lServed = FALSE:
        INPUT FROM VALUE(cFile1).
        REPEAT:
            IMPORT UNFORMATTED cLine.
            IF INDEX(cLine,"multi-user") NE 0 
            OR INDEX(cLine,"errno = 2") NE 0 THEN ASSIGN 
                    lServed = TRUE. 
            ELSE IF cLine BEGINS "Backup requires" THEN ASSIGN
                        iSize = DECIMAL(ENTRY(5,cLine," "))
                        cUnit = ENTRY(6,cLine," ").
        END.
        INPUT CLOSE.
    END.
    IF lServed THEN 
    DO:
        ASSIGN 
            cCmdLine = cDLCDir + "\bin\_mprshut " + ipcDbLongName + " -C backup online NUL -verbose > " + cFile2.

        OS-COMMAND SILENT VALUE(cCmdLine).
        DO WHILE iSize = 0:
            INPUT FROM VALUE(cFile2).
            REPEAT:
                IMPORT UNFORMATTED cLine.
                IF INDEX(cLine,"errno = 2") NE 0 THEN 
                    LEAVE. 
                IF cLine BEGINS "Backup requires" THEN ASSIGN 
                        iSize = DECIMAL(ENTRY(5,cLine," "))
                        cUnit = ENTRY(6,cLine," ").
            END.
            INPUT CLOSE.
        END.
        {&Woff}
    END.
        
    STATUS DEFAULT "".   

    IF cUnit BEGINS "K" THEN ASSIGN 
            iSize = iSize / 1024.
    ELSE IF cUnit BEGINS "G" THEN ASSIGN 
                iSize = iSize * 1024.
        
    CREATE ttBackupSize.
    ASSIGN 
        ttBackupSize.ttType = "DB" 
        ttBackupSize.ttID = ipcDbName
        ttBackupSize.ttSize = iSize.


    {&Won}
    DO WHILE SEARCH(cFile1) NE ?:
        OS-DELETE VALUE(cFile1).
        OS-DELETE VALUE(cFile2).
    END.
    {&Woff}
    ASSIGN 
        opiBakSize = iSize.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetDiskSpace C-Win 
PROCEDURE ipGetDiskSpace :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip_drive   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip_unit    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDiskFreeSpace    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDiskTotalSpace   AS DECIMAL   NO-UNDO.
    
    DEF VAR cDivisor AS INT NO-UNDO.
    DEF VAR iMem1 AS MEMPTR NO-UNDO.
    DEF VAR iMem2 AS MEMPTR NO-UNDO.
    DEF VAR iMem3 AS MEMPTR NO-UNDO.
    DEF VAR iReturnVal AS INT NO-UNDO.
    DEF VAR iDiskFreeSpace AS DECIMAL NO-UNDO.
    DEF VAR iDiskTotalSpace AS DECIMAL NO-UNDO.

    IF CAN-DO("KB,Kilo,Kilobyte,Kilobytes", ip_unit)
        THEN cDivisor = 1024.
    ELSE
        IF CAN-DO("MB,Mega,Megabyte,Megabytes", ip_unit)
            THEN cDivisor = 1024 * 1024.
        ELSE
            IF CAN-DO("GB,Giga,Gigabyte,Gigabytes", ip_unit)
                THEN cDivisor = 1024 * 1024 * 1024.
            ELSE cDivisor = 1.
 
    /* No directory specified? Then use the current directory */
    IF (ip_drive = "") OR (ip_drive=?) THEN 
    DO:
        FILE-INFO:FILE-NAME = ".".
        ip_drive = FILE-INFO:FULL-PATHNAME.
    END.
 
    /* If a UNC name was specified, make sure it ends with a backslash ( \\drive\share\dir\ )
       This won't hurt for a mapped drive too */
    IF SUBSTR(ip_drive, LENGTH(ip_drive), 1) NE "\"
        THEN ip_drive = ip_drive + "\".
 
    SET-SIZE(iMem1) = 8.  /* 64 bit integer! */
    SET-SIZE(iMem2) = 8.
    SET-SIZE(iMem3) = 8.
 
    RUN GetDiskFreeSpaceExA ( ip_drive + CHR(0),
        OUTPUT iMem1,
        OUTPUT iMem2,
        OUTPUT iMem3,
        OUTPUT iReturnVal  ).
    IF iReturnVal NE {&WTRUE} THEN 
    DO:
        iDiskFreeSpace = ?.
        iDiskTotalSpace = ?.
    END.
    ELSE 
    DO:
        ASSIGN
            iDiskFreeSpace  = TRUNC( get64BitValue(iMem3) / cDivisor, 3)
            iDiskTotalSpace = TRUNC( get64BitValue(iMem2) / cDivisor, 3)
            opdDiskFreeSpace = iDiskFreeSpace
            opdDiskTotalSpace = iDiskTotalSpace.
    END.
 
    SET-SIZE(iMem1) = 0.
    SET-SIZE(iMem2) = 0.
    SET-SIZE(iMem3) = 0.


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

    IF SEARCH(cUpdatesDir + "\patch.mft") EQ ? THEN DO:
        MESSAGE 
            "Unable to locate the patch manifest file."
            VIEW-AS ALERT-BOX ERROR.
        lSuccess = FALSE.
        RETURN.
    END. 
    
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
        fiToVersion:{&SV} = cPatchList
        iPatchDbVer  = fIntVer(cAsiDbVer)
        iPatchAudVer  = fIntVer(cAudDbVer)
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcess C-Win 
PROCEDURE ipProcess :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConnect AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAudDB AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iListItem AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEnvVer AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEnv AS INTEGER NO-UNDO.
    DEFINE VARIABLE cFileToRun AS CHARACTER NO-UNDO.
        
    ASSIGN 
        lSuccess = TRUE.

    IF lKeepFiles THEN        
        RUN ipSetKeepFlag.

    IF iCurrDbVer LT iPatchDbVer
    OR iCurrAudVer LT iPatchAudVer 
    OR lBackupASI 
    OR lBackupAudit
    THEN 
    DO:
        ASSIGN 
            iStatus = 5.
        RUN ipStatus("Database requires upgrade/backup...").
        
        ASSIGN
            iEnv = LOOKUP (slEnvList:{&SV},slEnvList:list-items)
            lSuccess = FALSE 
            c-Win:VISIBLE = FALSE. 
        
        IF SEARCH("asiUpdateDB.r") NE ? 
        OR SEARCH("asiUpdateDB.w") NE ? THEN ASSIGN 
            cFileToRun = "asiUpdateDB.w".
        ELSE ASSIGN 
            cFileToRun = "N:\Repository\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateDB.w".
 
        /* Give the OS time to close the log file so next pgm can re-open it */
        ETIME(YES).
        DO WHILE ETIME LE 2000:
            ASSIGN 
                iCtr = iCtr.
        END.
        
        ASSIGN
            c-Win:VISIBLE = FALSE.

        RUN VALUE(cFileToRun) (
            cAsiDbName,
            cAsiDbLongName,
            iAsiDbPort,
            cAsiDbDir,
            cAudDbName,
            cAudDbLongName,
            iAudDbPort,
            cAudDbDir,
            iCurrDbVer,
            iPatchDbVer,
            iCurrAudVer,
            iPatchAudVer,
            iUserLevel,
            lBackupASI,
            lBackupAudit,
            cLogFile,
            OUTPUT lSuccess,
            INPUT-OUTPUT iStatus
            ).

        ASSIGN
            c-Win:VISIBLE = TRUE
            iStatus = 20.
        
        mTime(2).
                    
        RUN ipStatus("Return from asiUpdateDB.w").

        IF lSuccess THEN 
        DO:
            /* asiUpdateDB could change audit variables in ini file; must reread */
            RUN ipReadIniFile.
            RUN ipExpandVarNames.
            EMPTY TEMP-TABLE ttDatabases.
            DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                CREATE ttDatabases.
                ASSIGN
                    ttDatabases.cName = ENTRY(iCtr,cDBList)
                    ttDatabases.cDir = ENTRY(iCtr,cDbDirList)
                    ttDatabases.cPort = ENTRY(iCtr,cDBPortList)
                    ttDatabases.cVer = ENTRY(iCtr,cDBVerList)
                    ttDatabases.cAudName = ENTRY(iCtr,cAudDbList)
                    ttDatabases.cAudPort = ENTRY(iCtr,cAudPortList)
                    ttDatabases.cAudVer = ENTRY(iCtr,cAudVerList).
            END.
            ASSIGN 
                lBackupASI = FALSE 
                lBackupAudit = FALSE. 
        END. 
        ELSE 
        DO:
            MESSAGE 
                "The system upgrade was NOT successful.  Please" SKIP  
                "contact Advantzware technical support for assistance."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.   
        END.
    END.
  
    DO iCtr = 1 TO NUM-ENTRIES(cDbList):
        CREATE ttDatabases.
        ASSIGN
            ttDatabases.cName = ENTRY(iCtr,cDBList)
            ttDatabases.cDir = ENTRY(iCtr,cDbDirList)
            ttDatabases.cPort = ENTRY(iCtr,cDBPortList)
            ttDatabases.cVer = ENTRY(iCtr,cDBVerList)
            ttDatabases.cAudName = ENTRY(iCtr,cAudDbList)
            ttDatabases.cAudPort = ENTRY(iCtr,cAudPortList)
            ttDatabases.cAudVer = ENTRY(iCtr,cAudVerList).
    END.
        
    ASSIGN 
        iStatus = 23.
    RUN ipStatus("Connecting databases").
    RUN ipStatus("  Connecting ASI DB with statement...").
    ASSIGN
        cConnect = "-db " + cAsiDbName + 
                   " -H " + chostName +
                   " -S " + STRING(iAsiDbPort,"9999") +
                   " -N tcp -ld ASI".
    RUN ipStatus("    " + cConnect).

    CONNECT VALUE(cConnect) NO-ERROR.

    IF NOT CONNECTED ("asi") THEN 
    DO:
        RUN ipStatus("  ASI DB connection failed.  Cancelling.").
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN ipStatus ("    " + ERROR-STATUS:GET-MESSAGE(iCtr)).
        END.
        MESSAGE 
            "Unable to connect to the asi database.  Cannot continue." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            lSuccess = FALSE.
        RETURN.
    END.
    ASSIGN 
        iStatus = 24.
        
    RUN ipStatus("  Connecting Audit DB with statement...").
    ASSIGN
        cConnect = "-db " + cAudDbName + 
                   " -H " + chostName +
                   " -S " + STRING(iAudDbPort,"9999") +
                   " -N tcp -ld Audit".
    RUN ipStatus("    " + cConnect).
    CONNECT VALUE(cConnect) NO-ERROR.
    MTIME(3).
    IF NOT CONNECTED ("audit") THEN 
    DO:
        RUN ipStatus("  Audit DB connection failed.  Cancelling.").
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN ipStatus ("    " + ERROR-STATUS:GET-MESSAGE(iCtr)).
        END.
        MESSAGE 
            "Unable to connect to the audit database.  Cannot continue." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            lSuccess = FALSE.
        RETURN.
    END.

    IF SEARCH("asiUpdateENV.r") NE ? 
        OR SEARCH("asiUpdateENV.w") NE ? THEN ASSIGN 
            cFileToRun = "asiUpdateENV.w".
    ELSE ASSIGN 
            cFileToRun = "N:\Repository\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateENV.w".

    CREATE ALIAS "DICTDB" FOR DATABASE asi.
    
    ASSIGN iStatus = 25.
    RUN ipStatus("Initiating asiUpdateENV.w").

    /* Give the OS time to close the log file so next pgm can re-open it */
    mTime(2).

    ASSIGN
        c-Win:VISIBLE = FALSE. 
    RUN VALUE(cFileToRun) (
        cAsiDbName,
        iAsiDbPort,
        cAsiDbDir,
        cAsiDbVer,
        cAudDbName,
        iAudDbPort,
        cAudDbDir,
        cAudDbVer,
        slEnvList:{&SV},
        fiFromVersion:{&SV},
        fiToVersion:{&SV},
        iUserLevel,
        lPurgeAudit,
        INTEGER(fiPurgeLimit:{&SV}),
        cLogFile,
        OUTPUT lSuccess,
        INPUT-OUTPUT iStatus).
        
    /* Give the OS time to close the log file so this pgm can re-open it */
    mTime(2).

    ASSIGN
        THIS-PROCEDURE:CURRENT-WINDOW = c-Win
        c-Win:VISIBLE = TRUE
        iStatus = 95.
    mTime(2).
             
    RUN ipStatus("Return from asiUpdateENV.w").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSendVerification C-Win 
PROCEDURE ipSendVerification :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFTPxmit AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFtpCmd AS CHARACTER NO-UNDO.

    OUTPUT STREAM outFile TO VALUE(cOutFile) APPEND.
    INPUT STREAM logFile FROM VALUE(cLogFile).
    REPEAT:
        IMPORT STREAM logFile UNFORMATTED cLine.
        PUT STREAM outFile UNFORMATTED "  " + cLine + CHR(10).
    END.
    INPUT STREAM logFile CLOSE.
    OUTPUT STREAM outFile CLOSE.
    
    ASSIGN
        cFTPxmit = cEnvAdmin + "\FTPout.txt"
        cFtpCmd = cEnvDir + "\" + slEnvList:{&SV} + "\Resources\WinSCP\winscp.com /ini=nul /log=" + 
                  cFtpOutputFile + " /script=" + cFtpXmit.
        
    RUN ipStatus("  Building FTP Transmit file").
    RUN ipStatus("    File: " + cFTPxmit).
    RUN ipStatus("    FTP Addr: " + cIpAddress).
    
    OUTPUT STREAM sInstr TO VALUE(cFTPxmit).
    PUT STREAM sInstr UNFORMATTED "OPEN ftp://ftpTest:TestFTP1!@34.203.15.64" SKIP.
    PUT STREAM sInstr UNFORMATTED "CD Results" SKIP.
    PUT STREAM sInstr UNFORMATTED "MKDIR " + cOutDir SKIP.
    PUT STREAM sInstr UNFORMATTED "CD " + cOutDir SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cOutFile SKIP.

    PUT STREAM sInstr UNFORMATTED "MKDIR EnvFiles" SKIP.
    PUT STREAM sInstr UNFORMATTED "CD EnvFiles" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cAdminDir + "\advantzware.ini" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cAdminDir + "\advantzware.usr" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cEnvAdmin + "\advantzware.pf" SKIP.
    PUT STREAM sInstr UNFORMATTED "CD .." SKIP.

    PUT STREAM sInstr UNFORMATTED "MKDIR DbFiles" SKIP.
    PUT STREAM sInstr UNFORMATTED "CD DbFiles" SKIP.
    PUT STREAM sInstr UNFORMATTED "MPUT " + cDbProdDir + "\*.lg" SKIP.
    PUT STREAM sInstr UNFORMATTED "MPUT " + cDbTestDir + "\*.lg" SKIP.
    PUT STREAM sInstr UNFORMATTED "MPUT " + cDbAuditDir + "\*.lg" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cDLCDir + "\properties\AdminServerPlugins.properties" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cDLCDir + "\properties\conmgr.properties" SKIP.
    PUT STREAM sInstr UNFORMATTED "CD .." SKIP.

    PUT STREAM sInstr UNFORMATTED "MKDIR " + "DataFiles" SKIP.
    PUT STREAM sInstr UNFORMATTED "CD " + "DataFiles" SKIP.
    PUT STREAM sInstr UNFORMATTED "MPUT " + cUpdDataDir + "\*.OLD" SKIP.
    PUT STREAM sInstr UNFORMATTED "MPUT " + cUpdDataDir + "\*.NEW" SKIP.
    
    PUT STREAM sInstr UNFORMATTED "exit".
    OUTPUT STREAM sInstr CLOSE.
    
    IF SEARCH(cFtpxmit) EQ ? THEN 
    DO:
        RUN ipStatus("  FTP Transmit File build failed. Aborting...").
        APPLY 'choose' TO bCancel IN FRAME {&FRAME-NAME}.
    END.

    RUN ipStatus("  Starting FTP session").
    OS-COMMAND SILENT VALUE(cFtpCmd).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetKeepFlag C-Win 
PROCEDURE ipSetKeepFlag :
/*------------------------------------------------------------------------------
 Purpose: This creates a file that the upgrade batch file will use to determine whether or not to remove 
           upgrade files when complete
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT STREAM sKeep TO VALUE(cEnvAdmin + "\KeepFiles.txt").
    PUT STREAM sKeep UNFORMATTED "keep".
    OUTPUT STREAM sKeep CLOSE.

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
    DEFINE INPUT PARAMETER ipcStatus AS CHARACTER NO-UNDO.
    DEF VAR cLogLine AS CHAR NO-UNDO.
                
    ASSIGN
        cLogFile = cEnvAdmin + "\UpdateLog.txt".

    IF ipcStatus = "Initialize" THEN 
    DO:
        OUTPUT STREAM logStream TO VALUE(cLogFile).

        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED "Customer Site: " + cSiteName + CHR(10).
        PUT STREAM logStream UNFORMATTED "Host Server: " + cHostName + CHR(10).
        PUT STREAM logStream UNFORMATTED "Environments: " + cEnvList + CHR(10).
        PUT STREAM logStream UNFORMATTED "Env. Versions: " + cEnvVerList + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Date: " + STRING(TODAY,"99/99/99") + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Time: " + STRING(TIME,"HH:MM:SS") + CHR(10).
        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED CHR(10).
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            "Initializing log" FORMAT "x(160)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
        INPUT STREAM logStream FROM VALUE(cLogFile).
        REPEAT:
            CREATE ttUpdateLog.
            IMPORT STREAM logStream ttUpdateLog.cLine.
        END.
        INPUT STREAM logStream CLOSE.
        FOR EACH ttUpdateLog:
            FIND FIRST ttUpdateHist NO-ERROR.
            IF AVAIL ttUpdateHist THEN ASSIGN 
                ttUpdateHist.updLog = ttUpdateHist.updLog + ttUpdateLog.cLine + CHR(10).
        END.   
        EMPTY TEMP-TABLE ttUpdateLog.
        ASSIGN 
            ttUpdateHist.applyDate = TODAY 
            ttUpdateHist.startTimeInt = INT(TIME)
            ttUpdateHist.startTime = STRING(TIME,"HH:MM:SS AM").
    END.
    ELSE 
    DO:
        ASSIGN
            eStatus:{&SV} = eStatus:{&SV} + ipcStatus + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.

        IF INDEX(ipcStatus,"duplicate") EQ 0 THEN 
        DO:
            ASSIGN
                iMsgCtr = iMsgCtr + 1
                cMsgStr[iMsgCtr] = ipcStatus.
            FIND FIRST ttUpdateHist NO-ERROR.
            IF AVAIL ttUpdateHist THEN ASSIGN 
                ttUpdateHist.updLog = ttUpdateHist.updLog + STRING(TODAY,"99/99/99") + "  " + STRING(TIME,"HH:MM:SS") + "  " + cMsgStr[iMsgCtr] + CHR(10)
                ttUpdateHist.endTimeInt = INT(TIME)
                ttUpdateHist.endTime = STRING(TIME,"HH:MM:SS AM")
                ttUpdateHist.success = lSuccess.        
            OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
            PUT STREAM logStream
                STRING(TODAY,"99/99/99") AT 1
                STRING(TIME,"HH:MM:SS") AT 12
                cMsgStr[iMsgCtr] FORMAT "x(160)" AT 25
                SKIP.
            OUTPUT STREAM logStream CLOSE.
        END.
    END.

    IF CONNECTED("asi") 
    AND ipcStatus EQ "Upgrade Complete." THEN 
        RUN asiUpdateHist.p (cEnvDir + "\" + slEnvList:{&SV}).

    ASSIGN 
        rStatusBar:WIDTH IN FRAME {&frame-name} = MIN(75,(iStatus / 100) * 75).
                       
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValidateChoices C-Win 
PROCEDURE ipValidateChoices :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER lOK AS LOG NO-UNDO.
 /*   
    IF iCurrEnvVer GT iPatchEnvVer THEN 
    DO:
        MESSAGE
            "You have chosen to apply a patch OLDER than your current version.  " +
            "This is not allowed in this version of the automated update.  " +
            "Please contact Advantzware Support for more assistance."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN
            lOK = NO.
        RETURN.
    END.
    ELSE IF iCurrEnvVer EQ iPatchEnvVer THEN 
        DO:
            MESSAGE
                "You have chosen to apply a patch that is EQUAL to your current " +
                "version. This is allowed in this version of the automated update, " +
                "but should only be performed under certain circumstances.  Are you sure?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
            ASSIGN
                lOK = lSure.
            RETURN.
        END.
        ELSE */ ASSIGN
                lOK = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValidUser C-Win 
PROCEDURE ipValidUser :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplValidUser AS LOG.
    DEFINE VARIABLE cInpString AS CHARACTER.
    
    ASSIGN
        oplValidUser = FALSE.
    cUsrLoc = REPLACE(cIniLoc,".ini",".usr").
    
    INPUT FROM VALUE(cUsrLoc).
    REPEAT:
        IMPORT UNFORMATTED cInpString.
        IF ENTRY(2,cInpString,"|") NE "*" THEN 
        DO:
            RUN ipStatus("  User name NOT validated. Usr file problem.").
            MESSAGE
                "Can't validate user with this version" SKIP
                "of the system. Continuing..."
                VIEW-AS ALERT-BOX.
            ASSIGN
                oplValidUser = TRUE.
            RETURN.
        END.
        IF ENTRY(1,cInpString,"|") EQ fiUserID:{&SV} THEN 
        DO:
            RUN ipStatus("  User name validated.").
            ASSIGN
                oplValidUser = TRUE.
            RETURN.
        END.
    END.
    IF NOT oplValidUser THEN 
        RUN ipStatus("  User name failed validation.").
    
    RETURN.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
    ( INPUT cVerString AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Converts a version string like "16.4.8" or "16.7.12.2" to an integer
        Notes:  In the cases above, these would be 16040800 and 16071202
                Useful for version comparisons
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cStrVal AS CHARACTER EXTENT 4 NO-UNDO.
    DEFINE VARIABLE iIntVal AS INTEGER EXTENT 4 NO-UNDO.
    DEFINE VARIABLE iIntVer AS INTEGER NO-UNDO.
    ASSIGN
        cStrVal[1] = ENTRY(1,cVerString,".")
        cStrVal[2] = ENTRY(2,cVerString,".")
        cStrVal[3] = IF NUM-ENTRIES(cVerString,".") GT 2 THEN ENTRY(3,cVerString,".") ELSE "0"
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GT 3 THEN ENTRY(4,cVerString,".") ELSE "0"
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get64BitValue C-Win 
FUNCTION get64BitValue RETURNS DECIMAL
  ( INPUT m64 AS MEMPTR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* constant 2^32 */
    &SCOPED-DEFINE BigInt 4294967296
 
    DEFINE VARIABLE d1 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE d2 AS DECIMAL    NO-UNDO.
 
    d1 = GET-LONG(m64, 1).
    IF d1 < 0 
        THEN d1 = d1 + {&BigInt}.
 
    d2 = GET-LONG(m64, 5).
    IF d2 < 0 
        THEN d2 = d2 + {&BigInt}.
 
    IF d2 GT 0
        THEN d1 = d1 + (d2 * {&BigInt}).
 
    RETURN d1.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION mTime C-Win
FUNCTION mTime RETURNS LOGICAL 
  ( INPUT iNumSecs AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
    
    ETIME(TRUE).
    DO WHILE ETIME LE iNumSecs * 1000:
        ASSIGN 
            iCtr = iCtr.
    END.
    
    RETURN lresult.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


