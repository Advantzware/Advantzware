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
&SCOPED-DEFINE SV SCREEN-VALUE IN FRAME DEFAULT-FRAME

{iniFileVars.i}

DEFINE STREAM s1.
DEFINE STREAM sInstr.
DEFINE STREAM outFile.
DEFINE STREAM logFile.
DEFINE STREAM outStream.
DEFINE STREAM logStream.
DEFINE STREAM iniStream.

DEFINE TEMP-TABLE ttDatabases
    FIELD cName AS CHARACTER
    FIELD cDir AS CHARACTER
    FIELD cPort AS CHARACTER
    FIELD cVer AS CHARACTER
    FIELD cAudName AS CHARACTER
    FIELD cAudPort AS CHARACTER
    FIELD cAudVer AS CHARACTER.


DEFINE VARIABLE c7ZErrFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE c7ZOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsiDbVer AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE cMsgStr AS CHARACTER FORMAT "x(80)" EXTENT 100 NO-UNDO.
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
DEFINE VARIABLE iAsiDbVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iAudDbVer AS INTEGER NO-UNDO.
DEFINE VARIABLE iStatus AS INTEGER NO-UNDO.
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
DEFINE VARIABLE lConnectAudit AS LOG NO-UNDO.
DEFINE VARIABLE lCorrupt AS LOG NO-UNDO.
DEFINE VARIABLE lFoundIni AS LOG NO-UNDO.
DEFINE VARIABLE lFoundUsr AS LOG NO-UNDO.
DEFINE VARIABLE lHeader AS LOG NO-UNDO.
DEFINE VARIABLE ll-ans AS LOG NO-UNDO.
DEFINE VARIABLE lMakeBackup AS LOG NO-UNDO.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-4 RECT-6 fiUserID bCancel ~
fiPassword slEnvList eStatus 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiPassword slEnvList ~
fiFromVersion fiToVersion eStatus 

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
     SIZE 75 BY 3.81 NO-UNDO.

DEFINE VARIABLE fiFromVersion AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Version" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiToVersion AS CHARACTER FORMAT "X(256)":U 
     LABEL "To Version" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.33.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.86.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.43
     FGCOLOR 3 .

DEFINE RECTANGLE rStatusBar
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 10 BY 1.43
     BGCOLOR 3 FGCOLOR 3 .

DEFINE VARIABLE slEnvList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY 1.67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiUserID AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 18
     bCancel AT ROW 2.19 COL 63 WIDGET-ID 16 NO-TAB-STOP 
     fiPassword AT ROW 3.38 COL 19 COLON-ALIGNED WIDGET-ID 20 PASSWORD-FIELD 
     slEnvList AT ROW 6.24 COL 21 NO-LABEL WIDGET-ID 58
     bUpdate AT ROW 9.1 COL 5 WIDGET-ID 14
     fiFromVersion AT ROW 9.1 COL 62 COLON-ALIGNED WIDGET-ID 38
     fiToVersion AT ROW 10.29 COL 62 COLON-ALIGNED
     eStatus AT ROW 12.43 COL 3 NO-LABEL WIDGET-ID 52
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.71 COL 3 WIDGET-ID 54
     RECT-2 AT ROW 1.48 COL 2 WIDGET-ID 44
     RECT-4 AT ROW 5.52 COL 2 WIDGET-ID 48
     rStatusBar AT ROW 16.71 COL 3
     RECT-6 AT ROW 16.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.8 BY 17.86 WIDGET-ID 100.


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
         COLUMN             = 5
         ROW                = 5
         HEIGHT             = 17.86
         WIDTH              = 79.8
         MAX-HEIGHT         = 26.67
         MAX-WIDTH          = 81
         VIRTUAL-HEIGHT     = 26.67
         VIRTUAL-WIDTH      = 81
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
/* SETTINGS FOR FILL-IN fiFromVersion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToVersion IN FRAME DEFAULT-FRAME
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
                    RUN ipStatus("User chose Start Update button").
            
                    RUN ipValidateChoices (OUTPUT lOKtoProceed).
                    IF NOT lOKtoProceed THEN 
                    DO:
                        RUN ipStatus("User made invalid choices for application").
                        RETURN.
                    END.
    
                    RUN ipStatus(" ").
                    RUN ipStatus("UPGRADING ENVIRONMENT " + slEnvList:{&SV}).
                    RUN ipStatus("  from version " + fiFromVersion:{&SV} + " to version: " + fiToVersion:{&SV}).
                    RUN ipStatus(" ").
    
                    RUN ipProcess.
                    
                    RUN ipStatus("Upgrade Complete.").
                    RUN ipStatus(" ").
            
                    IF lSuccess THEN DO:
                        ASSIGN 
                            cOutFile = cOutDir + "-SUCCESS.txt".
                        RUN ipSendVerification.
                        MESSAGE 
                            "Congratulations! Your Advantzware update completed successfully."
                            VIEW-AS ALERT-BOX.
                        APPLY 'close' TO THIS-PROCEDURE.
                    END.
                    ELSE DO:
                        ASSIGN 
                            cOutFile = cOutDir + "-FAILED.txt".
                        RUN ipSendVerification.
                        MESSAGE 
                            "There was an issue with update processing." SKIP 
                            "Please contact Advantzware Support."
                            VIEW-AS ALERT-BOX.
                        APPLY 'close' TO THIS-PROCEDURE.
                    END.
                END.
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword C-Win
ON LEAVE OF fiPassword IN FRAME DEFAULT-FRAME /* Password */
DO:

        IF SELF:{&SV} EQ "" THEN 
        DO:
            RUN ipStatus("  Entered blank Password").
            MESSAGE
                "This function does not allow blank passwords." SKIP
                "If your user id has a blank password, you must" SKIP
                "use a userid with higher-level privileges."
                VIEW-AS ALERT-BOX ERROR.
            RUN ipStatus("  Advised that this is not allowed in upgrade process").
            APPLY 'entry' TO fiUserID.
            RETURN NO-APPLY.
        END.
    
        IF (fiUserID:{&SV} EQ "asi" AND NOT SELF:{&SV} EQ "Pack@$!")
        OR (fiUserID:{&SV} EQ "admin" AND NOT SELF:{&SV} EQ "installme") THEN 
        DO:
            MESSAGE 
                "You entered an invalid Password." SKIP 
                "Please re-enter or cancel."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                SELF:{&SV} = "".
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
        END. 
        
        IF NUM-ENTRIES(slEnvList:LIST-ITEMS) GT 1 THEN DO:
            ASSIGN
                slEnvList:SENSITIVE = TRUE
                bUpdate:SENSITIVE = TRUE.
            APPLY 'entry' TO slEnvList.
        END.
        ELSE DO:
            ASSIGN
                bUpdate:SENSITIVE = TRUE.
            APPLY 'entry' TO bUpdate.
        END.
    
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON ENTRY OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
OR ENTRY OF fiPassword
    OR ENTRY OF slEnvList
    OR ENTRY OF bUpdate
    DO:
        CASE SELF:NAME:
            WHEN "fiUserID" THEN 
                ASSIGN
                    eStatus:{&SV}       = eStatus:{&SV} + "Enter a valid ASI update userid..." + CHR(10)
                    eStatus:CURSOR-LINE = eStatus:NUM-LINES.
            WHEN "fiPassword" THEN 
                ASSIGN
                    eStatus:{&SV}       = eStatus:{&SV} + "Enter the password for this user..." + CHR(10)
                    eStatus:CURSOR-LINE = eStatus:NUM-LINES.
            WHEN "slEnvList" THEN 
                ASSIGN
                    eStatus:{&SV}       = eStatus:{&SV} + "Select the environment to update..." + CHR(10)
                    eStatus:CURSOR-LINE = eStatus:NUM-LINES.
            WHEN "bUpdate" THEN 
                ASSIGN
                    eStatus:{&SV}       = eStatus:{&SV} + "Choose to start the update process..." + CHR(10)
                    eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        END CASE.
    END.

ON LEAVE OF slEnvList
    DO:
        RUN ipStatus("  User chose the " + SELF:{&SV} + " environment.").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
        IF SELF:{&SV} = "" THEN 
        DO:
            RUN ipStatus("  Blank UserID - ENTRY to Exit button").
            APPLY 'entry' TO bCancel.
            RETURN NO-APPLY.
        END.

        IF SELF:{&SV} NE "asi"
        AND SELF:{&SV} NE "admin" THEN 
        DO:
            MESSAGE 
                "This is not a valid user id for this function." SKIP 
                "Please re-enter or Exit."
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
        END.
    
        RUN ipStatus("  Entered UserID " + SELF:{&SV}).
    
        APPLY 'entry' TO fiPassword.
        RETURN NO-APPLY.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slEnvList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slEnvList C-Win
ON VALUE-CHANGED OF slEnvList IN FRAME DEFAULT-FRAME
DO:
        CASE SELF:NAME:
            WHEN "slEnvList" THEN 
                DO:
                    ASSIGN
                        iIndex = LOOKUP(SELF:{&SV},SELF:LIST-ITEMS)
                        fiFromVersion:{&SV} = ENTRY(iIndex,cEnvVerList)
                        iCurrEnvVer = fIntVer(fiFromVersion:{&SV})
                        iCurrDbVer = fIntVer(ENTRY(iIndex,cDBVerList))
                        iCurrAudVer = fIntVer(ENTRY(iIndex,cAudVerList))
                        ENTRY(3,cOutDir,"-") = SELF:{&SV}.
                END.
        END CASE.
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
    RUN BringWindowToTop (c-Win:hwnd, OUTPUT rc).
    
    /* There should not be any connected DBs at this point.  If there are, disconnect */
    DO ictr = 1 TO NUM-DBS:
        DISCONNECT VALUE (LDBNAME(ictr)).
    END. 
    
    RUN ipFindIniFile ("..\advantzware.ini",
                       OUTPUT cIniLoc).
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipExpandVarNames.
    
    ASSIGN
        lMakeBackup = TRUE 
        slEnvList:LIST-ITEMS = cEnvList
        slEnvList:SCREEN-VALUE = ENTRY(1,cEnvList)
        iIndex              = LOOKUP(slEnvList:{&SV},slEnvList:LIST-ITEMS)
        fiFromVersion:{&SV} = ENTRY(iIndex,cEnvVerList)
        iCurrEnvVer         = fIntVer(fiFromVersion:{&SV})
        iCurrDbVer          = fIntVer(ENTRY(iIndex,cDBVerList))
        iCurrAudVer         = fIntVer(ENTRY(iIndex,cAudVerList))
        iStatus             = 1
        rStatusBar:WIDTH    = MIN(75,(iStatus / 100) * 75).

    RUN ipGetPatchList.

    ASSIGN
        slEnvList:SENSITIVE = FALSE
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
        .    
    
    RUN ipStatus("Initialize").
    
    IF fIntVer(fiToVersion:{&SV}) LE 16087000 THEN DO:
        ASSIGN
            fiUserID:{&SV} = "admin"
            fiPassword:{&SV} = "installme".
        IF NUM-ENTRIES(slEnvList:LIST-ITEMS) EQ 1 THEN
            APPLY 'choose' TO bUpdate.
        ELSE DO:
            APPLY 'leave' TO fiUserID.
            APPLY 'leave' TO fiPassword.
            APPLY 'entry' TO slEnvList.
        END.
    END.
    
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
  DISPLAY fiUserID fiPassword slEnvList fiFromVersion fiToVersion eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-4 RECT-6 fiUserID bCancel fiPassword slEnvList eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
        lmakeBackup = IF INDEX(cMakeBackup,"Y") NE 0 OR INDEX(cMakeBackup,"T") NE 0 THEN TRUE ELSE FALSE
        lConnectAudit = IF INDEX(cConnectAudit,"Y") NE 0 OR INDEX(cConnectAudit,"T") NE 0 THEN TRUE ELSE FALSE
        cLockoutTries = SUBSTRING(cLockoutTries,1,1)
        iLockoutTries = IF cLockoutTries NE "" 
                        AND ASC(cLockoutTries) GE 48
                        AND ASC(cLockoutTries) LE 57 THEN INT(cLockoutTries) ELSE 0
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
        
    IF fiUserID:{&SV} EQ "asi" THEN ASSIGN
        iUserLevel = 10.
    ELSE IF fiUserID:{&SV} EQ "admin" THEN ASSIGN
        iUserLevel = 6.
    ELSE DO:
        MESSAGE 
            "Unable to start this process with the credentials supplied."
            VIEW-AS ALERT-BOX.
        RUN ipStatus("Invalid credentials.  Abort.").
        QUIT.
    END.

    IF iCurrDbVer LT iPatchDbVer
    OR iCurrAudVer LT iPatchAudVer
    OR iCurrEnvVer = 16070000 THEN 
    DO:
                
        RUN ipStatus("Database requires upgrade...").
        
        ASSIGN
            iEnv = LOOKUP (slEnvList:{&SV},slEnvList:list-items)
            c-Win:VISIBLE = FALSE 
            lSuccess = FALSE 
            .

        RUN ipStatus("Initiating asiUpdateDB.w").
        ASSIGN
            c-Win:VISIBLE = FALSE. 
        
        IF SEARCH("asiUpdateDB.r") NE ? 
        OR SEARCH("asiUpdateDB.w") NE ? THEN 
        DO:
            ASSIGN 
                cFileToRun = "asiUpdateDB.w".
        END.
        ELSE ASSIGN 
            cFileToRun = "N:\Repository\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateDB.w".
 
        RUN  VALUE (cFileToRun) (ENTRY(iEnv,cDBList),
            ENTRY(iEnv,cDBPortList),
            ENTRY(iEnv,cDbDirList),
            iEnv,
            iCurrDbVer,
            iPatchDbVer,
            iCurrAudVer,
            iPatchAudVer,
            iUserLevel,
            OUTPUT lSuccess,
            INPUT-OUTPUT iStatus).
        ASSIGN
            c-Win:VISIBLE = TRUE
            rStatusBar:WIDTH = MIN(75,(iStatus / 100) * 75). 
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
                lMakeBackup = FALSE.
            
        END. 
        ELSE 
        DO:
            MESSAGE 
                "The system upgrade was NOT successful.  Please" SKIP  
                "contact Advantzware technical support for assistance."
                VIEW-AS ALERT-BOX ERROR.
            QUIT.
        END.
    END.
    ELSE ASSIGN 
        iStatus = iStatus + 2
        rStatusBar:WIDTH = MIN(75,(iStatus / 100) * 75).
  
    ASSIGN
        iEnv = LOOKUP (slEnvList:{&SV},slEnvList:list-items)
        .

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
        
    FIND FIRST ttDatabases WHERE
        ttDatabases.cName EQ ENTRY(iEnv,cDBList) AND
        ttDatabases.cPort EQ ENTRY(iEnv,cDBPortList)
        NO-ERROR.
    IF AVAILABLE ttDatabases THEN ASSIGN
            cAudDb = ttDatabases.cAudName
            cPort = ttDatabases.cAudPort.

    RUN ipStatus("Connecting databases").
    RUN ipStatus("  Connecting ASI DB with statement...").
    ASSIGN
        cConnect = "-db " + ENTRY(iEnv,cDBList) + 
                   " -H " + chostName +
                   " -S " + ENTRY(iEnv,cDBPortList) +
                   " -N tcp -ld ASI".
    RUN ipStatus("    " + cConnect).
    CONNECT VALUE(cConnect) NO-ERROR.
    IF NOT CONNECTED ("asi") THEN 
    DO:
        RUN ipStatus("  DB connection failed.  Cancelling.").
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN ipStatus ("    " + ERROR-STATUS:GET-MESSAGE(iCtr)).
        END.
        MESSAGE 
            "Unable to connect to the asi database.  Cannot continue." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    IF cAudDb NE "" THEN 
    DO:
        RUN ipStatus("  Connecting Audit DB with statement...").
        ASSIGN
            cConnect = "-db " + cAudDb + 
                       " -H " + chostName +
                       " -S " + cPort +
                       " -N tcp -ld Audit".
        RUN ipStatus("    " + cConnect).
        CONNECT VALUE(cConnect) NO-ERROR.
    END.
    IF NOT CONNECTED ("audit") THEN 
    DO:
        RUN ipStatus("  DB connection failed.  Cancelling.").
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN ipStatus ("    " + ERROR-STATUS:GET-MESSAGE(iCtr)).
        END.
        MESSAGE 
            "Unable to connect to the audit database.  Cannot continue." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF SEARCH("asiUpdateENV.r") NE ? 
        OR SEARCH("asiUpdateENV.w") NE ? THEN ASSIGN 
            cFileToRun = "asiUpdateENV.w".
    ELSE ASSIGN 
            cFileToRun = "N:\Repository\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateENV.w".

    CREATE ALIAS "DICTDB" FOR DATABASE asi.
    RUN ipStatus("Initiating asiUpdateENV.w").
    ASSIGN
        c-Win:VISIBLE = FALSE. 

    RUN VALUE(cFileToRun) (ttDatabases.cName,
        ttDatabases.cPort,
        ttDatabases.cDir,
        ttDatabases.cVer,
        slEnvList:{&SV},
        fiFromVersion:{&SV},
        fiToVersion:{&SV},
        iUserLevel,
        lMakeBackup, /* Need backup? */
        OUTPUT lSuccess,
        INPUT-OUTPUT iStatus).
        
    ASSIGN
        c-Win:VISIBLE = TRUE
        rStatusBar:WIDTH = MIN(75,(iStatus / 100) * 75). 
         
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
    PUT STREAM sInstr UNFORMATTED "PUT " + cAdminDir + "\advantzware.ini" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cAdminDir + "\advantzware.usr" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cEnvAdmin + "\advantzware.pf" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cDLCDir + "\properties\AdminServerPlugins.properties" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cDLCDir + "\properties\conmgr.properties" SKIP.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipStatus C-Win 
PROCEDURE ipStatus :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStatus AS CHARACTER NO-UNDO.
                
    ASSIGN
        cLogFile = cEnvAdmin + "\UpdateLog.txt".

    IF ipcStatus = "Initialize" THEN 
    DO:
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.

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
        RETURN.
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
            OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
            PUT STREAM logStream
                STRING(TODAY,"99/99/99") AT 1
                STRING(TIME,"HH:MM:SS") AT 12
                cMsgStr[iMsgCtr] FORMAT "x(160)" AT 25
                SKIP.
            OUTPUT STREAM logStream CLOSE.
        END.
    END.
        
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
        ELSE ASSIGN
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
        iIntVal[3] = IF INT(cStrVal[3]) LT 10 THEN INT(cStrVal[3]) * 10 ELSE INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

