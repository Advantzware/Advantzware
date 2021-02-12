&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:             asiLogin.w
  Description:      General login program for ASI application

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE createSingleUserPFs YES
&SCOPED-DEFINE execProgram mainmenu.    
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES

DEFINE STREAM usrStream.

DEFINE NEW GLOBAL SHARED VARIABLE fwd-embedded-mode AS LOG NO-UNDO INITIAL FALSE.
DEFINE NEW GLOBAL SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_track_usage AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_header_line AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_groups AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE init_menu AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_developer AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_version AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_rec_key AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_pageno AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_mainmenu AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_sharpshooter AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cUsrLoc AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE persistent-handle AS HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG NO-UNDO.

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_sysdate AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE g_period AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_init AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE NEW SHARED VARIABLE miscflds_reckey AS CHARACTER.
DEFINE NEW SHARED VARIABLE table_reckey AS CHARACTER.
DEFINE NEW SHARED VARIABLE ListLogic-Handle AS HANDLE.
DEFINE NEW SHARED VARIABLE igsSessionID AS INTEGER.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.

DEFINE VARIABLE cAvailDbList AS CHAR NO-UNDO.
DEFINE VARIABLE cAvailDbVerList AS CHAR NO-UNDO.
DEFINE VARIABLE cAvailEnvList AS CHAR NO-UNDO.
DEFINE VARIABLE cAvailEnvVerList AS CHAR NO-UNDO.
DEFINE VARIABLE cAvailModeList AS CHAR NO-UNDO.
DEFINE VARIABLE cAvailPgmList AS CHAR NO-UNDO.

DEFINE VARIABLE char-hdl AS CHAR NO-UNDO.
DEFINE VARIABLE cModeRunList AS CHAR NO-UNDO.
DEFINE VARIABLE cNewColors AS CHAR NO-UNDO.
DEFINE VARIABLE cNewFonts AS CHAR NO-UNDO.
DEFINE VARIABLE cOldColors AS CHAR NO-UNDO.
DEFINE VARIABLE cOldFonts AS CHAR NO-UNDO.
DEFINE VARIABLE connectStatement AS CHAR NO-UNDO.
DEFINE VARIABLE cRunPgm AS CHAR NO-UNDO.
DEFINE VARIABLE cSelectedDatabase AS CHAR NO-UNDO.
DEFINE VARIABLE cSelectedDatabaseVer AS CHAR NO-UNDO.
DEFINE VARIABLE cSelectedEnvironment AS CHAR NO-UNDO.
DEFINE VARIABLE cSelectedEnvironmentVer AS CHAR NO-UNDO.
DEFINE VARIABLE cSelectedMode AS CHAR NO-UNDO.
DEFINE VARIABLE cSelectedPgm AS CHAR NO-UNDO.
DEFINE VARIABLE cSessionParam AS CHAR NO-UNDO.
DEFINE VARIABLE cUserID AS CHAR NO-UNDO.
DEFINE VARIABLE cUsrFileName AS CHAR NO-UNDO.
DEFINE VARIABLE cUsrLine AS CHAR NO-UNDO.
DEFINE VARIABLE cUsrList AS CHAR NO-UNDO.
DEFINE VARIABLE cVarName AS CHAR EXTENT 100 NO-UNDO.
DEFINE VARIABLE cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEFINE VARIABLE hPreRun AS HANDLE.
DEFINE VARIABLE iCtr AS INT NO-UNDO.
DEFINE VARIABLE iDbLevel AS INT NO-UNDO.
DEFINE VARIABLE iDbPos AS INT NO-UNDO.
DEFINE VARIABLE iDbVer AS INT NO-UNDO.
DEFINE VARIABLE iEnvLevel AS INT NO-UNDO.
DEFINE VARIABLE iEnvPos AS INT NO-UNDO.
DEFINE VARIABLE iEnvVer AS INT NO-UNDO.
DEFINE VARIABLE iLockoutTries AS INT NO-UNDO.
DEFINE VARIABLE iModePos AS INT NO-UNDO.
DEFINE VARIABLE intBufferSize AS INT NO-UNDO INITIAL 256.
DEFINE VARIABLE intResult AS INT NO-UNDO.
DEFINE VARIABLE iNumUsers AS INT NO-UNDO.
DEFINE VARIABLE is-running AS LOG NO-UNDO.
DEFINE VARIABLE iTries AS INT NO-UNDO.
DEFINE VARIABLE iTruncLevel AS INT NO-UNDO.
DEFINE VARIABLE jCtr AS INT NO-UNDO.
DEFINE VARIABLE lCorrupt AS LOG NO-UNDO.
DEFINE VARIABLE ldummy AS LOG NO-UNDO.
DEFINE VARIABLE lExit AS LOG NO-UNDO.
DEFINE VARIABLE lFound AS LOG NO-UNDO.
DEFINE VARIABLE lFoundIni AS LOG NO-UNDO.
DEFINE VARIABLE lFoundUsr AS LOG NO-UNDO.
DEFINE VARIABLE lSysError AS LOG NO-UNDO.
DEFINE VARIABLE lUpdUsr AS LOG NO-UNDO.
DEFINE VARIABLE lUserOK AS LOG NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
DEFINE VARIABLE origDirectoryName AS CHAR NO-UNDO FORMAT "X(256)".
DEFINE VARIABLE origPropath AS CHAR NO-UNDO.
DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ptrToString AS MEMPTR    NO-UNDO.
DEFINE VARIABLE tslogin-cha AS CHAR NO-UNDO.
DEFINE VARIABLE tslogin-log AS LOG NO-UNDO.
DEFINE VARIABLE xDbDir AS CHAR NO-UNDO.

{iniFileVars.i}

DEFINE TEMP-TABLE ttUsers
    FIELD ttfPdbname AS CHARACTER
    FIELD ttfUserID AS CHARACTER
    FIELD ttfUserAlias AS CHARACTER
    FIELD ttfEnvList AS CHARACTER
    FIELD ttfDbList AS CHARACTER
    FIELD ttfModeList AS CHARACTER
    INDEX iUserID IS UNIQUE ttfUserID ttfPdbName
    INDEX iDatabase IS UNIQUE ttfPdbName ttfUserID.
DEFINE BUFFER bttUsers FOR ttUsers.

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

PROCEDURE fwdRunProgram.
    DEFINE INPUT PARAMETER pname AS CHARACTER.
    DEFINE INPUT PARAMETER runPersistent AS LOG.
    DEFINE OUTPUT PARAMETER phandle AS HANDLE.
   
   // these need to be executed in the context of asiLogin.w, in embedded mode
    IF runPersistent
        THEN RUN value(pname) PERSISTENT SET phandle.
    ELSE RUN value(pname).
END.

IF origDirectoryName = "" THEN DO:
&IF DEFINED(FWD-VERSION) > 0 &THEN
    ASSIGN 
        origDirectoryName = get-working-directory().
&ELSE
    SET-SIZE(ptrToString) = 256.
    RUN GetCurrentDirectoryA (INPUT        intBufferSize,
        INPUT-OUTPUT ptrToString,
        OUTPUT       intResult).
    ASSIGN 
        origDirectoryName = GET-STRING(ptrToString,1).    
&ENDIF
END.
ELSE DO:
    RUN ipSetCurrentDir (origDirectoryName). 
END.

/* Find (and read) the .ini file containing variables and values */
RUN ipFindIniFile ("..\advantzware.ini",
    OUTPUT cIniLoc).
ASSIGN 
    FILE-INFO:FILE-NAME = cIniLoc
    cIniLoc = FILE-INFO:FULL-PATHNAME.

IF cIniLoc EQ "" THEN DO:
    MESSAGE
        "Unable to locate an 'advantzware.ini' file." SKIP
        "Please contact Advantzware Support. Exiting..."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
ELSE DO:
    RUN ipReadIniFile.
END.

/* Pre-visualization tasks */
IF PROCESS-ARCHITECTURE = 64 THEN DO:
    LOAD "dbms".
    USE "dbms".
END.
ELSE DO:
    IF SEARCH("C:\Progress\OE116_32\progress.cfg") NE ? 
    AND SEARCH("dbms32s.ini") NE "" THEN DO:
        LOAD "dbms32s".
        USE "dbms32s".
    END.
    ELSE DO:
        LOAD "dbms32".
        USE "dbms32".
    END.
END.

cSessionParam = SESSION:PARAM.

ASSIGN
    g_lookup-var = ""
    g_init = YES
    g_sysdate = TODAY
    g_version = "2.1A-8.2A"
    origPropath = PROPATH.

        
ASSIGN         /*   0      1       2       3         4       5         6          7           8          9      10        11       12       13        14          15        16    17     18   19      20       21    22    23    24    25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47 */
    cOldColors = "0,0,0|0,0,128|0,128,0|0,128,128|128,0,0|128,0,128|128,128,0|128,128,128|192,192,192|0,0,255|0,255,0|0,255,255|255,0,0|255,0,255|255,255,187|255,255,255|0,0,0|0,0,0|0,0,0|0,0,0|120,50,255|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0"
               /*   0      1       2       3         4       5         6          7           8          9      10        11       12       13        14          15        16    17     18   19      20       21            22          23          24          25          26        27      28         29          30          31          32         33          34          35        36    37    38    39    40    41    42    43    44    45    46    47 */
    cNewColors = "0,0,0|0,0,128|0,128,0|0,128,128|128,0,0|128,0,128|128,128,0|128,128,128|192,192,192|0,0,155|0,255,0|0,255,255|255,0,0|255,0,255|163,188,249|255,255,255|0,0,0|0,0,0|0,0,0|0,0,0|120,50,255|119,150,203|163,188,249|251,251,251|139,139,139|245,246,246|236,242,249|0,0,0|239,255,232|87,100,144|219,254,202|246,242,242|251,251,251|87,100,144|209,210,249|200,217,246|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0|0,0,0"
    .
ASSIGN        /*       0                    1                2                   3                   4             5                 6                    7            8             9                 10                    11                   12                  13                  14                   15                 16                                 17                     18                19                  20                       21                22                23                      24                25                    26                  27                   28                   29                   30                     31                       32            33                     34               35                36                37                 38              39      */
    cOldFonts = "Courier New, size=8|Tahoma, size=8|Courier New, size=8|Courier New, size=8|Tahoma, size=8|Tahoma, size=10|Tahoma, size=8, bold|Tahoma, size=8|Tahoma, size=8|Courier, size=10|Courier New, size=6|Courier New, size=7|Courier New, size=8|Courier New, size=9|Courier New, size=10|Courier New, size=12|Webdings, size=10 Script=symbol|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|Arial, size=14 italic bold|Tahoma, size=8|Tahoma, size=8, bold|Tahoma, size=10|Tahoma, size=10, bold|Tahoma, size=12|Tahoma, size=12, bold|Tahoma, size=8|Tahoma, size=8" 
              /*       0                    1                2                   3               4              5               6                    7              8               9                 10                  11                   12                  13                  14                   15                 16                               17                  18                  19                   20                   21               22                23                   24              25                   26                   27                   28                   29                   30                   31                          32              33                      34               35                     36               37                     38                        39      */
    cNewFonts = "Calibri, size=10|Tahoma, size=8|Courier New, size=8|Courier New, size=8|Tahoma, size=8|Tahoma, size=10|Tahoma, size=8, bold|Tahoma, size=8|Tahoma, size=8|Courier, size=10|Courier New, size=6|Courier New, size=7|Courier New, size=8|Courier New, size=9|Courier New, size=10|Courier New, size=12|Webdings, size=10 Script=symbol|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|Corbel, size=10|Calibri, size=10|Calibri, size=10 bold|Calibri, size=12|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|MS Sans Serif size=8|Arial, size=14 italic bold|Calibri, size=10|Calibri, size=10, bold|Calibri, size=12|Calibri, size=12, bold|Calibri, size=14|Calibri, size=14, bold|Trebuchet MS, size=14 bold|Tahoma, size=8"
    .
       

IF SEARCH(cMapDir + "\" + cAdminDir + "\advantzware.ini") EQ ? THEN 
DO:
    MESSAGE
        "There is a problem with your network connections." SKIP
        "Please contact your local System Administrator."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.

/* Find the .usr file containing user-level settings */
RUN ipFindIniFile ("..\advantzware.usr",
    OUTPUT cUsrLoc).
ASSIGN 
    FILE-INFO:FILE-NAME = cUsrLoc
    cUsrLoc = FILE-INFO:FULL-PATHNAME.

IF cUsrLoc EQ "" THEN DO:
    MESSAGE
        "Unable to locate an 'advantzware.usr' file." SKIP
        "Please contact Advantzware Support. Exiting..."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
ELSE DO:
    RUN ipReadUsrFile.
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
&Scoped-Define ENABLED-OBJECTS IMAGE-2 fiUserID fiPassword cbMode ~
cbEnvironment cbDatabase Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiPassword cbMode cbEnvironment ~
cbDatabase 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD intVer C-Win 
FUNCTION intVer RETURNS INTEGER
    ( INPUT cVerString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 35.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Login" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 35.

DEFINE VARIABLE cbDatabase AS CHAR FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbEnvironment AS CHAR FORMAT "X(256)":U 
     LABEL "Environment" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 25 BY 1.1 NO-UNDO.

DEFINE VARIABLE cbMode AS CHAR FORMAT "X(256)":U 
     LABEL "Mode" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHAR FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiUserID AS CHAR FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "asilogo.jpg":U
     SIZE 40 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiUserID AT ROW 1.71 COL 18 COLON-ALIGNED WIDGET-ID 4
     fiPassword AT ROW 3.14 COL 18 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD 
     cbMode AT ROW 5.76 COL 18 COLON-ALIGNED WIDGET-ID 10
     cbEnvironment AT ROW 7.19 COL 18 COLON-ALIGNED WIDGET-ID 8
     cbDatabase AT ROW 8.62 COL 18 COLON-ALIGNED WIDGET-ID 18
     Btn_OK AT ROW 8.62 COL 53 WIDGET-ID 26
     Btn_Cancel AT ROW 8.62 COL 77 WIDGET-ID 22
     IMAGE-2 AT ROW 1.71 COL 53 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.2 BY 9.67
         BGCOLOR 35 FONT 21
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
         TITLE              = "Advantzware Login"
         HEIGHT             = 9.71
         WIDTH              = 97.4
         MAX-HEIGHT         = 21.48
         MAX-WIDTH          = 106.2
         VIRTUAL-HEIGHT     = 21.48
         VIRTUAL-WIDTH      = 106.2
         SMALL-TITLE        = YES
         SHOW-IN-TASKBAR    = YES
         CONTROL-BOX        = NO
         MIN-BUTTON         = NO
         MAX-BUTTON         = NO
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       cbDatabase:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Advantzware Login */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Advantzware Login */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        QUIT.
        /* RETURN NO-APPLY. */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Exit */
    DO:
        IF VALID-HANDLE(hPreRun) THEN 
            RUN epDisconnectDB IN hPreRun.
        APPLY 'window-close' TO C-Win.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Login */
OR RETURN OF fiPassword
OR RETURN OF cbEnvironment
OR RETURN OF cbDatabase
OR RETURN OF cbMode
    DO:
        ASSIGN fiPassword.
        RUN ipClickOK.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDatabase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDatabase C-Win
ON VALUE-CHANGED OF cbDatabase IN FRAME DEFAULT-FRAME /* Database */
OR VALUE-CHANGED OF cbEnvironment
OR VALUE-CHANGED OF cbMode
    DO:
        CASE SELF:NAME:
            WHEN "cbDatabase" THEN DO:
                ASSIGN 
                    cSelectedDatabase = cbDatabase:{&SV}.
                RUN ipChangeDatabase.
            END.
            WHEN "cbEnvironment" THEN DO:
                ASSIGN 
                    cSelectedEnvironment = cbEnvironment:{&SV}.
                RUN ipChangeEnvironment.
            END.
            WHEN "cbMode" THEN DO:
                ASSIGN 
                    cSelectedMode = cbMode:{&SV}.
                RUN ipChangeMode.
            END.
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
    DEF VAR lUserError AS LOG NO-UNDO.
    
        ASSIGN fiUserID.
        
        RUN ipFindUser IN THIS-PROCEDURE (OUTPUT lUserError).
    
        IF lUserError THEN DO:
            IF fwd-embedded-mode THEN 
                RETURN NO-APPLY "Unable to locate this user in the advantzware.usr file." +
                    "Please contact your system administrator for assistance.".
            ELSE DO:
                MESSAGE
                    "Unable to locate this user in the advantzware.usr file." SKIP
                    "Please contact your system administrator for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.
        ELSE DO:
            IF SELF:SCREEN-VALUE EQ "Monitor" THEN DO:
                MESSAGE 
                    "The user 'Monitor' cannot be used for interactive" SKIP 
                    "logins. Please choose another user id."
                    VIEW-AS ALERT-BOX ERROR.
                ASSIGN 
                    SELF:SCREEN-VALUE  = "".
                RETURN NO-APPLY.
            END.
            ASSIGN
                cSelectedMode = ENTRY(1,cAvailModeList)
                cSelectedEnvironment = ENTRY(1,cAvailEnvList)
                cSelectedDatabase = ENTRY(1,cAvailDbList).
            IF NUM-ENTRIES(cAvailEnvList) GT 1 THEN ASSIGN 
                cbEnvironment:VISIBLE = TRUE
                cbEnvironment:SENSITIVE = TRUE 
                cbEnvironment:LIST-ITEMS = cAvailEnvList 
                cbEnvironment:{&SV} = ENTRY(1,cbEnvironment:LIST-ITEMS)
                .
            IF NUM-ENTRIES(cAvailDbList) GT 1 THEN ASSIGN 
                cbDatabase:VISIBLE = TRUE 
                cbDatabase:SENSITIVE = TRUE 
                cbDatabase:LIST-ITEMS = cAvailDbList
                cbDatabase:{&SV} = ENTRY(1,cbDatabase:LIST-ITEMS).
                .                
            IF NUM-ENTRIES(cAvailModeList) GT 1 THEN ASSIGN
                cbMode:VISIBLE = TRUE 
                cbMode:SENSITIVE = TRUE   
                cbMode:LIST-ITEMS = cAvailModeList
                cbEnvironment:LIST-ITEMS = cAvailEnvList 
                cbMode:{&SV} = ENTRY(1, cbMode:LIST-ITEMS)
                .
            APPLY 'value-changed' TO cbEnvironment.
            APPLY 'value-changed' TO cbDatabase.
            APPLY 'value-changed' TO cbMode.
        END.
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
    DO:
        RUN disable_UI.
    END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IMAGE-2:LOAD-IMAGE("asilogosm.jpg").

    ASSIGN
        cAvailModeList = cModeList
        cAvailEnvList = cEnvList
        cAvailDbList = cDbList
        cbMode:LIST-ITEMS = cAvailModeList
        cbEnvironment:LIST-ITEMS = cAvailEnvList 
        cbDatabase:LIST-ITEMS = cAvailDbList
        cSelectedMode = ENTRY(1,cAvailModeList)
        cSelectedEnvironment = ENTRY(1,cAvailEnvList)
        cSelectedDatabase = ENTRY(1,cAvailDbList).
    
    IF cSessionParam EQ "" THEN 
    DO:
        RUN enable_UI.
        
        ASSIGN 
            cbMode:{&SV} = ENTRY(1, cbMode:LIST-ITEMS)
            cbEnvironment:{&SV} = ENTRY(1,cbEnvironment:LIST-ITEMS)
            cbDatabase:{&SV} = ENTRY(1,cbDatabase:LIST-ITEMS)
            fiUserID:{&SV} = OS-GETENV("USERNAME")
            cbMode:VISIBLE = FALSE 
            cbEnvironment:VISIBLE = FALSE 
            cbDatabase:VISIBLE = FALSE.

        APPLY 'entry' TO fiUserID.
    
    END. /* If there is a UI */
    ELSE 
        RUN ipAutoLogin.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY fiUserID fiPassword cbMode cbEnvironment cbDatabase 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-2 fiUserID fiPassword cbMode cbEnvironment cbDatabase Btn_OK 
         Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAutoLogin C-Win 
PROCEDURE ipAutoLogin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        fiUserID = ENTRY(1, cSessionParam)
        fiPassword = ENTRY(2, cSessionParam)
        cSelectedEnvironment = ENTRY(3, cSessionParam)
        cSelectedMode = REPLACE(ENTRY(4, cSessionParam),"_"," ")
        cSelectedDatabase = ENTRY(5, cSessionParam)
        .   
    
    RUN ipClickOK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeDatabase C-Win 
PROCEDURE ipChangeDatabase :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLookup AS INT NO-UNDO.
    DEFINE VARIABLE xdbName AS CHAR NO-UNDO.
    DEFINE VARIABLE xdbPort AS CHAR NO-UNDO.
    DEFINE VARIABLE lUserError AS LOG NO-UNDO.

    ASSIGN
        iEnvPos = LOOKUP(cSelectedEnvironment,cEnvList)
        iEnvLevel = intVer(ENTRY(iEnvPos,cEnvVerList))
        iDbPos = LOOKUP(cSelectedDatabase,cDbList)
        iDbLevel = intVer(ENTRY(iDbPos,cDbVerList)).

    ASSIGN
        connectStatement = ""
        xdbName = cSelectedDatabase
        iLookup = LOOKUP(xdbName,cDbList)
        xDbDir = ENTRY(iLookup,cDbDirList)
        xdbPort = ENTRY(iLookup,cDbPortList).
        
    IF xdbName <> ""
    AND xdbPort <> "" THEN ASSIGN
        connectStatement = "-db " + xdbName + 
                           " -H " + chostName +
                           " -S " + xdbPort +
                           " -N tcp -ld ASI".
    ELSE 
    DO:
        MESSAGE
            "There is a problem with your database connection list." SKIP
            "Pleasse contact Advantzware support for assistance."
            VIEW-AS ALERT-BOX.
        APPLY 'choose' TO btn_Cancel IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
    END.
    
/* APPLY 'value-changed' TO cbMode. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeEnvironment C-Win 
PROCEDURE ipChangeEnvironment :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTop AS CHAR NO-UNDO.
    DEFINE VARIABLE preProPath AS CHAR NO-UNDO.
    DEFINE VARIABLE iLookup AS INT NO-UNDO.
    DEFINE VARIABLE iCtr AS INT NO-UNDO.
    DEFINE VARIABLE cTestList AS CHAR NO-UNDO.
    DEFINE VARIABLE iDbLevelM AS INT NO-UNDO.
    
    ASSIGN
        iEnvPos = LOOKUP(cSelectedEnvironment,cEnvList)
        iEnvLevel = intVer(ENTRY(iEnvPos,cEnvVerList))
        iDbPos = LOOKUP(cSelectedDatabase,cDbList)
        iDbLevel = intVer(ENTRY(iDbPos,cDbVerList))
        .

    IF cSessionParam NE "" THEN DO:
        ASSIGN
            iLookup = LOOKUP(cSelectedEnvironment,cEnvList)
            cTop = cMapDir + "\" + cEnvDir + "\" + cSelectedEnvironment + "\"
            preProPath = cMapDir + "\" + cEnvDir + "\" + cSelectedEnvironment + "," +
                        cTop + cEnvCustomerDir + "," +
                        cTop + cEnvOverrideDir + "\Addon," +
                        cTop + cEnvOverrideDir + "," +
                        cTop + cEnvProgramsDir + "\Addon" + "," +
                        cTop + cEnvProgramsDir + "," +
                        cTop + cEnvCustFiles + "," +
                        cTop + cEnvResourceDir + "," +
                        cTop + cEnvResourceDir + "\Addon" + "," +
                        cMapDir + "\" + cAdminDir + "\" + cEnvAdmin + ","
            PROPATH = preProPath + origPropath.      
        RETURN.
    END.
    ELSE DO:
        CASE cbEnvironment:{&SV}:
            WHEN "Prod" THEN DO:
                DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                    IF INDEX(ENTRY(iCtr,cDbList),"Prod") NE 0 THEN ASSIGN
                        cAvailDbList = cAvailDbList + ENTRY(iCtr,cDbList) + ",".
                END.
                ASSIGN 
                    cAvailDbList = TRIM(cAvailDbList,",")
                    cSelectedDatabase = ENTRY(1,cAvailDbList)
                    cbDatabase:LIST-ITEMS = cAvailDbList
                    cbDatabase:{&SV} = cSelectedDatabase.
            END.
            WHEN "Test" THEN DO:
                DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                    IF INDEX(ENTRY(iCtr,cDbList),"Test") NE 0 THEN ASSIGN
                            cAvailDbList = cAvailDbList + ENTRY(iCtr,cDbList) + ",".
                END.
                ASSIGN 
                    cAvailDbList = TRIM(cAvailDbList,",")
                    cSelectedDatabase = ENTRY(1,cAvailDbList)
                    cbDatabase:LIST-ITEMS = cAvailDbList
                    cbDatabase:{&SV} = cSelectedDatabase.
            END.
            OTHERWISE DO:
                ASSIGN 
                    cTestList = ""
                    cbDatabase:LIST-ITEMS = "".
                DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                    IF intVer(ENTRY(iCtr,cDbVerList)) EQ iEnvLevel THEN ASSIGN
                        cTestList = cTestList + ENTRY(iCtr,cDbList) + ",".
                END.
                ASSIGN 
                    cTestList = TRIM(cTestList,",")
                    cAvailDbList = cTestList
                    cbDatabase:LIST-ITEMS = cAvailDbList
                    cbDatabase:{&SV} = ENTRY(1,cAvailDbList)
                    cSelectedDatabase = ENTRY(1,cAvailDbList).
            END.
        END CASE.
        IF cSessionParam EQ "" THEN 
            APPLY 'value-changed' TO cbDatabase.

        ASSIGN
            iLookup = LOOKUP(cSelectedEnvironment,cEnvList)
            cTop = cMapDir + "\" + cEnvDir + "\" + cSelectedEnvironment + "\"
            preProPath = cMapDir + "\" + cEnvDir + "\" + cSelectedEnvironment + "," +
                        (IF iEnvLevel GE 21000000 THEN cMapDir + "\" + cEnvDir + "\" + cSelectedEnvironment + "\asiObjects.pl," ELSE "") +
                        (IF iEnvLevel GE 21000000 THEN cMapDir + "\" + cEnvDir + "\" + cSelectedEnvironment + "\asigraphics.pl," ELSE "") +
                        cTop + cEnvCustomerDir + "," +
                        cTop + cEnvOverrideDir + "\Addon," +
                        cTop + cEnvOverrideDir + "," +
                        cTop + cEnvProgramsDir + "\Addon" + "," +
                        cTop + cEnvProgramsDir + "," +
                        cTop + cEnvCustFiles + "," +
                        cTop + cEnvResourceDir + "," +
                        cTop + cEnvResourceDir + "\Addon" + "," +
                        cMapDir + "\" + cAdminDir + "\" + cEnvAdmin + ","
            PROPATH = preProPath + origPropath.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeMode C-Win 
PROCEDURE ipChangeMode :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iModeIndex AS INT NO-UNDO.
    
    ASSIGN
        g-sharpshooter = IF cSelectedMode EQ "Sharpshooter" THEN TRUE ELSE FALSE
        iModeIndex = LOOKUP(cSelectedMode,cModeList)
        cSelectedPgm = ENTRY(iModeIndex,cPgmList)
        cRunPgm = cSelectedPgm.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipClickOk C-Win 
PROCEDURE ipClickOk :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError AS LOG NO-UNDO.
    DEFINE VARIABLE cMessage AS CHAR NO-UNDO.
    DEFINE VARIABLE cCmdString AS CHAR NO-UNDO.
    DEFINE VARIABLE cDbLevel AS CHAR NO-UNDO.
 
    ASSIGN
        iEnvPos = LOOKUP(cSelectedEnvironment,cEnvList)
        iEnvLevel = intVer(ENTRY(iEnvPos,cEnvVerList))
        iDbPos = LOOKUP(cSelectedDatabase,cDbList)
        iDbLevel = intVer(ENTRY(iDbPos,cDbVerList))
        cDbLevel = STRING(iDbLevel)
        cDbLevel = SUBSTRING(cDbLevel,1,6)
        iTruncLevel = INT(cDbLevel)
        NO-ERROR.

    IF connectStatement <> "" 
    AND cbMode NE "Monitor Users" THEN DO:
        RUN ipDisconnectDB IN THIS-PROCEDURE.
        RUN ipConnectDB IN THIS-PROCEDURE (connectStatement,
            OUTPUT lError).
    END.

    /* This is the normal operation for Mode choices */
    IF NOT cbMode = "Monitor Users" THEN DO: 

        /* 95512 Optimize use of .ini files to set color and fonts */
        IF PROCESS-ARCHITECTURE = 64 THEN DO:
            LOAD "dbms".
            USE "dbms".
        END.
        ELSE DO:
            IF SEARCH("C:\Progress\OE116_32\progress.cfg") NE ? 
            AND SEARCH("dbms32s.ini") NE "" THEN DO:
                LOAD "dbms32s".
                USE "dbms32s".
            END.
            ELSE DO:
                LOAD "dbms32".
                USE "dbms32".
            END.
        END.
        IF iEnvLevel GE 21000000 THEN DO:
            PUT-KEY-VALUE SECTION "Colors" KEY "Normal" VALUE "".
            PUT-KEY-VALUE SECTION "Colors" KEY "Input" VALUE "".
            PUT-KEY-VALUE SECTION "Colors" KEY "Messages" VALUE "".
            DO iCtr = 1 TO NUM-ENTRIES(cNewFonts,"|"):
                PUT-KEY-VALUE SECTION "Fonts" KEY "Font" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cNewFonts,"|").
            END.
            DO iCtr = 1 TO NUM-ENTRIES(cNewColors,"|"):
                PUT-KEY-VALUE SECTION "Colors" KEY "Color" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cNewColors,"|").
            END.
        END.
        ELSE DO:
            PUT-KEY-VALUE SECTION "Colors" KEY "Normal" VALUE "".
            PUT-KEY-VALUE SECTION "Colors" KEY "Input" VALUE "".
            PUT-KEY-VALUE SECTION "Colors" KEY "Messages" VALUE "".
            DO iCtr = 1 TO NUM-ENTRIES(cOldFonts,"|"):
                PUT-KEY-VALUE SECTION "Fonts" KEY "Font" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cOldFonts,"|").
            END.
            DO iCtr = 1 TO NUM-ENTRIES(cOldColors,"|"):
                PUT-KEY-VALUE SECTION "Colors" KEY "Color" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cOldColors,"|").
            END.
        END.
        PUT-KEY-VALUE COLOR ALL.
        PUT-KEY-VALUE FONT ALL.
        IF PROCESS-ARCHITECTURE = 64 THEN DO:
            LOAD "dbms".
            USE "dbms".
        END.
        ELSE DO:
            IF SEARCH("C:\Progress\OE116_32\progress.cfg") NE ? 
            AND SEARCH("dbms32s.ini") NE "" THEN DO:
                LOAD "dbms32s".
                USE "dbms32s".
            END.
            ELSE DO:
                LOAD "dbms32".
                USE "dbms32".
            END.
        END.
        

        /* Set current dir */
        RUN ipSetCurrentDir (cMapDir + "\" + cEnvDir + "\" + cbEnvironment). 
        
        /* Run the mode program selected in the login dialog */
        RUN VALUE(cRunPgm).
        
        /* On exit of the run pgm, ensure the user is logged out and disconnected */
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        
        /* Close the dialog box and leave prowin */
        QUIT.
    END.
    /* This is only used for the OLD monitor users (proenv) */
    ELSE DO: 
        ASSIGN 
            cCmdString = cDLCDir + "\bin\proshut.bat" + " -db " +
                         cDrive + "\" + cTopDir + "\" + cDbDir + "\" + xDbDir + "\" + PDBNAME(1).
        OS-COMMAND VALUE(cCmdString).
    END.
    
    IF cSessionParam EQ "" THEN DO:
        RUN system/userLogout.p (YES, 0).
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        QUIT.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConnectDb C-Win 
PROCEDURE ipConnectDb :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cStatement AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER lError AS LOG NO-UNDO.

    DEFINE VARIABLE iLookup AS INT NO-UNDO.
    DEFINE VARIABLE xdbName AS CHAR NO-UNDO.
    DEFINE VARIABLE xdbPort AS CHAR NO-UNDO.
    DEFINE VARIABLE cCmdSTring AS CHAR NO-UNDO.
    DEFINE VARIABLE cMessString AS CHAR NO-UNDO.
    DEFINE VARIABLE iPos AS INT NO-UNDO.

    /* Force user id and password from screen values, trap errors in ERROR-STATUS */
    IF fiPassword NE "" THEN ASSIGN
        cStatement = cStatement + " -U " + cUserID + " -P '" + fiPassword + "'".
    ELSE ASSIGN
        cStatement = cStatement + " -U " + cUserID + " -ct 2".
    
    CONNECT VALUE(cStatement) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN DO:
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            /* Strip off the "Progress-y" elements of the error */
            ASSIGN
                cMessString = SUBSTRING(ERROR-STATUS:GET-MESSAGE(iCtr),3)
                iPos = INDEX(cMessString,"(")
                cMessString = "   " + SUBSTRING(cMessString,1,iPos - 1).
            MESSAGE
                "Unable to connect to the ASI database due to the following error:" SKIP
                cMessString
                VIEW-AS ALERT-BOX ERROR.
        END.
        /* Quit after lockout tries exceeded */
        ASSIGN
            iTries = iTries + 1.
        IF iLockoutTries > 0 AND iTries > iLockoutTries THEN 
        DO:
            MESSAGE
                "You have exceeded the maximum allowed login attempts." SKIP
                "Exiting..."
                VIEW-AS ALERT-BOX ERROR.
            DO ictr = 1 TO NUM-DBS:
                DISCONNECT VALUE(LDBNAME(iCtr)).
            END.
            QUIT.
        END.
        RETURN ERROR.
    END.

    IF CONNECTED(LDBNAME(1)) THEN DO:
        CREATE ALIAS nosweat FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS jobs FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS rfq FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asihelp FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asihlp FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asinos FOR DATABASE VALUE(LDBNAME(1)).

        IF SEARCH(origDirectoryName + "\preRun" + STRING(iTruncLevel,"999999") + ".r") NE ? THEN
            RUN VALUE(origDirectoryName + "\preRun" + STRING(iTruncLevel,"999999") + ".p") PERSISTENT SET hPreRun.
        ELSE RUN VALUE("prerun.p") PERSISTENT SET hPreRun.
    END.

    /* Connect AUDIT database */
    ASSIGN
        iPos = LOOKUP(cSelectedEnvironment,cEnvList)
        iEnvLevel = intVer(ENTRY(iPos,cEnvVerList))
        xdbName = cbDatabase
        iLookup = LOOKUP(cSelectedDatabase,cDbList)
        xDbName = ENTRY(iLookup,cAudDbList)
        xdbPort = ENTRY(iLookup,cAudPortList)
        connectStatement = "".
    IF xDbName NE "" THEN DO:
        ASSIGN
            connectStatement = "-db " + xDbName + 
                               " -H " + chostName +
                               " -S " + xdbPort + 
                               " -N tcp -ld AUDIT".

        CONNECT VALUE(connectStatement) NO-ERROR.

        IF ERROR-STATUS:ERROR 
        AND NOT CONNECTED(LDBNAME(2)) THEN 
        DO:
            DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
                /* Strip off the "Progress-y" elements of the error */
                ASSIGN
                    cMessString = SUBSTRING(ERROR-STATUS:GET-MESSAGE(iCtr),3)
                    iPos = INDEX(cMessString,"(")
                    cMessString = "   " + SUBSTRING(cMessString,1,iPos - 1).
                MESSAGE
                    "Unable to connect to the AUDIT database due to the following error:" SKIP
                    cMessString
                    VIEW-AS ALERT-BOX ERROR.
            END.
        END.
    END.
    
    RUN ipPreRun IN THIS-PROCEDURE.

    ASSIGN
        c-Win:VISIBLE = FALSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDisconnectDB C-Win 
PROCEDURE ipDisconnectDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CONNECTED(LDBNAME(2)) THEN
        DISCONNECT VALUE(LDBNAME(2)).

    IF CONNECTED(LDBNAME(1)) THEN
        DISCONNECT VALUE(LDBNAME(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFindUser C-Win 
PROCEDURE ipFindUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lNotFound AS LOG NO-UNDO.
    
    DEF VAR cTestModes AS CHAR NO-UNDO.
    /* First get the 'generic' part of the user - alias, db list, env list */
    FIND FIRST ttUsers NO-LOCK WHERE
        ttUsers.ttfUserID = fiUserID:{&SV} AND
        ttUsers.ttfPdbName = "*"
        NO-ERROR.
    IF NOT AVAILABLE ttUsers THEN FIND FIRST ttUsers NO-LOCK WHERE
        ttUsers.ttfUserAlias = fiUserID AND
        ttUsers.ttfPdbName = "*"
        NO-ERROR.
         
    /* If this has not yet been built, build off of any DB */
    IF NOT AVAILABLE ttUsers THEN 
    DO:
        ASSIGN lNotFound = TRUE. 
        RETURN.
    END.

    ASSIGN
        cUserID = ttUsers.ttfUserID
        cAvailDbList = ttUsers.ttfDbList
        cAvailEnvList = ttUsers.ttfEnvList.
            
    FOR EACH ttUsers NO-LOCK WHERE
        ttUsers.ttfUserID = cUserID AND
        ttUsers.ttfPdbName NE "":
        IF ttUsers.ttfModeList NE "" THEN DO:
            DO iCtr = 1 TO NUM-ENTRIES(ttUsers.ttfModeList):
                IF NOT CAN-DO(cTestModes, ENTRY(iCtr,ttUsers.ttfModeList)) THEN ASSIGN 
                    cTestModes = cTestModes + ENTRY(iCtr,ttUsers.ttfModeList)+ ",".
            END.
            ASSIGN 
                cTestModes = TRIM(cTestModes,",").
        END. 
    END.     

    ASSIGN 
        cAvailModeList = cTestModes.
        
    ASSIGN
        cAvailDbList = IF cAvailDbList NE "" THEN cAvailDbList ELSE cDbList
        cAvailEnvList = IF cAvailEnvList NE "" THEN cAvailEnvList ELSE cEnvList
        cAvailModeList = IF cAvailModeList NE "" THEN cAvailModeList ELSE cModeList
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipPreRun C-Win 
PROCEDURE ipPreRun :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lOK AS LOG INITIAL TRUE NO-UNDO.
    DEFINE VARIABLE lExit AS LOG INITIAL TRUE NO-UNDO.
    DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
    DEFINE VARIABLE hTags AS HANDLE NO-UNDO.
    DEFINE VARIABLE hCommonProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hCreditProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hPurgeProcs AS HANDLE NO-UNDO.

    ASSIGN
        iEnvPos = LOOKUP(cSelectedEnvironment,cEnvList)
        iEnvLevel = intVer(ENTRY(iEnvPos,cEnvVerList))
        iDbPos = LOOKUP(cSelectedDatabase,cDbList)
        iDbLevel = intVer(ENTRY(iDbPos,cDbVerList))
        iTruncLevel = iDbLevel
        .
    
    /* Run various procedures and programs DEPENDING ON ENV OR DB LEVEL */
    /* Here the format for both is 16070400 */
    IF USERID(LDBNAME(1)) NE "asi" THEN DO:
        RUN epCheckPwdExpire IN hPreRun (INPUT-OUTPUT lOK).
        IF NOT lOK THEN DO:
            RUN system/userLogout.p (YES, 0).
            DO ictr = 1 TO NUM-DBS:
                DISCONNECT VALUE(LDBNAME(iCtr)).
            END.
            QUIT.
        END.
    END.

    IF NOT VALID-HANDLE(hSession) THEN DO:
        RUN system/session.p PERSISTENT SET hSession.
        SESSION:ADD-SUPER-PROCEDURE (hSession).
        RUN spSetSessionParam ("Password", fiPassword). 
    END.
    
    /* Find running super procedures (later versions of session.p will start some of these, so don't need to be restarted */
    DEFINE VARIABLE cRunningSupers AS CHAR NO-UNDO.
    DEFINE VARIABLE hTestHandle AS HANDLE NO-UNDO.
    DO iCtr = 1 TO NUM-ENTRIES(SESSION:SUPER-PROCEDURES):
        hTestHandle = HANDLE(ENTRY(iCtr,SESSION:SUPER-PROCEDURES)).
        cRunningSupers = cRunningSupers + hTestHandle:NAME + ",".
    END.
    ASSIGN 
        cRunningSupers = TRIM(cRunningSupers,",").
        
    IF NOT CAN-DO(cRunningSupers,"system/TagProcs.p") THEN DO: 
        RUN system/TagProcs.p PERSISTENT SET hTags.
        SESSION:ADD-SUPER-PROCEDURE (hTags).
    END.
    
    IF iEnvLevel GE 16130000 THEN DO:
        IF NOT CAN-DO(cRunningSupers,"system/CommonProcs.p") THEN DO: 
            RUN system/commonProcs.p PERSISTENT SET hCommonProcs.
            SESSION:ADD-SUPER-PROCEDURE (hCommonProcs).
        END.
        IF NOT CAN-DO(cRunningSupers,"system/CreditProcs.p") THEN DO: 
            RUN system/creditProcs.p PERSISTENT SET hCreditProcs.
            SESSION:ADD-SUPER-PROCEDURE (hCreditProcs).
        END.
        /* Take this out ASAP; purge procs do NOT need to be supered */
        IF NOT CAN-DO(cRunningSupers,"system/PurgeProcs.p") THEN DO: 
            RUN system/purgeProcs.p PERSISTENT SET hPurgeProcs.
            SESSION:ADD-SUPER-PROCEDURE (hPurgeProcs).
        END.
    END.
    
    IF NOT VALID-HANDLE(persistent-handle) THEN
        RUN nosweat/persist.p PERSISTENT SET persistent-handle.
    IF NOT VALID-HANDLE(listlogic-handle) THEN
        RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    IF cbMode EQ "Touchscreen" THEN 
        RUN epTouchLogin IN hPreRun (OUTPUT tslogin-log).

    RUN epUserRecordCheck IN hPreRun (OUTPUT lOK, OUTPUT g_track_usage).
    IF NOT lOK THEN DO:
        RUN system/userLogout.p (YES, 0).
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        QUIT.
    END.

    /* Run user-level tests and create the userlog before doing the session-level work */
    IF cbMode NE "Monitor Users" 
    AND cbMode NE "Editor" THEN DO:
        IF iEnvLevel GE 16150300 THEN 
            RUN epUserLogin IN hPreRun (cbMode, OUTPUT lExit).
        ELSE 
            RUN epUserLogin IN hPreRun (OUTPUT lExit).
        IF lExit THEN DO:
            RUN system/userLogout.p (YES, 0).
            DO ictr = 1 TO NUM-DBS:
                DISCONNECT VALUE(LDBNAME(iCtr)).
            END.
            QUIT.
        END.
    END.
     
    RUN epUpdateUsrFile IN hPreRun (OUTPUT cUsrList).
    RUN ipUpdUsrFile IN THIS-PROCEDURE (cUsrList).
    RUN epGetUserGroups IN hPreRun (OUTPUT g_groups).
    RUN epSetUpEDI IN hPreRun.
    RUN epCheckExpiration IN hPreRun (OUTPUT lOK).
    IF NOT lOK THEN DO:
        RUN system/userLogout.p (YES, 0).
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        QUIT.
    END.
    RUN epGetDeveloperList IN hPreRun (OUTPUT g_developer).
    RUN epGetUsercomp IN hPreRun (OUTPUT g_company, OUTPUT g_loc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadUsrFile C-Win 
PROCEDURE ipReadUsrFile :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    ASSIGN 
        lCorrupt = FALSE
        iCtr = 1.
    INPUT FROM VALUE(SEARCH(cUsrLoc)).
    REPEAT:
        IMPORT UNFORMATTED cUsrLine.
        IF INDEX(cUsrLine,"|") NE 0 THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfUserID = ENTRY(1,cUsrLine,"|")
                ttUsers.ttfPdbname = ENTRY(2,cUsrLine,"|")
                ttUsers.ttfUserAlias = ENTRY(3,cUsrLine,"|")
                ttUsers.ttfEnvList = ENTRY(4,cUsrLine,"|")
                ttUsers.ttfDbList = ENTRY(5,cUsrLine,"|")
                ttUsers.ttfModeList = ENTRY(6,cUsrLine,"|")
                iCtr = iCtr + 1.
        END.
    END.
    INPUT CLOSE.
    ASSIGN
        iNumUsers = iCtr.

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
    DEFINE INPUT PARAMETER ipTgtDir AS CHAR NO-UNDO.
    DEFINE VARIABLE iResult AS INT NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INT NO-UNDO.
    
    &IF DEFINED(FWD-VERSION) > 0 &THEN
    set-working-directory(ipTgtDir).
    &ELSE
    
    RUN SetCurrentDirectoryA (ipTgtDir, OUTPUT iResult).

    IF iResult NE 1 THEN DO:
        RUN GetLastError (OUTPUT iReturnValue).
        MESSAGE 
            "Unable to set working directory." SKIP
            "Error code:" iReturnValue 
            VIEW-AS ALERT-BOX.
    END.
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdUsrFile C-Win 
PROCEDURE ipUpdUsrFile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       The ONLY reason to run this is if in "healing" mode; i.e there's been
                   a change to the structure of the .usr file or the ttUsers temp-table
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcUserList AS CHAR NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR cOutString AS CHAR.
    
    /* ipcUserList is a list of all _user records in the connected DB; we need
    one ttUser for the generics (ttfPdbName = "*") and one for the specific. */
    DO iCtr = 1 TO NUM-ENTRIES(ipcUserList):
        FIND FIRST ttUsers WHERE
            ttUsers.ttfPdbName = "*" AND
            ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                lUpdUsr = TRUE
                ttUsers.ttfPdbname = "*"
                ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
                ttUsers.ttfUserAlias = ENTRY(iCtr,ipcUserList)
                .
        END.
        FIND FIRST ttUsers WHERE
            ttUsers.ttfPdbName = cbDatabase AND
            ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                lUpdUsr = TRUE
                ttUsers.ttfPdbname = cbDatabase
                ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
                .
        END.
    END.
    FOR EACH ttUsers WHERE ttUsers.ttfPdbName = cbDatabase:
        IF LOOKUP(ttUsers.ttfUserID,ipcUserList) = 0 THEN DO:
            ASSIGN
                lUpdUsr = TRUE.
            DELETE ttUsers.
        END.
    END.
    IF lUpdUsr = TRUE THEN DO:
        OUTPUT STREAM usrStream TO VALUE(cUsrLoc).
        FOR EACH ttUsers BY ttUsers.ttfPdbname by ttUsers.ttfUserID:
            ASSIGN 
                cOutString = 
                ttUsers.ttfUserID + "|" + 
                ttUsers.ttfPdbName + "|" +
                ttUsers.ttfUserAlias + "|" + 
                ttUsers.ttfEnvList + "|" +
                ttUsers.ttfDbList + "|" +
                ttUsers.ttfModeList.
            PUT STREAM usrStream UNFORMATTED cOutString + CHR(10).
        END.
        OUTPUT STREAM usrStream CLOSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION intVer C-Win 
FUNCTION intVer RETURNS INTEGER
    ( INPUT cVerString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStrVal AS CHAR EXTENT 4 NO-UNDO.
    DEFINE VARIABLE iIntVal AS INT EXTENT 4 NO-UNDO.
    DEFINE VARIABLE iIntVer AS INT NO-UNDO.
    ASSIGN
        cStrVal[1] = IF NUM-ENTRIES(cVerString,".") GE 1 THEN ENTRY(1,cVerString,".") ELSE "00"
        cStrVal[2] = IF NUM-ENTRIES(cVerString,".") GE 2 THEN ENTRY(2,cVerString,".") ELSE "00"
        cStrVal[3] = IF NUM-ENTRIES(cVerString,".") GE 3 THEN ENTRY(3,cVerString,".") ELSE "00"
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GE 4 THEN ENTRY(4,cVerString,".") ELSE "00"
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

