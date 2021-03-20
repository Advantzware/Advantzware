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
&SCOPED-DEFINE IN IN FRAME DEFAULT-FRAME
&SCOPED-DEFINE SV SCREEN-VALUE {&IN}
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE createSingleUserPFs YES
&SCOPED-DEFINE execProgram mainmenu.    
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES

DEF STREAM usrStream.

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

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_sysdate AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE g_period AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_init AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE NEW SHARED VARIABLE miscflds_reckey AS CHARACTER.
DEFINE NEW SHARED VARIABLE table_reckey AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE persistent-handle AS HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE ListLogic-Handle AS HANDLE.
DEFINE NEW SHARED VARIABLE igsSessionID AS INTEGER.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.

DEFINE VARIABLE cAsiConnectString AS CHAR NO-UNDO.
DEFINE VARIABLE cAudConnectString AS CHAR NO-UNDO.
DEFINE VARIABLE cAuditDbSelected AS CHAR NO-UNDO.
DEFINE VARIABLE cAuditPort AS CHAR NO-UNDO.
DEFINE VARIABLE cDbPort AS CHAR NO-UNDO.
DEFINE VARIABLE cDbSelected AS CHAR NO-UNDO.
DEFINE VARIABLE cDbValidList AS CHAR NO-UNDO.
DEFINE VARIABLE cEnvSelected AS CHAR NO-UNDO.
DEFINE VARIABLE cEnvValidList AS CHAR NO-UNDO.
DEFINE VARIABLE cModeSelected AS CHAR NO-UNDO.
DEFINE VARIABLE cModeValidList AS CHAR NO-UNDO.
DEFINE VARIABLE cNewColors AS CHAR NO-UNDO.
DEFINE VARIABLE cNewFonts AS CHAR NO-UNDO.
DEFINE VARIABLE cOldColors AS CHAR NO-UNDO.
DEFINE VARIABLE cOldFonts AS CHAR NO-UNDO.
DEFINE VARIABLE cPatchNo AS CHAR NO-UNDO.
DEFINE VARIABLE cRunPgm AS CHAR NO-UNDO.
DEFINE VARIABLE cSessionParam AS CHAR NO-UNDO.
DEFINE VARIABLE cTsLogin AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID AS CHAR NO-UNDO.
DEFINE VARIABLE cUsrFileName AS CHAR NO-UNDO.
DEFINE VARIABLE cUsrLine AS CHAR NO-UNDO.
DEFINE VARIABLE cUsrList AS CHAR NO-UNDO.
DEFINE VARIABLE cVarName AS CHAR EXTENT 100 NO-UNDO.
DEFINE VARIABLE cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEFINE VARIABLE help-page AS INTEGER NO-UNDO.
DEFINE VARIABLE hPreRun AS HANDLE.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iCtr AS INT NO-UNDO.
DEFINE VARIABLE iDbLevel AS INT NO-UNDO.
DEFINE VARIABLE iDbPos AS INT NO-UNDO.
DEFINE VARIABLE iDbVer AS INT NO-UNDO.
DEFINE VARIABLE iEnvLevel AS INT NO-UNDO.
DEFINE VARIABLE iEnvPos AS INT NO-UNDO.
DEFINE VARIABLE iEnvVer AS INT NO-UNDO.
DEFINE VARIABLE iLockoutTries AS INT NO-UNDO.
DEFINE VARIABLE iModePos AS INT NO-UNDO.
DEFINE VARIABLE intBufferSize AS INTEGER NO-UNDO INITIAL 256.
DEFINE VARIABLE intResult AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumUsers AS INT NO-UNDO.
DEFINE VARIABLE is-running AS LOGICAL NO-UNDO.
DEFINE VARIABLE iTries AS INT NO-UNDO.
DEFINE VARIABLE iTruncLevel AS INT NO-UNDO.
DEFINE VARIABLE jCtr AS INT NO-UNDO.
DEFINE VARIABLE lConnectAudit AS LOG NO-UNDO.
DEFINE VARIABLE lCorrupt AS LOG NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE lExit AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFoundIni AS LOG NO-UNDO.
DEFINE VARIABLE lFoundUsr AS LOG NO-UNDO.
DEFINE VARIABLE lMakeBackup AS LOG NO-UNDO.
DEFINE VARIABLE lSysError AS LOG NO-UNDO.
DEFINE VARIABLE lUpdUsr AS LOG NO-UNDO.
DEFINE VARIABLE lUserOK AS LOG NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
DEFINE VARIABLE origDirectoryName AS CHARACTER NO-UNDO FORMAT "X(256)".
DEFINE VARIABLE origPropath AS CHAR NO-UNDO.
DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ptrToString AS MEMPTR NO-UNDO.
DEFINE VARIABLE run-proc AS CHARACTER.
DEFINE VARIABLE tslogin-cha AS CHAR NO-UNDO.
DEFINE VARIABLE tslogin-log AS LOGICAL NO-UNDO.
DEFINE VARIABLE xDbDir AS CHAR NO-UNDO.

{iniFileVars.i}

DEF TEMP-TABLE ttUsers
    FIELD ttfUserID AS CHAR
    FIELD ttfUserAlias AS CHAR
    FIELD ttfPdbName AS CHAR 
    FIELD ttfEnvList AS CHAR
    FIELD ttfDbList AS CHAR
    FIELD ttfModeList AS CHAR
    INDEX iUserID IS UNIQUE ttfUserID ttfPdbName.
DEF BUFFER bttUsers FOR ttUsers.

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
    DEF INPUT PARAM pname AS CHAR.
    DEF INPUT PARAM runPersistent AS LOG.
    DEF OUTPUT PARAM phandle AS HANDLE.
    // these need to be executed in the context of asiLogin.w, in embedded mode
    IF runPersistent THEN 
        RUN value(pname) PERSISTENT SET phandle.
    ELSE 
        RUN value(pname).
END.

IF origDirectoryName = "" THEN DO:
    &IF DEFINED(FWD-VERSION) > 0 &THEN
        ASSIGN 
            origDirectoryName = get-working-directory().
    &ELSE
        SET-SIZE(ptrToString) = 256.
        RUN GetCurrentDirectoryA (INPUT intBufferSize,
                                  INPUT-OUTPUT ptrToString,
                                  OUTPUT intResult).
        ASSIGN 
            origDirectoryName = GET-STRING(ptrToString,1).    
    &ENDIF
END.
ELSE DO:
    RUN ipSetCurrentDir (origDirectoryName). 
END.
        
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
       
/* Find the .ini file containing variables and values */
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

IF SEARCH(cMapDir + "\" + cAdminDir + "\advantzware.ini") EQ ? THEN DO:
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
&Scoped-Define ENABLED-OBJECTS IMAGE-2 fiUserID fiPassword Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiPassword 

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
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Login" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE cbDatabase AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbEnvironment AS CHARACTER FORMAT "X(256)":U 
     LABEL "Environment" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "Live","Test" 
     DROP-DOWN-LIST
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbMode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mode" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "Standard","Touchscreen","Sharpshooter","Monitor" 
     DROP-DOWN-LIST
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
    SIZE 25 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-2
     SIZE 40 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    fiUserID AT ROW 8.38 COL 15 COLON-ALIGNED WIDGET-ID 4
    fiPassword AT ROW 9.81 COL 15 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD 
    cbMode AT ROW 11.24 COL 15 COLON-ALIGNED WIDGET-ID 10
    cbEnvironment AT ROW 12.67 COL 15 COLON-ALIGNED WIDGET-ID 8
    cbDatabase AT ROW 14.1 COL 15 COLON-ALIGNED WIDGET-ID 18
    Btn_Cancel AT ROW 15.76 COL 6 WIDGET-ID 22
    Btn_OK AT ROW 15.76 COL 27 WIDGET-ID 26
    IMAGE-2 AT ROW 1.48 COL 4 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
    SIZE 46.8 BY 17.19
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
IF SESSION:DISPLAY-TYPE = "GUI":U 
    AND cSessionParam EQ "" THEN
  CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Login"
        HEIGHT             = 16.86
        WIDTH              = 45.6
        MAX-HEIGHT         = 21.48
        MAX-WIDTH          = 83.2
        VIRTUAL-HEIGHT     = 21.48
        VIRTUAL-WIDTH      = 83.2
        SMALL-TITLE        = yes
        SHOW-IN-TASKBAR    = yes
        CONTROL-BOX        = no
        MIN-BUTTON         = no
        MAX-BUTTON         = no
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
/* SETTINGS FOR COMBO-BOX cbDatabase IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbDatabase:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX cbEnvironment IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbEnvironment:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX cbMode IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbMode:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    DO ictr = 1 TO NUM-DBS:
        DISCONNECT VALUE(LDBNAME(iCtr)).
    END.
    QUIT.
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
    RUN ipAssignSV.
    RUN ipChangeMode.
    RUN ipChangeEnvironment.
    RUN ipChangeDatabase.
    RUN ipClickOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDatabase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDatabase C-Win
ON VALUE-CHANGED OF cbDatabase IN FRAME DEFAULT-FRAME /* Database */
OR VALUE-CHANGED OF cbEnvironment
OR VALUE-CHANGED OF cbMode
OR LEAVE OF cbDatabase
OR LEAVE OF cbEnvironment
OR LEAVE OF cbMode
DO:
    RUN ipAssignSV.
    CASE SELF:NAME:
        WHEN "cbDatabase" THEN RUN ipChangeDatabase.
        WHEN "cbEnvironment" THEN RUN ipChangeEnvironment.
        WHEN "cbMode" THEN RUN ipChangeMode.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
    RUN ipAssignSV.
    FIND FIRST ttUsers NO-LOCK WHERE
        ttUsers.ttfUserID = fiUserID
        NO-ERROR.
    IF NOT AVAIL ttUsers THEN DO:
        IF fwd-embedded-mode THEN 
            RETURN NO-APPLY 
                "Unable to locate this user in the advantzware.usr file." +
                "Please contact your system administrator for assistance.".
        ELSE DO:
            MESSAGE
                "Unable to locate this user in the advantzware.usr file." SKIP
                "Please contact your system administrator for assistance."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    ELSE ASSIGN 
        cEnvValidList = IF ttUsers.ttfEnvList NE "" THEN ttUsers.ttfEnvList ELSE cEnvList
        cModeValidList = IF ttUsers.ttfModeList NE "" THEN ttUsers.ttfModeList ELSE cModeList
        cDbValidList = IF ttUsers.ttfDbList NE "" THEN ttUsers.ttfDbList ELSE cDbList.

    IF SELF:SCREEN-VALUE EQ "Monitor" THEN DO:
        MESSAGE 
            "The user 'Monitor' cannot be used for interactive" SKIP 
            "logins. Please choose another user id."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
    
    ASSIGN
        /* set the combo box possible values */
        cbMode:LIST-ITEMS = cModeValidList
        cbEnvironment:LIST-ITEMS = cEnvValidList
        cbDatabase:LIST-ITEMS = cDbValidList
        /* set the combo box screen-values */
        cbMode:SCREEN-VALUE = ENTRY(1,cbMode:LIST-ITEMS)
        cbEnvironment:SCREEN-VALUE = ENTRY(1,cbEnvironment:LIST-ITEMS)
        cbDatabase:SCREEN-VALUE = ENTRY(1,cbDatabase:LIST-ITEMS)
        /* Turn visibility on or off depending on number of valid entries */
        cbMode:VISIBLE = TRUE /* NUM-ENTRIES(cbMode:LIST-ITEMS) NE 1 */
        cbMode:SENSITIVE = NUM-ENTRIES(cbMode:LIST-ITEMS) NE 1
        cbEnvironment:VISIBLE = TRUE /* NUM-ENTRIES(cbEnvironment:LIST-ITEMS) NE 1  */
        cbEnvironment:SENSITIVE = NUM-ENTRIES(cbEnvironment:LIST-ITEMS) NE 1
        cbDatabase:VISIBLE = TRUE /* NUM-ENTRIES(cbDatabase:LIST-ITEMS) NE 1  */
        cbDatabase:SENSITIVE = NUM-ENTRIES(cbDatabase:LIST-ITEMS) NE 1
        /* set the 'selected' values for use in ipClickOK */
        cEnvSelected = cbEnvironment:SCREEN-VALUE 
        cModeSelected = cbMode:SCREEN-VALUE 
        cDbSelected = cbDatabase:SCREEN-VALUE 
        .
    
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

    /* Pre-visualization tasks */
    IMAGE-2:LOAD-IMAGE("asilogosm.jpg").
    ASSIGN
        cSessionParam = SESSION:PARAM
        g_lookup-var = ""
        g_init = yes
        g_sysdate = TODAY
        g_version = "2.1A-8.2A"
        origPropath = PROPATH.

    IF cSessionParam EQ "" THEN DO:
        ASSIGN
            cbEnvironment:LIST-ITEMS = TRIM(cEnvList,",")
            cbMode:LIST-ITEMS = TRIM(cModeList,",")
            cbDatabase:LIST-ITEMS = TRIM(cDbList,",")
            cbDatabase:SCREEN-VALUE = ENTRY(1,cDbList)
            cbEnvironment:SCREEN-VALUE = ENTRY(1,cEnvList)
            cbMode:SCREEN-VALUE = ENTRY(1,cModeList)
            cbMode:SENSITIVE = FALSE 
            cbEnvironment:SENSITIVE = FALSE 
            cbDatabase:SENSITIVE = FALSE 
            cbMode:VISIBLE = TRUE
            cbEnvironment:VISIBLE = TRUE 
            cbDatabase:VISIBLE = TRUE. 
        RUN enable_UI.
        ASSIGN
            fiUserID:SCREEN-VALUE = OS-GETENV("USERNAME").
        APPLY 'entry' TO fiUserID.
    END. /* If there is a UI */
    ELSE DO:
        ASSIGN
            CURRENT-WINDOW:VISIBLE = FALSE.
        RUN ipAutoLogin.
    END.

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
  DISPLAY fiUserID fiPassword 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-2 fiUserID fiPassword Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAssignSV C-Win 
PROCEDURE ipAssignSV :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fiUserID 
            fiPassword 
            cEnvSelected = cbEnvironment:{&SV}
            cModeSelected = cbMode:{&SV}      
            cDbSelected = cbDatabase:{&SV}  
            .
    END.
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
        cEnvSelected = ENTRY(3, cSessionParam)
        cModeSelected = REPLACE(ENTRY(4, cSessionParam),"_"," ")
        cDbSelected = ENTRY(5, cSessionParam)
        .   
    
    RUN ipChangeMode. /* Gets program to run based on mode */
    RUN ipChangeEnvironment. /* Gets env version and 'can' change database availability */
    RUN ipChangeDatabase. /* sets up DB connection strings */
    
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
    DEFINE VARIABLE asiDbName AS CHAR NO-UNDO.
    DEFINE VARIABLE asiDbPort AS CHAR NO-UNDO.
    DEFINE VARIABLE audDbName AS CHAR NO-UNDO.
    DEFINE VARIABLE audDbPort AS CHAR NO-UNDO.

    ASSIGN
        cAsiConnectString = ""
        cAudConnectString = ""
        iDbPos = LOOKUP(cDbSelected,cDbList)
        iDbLevel = intVer(ENTRY(iDbPos,cDbVerList))
        asidbName = cDbSelected
        asiDbPort = ENTRY(iDbPos,cDbPortList)
        audDbName = ENTRY(iDbPos,cAudDbList)
        audDbPort = ENTRY(iDbPos,cAudPortList)
        .
    IF asiDbName <> ""
    AND asiDbPort <> "" THEN ASSIGN
        cAsiConnectString = "-db " + asiDbName + 
                            " -H " + chostName +
                            " -S " + asiDbPort +
                            " -N tcp -ld ASI -U " +
                            fiUserID + 
                            " -P '" +
                            fiPassword + 
                            "' -ct 2".
    ELSE DO:
        MESSAGE
            "There is a problem with your database connection list." SKIP
            "Pleasse contact Advantzware support for assistance."
            VIEW-AS ALERT-BOX.
        APPLY 'choose' to btn_Cancel IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
    END.

    IF audDbName <> ""
    AND audDbPort <> "" THEN ASSIGN
        cAudConnectString = "-db " + audDbName + 
                            " -H " + chostName +
                            " -S " + audDbPort +
                            " -N tcp -ld Audit -ct 2".
    

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
    DEFINE VARIABLE iEnvLevelM AS INT NO-UNDO.
    
    ASSIGN
        iEnvPos = LOOKUP(cEnvSelected,cEnvList)
        iEnvLevel = intVer(ENTRY(iEnvPos,cEnvVerList))
        .

    IF cSessionParam NE "" THEN DO: /* Set propath for autologin mode */
        ASSIGN
            cTop = cMapDir + "\" + cEnvDir + "\" + cEnvSelected + "\" 
            preProPath = cTop + "," +
                         (IF iEnvLevel GE 21000000 THEN cTop + "asiObjects.pl," ELSE "") +
                         (IF iEnvLevel GE 21000000 THEN cTop + "asigraphics.pl," ELSE "") +
                         cTop + cEnvCustomerDir + "," +
                         cTop + cEnvOverrideDir + "," +
                         cTop + cEnvProgramsDir + "," +
                         cTop + cEnvProgramsDir + "\Addon" + "," +
                         cTop + cEnvResourceDir + "," +
                         cTop + cEnvCustFiles + "," +
                         cMapDir + "\" + cAdminDir + "\" + cEnvAdmin + ","
            PROPATH = preProPath + origPropath.      
        RETURN.
    END.
    ELSE DO: /* Normal processing, with changes to drop downs */
	CASE cEnvSelected:
            WHEN "Prod" THEN DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                IF ENTRY(iCtr,cDbList) EQ "asiProd" THEN ASSIGN
                    cDbValidList = "asiProd".
            END.
            WHEN "Test" THEN DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                IF ENTRY(iCtr,cDbList) EQ "asiTest" THEN ASSIGN
                    cDbValidList = "asiTest".
            END.
            OTHERWISE DO:
                ASSIGN 
                    cTestList = ""
                    cDbValidList = cDbList.
                DO iCtr = 1 TO NUM-ENTRIES(cDbList):
                    IF intVer(ENTRY(iCtr,cDbVerList)) EQ iEnvLevel THEN ASSIGN
                        cTestList = cTestList + ENTRY(iCtr,cDbList) + ",".
                END.
                ASSIGN 
                    cTestList = TRIM(cTestList,",")
                    cDbValidList = cTestList.
            END.
        END CASE.

        ASSIGN
            cbDatabase:LIST-ITEMS IN FRAME {&frame-name} = cDBValidList
            cbDatabase:VISIBLE = TRUE /* NUM-ENTRIES(cDbValidList) NE 1 */
            cTop = cMapDir + "\" + cEnvDir + "\" + cEnvSelected + "\" 
            preProPath = cTop + "," +
                         (IF iEnvLevel GE 21000000 THEN cTop + "asiObjects.pl," ELSE "") +
                         (IF iEnvLevel GE 21000000 THEN cTop + "asigraphics.pl," ELSE "") +
                         cTop + cEnvCustomerDir + "," +
                         cTop + cEnvOverrideDir + "," +
                         cTop + cEnvProgramsDir + "," +
                         cTop + cEnvProgramsDir + "\Addon" + "," +
                         cTop + cEnvResourceDir + "," +
                         cTop + cEnvCustFiles + "," +
                         cMapDir + "\" + cAdminDir + "\" + cEnvAdmin + ","
            PROPATH = preProPath + origPropath.

        IF NUM-ENTRIES(cDbValidList) EQ 1 THEN DO:
            ASSIGN
                cDBSelected = cDbValidList
                cbDatabase:LIST-ITEMS = cDbValidList
                cbDatabase:SCREEN-VALUE = cDbSelected
                cbDatabase:SENSITIVE = FALSE.
            RUN ipChangeDatabase.
        END.        
        ELSE DO:
            ASSIGN
                cbDatabase:LIST-ITEMS = cDbValidList
                cbDatabase:SCREEN-VALUE = IF NOT CAN-DO(cDbValidList,cDbSelected) THEN ENTRY(1,cDbValidList) ELSE cDbSelected
                cbDatabase:SENSITIVE = TRUE.
            RETURN NO-APPLY.
        END.

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
    DEFINE VARIABLE cPgmItem AS CHAR NO-UNDO.
    
    ASSIGN
        cModeSelected = IF cbMode:VISIBLE IN FRAME {&frame-name} THEN cbMode:{&SV} ELSE cModeSelected
        g-sharpshooter = IF cModeSelected EQ "Sharpshooter" THEN true else false
        iModePos = LOOKUP(cModeSelected,cModeList)
        cPgmItem = ENTRY(iModePos,cPgmList)
        cRunPgm = cPgmItem.
        
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
    DEFINE VARIABLE cDLC AS CHAR NO-UNDO.
    DEFINE VARIABLE cBitness AS CHAR NO-UNDO.
    DEFINE VARIABLE cVersion AS CHAR NO-UNDO.
 
    IF cAsiConnectString <> "" 
    AND cbMode NE "Monitor Users" THEN 
    DO:
        RUN ipDisconnectDB IN THIS-PROCEDURE.
        /* This connects both asi and audit DBs */
        RUN ipConnectDBs in THIS-PROCEDURE (OUTPUT lError).
    END.

    /* This is the normal operation for Mode choices */
    IF NOT cbMode = "Monitor Users" THEN DO: 

        /*  This is hideously complicated, as there are two independent variables:  64/32 bit architecture, and ASI versioo.
            We have to account for both, leading to 4 possible ini files, and we DON'T want to build on the fly, as these are
            server-level, and start-of-day logins overwhelm the filesystem.  Therefore we build these ini files once and, if built,
            just use them */
                    
        /* First, lets see what the bitness is, and get the appropriate DLC variable */
        IF PROCESS-ARCHITECTURE = 64 THEN DO:
            LOAD "dbms".
            USE "dbms".
            GET-KEY-VALUE SECTION "Startup" KEY "DLC" VALUE cDLC.
            cBitness = "64".
        END.
        ELSE DO:
            LOAD "dbms32".
            USE "dbms32".
            GET-KEY-VALUE SECTION "Startup" KEY "DLC" VALUE cDLC.
            cBitness = "32".
        END.

        /* Now determine the ASI version level */
        IF iEnvLevel GE 21000000 THEN ASSIGN 
            cVersion = "21".
        ELSE ASSIGN 
            cVersion = "16".
             
        /* If the required ini file already exists, just use it */
        IF SEARCH("dbms_" + cBitness + "_" + cVersion + ".ini") NE ? THEN DO:
            LOAD "dbms_" + cBitness + "_" + cVersion + ".ini" BASE-KEY "ini".
            USE "dbms_" + cBitness + "_" + cVersion + ".ini".
        END.               
        /* Otherwise, create it and then use it */
        ELSE DO:      
            LOAD "dbms_" + cBitness + "_" + cVersion + ".ini" NEW BASE-KEY "ini".
            USE "dbms_" + cBitness + "_" + cVersion + ".ini".
            
            PUT-KEY-VALUE SECTION "Startup" KEY "DLC" VALUE cDlc.
            PUT-KEY-VALUE SECTION "Colors" KEY "Normal" VALUE "".
            PUT-KEY-VALUE SECTION "Colors" KEY "Input" VALUE "".
            PUT-KEY-VALUE SECTION "Colors" KEY "Messages" VALUE "".
            IF cVersion EQ "21" THEN DO:
                DO iCtr = 1 TO NUM-ENTRIES(cNewFonts,"|"):
                    PUT-KEY-VALUE SECTION "Fonts" KEY "Font" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cNewFonts,"|").
                END.
                DO iCtr = 1 TO NUM-ENTRIES(cNewColors,"|"):
                    PUT-KEY-VALUE SECTION "Colors" KEY "Color" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cNewColors,"|").
                END.
            END.
            ELSE IF cVersion EQ "16" THEN DO:
                DO iCtr = 1 TO NUM-ENTRIES(cOldFonts,"|"):
                    PUT-KEY-VALUE SECTION "Fonts" KEY "Font" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cOldFonts,"|").
                END.
                DO iCtr = 1 TO NUM-ENTRIES(cOldColors,"|"):
                    PUT-KEY-VALUE SECTION "Colors" KEY "Color" + STRING(iCtr - 1) VALUE ENTRY(iCtr,cOldColors,"|").
                END.
            END.
            PUT-KEY-VALUE COLOR ALL.
            PUT-KEY-VALUE FONT ALL.
        END. 

        /* Set current dir */
        RUN ipSetCurrentDir (cMapDir + "\" + cEnvDir + "\" + cEnvSelected). 
        
        /* Run the mode program selected in the login dialog */
        RUN VALUE(cRunPgm).
        
        /* On exit of the run pgm, ensure the user is logged out and disconnected */
        RUN system/userLogout.p (YES, 0).
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        
        /* Close the dialog box and leave prowin */
        QUIT.
    END.
    /* This is only used to monitor users */
    ELSE 
    DO: 
        ASSIGN 
            cCmdString = cDLCDir + "\bin\proshut.bat" + " -db " +
                         cDrive + "\" + cTopDir + "\" + cDbDir + "\" + xDbDir + "\" + PDBNAME(1).
        OS-COMMAND VALUE(cCmdString).
    END.
    
    IF cSessionParam EQ "" THEN 
    DO:
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        QUIT.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConnectDbs C-Win 
PROCEDURE ipConnectDbs :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lError AS LOG NO-UNDO.
    DEF VAR iPos AS INT NO-UNDO.

    DEFINE VARIABLE cMessString AS CHAR NO-UNDO.

    /* Connect the selected ASI database */
    CONNECT VALUE(cAsiConnectString) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            /* Strip off the "Progress-y" elements of the error */
            ASSIGN
                cMessString = SUBSTRING(ERROR-STATUS:GET-MESSAGE(iCtr),3)
                iPos = INDEX(cMessString,"(")
                cMessString = "   " + SUBSTRING(cMessString,1,iPos - 1).
            MESSAGE
                "Unable to connect to the ASI database due to the following error:" skip
                cMessString
                VIEW-AS ALERT-BOX ERROR.
        END.
        /* Quit after lockout tries exceeded */
        ASSIGN
            iTries = iTries + 1.
        IF iLockoutTries > 0 AND iTries > iLockoutTries THEN DO:
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

    /* Connect the associated Audit database */
    CONNECT VALUE(cAudConnectString) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            /* Strip off the "Progress-y" elements of the error */
            ASSIGN
                cMessString = SUBSTRING(ERROR-STATUS:GET-MESSAGE(iCtr),3)
                iPos = INDEX(cMessString,"(")
                cMessString = "   " + SUBSTRING(cMessString,1,iPos - 1).
            MESSAGE
                "Unable to connect to the AUDIT database due to the following error:" skip
                cMessString
                VIEW-AS ALERT-BOX ERROR.
        END.
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

    RUN ipPreRun IN THIS-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpandVarNames C-Win 
PROCEDURE ipExpandVarNames :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Modify variables for ease of use */
    ASSIGN
        cAdminDir = cMapDir + "\" + cAdminDir
        cBackupDir = cMapDir + "\" + cBackupDir
        cDBDir = cMapDir + "\" + cDbDir
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
        lmakeBackup = TRUE
        cLockoutTries = SUBSTRING(cLockoutTries,1,1)
        iLockoutTries = IF cLockoutTries NE "" 
                        AND ASC(cLockoutTries) GE 48
                        AND ASC(cLockoutTries) LE 57 THEN INT(cLockoutTries) ELSE 0
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
    DEFINE VARIABLE lOK AS LOGICAL INITIAL TRUE NO-UNDO.
    DEFINE VARIABLE lExit AS LOGICAL INITIAL TRUE NO-UNDO.
    DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
    DEFINE VARIABLE hTags AS HANDLE NO-UNDO.
    DEFINE VARIABLE hCommonProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hCreditProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hPurgeProcs AS HANDLE NO-UNDO.

    ASSIGN
        iEnvPos = LOOKUP(cEnvSelected,cEnvList)
        iEnvLevel = intVer(ENTRY(iEnvPos,cEnvVerList))
        iDbPos = LOOKUP(cDbSelected,cDbList)
        iDbLevel = intVer(ENTRY(iDbPos,cDbVerList))
        iTruncLevel = iDbLevel
        .
    
    /* Run various procedures and programs DEPENDING ON ENV OR DB LEVEL */
    /* Here the format for both is 16070400 */
    IF USERID(LDBNAME(1)) NE "asi" THEN DO:
        RUN epCheckPwdExpire IN hPreRun (INPUT-OUTPUT lOK).
        IF NOT lOK THEN DO:
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
        IF NOT CAN-DO(cRunningSupers,"system/TagProcs.p") THEN DO: 
            RUN system/commonProcs.p PERSISTENT SET hCommonProcs.
            SESSION:ADD-SUPER-PROCEDURE (hCommonProcs).
        END.
        IF NOT CAN-DO(cRunningSupers,"system/TagProcs.p") THEN DO: 
            RUN system/creditProcs.p PERSISTENT SET hCreditProcs.
            SESSION:ADD-SUPER-PROCEDURE (hCreditProcs).
        END.
        IF NOT CAN-DO(cRunningSupers,"system/TagProcs.p") THEN DO: 
            RUN system/purgeProcs.p PERSISTENT SET hPurgeProcs.
            SESSION:ADD-SUPER-PROCEDURE (hPurgeProcs).
        END.
    END.
    
    IF NOT VALID-HANDLE(persistent-handle) THEN
        RUN nosweat/persist.p PERSISTENT SET persistent-handle.
    IF NOT VALID-HANDLE(listlogic-handle) THEN
        RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    IF cbMode EQ "Touchscreen" THEN 
        RUN epTouchLogin in hPreRun (OUTPUT tslogin-log).

    RUN epUserRecordCheck IN hPreRun (OUTPUT lOK, OUTPUT g_track_usage).
    IF NOT lOK THEN DO:
        DO ictr = 1 TO NUM-DBS:
            DISCONNECT VALUE(LDBNAME(iCtr)).
        END.
        QUIT.
    END.

    /* Run user-level tests and create the userlog before doing the session-level work */
    IF iDbLevel GT 16050000
    AND cbMode NE "Monitor Users" 
    AND cbMode NE "Editor" THEN DO:
        IF iEnvLevel GE 16150300 THEN 
            RUN epUserLogin IN hPreRun (cbMode, OUTPUT lExit).
        ELSE 
            RUN epUserLogin IN hPreRun (OUTPUT lExit).
        IF lExit THEN DO:
            DO ictr = 1 TO NUM-DBS:
                DISCONNECT VALUE(LDBNAME(iCtr)).
            END.
            QUIT.
        END.
    END.
     
    RUN epUpdateUsrFile IN hPreRun (OUTPUT cUsrList).

    RUN ipUpdUsrFile IN THIS-PROCEDURE (cUsrList).

    RUN epGetUserGroups IN hPreRun (OUTPUT g_groups).

    IF iDbLevel GT 16061200 THEN 
        RUN epSetUpEDI IN hPreRun.

    RUN epCheckExpiration IN hPreRun (OUTPUT lOK).
    IF NOT lOK THEN DO:
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
        IF INDEX(cUsrLine,"|") NE 0 THEN 
        DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfUserID = ENTRY(1,cUsrLine,"|")
                ttUsers.ttfPdbName = ENTRY(2,cUsrLine,"|")
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
    DEF INPUT PARAMETER ipTgtDir AS CHAR NO-UNDO.
    DEFINE VARIABLE iResult AS INT NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INT NO-UNDO.
    
    &IF DEFINED(FWD-VERSION) > 0 &THEN
    set-working-directory(ipTgtDir).
    &ELSE
    
    RUN SetCurrentDirectoryA (ipTgtDir, OUTPUT iResult).

    IF iResult NE 1 THEN 
    DO:
        RUN GetLastError (output iReturnValue).
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
    DEFINE VARIABLE iCtr AS INT NO-UNDO.
    DEFINE VARIABLE cOutString AS CHAR.
    DEFINE VARIABLE cUserModes AS CHAR NO-UNDO.
    
    DEFINE BUFFER bttUsrs FOR ttUsers.

    /* ipcUserList is a list of all _user records in the connected DBs. */
    ASSIGN 
        lUpdUsr = FALSE.
    
    /* If we haven't previiously deleted the DB-specific records, do it now. */    
    IF CAN-FIND (FIRST ttUsers WHERE 
                ttUsers.ttfPdbName NE "*") THEN DO:
        ASSIGN 
            lUpdUsr = TRUE.
        FOR EACH ttUsers WHERE 
            ttUsers.ttfPdbName NE "*":
            DELETE ttUsers.
        END.
    END.   
    
    /* If a new user has been created in the DB, add to .usr file */
    DO iCtr = 1 TO NUM-ENTRIES(ipcUserList):
        FIND FIRST bttUsers WHERE 
            ttUsers.ttfUserID EQ ENTRY(iCtr,ipcUserList) AND
            ttUsers.ttfPdbName EQ "PROD"
            NO-LOCK NO-ERROR.
        IF AVAIL bttUsers THEN ASSIGN 
            cUserModes = bttUsers.ttfModeList.
        ELSE ASSIGN
            cUserModes = "".

        FOR EACH bttUsers WHERE 
            bttUsers.ttfUserID EQ ENTRY(iCtr,ipcUserList) AND
            bttUsers.ttfPdbName NE "*":
            DELETE ttUsers.
        END.

        FIND FIRST ttUsers WHERE
            ttUsers.ttfUserID EQ ENTRY(iCtr,ipcUserList) AND
            ttUsers.ttfPdbName EQ "*"
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                lUpdUsr = TRUE
                ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
                ttUsers.ttfUserAlias = ENTRY(iCtr,ipcUserList)
                ttUsers.ttfPdbName = "*"
                ttUsers.ttfModeList = cUserModes
                .
        END.
        ELSE IF ttUsers.ttfModeList EQ "" 
        AND cUserModes NE "" THEN ASSIGN 
            lUpdUsr = TRUE
            ttUsers.ttfModeList = cUserModes.
        
    END.
    
    IF lUpdUsr = TRUE THEN DO:
        OUTPUT STREAM usrStream TO VALUE(cUsrLoc).
        FOR EACH ttUsers by ttUsers.ttfUserID:
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

