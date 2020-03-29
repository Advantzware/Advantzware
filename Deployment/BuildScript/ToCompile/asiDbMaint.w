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

DEF STREAM usrStream.
DEF STREAM logStream.

DEF VAR cAdminPort2 AS CHAR NO-UNDO.
DEF VAR cAllDbDirList AS CHAR NO-UNDO.
DEF VAR cAllDbList AS CHAR NO-UNDO.
DEF VAR cAllDbPortList AS CHAR NO-UNDO.
DEF VAR cDatabaseList AS CHAR NO-UNDO.
DEF VAR cDbBakDate AS CHAR NO-UNDO.
DEF VAR cDbBakList AS CHAR NO-UNDO.
DEF VAR cDbBakSize AS CHAR NO-UNDO.
DEF VAR cDbLongBakList AS CHAR NO-UNDO.
DEF VAR cDbLongName AS CHAR NO-UNDO.
DEF VAR cDivisor AS INT NO-UNDO INIT 1.
DEF VAR cEnvBakDate AS CHAR NO-UNDO.
DEF VAR cEnvBakList AS CHAR NO-UNDO.
DEF VAR cEnvBakSize AS CHAR NO-UNDO.
DEF VAR cEnvLongBakList AS CHAR NO-UNDO.
DEF VAR cOrigDirName AS CHAR NO-UNDO FORMAT "X(256)".
DEF VAR cOrigPropath AS CHAR NO-UNDO.
DEF VAR cPort AS CHAR NO-UNDO.
DEF VAR iBufferSize AS INT NO-UNDO INITIAL 256.
DEF VAR iCurrAvail AS INT NO-UNDO.
DEF VAR iDbBakSize AS INT NO-UNDO.
DEF VAR iDbLevel AS INT NO-UNDO.
DEF VAR iDiskFreeSpace AS DECIMAL NO-UNDO.
DEF VAR iDiskTotalSpace AS DECIMAL NO-UNDO.
DEF VAR iDiskUsedSpace AS DECIMAL NO-UNDO.
DEF VAR iEnvBakSize AS INT NO-UNDO.
DEF VAR iMem1 AS MEMPTR NO-UNDO.
DEF VAR iMem2 AS MEMPTR NO-UNDO.
DEF VAR iMem3 AS MEMPTR NO-UNDO.
DEF VAR iNewAvail AS INT NO-UNDO.
DEF VAR iResult AS INT NO-UNDO.
DEF VAR iReturnVal AS INT NO-UNDO.
DEF VAR iSelBakSize AS INT NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFWDembedded AS LOG NO-UNDO INITIAL FALSE.
DEF VAR intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
DEF VAR intResult        AS INTEGER   NO-UNDO.
DEF VAR ptrToString      AS MEMPTR    NO-UNDO.
DEF VAR cOrigDirectoryName AS CHAR NO-UNDO.
    
{iniFileVars.i}

DEF TEMP-TABLE ttBackupSize
    FIELD ttType AS CHAR 
    FIELD ttID AS CHAR 
    FIELD ttSize AS INT.
    
PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER iBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER iResult     AS SHORT.
END PROCEDURE.

PROCEDURE SetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
   DEFINE INPUT  PARAMETER chrCurDir AS CHARACTER.
   DEFINE RETURN PARAMETER iResult AS LONG.
END PROCEDURE.

PROCEDURE GetLastError EXTERNAL "kernel32.dll":
    DEFINE RETURN PARAMETER iReturnValue AS LONG.
END.

PROCEDURE GetModuleHandleA EXTERNAL "kernel32.dll" :
    DEFINE  INPUT PARAMETER lpModuleName       AS CHARACTER NO-UNDO.
    DEFINE RETURN PARAMETER hModule            AS LONG      NO-UNDO.
END PROCEDURE.
 
PROCEDURE GetProcAddress EXTERNAL "kernel32.dll" :
    DEFINE  INPUT PARAMETER hModule            AS LONG NO-UNDO.
    DEFINE  INPUT PARAMETER lpProcName         AS CHARACTER NO-UNDO.
    DEFINE RETURN PARAMETER lpFarproc          AS LONG NO-UNDO.
END PROCEDURE.
 
PROCEDURE GetDiskFreeSpaceExA EXTERNAL "kernel32.dll" :
    DEFINE  INPUT  PARAMETER  lpDirectoryName        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT  PARAMETER  FreeBytesAvailable     AS MEMPTR    NO-UNDO.
    DEFINE OUTPUT  PARAMETER  TotalNumberOfBytes     AS MEMPTR    NO-UNDO.
    DEFINE OUTPUT  PARAMETER  TotalNumberOfFreeBytes AS MEMPTR    NO-UNDO.
    DEFINE RETURN  PARAMETER  iReturnVal                 AS LONG      NO-UNDO.
END PROCEDURE.
 

/* Pre-visualization tasks */
ASSIGN
    cOrigPropath = PROPATH.

IF cOrigDirName = "" THEN DO:

&IF DEFINED(FWD-VERSION) > 0 &THEN
    ASSIGN cOrigDirName = get-working-directory().
&ELSE
    SET-SIZE(ptrToString) = 256.
    RUN GetCurrentDirectoryA (INPUT        iBufferSize,
                              INPUT-OUTPUT ptrToString,
                              OUTPUT       iResult).
    ASSIGN cOrigDirName = GET-STRING(ptrToString,1).    
&ENDIF

END.
ELSE DO:
    RUN pSetCurrentDir (cOrigDirName). 
END.
        
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbEstimate rsBackType tbShowMsg fiUserID ~
fiPassword rsDoWhat slAvailDbs tbCreateNewDbBak rsDbNameType eWarning ~
slAvailDbBaks tbArchiveDbBak tbDelSelDbBak slAvailEnvs tbCreateNewEnvBak ~
rsEnvNameType slAvailEnvBaks tbArchiveEnvBak tbDelSelEnvBak Btn_Cancel ~
fiDbBkupFileName fiEnvBkupFileName eStatus eHelp RECT-1 RECT-2 RECT-3 rDisk ~
rUsed 
&Scoped-Define DISPLAYED-OBJECTS tbEstimate rsBackType tbShowMsg fiWarning ~
fiUserID fiPassword rsDoWhat slAvailDbs tbCreateNewDbBak rsDbNameType ~
eWarning slAvailDbBaks tbArchiveDbBak fiDoWhat tbDelSelDbBak slAvailEnvs ~
fiAvailDbs tbCreateNewEnvBak rsEnvNameType fiAvailBkups slAvailEnvBaks ~
tbArchiveEnvBak tbDelSelEnvBak fiAvailEnvs fiAvailEnvBaks fiDbBkupFileName ~
fiEnvBkupFileName fiCapacity fiStatus fiHelp fiDiskSpaceTotal eStatus eHelp ~
fiDiskSpaceAvail fiEstDbBakSize fiEstEnvBakSize fiSelBakFileSize ~
fiSelBakFileDate fiAvailAfterOpn fiBkupType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 slAvailDbs tbCreateNewDbBak rsDbNameType ~
slAvailDbBaks fiDbBkupFileName 
&Scoped-define List-2 rsBackType slAvailEnvs tbCreateNewEnvBak ~
rsEnvNameType slAvailEnvBaks fiEnvBkupFileName 
&Scoped-define List-3 slAvailDbBaks tbArchiveDbBak tbDelSelDbBak ~
slAvailEnvBaks tbArchiveEnvBak tbDelSelEnvBak 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get64BitValue C-Win 
FUNCTION get64BitValue RETURNS DECIMAL
  ( INPUT m64 AS MEMPTR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Quit" 
     SIZE 15 BY 1.67
     BGCOLOR 8 FONT 17.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Execute" 
     SIZE 15 BY 1.67
     BGCOLOR 8 FONT 17.

DEFINE VARIABLE eHelp AS CHARACTER INITIAL "(This is the user ID" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 6.67 NO-UNDO.

DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 53 BY 8.81 NO-UNDO.

DEFINE VARIABLE eWarning AS CHARACTER INITIAL "Many of these utilities" 
     VIEW-AS EDITOR
     SIZE 157 BY 3.57
     FONT 5 NO-UNDO.

DEFINE VARIABLE fiAvailAfterOpn AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Avail After" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiAvailBkups AS CHARACTER FORMAT "X(256)":U INITIAL "Available DB Backups:" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiAvailDbs AS CHARACTER FORMAT "X(256)":U INITIAL "Available Databases:" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiAvailEnvBaks AS CHARACTER FORMAT "X(256)":U INITIAL "Available ENV Backups:" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiAvailEnvs AS CHARACTER FORMAT "X(256)":U INITIAL "Available Environments:" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBkupType AS CHARACTER FORMAT "X(256)":U INITIAL "Backup Type:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiCapacity AS CHARACTER FORMAT "X(256)":U INITIAL "Capacity Info:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiDbBkupFileName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE fiDiskSpaceAvail AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Avail" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiDiskSpaceTotal AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "DiskSz (MB)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiDoWhat AS CHARACTER FORMAT "X(256)":U INITIAL "What do you want to do?" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEnvBkupFileName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE fiEstDbBakSize AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Est. DB Backup Size" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiEstEnvBakSize AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Est. ENV Backup Size" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiHelp AS CHARACTER FORMAT "X(256)":U INITIAL "Context-sensitive Help:" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiSelBakFileDate AS DATE FORMAT "99/99/99":U 
     LABEL "Sel. backup file date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiSelBakFileSize AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Sel. backup file size" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U INITIAL "Processing Status:" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiWarning AS CHARACTER FORMAT "X(256)":U INITIAL "WARNING!" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE VARIABLE rsBackType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Basic", "B",
"Full", "F"
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE rsDbNameType AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "STANDARD format", "S",
"Extended format (w/ time)", "E",
"Basic format", "B"
     SIZE 30 BY 3 NO-UNDO.

DEFINE VARIABLE rsDoWhat AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Backup Database", "B",
"Restore DB from Backup", "R",
"Backup Environment", "E",
"Restore Env from Backup", "X",
"Other", "O"
     SIZE 123 BY 1 NO-UNDO.

DEFINE VARIABLE rsEnvNameType AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "STANDARD format", "S",
"Extended format (w/ time)", "E",
"Basic format", "B"
     SIZE 30 BY 3 NO-UNDO.

DEFINE RECTANGLE rDisk
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 40 BY .95.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 11.91.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 11.91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 10.71.

DEFINE RECTANGLE rUsed
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 9.8 BY .95
     BGCOLOR 10 .

DEFINE VARIABLE slAvailDbBaks AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 39 BY 6.67 NO-UNDO.

DEFINE VARIABLE slAvailDbs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY 4.29 NO-UNDO.

DEFINE VARIABLE slAvailEnvBaks AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 39 BY 6.67 NO-UNDO.

DEFINE VARIABLE slAvailEnvs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY 4.29 NO-UNDO.

DEFINE VARIABLE tbArchiveDbBak AS LOGICAL INITIAL no 
     LABEL "Make archive copy of DB backup?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbArchiveEnvBak AS LOGICAL INITIAL no 
     LABEL "Make archive copy of ENV backup?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbCreateNewDbBak AS LOGICAL INITIAL no 
     LABEL "Create New DB Backup?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tbCreateNewEnvBak AS LOGICAL INITIAL no 
     LABEL "Create New ENV Backup?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tbDelSelDbBak AS LOGICAL INITIAL no 
     LABEL "Delete selected DB backup file?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbDelSelEnvBak AS LOGICAL INITIAL no 
     LABEL "Delete selected ENV backup file?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbEstimate AS LOGICAL INITIAL no 
     LABEL "Estimate Backup Sizes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tbShowMsg AS LOGICAL INITIAL no 
     LABEL "Show Messages?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbEstimate AT ROW 1.48 COL 125
     rsBackType AT ROW 16.24 COL 138 NO-LABEL
     tbShowMsg AT ROW 1.48 COL 99
     fiWarning AT ROW 1.48 COL 2 COLON-ALIGNED NO-LABEL
     fiUserID AT ROW 1.48 COL 39 COLON-ALIGNED
     fiPassword AT ROW 1.48 COL 72 COLON-ALIGNED PASSWORD-FIELD 
     rsDoWhat AT ROW 6.48 COL 36 NO-LABEL
     slAvailDbs AT ROW 9.33 COL 4 NO-LABEL
     tbCreateNewDbBak AT ROW 13.86 COL 4
     rsDbNameType AT ROW 16.24 COL 4 NO-LABEL
     eWarning AT ROW 2.67 COL 2 NO-LABEL NO-TAB-STOP 
     slAvailDbBaks AT ROW 9.33 COL 38 NO-LABEL
     tbArchiveDbBak AT ROW 17.43 COL 39
     fiDoWhat AT ROW 6.48 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     tbDelSelDbBak AT ROW 18.38 COL 39
     slAvailEnvs AT ROW 9.33 COL 84 NO-LABEL
     fiAvailDbs AT ROW 8.14 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     tbCreateNewEnvBak AT ROW 13.86 COL 84
     rsEnvNameType AT ROW 16.24 COL 84 NO-LABEL
     fiAvailBkups AT ROW 8.14 COL 36 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     slAvailEnvBaks AT ROW 9.33 COL 118 NO-LABEL
     tbArchiveEnvBak AT ROW 17.43 COL 119
     tbDelSelEnvBak AT ROW 18.38 COL 119
     fiAvailEnvs AT ROW 8.14 COL 82 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     Btn_Cancel AT ROW 28.38 COL 126 WIDGET-ID 22
     Btn_OK AT ROW 28.38 COL 142 WIDGET-ID 26
     fiAvailEnvBaks AT ROW 8.14 COL 116 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiDbBkupFileName AT ROW 15.05 COL 2 COLON-ALIGNED NO-LABEL
     fiEnvBkupFileName AT ROW 15.05 COL 82 COLON-ALIGNED NO-LABEL
     fiCapacity AT ROW 20.05 COL 3 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiStatus AT ROW 20.05 COL 46 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiHelp AT ROW 20.05 COL 102 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiDiskSpaceTotal AT ROW 22.91 COL 14 COLON-ALIGNED NO-TAB-STOP 
     eStatus AT ROW 21.24 COL 48 NO-LABEL NO-TAB-STOP 
     eHelp AT ROW 21.24 COL 104 NO-LABEL NO-TAB-STOP 
     fiDiskSpaceAvail AT ROW 23.86 COL 14 COLON-ALIGNED NO-TAB-STOP 
     fiEstDbBakSize AT ROW 26 COL 26 COLON-ALIGNED NO-TAB-STOP 
     fiEstEnvBakSize AT ROW 26.95 COL 26 COLON-ALIGNED NO-TAB-STOP 
     fiSelBakFileSize AT ROW 28.14 COL 26 COLON-ALIGNED NO-TAB-STOP 
     fiSelBakFileDate AT ROW 29.1 COL 26 COLON-ALIGNED NO-TAB-STOP 
     fiAvailAfterOpn AT ROW 24.81 COL 14 COLON-ALIGNED NO-TAB-STOP 
     fiBkupType AT ROW 16.24 COL 117 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     RECT-1 AT ROW 7.67 COL 2
     RECT-2 AT ROW 7.67 COL 81
     RECT-3 AT ROW 19.81 COL 2
     rDisk AT ROW 21.24 COL 5
     rUsed AT ROW 21.24 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 29.76
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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Backup/Restore Databases"
         HEIGHT             = 29.76
         WIDTH              = 160
         MAX-HEIGHT         = 30.19
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 30.19
         VIRTUAL-WIDTH      = 160
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON Btn_OK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       eHelp:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       eStatus:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       eWarning:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiAvailAfterOpn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAvailBkups IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiAvailBkups:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiAvailDbs IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiAvailDbs:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiAvailEnvBaks IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiAvailEnvBaks:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiAvailEnvs IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiAvailEnvs:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiBkupType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiBkupType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiCapacity IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiCapacity:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiDbBkupFileName IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN fiDiskSpaceAvail IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDiskSpaceTotal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDoWhat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiDoWhat:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiEnvBkupFileName IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FILL-IN fiEstDbBakSize IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEstEnvBakSize IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHelp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiHelp:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiSelBakFileDate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSelBakFileSize IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiStatus:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiWarning IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiWarning:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR RADIO-SET rsBackType IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR RADIO-SET rsDbNameType IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET rsEnvNameType IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR SELECTION-LIST slAvailDbBaks IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR SELECTION-LIST slAvailDbs IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR SELECTION-LIST slAvailEnvBaks IN FRAME DEFAULT-FRAME
   2 3                                                                  */
/* SETTINGS FOR SELECTION-LIST slAvailEnvs IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbArchiveDbBak IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbArchiveEnvBak IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbCreateNewDbBak IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbCreateNewEnvBak IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbDelSelDbBak IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbDelSelEnvBak IN FRAME DEFAULT-FRAME
   3                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Backup/Restore Databases */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Backup/Restore Databases */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    QUIT.
    /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Quit */
DO:
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cLongFileName AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.

    INPUT FROM OS-DIR ("c:\tmp").
    REPEAT:
        IMPORT 
            cFileName
            cLongFileName
            cFileType.
        IF cFileName BEGINS "bak"
            AND SUBSTRING(cFileName,length(cFileName) - 2,3) EQ "txt" THEN
            OS-DELETE VALUE(cLongFileName). 
    END.

    RUN pLog("User chose the QUIT button").
    
    APPLY 'window-close' TO C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Execute */
DO:
    IF rsDoWhat:{&SV} EQ "R"  
    AND fiUserID:{&SV} EQ "Admin"
    AND INDEX(slAvailDbs:{&SV},"prod") NE 0 THEN DO:
        MESSAGE 
            "You are not allowed to replace your existing PROD database." SKIP 
            "Please contact ASI Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            slAvailDbs:{&SV} = ?.
        RUN pLog("Can't overwrite PROD DB.  Prevented.").
        RETURN NO-APPLY.
    END.

    RUN pLog("User chose the EXECUTE button").
    RUN pClickOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword C-Win
ON LEAVE OF fiPassword IN FRAME DEFAULT-FRAME /* Password */
OR RETURN OF fiPassword
DO:
    APPLY 'leave' TO fiUserID {&IN}.

    IF SELF:{&SV} EQ "" THEN 
        RETURN.
        
    IF (fiUserID:{&SV} EQ "asi" AND NOT SELF:{&SV} EQ "Pack@$!")
    OR (fiUserID:{&SV} EQ "admin" AND NOT SELF:{&SV} EQ "installme") THEN 
    DO:
        RUN pLog("User entered an invalid password").
        MESSAGE 
            "You entered an invalid UserID/Password combination." SKIP 
            "Please try again, or contact ASI Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            SELF:{&SV} = "".
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
    END. 
    
    ASSIGN 
        btn_OK:SENSITIVE = TRUE.
    
    APPLY 'entry' TO rsDoWhat {&IN}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
    IF SELF:{&SV} EQ "" THEN 
        RETURN.
        
    IF SELF:{&SV} NE "admin"
    AND SELF:{&SV} NE "asi" THEN DO:
        RUN pLog("User entered an invalid userid").
        MESSAGE 
            "This is not a valid User ID for this application." SKIP 
            "Please try again, or contact ASI Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsBackType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsBackType C-Win
ON VALUE-CHANGED OF rsBackType IN FRAME DEFAULT-FRAME
DO:
    RUN pLog("User set Backup Type to '" + rsBackType:{&SV} + "'").
    RUN pGetEnvFileStats (slAvailEnvs:{&SV}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDbNameType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDbNameType C-Win
ON VALUE-CHANGED OF rsDbNameType IN FRAME DEFAULT-FRAME
OR VALUE-CHANGED OF rsEnvNameType
DO:
    CASE SELF:NAME:
        WHEN "rsDbNameType" THEN DO:
            RUN pLog("User set DB Backup Type to '" + rsDbNameType:{&SV} + "'").
            APPLY 'value-changed' TO tbCreateNewDbBak.
        END.  
        WHEN "rsEnvNameType" THEN DO: 
            RUN pLog("User set ENV Backup Type to '" + rsDbNameType:{&SV} + "'").
            APPLY 'value-changed' TO tbCreateNewDbBak.
        END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDoWhat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDoWhat C-Win
ON VALUE-CHANGED OF rsDoWhat IN FRAME DEFAULT-FRAME
DO:
    RUN pLog("User set Requested Action to '" + rsDoWhat:{&SV} + "'").
    IF CAN-DO("B,R",SELF:{&SV}) THEN DO:
        DISABLE {&list-2} {&list-3} WITH FRAME {&frame-name}.
        ENABLE {&list-1} WITH FRAME {&frame-name}.
        APPLY 'value-changed' TO slAvailDbBaks.
        ASSIGN 
            tbArchiveEnvBak:CHECKED = FALSE 
            tbDelSelEnvBak:CHECKED = FALSE 
            fiEstEnvBakSize:{&SV} = "0".
    END.
    ELSE IF CAN-DO("E,X",SELF:{&SV}) THEN DO:
        DISABLE {&list-1} {&list-3} WITH FRAME {&frame-name}.
        ENABLE {&list-2} WITH FRAME {&frame-name}.
        APPLY 'value-changed' TO slAvailEnvBaks.
        ASSIGN 
            tbArchiveDbBak:CHECKED = FALSE 
            tbDelSelDbBak:CHECKED = FALSE 
            fiEstDbBakSize:{&SV} = "0".
    END.
    ELSE DO:
        DISABLE {&list-1} {&list-2} WITH FRAME {&frame-name}.
        ENABLE {&list-3} WITH FRAME {&frame-name}.
        APPLY 'entry' TO tbArchiveDbBak.
        ASSIGN 
            fiEstDbBakSize:{&SV} = "0"
            fiEstEnvBakSize:{&SV} = "0".
    END.
    RUN pCalcAvail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slAvailDbBaks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slAvailDbBaks C-Win
ON VALUE-CHANGED OF slAvailDbBaks IN FRAME DEFAULT-FRAME
OR VALUE-CHANGED OF slAvailEnvBaks
DO:
    CASE SELF:NAME:
        WHEN  "slAvailDbBaks" THEN DO:
            RUN pLog("User selected DB Backup '" + slAvailDbBaks:{&SV} + "'").
            ASSIGN 
                slAvailEnvBaks:{&SV} = ?
                tbArchiveEnvBak:CHECKED = FALSE 
                tbDelSelEnvBak:CHECKED = FALSE 
                fiSelBakFileSize:{&SV} = ENTRY(LOOKUP(SELF:{&SV},self:list-items),cDbBakSize)
                fiSelBakFileDate:{&SV} = ENTRY(LOOKUP(SELF:{&SV},self:list-items),cDbBakDate)
                tbCreateNewDbBak:CHECKED = FALSE 
                fiDbBkupFileName:{&SV} = SELF:{&SV}
                .
        END.
        WHEN "slAvailEnvBaks" THEN DO:
            RUN pLog("User selected ENV Backup '" + slAvailDbBaks:{&SV} + "'").
            ASSIGN 
                slAvailDbBaks:{&SV} = ?
                tbArchiveDbBak:CHECKED = FALSE 
                tbDelSelDbBak:CHECKED = FALSE 
                fiSelBakFileSize:{&SV} = ENTRY(LOOKUP(SELF:{&SV},self:list-items),cEnvBakSize)
                fiSelBakFileDate:{&SV} = ENTRY(LOOKUP(SELF:{&SV},self:list-items),cEnvBakDate)
                tbCreateNewEnvBak:CHECKED = FALSE 
                fiEnvBkupFileName:{&SV} = SELF:{&SV}
                .
        END.
    END.
    RUN pCalcAvail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slAvailDbs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slAvailDbs C-Win
ON ENTRY OF slAvailDbs IN FRAME DEFAULT-FRAME
OR ENTRY OF slAvailDbBaks
OR ENTRY OF slAvailEnvs
OR ENTRY OF slAvailEnvBaks
DO:
    IF SELF:{&SV} EQ ? THEN ASSIGN 
        SELF:{&SV} = ENTRY(1,self:LIST-ITEMS).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slAvailDbs C-Win
ON VALUE-CHANGED OF slAvailDbs IN FRAME DEFAULT-FRAME
OR VALUE-CHANGED OF slAvailEnvs
DO:
    IF rsDoWhat:{&SV} EQ "B"
    OR rsDoWhat:{&SV} EQ "E" THEN DO:
        CASE SELF:NAME:
            WHEN "slAvailDbs" THEN DO: 
                RUN pLog("User selected Database '" + slAvailDbs:{&SV} + "'").
                RUN pGetDbFileStats (SELF:{&SV}).
            END.
            WHEN "slAvailEnvs" THEN DO: 
                RUN pLog("User selected Environment '" + slAvailEnvs:{&SV} + "'").
                RUN pGetEnvFileStats (SELF:{&SV}).
            END.
        END.
        RUN pCalcAvail.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCreateNewDbBak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCreateNewDbBak C-Win
ON VALUE-CHANGED OF tbCreateNewDbBak IN FRAME DEFAULT-FRAME /* Create New DB Backup? */
OR VALUE-CHANGED OF tbCreateNewEnvBak
DO:
    CASE SELF:NAME:
        WHEN "tbCreateNewDbBak" THEN DO:
            IF SELF:CHECKED {&IN} THEN DO:
                RUN pLog("User checked Create New DB Backup").
                IF slAvailDbs:{&SV} EQ ? THEN ASSIGN 
                    slAvailDbs:{&SV} = ENTRY(1,slAvailDbs:LIST-ITEMS).
                CASE rsDbNameType:{&SV}:
                    WHEN "S" THEN ASSIGN 
                        fiDbBkupFileName:{&SV} = 
                            slAvailDbs:{&SV} + 
                            "-" +
                            SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
                            STRING(MONTH(TODAY),"99") +
                            STRING(DAY(TODAY),"99") +
                            ".bak".
                    WHEN "E" THEN ASSIGN 
                        fiDbBkupFileName:{&SV} = 
                            slAvailDbs:{&SV} + 
                            "-" +
                            SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
                            STRING(MONTH(TODAY),"99") +
                            STRING(DAY(TODAY),"99") +
                            "-" +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) +
                            ".bak".
                    WHEN "B" THEN ASSIGN 
                        fiDbBkupFileName:{&SV} = 
                            slAvailDbs:{&SV} + 
                            ".bak".
                END CASE.
                ASSIGN 
                    fiSelBakFileSize:{&SV} = "0"
                    slAvailDbBaks:{&SV} = ?.
            END.
            ELSE DO:
                RUN pLog("User UNchecked Create New DB Backup").
                ASSIGN 
                    fiDbBkupFileName:{&SV} = ""
                    slAvailDbBaks:{&SV} = ENTRY(1,slAvailDbBaks:LIST-ITEMS).
                APPLY 'value-changed' TO slAvailDbBaks.
            END.
        END.
        WHEN "tbCreateNewEnvBak" THEN DO:
            IF SELF:CHECKED {&IN} THEN DO:
                RUN pLog("User checked Create New ENV Backup").
                IF slAvailEnvs:{&SV} EQ ? THEN ASSIGN 
                    slAvailEnvs:{&SV} = ENTRY(1,slAvailEnvs:LIST-ITEMS).
                CASE rsEnvNameType:{&SV}:
                    WHEN "S" THEN ASSIGN 
                        fiEnvBkupFileName:{&SV} = 
                            slAvailEnvs:{&SV} + 
                            "-" +
                            SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
                            STRING(MONTH(TODAY),"99") +
                            STRING(DAY(TODAY),"99") +
                            ".7z".
                    WHEN "E" THEN ASSIGN 
                        fiEnvBkupFileName:{&SV} = 
                            slAvailEnvs:{&SV} + 
                            "-" +
                            SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
                            STRING(MONTH(TODAY),"99") +
                            STRING(DAY(TODAY),"99") +
                            "-" +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) +
                            ".7z".
                    WHEN "B" THEN ASSIGN 
                        fiEnvBkupFileName:{&SV} = 
                            slAvailEnvs:{&SV} + 
                            ".7z".
                END CASE.
                ASSIGN 
                    fiSelBakFileSize:{&SV} = "0"
                    slAvailDbBaks:{&SV} = ?
                    slAvailEnvBaks:{&SV} = ?.
            END.
            ELSE  DO:
                RUN pLog("User UNchecked Create New ENV Backup").
                ASSIGN 
                    fiEnvBkupFileName:{&SV} = ""
                    slAvailEnvBaks:{&SV} = ENTRY(1,slAvailEnvBaks:LIST-ITEMS).
                APPLY 'value-changed' TO slAvailEnvBaks.
            END.
        END.
    END CASE.
    
    RUN pCalcAvail.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbDelSelDbBak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbDelSelDbBak C-Win
ON VALUE-CHANGED OF tbDelSelDbBak IN FRAME DEFAULT-FRAME /* Delete selected DB backup file? */
OR VALUE-CHANGED OF tbDelSelEnvBak
OR VALUE-CHANGED OF tbArchiveEnvBak
OR VALUE-CHANGED OF tbArchiveDbBak
DO:
    CASE SELF:NAME:
        WHEN "tbDelSelDbBak" THEN
            RUN pLog("User " + (IF NOT SELF:CHECKED THEN "UN" ELSE "") + "checked Delete Selected DB Backup").
        WHEN "tbDelSelEnvBak" THEN
            RUN pLog("User " + (IF NOT SELF:CHECKED THEN "UN" ELSE "") + "checked Delete Selected ENV Backup").
        WHEN "tbArchiveDbBak" THEN
            RUN pLog("User " + (IF NOT SELF:CHECKED THEN "UN" ELSE "") + "checked Archive DB Backup").
        WHEN "tbArchiveEnvBak" THEN
        RUN pLog("User " + (IF NOT SELF:CHECKED THEN "UN" ELSE "") + "checked Archive ENV Backup").
    END CASE.
    RUN pCalcAvail.  
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

    
    SET-SIZE(ptrToString) = 256.
    RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                              INPUT-OUTPUT ptrToString,
                              OUTPUT       intResult).
    ASSIGN 
        cOrigDirectoryName = GET-STRING(ptrToString,1).      RUN enable_UI.
   
    RUN pGetDbBakList (cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases",
                        OUTPUT cDbBakList).
    RUN pGetEnvBakList (cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments",
                        OUTPUT cEnvBakList).
    RUN pGetAdminSvcProps.
    
    ON ENTRY ANYWHERE DO:
        RUN pSetHelp (SELF:NAME).
    END.    

    ASSIGN  
        eWarning:{&SV} = 
            "Many of these utilities will make PERMANENT changes to your Advantzware system, and CANNOT be undone.  " +
            "These utilities and commands should be used with caution by a system administrator who is completely " +
            "familiar with the operation and the underlying technical details.  A full user manual for this program " +
            "is available from your ASI Support Team, and should be read and understood before proceeding with ANY " +
            "of these operations."
        cAllDbList = cDBList + "," + cAudDbList
        cAllDbDirList = cDbDirList + "," + cAudDirList
        cAllDbPortList = cDbPortList + "," + cAudPortList
        slAvailDBs:LIST-ITEMS = cAllDbList
        slAvailEnvs:LIST-ITEMS = cEnvList
        slAvailDbBaks:LIST-ITEMS = cDbBakList
        slAvailEnvBaks:LIST-ITEMS = cEnvBakList. 
        
    /* Ensure that all required directories exist */
    OS-CREATE-DIR VALUE (cDrive + "\" + cTopDir + "\" + cBackupDir + "\" + "Databases").
    OS-CREATE-DIR VALUE (cDrive + "\" + cTopDir + "\" + cBackupDir + "\" + "Environments").
    OS-CREATE-DIR VALUE (cDrive + "\" + cTopDir + "\" + cBackupDir + "\" + "Archives").
    OS-CREATE-DIR VALUE (cDrive + "\" + cTopDir + "\" + cBackupDir + "\" + "Archives\Databases").
    OS-CREATE-DIR VALUE (cDrive + "\" + cTopDir + "\" + cBackupDir + "\" + "Archives\Environments").
    
    RUN pGetDiskSpace (cDrive,
                       "MB",
                       OUTPUT iDiskFreeSpace,
                       OUTPUT iDiskTotalSpace).
    ASSIGN 
        iDiskUsedSpace = iDiskTotalSpace - iDiskFreeSpace
        fiDiskSpaceTotal:{&SV} = STRING(iDiskTotalSpace)
        fiDiskSpaceAvail:{&SV} = STRING(iDiskFreeSpace)
        rUsed:WIDTH = 40 * (iDiskUsedSpace / iDiskTotalSpace)
        rUsed:BGCOLOR = IF iDiskUsedSpace GT iDiskTotalSpace * .95 THEN 12
                        ELSE IF iDiskUsedSpace GT iDiskTotalSpace * .90 THEN 14 
                        ELSE 10
        rDisk:TOOLTIP = "Avail=" + STRING(iDiskFreeSpace,">>>,>>>,>>9") + " MB"
        rUsed:TOOLTIP = "Used=" + STRING(iDiskUsedSpace,">>>,>>>,>>9") + " MB"
        .
    IF iDiskFreeSpace LT 200 THEN ASSIGN   /* If LT 200MB avail, force estimate checking */ 
        tbEstimate:CHECKED = TRUE 
        tbEstimate:SENSITIVE = FALSE. 
        
    APPLY 'value-changed' TO rsDoWhat {&IN}.
    APPLY 'entry' TO fiUserID {&IN}.

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
  DISPLAY tbEstimate rsBackType tbShowMsg fiWarning fiUserID fiPassword rsDoWhat 
          slAvailDbs tbCreateNewDbBak rsDbNameType eWarning slAvailDbBaks 
          tbArchiveDbBak fiDoWhat tbDelSelDbBak slAvailEnvs fiAvailDbs 
          tbCreateNewEnvBak rsEnvNameType fiAvailBkups slAvailEnvBaks 
          tbArchiveEnvBak tbDelSelEnvBak fiAvailEnvs fiAvailEnvBaks 
          fiDbBkupFileName fiEnvBkupFileName fiCapacity fiStatus fiHelp 
          fiDiskSpaceTotal eStatus eHelp fiDiskSpaceAvail fiEstDbBakSize 
          fiEstEnvBakSize fiSelBakFileSize fiSelBakFileDate fiAvailAfterOpn 
          fiBkupType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbEstimate rsBackType tbShowMsg fiUserID fiPassword rsDoWhat 
         slAvailDbs tbCreateNewDbBak rsDbNameType eWarning slAvailDbBaks 
         tbArchiveDbBak tbDelSelDbBak slAvailEnvs tbCreateNewEnvBak 
         rsEnvNameType slAvailEnvBaks tbArchiveEnvBak tbDelSelEnvBak Btn_Cancel 
         fiDbBkupFileName fiEnvBkupFileName eStatus eHelp RECT-1 RECT-2 RECT-3 
         rDisk rUsed 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBackupDB C-Win 
PROCEDURE pBackupDB :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR lOnline AS LOG NO-UNDO.
    DEF VAR cLockFile AS CHAR NO-UNDO.
    DEF VAR cCmdString AS CHAR NO-UNDO.
    
    IF tbShowMsg:CHECKED {&IN} THEN 
    DO:
        MESSAGE 
            "This procedure will back up the " + slAvailDbs:{&SV} + " database into the" SKIP 
            cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV} + " file." SKIP 
            "Are you sure?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
        IF NOT lSure THEN RETURN.
    END.    
    /* Is the database online? */
    ASSIGN
        cLockFile = REPLACE(cDbLongName,".db",".lk") 
        lOnline = SEARCH(cLockFile) NE ?.

    RUN pLog("  Beginning DB Backup").
    STATUS DEFAULT "Backing up database " + slAvailDbs:{&SV}.
    
    IF lOnline THEN ASSIGN 
        cCmdString = cDLCDir + "\bin\_mprshut " + cDbLongName + " -C backup online " + 
                     cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV}.
    ELSE ASSIGN 
            cCmdString = cDLCDir + "\bin\_dbutil probkup " + cDbLongName + " " + 
                     cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV}.
{&Won}
    OS-COMMAND SILENT VALUE(cCmdString).
{&Woff}    
    IF tbShowMsg:CHECKED {&IN}
    AND SEARCH(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV}) NE ? THEN DO:
        RUN pLog("  Backup succeeded").
        MESSAGE 
            "Backup successful!"
            VIEW-AS ALERT-BOX.
    END.
    ELSE IF SEARCH(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV}) EQ ? THEN 
    DO:
        RUN pLog("  Backup FAILED").
        MESSAGE 
            "Backup FAILED!"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    RUN pGetDbBakList (cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases",
                       OUTPUT cDbBakList).
    ASSIGN 
        slAvailDbBaks:LIST-ITEMS = cDbBakList.


    RUN pLog("  Backup complete").
    STATUS DEFAULT "Database Backup completed".
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBackupEnv C-Win 
PROCEDURE pBackupEnv :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lTestExists AS LOG NO-UNDO.
    DEF VAR cTgtLoc AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cNewEntry AS CHAR NO-UNDO.
    DEF VAR cListItems AS CHAR NO-UNDO.
    DEF VAR cScreenValue AS CHAR NO-UNDO.
    DEF VAR c7zLoc AS CHAR NO-UNDO.
    DEF VAR cSrcLoc AS CHAR NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR cDirList AS CHAR NO-UNDO INITIAL "Programs,Resources,Override".
    
    RUN pLog("  Beginning Environment backup").

    ASSIGN 
        c7zLoc = cDrive + "\" + cTopDir + "\" + cAdminDir + "\" + cEnvAdmin + "\7z.exe"
        cTgtLoc = cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments\" + fiEnvBkupFilename:{&SV}.

    IF SEARCH(cTgtLoc) NE ? THEN 
    DO:
        RUN pLog("    Removing old backup file").
        STATUS DEFAULT "Removing old backup file".
        OS-DELETE VALUE(cTgtLoc).
    END.
    
    STATUS DEFAULT "Backing up Environment to Filesystem".
{&Won}
    IF rsBackType:{&SV} EQ "F" THEN DO:
        ASSIGN 
            cSrcLoc = cDrive + "\" + cTopDir + "\" + cEnvDir + "\" + slAvailEnvs:{&SV} + "\*"
            cCmdLine1 = c7zLoc + " a " + cTgtLoc + " " + cSrcLoc.
        OS-COMMAND SILENT VALUE(cCmdLine1).
    END.
    ELSE DO iCtr = 1 TO NUM-ENTRIES(cDirList):
        ASSIGN 
            cSrcLoc = cDrive + "\" + cTopDir + "\" + cEnvDir + "\" + slAvailEnvs:{&SV} + "\" + ENTRY(iCtr,cDirList) + "\"
            cCmdLine1 = c7zLoc + " a " + cTgtLoc + " " + cSrcLoc.
        OS-COMMAND SILENT VALUE(cCmdLine1).
    END.
    {&Woff}    

    RUN pGetEnvBakList (cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments",
                        OUTPUT cEnvBakList).
    ASSIGN 
        slAvailEnvBaks:LIST-ITEMS = cEnvBakList.


    RUN pLog("  Backup complete").
    STATUS DEFAULT "Environment Backup complete".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcAvail C-Win 
PROCEDURE pCalcAvail :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR iNewUsed AS DECIMAL NO-UNDO.
    ASSIGN 
        iDbBakSize = INT(fiEstDbBakSize:{&SV})
        iEnvBakSize = INT(fiEstDbBakSize:{&SV})
        iSelBakSize = INT(fiSelBakFileSize:{&SV})
        iCurrAvail = INT(fiDiskSpaceAvail:{&SV})
        iNewAvail = iCurrAvail - iDbBakSize - iEnvBakSize + (IF iSelBakSize NE ? THEN iSelBakSize ELSE 0)
        iNewUsed = iDiskTotalSpace - iNewAvail.
    
    IF tbArchiveDbBak:CHECKED THEN ASSIGN 
        iNewAvail = iNewAvail - iSelBakSize.
    IF tbDelSelDbBak:CHECKED THEN ASSIGN 
        iNewAvail = iNewAvail + iSelBakSize.
    
    IF tbArchiveEnvBak:CHECKED THEN ASSIGN 
        iNewAvail = iNewAvail - iSelBakSize.
    IF tbDelSelEnvBak:CHECKED THEN ASSIGN 
        iNewAvail = iNewAvail + iSelBakSize.
    
    ASSIGN     
        fiAvailAfterOpn:{&SV} = STRING(iNewAvail)
        rUsed:WIDTH = rDisk:WIDTH * (iNewUsed / iDiskTotalSpace)
        rUsed:BGCOLOR = IF iNewUsed GT iDiskTotalSpace * .95 THEN 12
                        ELSE IF iNewUsed GT iDiskTotalSpace * .90 THEN 14 
                        ELSE 10.
                        
    IF iNewAvail LT 0 THEN DO:
        MESSAGE 
            "YOU DO NOT HAVE ENOUGH DISK SPACE TO CREATE A NEW BACKUP!!!" SKIP
            "You should use an existing backup file, or delete one or" SKIP 
            "more existing backups before creating a new one."
            VIEW-AS ALERT-BOX ERROR.
        RUN pLog("User's choice for backup would exceed disk space").
            
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClickOk C-Win 
PROCEDURE pClickOk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEF VAR cCmdString AS CHAR NO-UNDO.
    DEF VAR cDbLevel AS CHAR NO-UNDO.
    
    RUN pLog("  Validating selections").
    RUN pValidateValues (rsDoWhat:{&SV},
                         OUTPUT lError,
                         OUTPUT cMessage).
    IF lError THEN DO:
        CASE rsDoWhat:{&SV}:
            WHEN "B" THEN MESSAGE
                "You have not entered enough information to support this operation." SKIP 
                "To backup a database, you must select the database and select an" SKIP 
                "available backup to overwrite or select the Create New Db Backup option."
                VIEW-AS ALERT-BOX ERROR.
            WHEN "R" THEN MESSAGE
                "You have not entered enough information to support this operation." SKIP 
                "To restore a database, you must select and available backup file and" SKIP 
                "the database you want to restore."
                VIEW-AS ALERT-BOX ERROR.
            WHEN "E" THEN MESSAGE
                "You have not entered enough information to support this operation." SKIP 
                "To backup an environment, you must select the environment and select an" SKIP 
                "available backup to overwrite or select the Create New Env Backup option." 
                VIEW-AS ALERT-BOX ERROR.
            WHEN "X" THEN MESSAGE
                "You have not entered enough information to support this operation." SKIP 
                "To restore an environment, you must select and available backup file and" SKIP 
                "the environment you want to restore."
                VIEW-AS ALERT-BOX ERROR.
            WHEN "O" THEN MESSAGE
                "You have not entered enough information to support this operation." SKIP 
                "To create an archive, or to delete a backup, you must select one or" SKIP 
                "both options, as well as the backup file you wish to use."
                VIEW-AS ALERT-BOX ERROR.
        END CASE.
        RETURN.
    END.    
    RUN pLog("  Validation complete").
        

    CASE rsDoWhat:{&SV}:
        WHEN "B" THEN DO:   /* Backup a database */
            RUN pBackupDB.
        END.
        WHEN "R" THEN DO:   /* Restore a database */
            RUN pRestoreDB.
        END.
        WHEN "E" THEN DO:   /* Backup an environment */
            RUN pBackupEnv.
        END.
        WHEN "X" THEN DO:   /* Restore an environment */
            RUN pRestoreEnv.
        END.
        WHEN "O" THEN DO:   /* File maint */
            RUN pFileMaint.
        END.
    END CASE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMaint C-Win 
PROCEDURE pFileMaint :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    IF tbArchiveDbBak:CHECKED {&IN} THEN 
    DO:
        RUN pLog("  Beginning DB Backup archive").
        OS-COPY VALUE(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + slAvailDbBaks:{&SV})
                VALUE(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Archives\Databases\" + slAvailDbBaks:{&SV}).
        RUN pLog("  DB Backup archive complete").
    END.
    IF tbDelSelDbBak:CHECKED {&IN} THEN DO:
        IF tbShowMsg:CHECKED THEN DO:
            MESSAGE 
                "You are about to permanently delete a backup file.  Are you sure?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
            IF NOT lSure THEN RETURN.
        END.
        RUN pLog("  Beginning DB Backup deletion").
        OS-DELETE VALUE(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + slAvailDbBaks:{&SV}).
        RUN pGetDbBakList (cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases",
                            OUTPUT cDbBakList).
        ASSIGN 
            slAvailDbBaks:LIST-ITEMS {&IN} = cDbBakList
            slAvailDbBaks:{&SV} = ENTRY(1,slAvailDbBaks:LIST-ITEMS).
        RUN pLog("  DB Backup deletion complete").
    END.
    IF tbArchiveEnvBak:CHECKED {&IN} THEN DO:
        RUN pLog("  Beginning ENV Backup deletion").
        OS-COPY VALUE(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments\" + slAvailEnvBaks:{&SV})
                VALUE(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Archives\Environments\" + slAvailEnvBaks:{&SV}).
        RUN pLog("  ENV Backup deletion complete").
    END.
    IF tbDelSelEnvBak:CHECKED {&IN} THEN DO:
        IF tbShowMsg:CHECKED THEN DO:
            MESSAGE 
                "You are about to permanently delete a backup file.  Are you sure?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure2 AS LOG.
            IF NOT lSure THEN RETURN.
        END.
        RUN pLog("  Beginning ENV Backup archive").
        OS-DELETE VALUE(cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments\" + slAvailEnvBaks:{&SV}).
        RUN pGetEnvBakList (cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments",
                           OUTPUT cEnvBakList).
        ASSIGN 
            slAvailEnvBaks:LIST-ITEMS {&IN} = cEnvBakList
            slAvailEnvBaks:{&SV} = ENTRY(1,slAvailEnvBaks:LIST-ITEMS).
        RUN pLog("  ENV Backup archive complete").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetAdminSvcProps C-Win 
PROCEDURE pGetAdminSvcProps :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cLine AS CHAR NO-UNDO.
    
    RUN pLog ("  Reading admin service props file " + slAvailDbs:{&SV}).

    INPUT FROM VALUE (cDLCDir + "\properties\AdminServerPlugins.properties").
    REPEAT:
        IMPORT cLine.
        ASSIGN 
            cLine = TRIM(cLine).
        IF ENTRY(1,cLine,"=") EQ "adminport" THEN 
        DO:
            ASSIGN
                cAdminPort2 = ENTRY(2,cLine,"=").
        END.
        IF ENTRY(1,cLine,"=") EQ "port" THEN 
        DO:
            ASSIGN
                cPort = ENTRY(2,cLine,"=").
        END.
    END.
    INPUT CLOSE.
    IF cPort = "" THEN ASSIGN 
        cPort = "20931".
    IF cAdminPort2 = "" THEN ASSIGN 
        cAdminPort2 = "7843".
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDbBakList C-Win 
PROCEDURE pGetDbBakList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDbBakDir AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opcDbBakList AS CHAR NO-UNDO.
    
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cLongFileName AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR iFileSize AS INT NO-UNDO.
    DEF VAR daFileDate AS DATE NO-UNDO.
    
    OS-CREATE-DIR VALUE(ipcDbBakDir).
    INPUT FROM OS-DIR(ipcDbBakDir).
    REPEAT:
        IMPORT 
            cFileName
            cLongFileName
            cFileType.
        IF INDEX(cFileName,".bak") NE 0 THEN DO:
            ASSIGN
                FILE-INFO:FILE-NAME = cLongFileName
                iFileSize = FILE-INFO:FILE-SIZE / 1024000
                daFileDate = FILE-INFO:FILE-MOD-DATE 
                opcDbBakList = opcDbBakList + cFileName + ","
                cDbLongBakList = cDbLongBakList + cLongFileName + ","
                cDbBakSize = cDbBakSize + STRING(iFileSize) + ","
                cDbBakDate = cDbBakDate + STRING(daFileDate,"99/99/99") + ",".
        END.
    END.
    
    ASSIGN 
        opcDbBakList = TRIM(opcDbBakList,",")
        cDbLongBakList = TRIM(cDbLongBakList,",")
        cDbBakSize = TRIM(cDbBakSize,",")
        cDbBakDate = TRIM(cDbBakDate,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDbBakSize C-Win 
PROCEDURE pGetDbBakSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDbName AS CHAR NO-UNDO.
    
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLine AS CHAR NO-UNDO.
    DEF VAR iSize AS DECI NO-UNDO.
    DEF VAR cUnit AS CHAR NO-UNDO.
    DEF VAR lServed AS LOG NO-UNDO.
    DEF VAR iSeed AS INT NO-UNDO.
    DEF VAR cFile1 AS CHAR NO-UNDO.
    DEF VAR cFile2 AS CHAR NO-UNDO.
    
    FIND FIRST ttBackupSize WHERE 
        ttBackupSize.ttType = "DB" AND 
        ttBackupSize.ttID = ipcDbName
        NO-LOCK NO-ERROR.
    IF AVAIL ttBackupSize THEN DO:
        ASSIGN 
            fiEstDbBakSize:{&SV} = STRING(ttBackupSize.ttSize).
        RETURN.        
    END.
        
    ASSIGN 
        iSize = 0
        lServed = FALSE 
        fiEstDbBakSize:{&SV} = STRING(0)
        iSeed = (TIME * 1000) + RANDOM(1,998)
        cFile1 = "c:\tmp\bak" + STRING(iSeed,"99999999") + ".txt"
        cFile2 = "c:\tmp\bak" + STRING ((iSeed + 1),"99999999") + ".txt".        

    RUN pLog("  Calculating estimated DB Backup size").
    STATUS DEFAULT "Calculating estimated backup size...".   

    ASSIGN 
        cCmdLine = cDLCDir + "\bin\_dbutil probkup " + ipcDbName + " c:\tmp\dbname.bak -estimate > " + cFile1 + " && exit".
    
    OS-COMMAND SILENT VALUE(cCmdLine).

{&Won}
    DO WHILE iSize = 0 AND lServed = FALSE:
        INPUT FROM VALUE(cFile1).
        REPEAT:
            IMPORT UNFORMATTED cLine.
            IF INDEX(cLine,"multi-user") NE 0 THEN ASSIGN 
                lServed = TRUE. 
            ELSE IF cLine BEGINS "Backup requires" THEN ASSIGN
                iSize = DECIMAL(ENTRY(5,cLine," "))
                cUnit = ENTRY(6,cLine," ").
        END.
        INPUT CLOSE.
    END.
    
    IF lServed THEN DO:
        ASSIGN 
            cCmdLine = cDLCDir + "\bin\_mprshut " + ipcDbName + " -C backup online NUL -verbose > " + cFile2.
        OS-COMMAND SILENT VALUE(cCmdLine).
        DO WHILE iSize = 0:
            INPUT FROM VALUE(cFile2).
            REPEAT:
                IMPORT UNFORMATTED cLine.
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

    ASSIGN 
        fiEstDbBakSize:{&SV} = STRING(iSize).        

{&Won}
    DO WHILE SEARCH(cFile1) NE ?:
        OS-DELETE VALUE(cFile1).
        OS-DELETE VALUE(cFile2).
    END.
{&Woff}
    RUN pLog("  Estimated DB Backup size is " + fiEstDbBakSize:{&SV}).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDbFileStats C-Win 
PROCEDURE pGetDbFileStats :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDbName AS CHAR NO-UNDO.
    
    DEF VAR iIndex AS INT NO-UNDO.
    DEF VAR cDbSubdir AS CHAR NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cLongFileName AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR iFileSize AS INT NO-UNDO.
    
    ASSIGN 
        iIndex = LOOKUP(ipcDbName,slAvailDbs:LIST-ITEMS {&IN}).
        
    INPUT FROM OS-DIR (cDbDrive + "\" + cTopDir + "\" + cDbDir + "\" + ENTRY(iIndex,cAllDbDirList)).
    REPEAT:
        IMPORT 
            cFileName
            cLongFileName
            cFileType.
        IF NOT cFileName BEGINS ipcDbName THEN NEXT.
        IF SUBSTRING(cLongFileName,length(cLongFileName) - 2,3) EQ ".db" THEN ASSIGN 
            cDbLongName = cLongFileName.
        ASSIGN 
            FILE-INFO:FILE-NAME = cLongFileName
            iFileSize = FILE-INFO:FILE-SIZE / 1024000.
    END.
        
    IF rsDoWhat:{&SV} EQ "B" 
    AND tbEstimate:CHECKED THEN 
        RUN pGetDbBakSize (cDbLongName).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDiskSpace C-Win 
PROCEDURE pGetDiskSpace :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip_drive   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip_unit    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER iDiskFreeSpace    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER iDiskTotalSpace   AS DECIMAL   NO-UNDO.

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
            iDiskTotalSpace = TRUNC( get64BitValue(iMem2) / cDivisor, 3).
    END.
 
    SET-SIZE(iMem1) = 0.
    SET-SIZE(iMem2) = 0.
    SET-SIZE(iMem3) = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetEnvBakList C-Win 
PROCEDURE pGetEnvBakList :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcEnvBakDir AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opcEnvBakList AS CHAR NO-UNDO.
    
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cLongFileName AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR iFileSize AS INT NO-UNDO.
    DEF VAR daFileDate AS DATE NO-UNDO.

    OS-CREATE-DIR VALUE(ipcEnvBakDir).
    INPUT FROM OS-DIR(ipcEnvBakDir).
    REPEAT:
        IMPORT 
            cFileName
            cLongFileName
            cFileType.
        IF INDEX(cFileName,".zip") NE 0 
        OR INDEX(cFileName,".7z") NE 0 THEN DO:
            ASSIGN
                FILE-INFO:FILE-NAME = cLongFileName
                iFileSize = FILE-INFO:FILE-SIZE / 1024000
                daFileDate = FILE-INFO:FILE-MOD-DATE 
                opcEnvBakList = opcEnvBakList + cFileName + ","
                cEnvLongBakList = cEnvLongBakList + cLongFileName + ","
                cEnvBakSize = cEnvBakSize + STRING(iFileSize) + ",".
                cEnvBakDate = cEnvBakDate + STRING(daFileDate,"99/99/99") + ",".
        END.
    END.
    INPUT CLOSE.
    
    ASSIGN 
        opcEnvBakList = TRIM(opcEnvBakList,",")
        cEnvBakList = TRIM(cEnvBakList,",")
        cEnvBakSize = TRIM(cEnvBakSize,",")
        cEnvBakDate = TRIM(cEnvBakDate,",").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetEnvFileStats C-Win 
PROCEDURE pGetEnvFileStats :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcEnvName AS CHAR NO-UNDO.

    DEF VAR cStartIn AS CHAR NO-UNDO.
    DEF VAR iIndex AS INT NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cLongFileName AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR deFileSize AS DEC NO-UNDO.
    DEF VAR deSubFileSize AS DEC NO-UNDO.
    DEF VAR dePgmFileSize AS DEC NO-UNDO.
    DEF VAR deResFileSize AS DEC NO-UNDO.
    DEF VAR deOvrFileSize AS DEC NO-UNDO.
    DEF VAR deThisFileSize AS DEC NO-UNDO.
    DEF VAR deTotFileSize AS DEC NO-UNDO.    

    ASSIGN 
        cStartIn = cDrive + "\" + cTopDir + "\" + cEnvDir + "\" + ipcEnvName.

    IF SEARCH (cStartIn + "\programs\nosweat.r") = ? THEN DO:
        MESSAGE 
            "Unable to locate any files in this environment."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF tbEstimate:CHECKED {&IN} THEN DO:
        RUN pLog("  Calculating estimated ENV Backup size").
        
{&Won}
        IF rsBackType:{&SV} EQ "B" THEN DO:
            RUN pGetSubDirSize (cStartIn + "\override", OUTPUT deOvrFileSize).
            RUN pGetSubDirSize (cStartIn + "\resources", OUTPUT deResFileSize).
            RUN pGetSubDirSize (cStartIn + "\programs", OUTPUT dePgmFileSize).
        
            STATUS DEFAULT "".   
            
            ASSIGN
                deTotFileSize = dePgmFileSize +
                               deResFileSize +
                               deOvrFileSize 
                deTotFileSize = deTotFileSize * .15 / 1024000
                fiEstEnvBakSize:{&SV} = STRING(INT(deTotFileSize)).
        END.
        ELSE DO:
            RUN pGetSubDirSize (cStartIn, OUTPUT dePgmFileSize).
        
            STATUS DEFAULT "".   
            
            ASSIGN
                deTotFileSize = dePgmFileSize 
                deTotFileSize = deTotFileSize * .06 / 1024000
                fiEstEnvBakSize:{&SV} = STRING(INT(deTotFileSize)).
        END.
{&Woff}    
        RUN pLog("  Estimated ENV Backup size is " + fiEstEnvBakSize:{&SV}).
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSubDirSize C-Win 
PROCEDURE pGetSubDirSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDirName AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opdeSubDirSize AS DEC NO-UNDO.
    
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cLongFileName AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR deSubFileSize AS DEC NO-UNDO.
    DEF VAR deThisFileSize AS DEC NO-UNDO.
        
    STATUS DEFAULT "Calculating (" + ipcDirName + ") size...".   
    INPUT FROM OS-DIR(ipcDirName).
    REPEAT:
        IMPORT 
            cFileName
            cLongFileName
            cFileType.
        IF cFileName BEGINS "." THEN NEXT.
        IF cFileType EQ "D" THEN DO:
            RUN pGetSubDirSize (cLongFileName, OUTPUT deSubFileSize).
            ASSIGN 
                opdeSubDirSize = opdeSubDirSize + deSubFileSize + 2.
        END.
        ELSE IF cFileType EQ "F" THEN DO:
            ASSIGN
                FILE-INFO:FILE-NAME = cLongFileName
                deThisFileSize = FILE-INFO:FILE-SIZE  
                opdeSubDirSize = opdeSubDirSize + deThisFileSize.
        END. 
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLog C-Win 
PROCEDURE pLog :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcStatus AS CHAR NO-UNDO.
    DEF VAR cLogFile AS CHAR NO-UNDO.
    DEF VAR cMsgStr AS CHAR NO-UNDO.
                
    IF INDEX(ipcStatus,"duplicate") EQ 0 THEN 
    DO:
        ASSIGN
            eStatus:{&SV}       = eStatus:{&SV} + ipcStatus + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        ASSIGN
            cLogFile = cDrive + "\" + cTopDir + "\" + cAdminDir + "\" + cEnvAdmin + "\DbbMaintLog.txt"
            cMsgStr = "  " + ipcStatus.
        
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr FORMAT "x(60)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
    END.
    
    PROCESS EVENTS.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRestoreDb C-Win 
PROCEDURE pRestoreDb :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR lOnline AS LOG NO-UNDO.
    DEF VAR cLockFile AS CHAR NO-UNDO.
    DEF VAR cCmdString AS CHAR NO-UNDO.
    DEF VAR cStopString AS CHAR NO-UNDO.
    DEF VAR cStartString AS CHAR NO-UNDO.
    DEF VAR iWaitCount AS INT NO-UNDO.
    DEF VAR cTmpDir AS CHAR NO-UNDO.
    DEF VAR iIdx AS INT NO-UNDO.
    
    ASSIGN 
        iIdx = LOOKUP(slAvailDbs:{&SV},cAllDbList)
        cTmpDir = ENTRY(iIdx,cAllDbDirList)
        cDbLongName = cDbDrive + "\" + cTopDir + "\" + cDbDir + "\" + cTmpDir + "\" + slAvailDbs:{&SV} + ".db".
    
    IF tbShowMsg:CHECKED {&IN} THEN DO:
        MESSAGE 
            "This procedure will restore the " + slAvailDbs:{&SV} + " database from the" SKIP 
            cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV} + " file." SKIP 
            "This action cannot be undone.  Are you sure?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
        IF NOT lSure THEN RETURN.
    END.    
    
    RUN pLog("  Beginning DB restore from backup").
    /* Is the database online? */
    ASSIGN
        cLockFile = REPLACE(cDbLongName,".db",".lk") 
        lOnline = SEARCH(cLockFile) NE ?.
    ASSIGN 
        cStopString = cDlcDir + "\bin\dbman" + 
            " -host " + cHostName + 
            " -port " + cPort + 
            " -database " + slAvailDbs:{&SV} + 
            " -stop"
        cStartString = cDlcDir + "\bin\dbman" + 
            " -host " + cHostName + 
            " -port " + cPort + 
            " -database " + slAvailDbs:{&SV} + 
            " -start"
            .
{&Won}
    IF lOnline THEN DO:
        STATUS DEFAULT "Stopping database server " + slAvailDbs:{&SV}.
        
        OS-COMMAND SILENT VALUE(cStopString).
       
        /* May have to wait for DB to shut down */
        WaitBlock:
        DO WHILE SEARCH(cLockFile) NE ?:
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            STATUS DEFAULT "Waiting for removal of lock file (" + STRING(iWaitCount) + ")".
            IF iWaitCount EQ 15 THEN 
            DO:
                LEAVE waitblock.
            END.
        END.
        IF SEARCH(cLockFile) NE ? THEN 
        DO:
            MESSAGE 
                "Unable to shut down server for " + slAvailDbs:{&SV} + "." SKIP 
                "Restore FAILED.".
            RETURN.
        END.
    END.

    STATUS DEFAULT "Restoring database " + slAvailDbs:{&SV} + " from backup".
    ASSIGN 
        cCmdString = "ECHO Y | " + cDLCDir + "\bin\_dbutil prorest " + cDbLongName + " " + 
                 cDrive + "\" + cTopDir + "\" + cBackupDir + "\Databases\" + fiDbBkupFileName:{&SV}
        iWaitcount = 0.
    OS-COMMAND SILENT VALUE(cCmdString).

    IF lOnline THEN 
    DO:
        STATUS DEFAULT "Restarting database server " + slAvailDbs:{&SV}.
        OS-COMMAND SILENT VALUE(cStartString).
        Waitblock3:
        DO WHILE SEARCH(cLockFile) EQ ?:
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            STATUS DEFAULT "Waiting for restoration of lock file (" + STRING(iWaitCount) + ")".
            IF iWaitCount EQ 15 THEN 
            DO:
                LEAVE waitblock3.
            END.
        END.
        IF SEARCH(cLockFile) EQ ? THEN 
        DO:
            MESSAGE 
                "Unable to restart the " + slAvailDbs:{&SV} + " database after restore." SKIP 
                "This may be a symptom of several problems.  You should contact" SKIP 
                "Advantzware support if you have additional issues."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.
{&Woff}
    IF tbShowMsg:CHECKED {&IN} THEN 
    MESSAGE 
        "Database restore successful!"
        VIEW-AS ALERT-BOX.
    
    RUN pLog("  DB restore from backup complete").
    STATUS DEFAULT "Database Restore completed".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRestoreEnv C-Win 
PROCEDURE pRestoreEnv :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lTestExists AS LOG NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cNewEntry AS CHAR NO-UNDO.
    DEF VAR cListItems AS CHAR NO-UNDO.
    DEF VAR cScreenValue AS CHAR NO-UNDO.
    DEF VAR c7zLoc AS CHAR NO-UNDO.
    DEF VAR cSrcLoc AS CHAR NO-UNDO.
    
    RUN pLog("  Beginning ENV restore from backup").
    STATUS DEFAULT "Restoring Environment from backup".

    ASSIGN 
        c7zLoc = cDrive + "\" + cTopDir + "\" + cAdminDir + "\" + cEnvAdmin + "\7z.exe"
        cSrcLoc = cDrive + "\" + cTopDir + "\" + cBackupDir + "\Environments\" + slAvailEnvBaks:{&SV}
        cTgtEnv = cDrive + "\" + cTopDir + "\" + cEnvDir + "\" + slAvailEnvs:{&SV}.

    ASSIGN
        cCmdLine1 = c7zLoc + " x " + cSrcLoc + " -y -o" + cTgtEnv.
{&Won}
    OS-COMMAND SILENT VALUE(cCmdLine1).
    {&Woff}
    RUN pLog("  ENV restore from backup complete").
    STATUS DEFAULT "Environment Restore complete".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetCurrentDir C-Win 
PROCEDURE pSetCurrentDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipTgtDir AS CHAR NO-UNDO.
    DEF VAR iResult AS INT NO-UNDO.
    DEF VAR iReturnValue AS INT NO-UNDO.
    
    &IF DEFINED(FWD-VERSION) > 0 &THEN
    set-working-directory(ipTgtDir).
    &ELSE
    
    RUN SetCurrentDirectoryA (ipTgtDir, OUTPUT iResult).

    IF iResult NE 1 THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetHelp C-Win 
PROCEDURE pSetHelp :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcName AS CHAR.

    CASE ipcName:
        WHEN "fiUserID" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "This is the user ID used for system upgrades, NOT your normal login.  If not known, please " +
                "contact Advantzware Support for assistance.  You MUST enter a valid User ID and password " +
                "to perform the operations listed below.".
        WHEN "fiPassword" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "This is the password used for system upgrades, NOT your normal login.  If not known, please " +
                "contact Advantzware Support for assistance.  You MUST enter a valid User ID and password " +
                "to perform the operations listed below.".
        WHEN "rsDoWhat" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Choose which operation you want to perform.  Operations can only be done on ONE database " +
                "or environment at a time, so you may need to run a command multiple times. Note that ASI " +
                "databases and AUDIT databases are listed separately.".
        WHEN "slAvailDbs" THEN 
            DO:
                IF rsDoWhat:{&SV} EQ "B" THEN ASSIGN eHelp:{&SV} =
                    "This is a list of databases that are known to the Advantzware system.  You may back up any " +
                    "one of these.  Choose a DB from this list, and either an existing backup file or check the " +
                    "'New Backup' checkbox to back up the DB.  Note that the database size on disk and estimated " +
                    "backup file size are noted at the bottom of this screen.".
                ELSE IF rsDoWhat:{&SV} EQ "R" THEN ASSIGN eHelp:{&SV} =
                    "This is a list of databases that are known to the Advantzware system.  You may restore any " +
                    "one of these from a backup file.  Select a DB from this list, and an existing backup file " +
                    "from the list on the right, then choose 'EXECUTE' to restore.  Note that this operation " +
                    "CANNOT be undone.".
            END.
        WHEN "slAvailDbBaks" THEN 
            DO:
                IF rsDoWhat:{&SV} EQ "B" THEN ASSIGN eHelp:{&SV} = 
                    "This is a list of database backups that are known to the Advantzware system.  Select a " +
                    "backup file from this list if you want to REPLACE it with a new backup, or check the " + 
                    "'New Backup' option to create a new database backup.  Note that the database size on " +
                    "disk and estimated backup file size are noted at the bottom of this screen.".
                ELSE IF rsDoWhat:{&SV} EQ "R" THEN ASSIGN eHelp:{&SV} =
                    "This is a list of database backups that are known to the Advantzware system.  Select a " +
                    "backup file from this list and a database on the left to RESTORE that DB with the data " + 
                    "in this file. Note that this operation CANNOT be undone.".
            END.  
        WHEN "tbCreateNewDbBak" THEN 
            ASSIGN 
                eHelp:{&SV} =
                "Check this box to create a new database backup file in the format selected.  Format options " +
                "are: Standard - includes DB name and date created; Extended - like Standard but adds the " +
                "time created (useful for multiple daily backups); Basic - includes ONLY the DB name (not " +
                "recommended).".  
        WHEN "tbEstimate" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "When this control is checked, the system will calculate the (approximate) size of the backup " +
                "you wish to create.  This calculation is useful if you have limited space available on your " +
                "filesystem.". 
        WHEN "tbShowMsg" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "When this control is checked, the system will display prompt messages (like 'Are you sure?') " +
                "and success messages (like 'DB backup successful!').  This can be useful when you are learning " +
                "how the program works.  Failure messages (when an operation does not complete successfully) are " +
                "always displayed.". 
        WHEN "rsDbNameType" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Use this to select the format of the new backup file name.  Format options are: Standard - " +
                "includes DB name and date created; Extended - like Standard but adds the time created " +
                "(useful for multiple daily backups); Basic - includes ONLY the DB name (not recommended).".  
        WHEN "rsFull" THEN 
            ASSIGN 
                eHelp:{&SV} =
                "Use this control to select the type (and therefore size) of the backup.  'Basic' backup " +
                "includes only the system files installed by ASI during an upgrade (Programs, Resources, and " +
                "Override directories).  'Full' backup includes all of the directories in the environment, " +
                "and may take over an hour to complete.".
        WHEN "bBackupDbBak" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Choose this button to create an archive copy of the backup file (this is like a 'backup of " +
                "a backup')  Archive files will NOT show in the list of backups you can use in processing.".
        WHEN "tbDelSelDbBak" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Select this option to permanently delete the selected DB backup file.  Use the EXECUTE " +
                "button to process the deletion.  Note that this operation CANNOT be undone.".
        WHEN "slAvailEnvs" THEN 
            DO:
                IF rsDoWhat:{&SV} EQ "E" THEN ASSIGN eHelp:{&SV} =
                    "This is a list of environments that are known to the Advantzware system.  You may back up any " +
                    "one of these.  Choose an item from this list, and either an existing backup file or check the " +
                    "'New Backup' checkbox to back up the environment.  This operation will ONLY back up the /programs " +
                    "and /Resources directories in the environment.  Note that the env size on disk and estimated " +
                    "backup file size are noted at the bottom of this screen.".
                ELSE IF rsDoWhat:{&SV} EQ "X" THEN ASSIGN eHelp:{&SV} =
                    "This is a list of environments that are known to the Advantzware system.  You may restore any " +
                    "one of these from a backup file.  Select an item from this list, and an existing backup file " +
                    "from the list on the right, then choose 'EXECUTE' to restore.  Note that this operation " +
                    "CANNOT be undone.".
            END.
        WHEN "slAvailEnvBaks" THEN 
            DO:
                IF rsDoWhat:{&SV} EQ "E" THEN ASSIGN eHelp:{&SV} = 
                    "This is a list of environment backups that are known to the Advantzware system.  Select a " +
                    "backup file from this list if you want to REPLACE it with a new backup, or check the " + 
                    "'New Backup' option to create a new environment backup.  Note that the environment size on " +
                    "disk and estimated backup file size are noted at the bottom of this screen.".
                ELSE IF rsDoWhat:{&SV} EQ "X" THEN ASSIGN eHelp:{&SV} =
                    "This is a list of environment backups that are known to the Advantzware system.  Select a " +
                    "backup file from this list and an environmente on the left to RESTORE that environment with the data " + 
                    "in this file. Note that this operation CANNOT be undone.".
            END.  
        WHEN "tbCreateNewEnvBak" THEN 
            ASSIGN 
                eHelp:{&SV} =
                "Check this box to create a new environment backup file in the format selected.  Format options " +
                "are: Standard - includes Env name and date created; Extended - like Standard but adds the " +
                "time created (useful for multiple daily backups); Basic - includes ONLY the Environment name (not " +
                "recommended).".  
        WHEN "rsEnvNameType" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Use this to select the format of the new backup file name.  Format options are: Standard - " +
                "includes DB name and date created; Extended - like Standard but adds the time created " +
                "(useful for multiple daily backups); Basic - includes ONLY the DB name (not recommended).".  
        WHEN "bBackupEnvBak" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Choose this button to create an archive copy of the backup file (this is like a 'backup of " +
                "a backup')  Archive files will NOT show in the list of backups you can use in processing.".
        WHEN "tbDelSelEnvBak" THEN 
            ASSIGN 
                eHelp:{&SV} = 
                "Select this option to permanently delete the selected Environment backup file.  Use the EXECUTE " +
                "button to process the deletion.  Note that this operation CANNOT be undone.".
        WHEN "btn_Cancel" THEN 
            ASSIGN 
                eHelp:{&SV} =
                "Choose this button to close this program".
        WHEN "btn_OK" THEN 
            ASSIGN 
                eHelp:{&SV} =
                "Choose this button to process the operation you selected above.  In most cases, you will be offered " +
                "the option to continue or cancel, particularly when the operation is not reversible.".
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateValues C-Win 
PROCEDURE pValidateValues :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDoWhat AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    CASE ipcDoWhat:
        WHEN "B" THEN DO:  /* source DB and target .bak names */
            IF slAvailDbs:{&SV} EQ ?
            OR fiDbBkupFileName:{&SV} EQ "?" THEN ASSIGN 
                oplError = TRUE 
                opcMessage = "Db=" + STRING(slAvailDbs:{&SV} NE ?) + ":Bkup=" + STRING(fiDbBkupFileName:{&SV} NE "?").
        END.
        WHEN "R" THEN DO:  /* source .bak and target DB names */
            IF slAvailDbBaks:{&SV} EQ ?
            OR slAvailDbs:{&SV} EQ ? THEN ASSIGN 
                oplError = TRUE 
                opcMessage = "Bkup=" + STRING(slAvailDbBaks:{&SV} NE ?) + "Db=" + STRING(slAvailDbs:{&SV} NE ?).
        END.
        WHEN "E" THEN DO:  /* source env and target .7z names */
            IF slAvailEnvs:{&SV} EQ ?
            OR fiEnvBkupFileName:{&SV} EQ "?" THEN ASSIGN 
                oplError = TRUE 
                opcMessage = "Env=" + STRING(slAvailEnvs:{&SV} NE ?) + ":Bkup=" + STRING(fiEnvBkupFileName:{&SV} NE "?").
        END.
        WHEN "X" THEN DO:  /* source .7z and target env names */
            IF slAvailEnvBaks:{&SV} EQ ?
            OR slAvailEnvs:{&SV} EQ ? THEN ASSIGN 
                oplError = TRUE 
                opcMessage = "Bkup=" + STRING(slAvailEnvBaks:{&SV} NE ?) + "Env=" + STRING(slAvailEnvs:{&SV} NE ?).
        END.
        WHEN "O" THEN DO:  /* source .bak or source .7z name, archive and/or delete */
            IF (tbArchiveDbBak:CHECKED OR tbDelSelDbBak:CHECKED)
            AND slAvailDbBaks:{&SV} EQ ? THEN DO:
                ASSIGN 
                    oplError = TRUE 
                    opcMessage = "Not enough info".
            END. 
            IF (tbArchiveEnvBak:CHECKED OR tbDelSelEnvBak:CHECKED)
            AND slAvailEnvBaks:{&SV} EQ ? THEN DO:
                ASSIGN 
                    oplError = TRUE 
                    opcMessage = "Not enough info".
            END. 
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

