&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: asiInstaller.w
  Description: utility to install ASI patches and releases
  Input Parameters:  ipcName - name of database to update
                     ipcPort - database port number
                     ipcDir - environment directory name
                     ipcVer - current version
                     ipcEnv - current Environment 
                     ipcFromVer - from Env version
                     ipcToVer - to Env version
                     ipiLevel - users security level
                     iplNeedBackup - backup database?
  Output Parameters: oplSuccess - upgrade successful
  Author: MYT
  Created: 10/1/2017 and highly modified/adapted over next several months
  Change History:
    12/19/2017 - MYT -  updated to handle 16.6.9 data fixes
                        added Clean Before Install to suppress deletion
                        of existing programs/resources directories prior
                        to install of new (lets customers stay live)
    10/04/2018 - MYT - documented process flow in asiUpdateENV_process_flow.txt
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

DEF INPUT PARAMETER ipcName AS CHAR NO-UNDO. 
DEF INPUT PARAMETER ipcPort AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcEnv AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcFromVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcToVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiLevel AS INT NO-UNDO.
DEF INPUT PARAMETER iplNeedBackup AS LOG NO-UNDO.
DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopiStatus AS INT NO-UNDO.

/*
/* FOR TEST PURPOSES ONLY */
DEF VAR ipcName AS CHAR NO-UNDO.
DEF VAR ipcPort AS CHAR NO-UNDO.
DEF VAR ipcDir AS CHAR NO-UNDO.
DEF VAR ipcVer AS CHAR NO-UNDO.
DEF VAR ipiLevel AS INT NO-UNDO.
DEF VAR iplNeedBackup AS LOG NO-UNDO.
DEF VAR oplSuccess AS LOG NO-UNDO.
ASSIGN
    ipcName = "asiTest167"
    ipcPort = "2856"
    ipcDir = "Test"
    ipcVer = "16.7"
    ipiLevel = 10.
*/
/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE SV SCREEN-VALUE IN FRAME DEFAULT-FRAME

{iniFileVars.i}

DEF STREAM s1.
DEF STREAM s2.
DEF STREAM apiFiles.
DEF STREAM sOutput.

DEF TEMP-TABLE ttAuditTbl LIKE AuditTbl.
DEF TEMP-TABLE ttCueCard LIKE cueCard.
DEF TEMP-TABLE ttCueCardText LIKE cueCardText.
DEF TEMP-TABLE ttPrgrms LIKE prgrms.
DEF TEMP-TABLE ttPrgmxref LIKE prgmxref.
DEF TEMP-TABLE ttEmailcod LIKE emailcod.
DEF TEMP-TABLE ttNotes LIKE notes.
DEF TEMP-TABLE ttModule LIKE module.
DEF TEMP-TABLE ttLookups LIKE lookups.
DEF TEMP-TABLE ttReftable LIKE reftable.
DEF TEMP-TABLE ttSysCtrl LIKE sys-ctrl.
DEF TEMP-TABLE ttSys-Ctrl LIKE sys-ctrl.
DEF TEMP-TABLE ttSysCtrlShipto LIKE sys-ctrl-shipto.
DEF TEMP-TABLE ttTranslation LIKE translation.
DEF TEMP-TABLE ttUserLanguage LIKE userlanguage.
DEF TEMP-TABLE ttXuserMenu LIKE xuserMenu.
DEF TEMP-TABLE ttUtilities LIKE utilities.
DEF TEMP-TABLE ttZmessage LIKE zMessage.

DEF TEMP-TABLE ttPfFile
    FIELD ttfLine AS INT  
    FIELD ttfRawLine AS CHAR 
    FIELD ttfParm AS CHAR 
    FIELD ttfValue AS CHAR
    FIELD ttfDesc AS CHAR 
    INDEX iLine IS PRIMARY 
        ttfLine.

DEFINE TEMP-TABLE ttUserMenu NO-UNDO
    FIELD prgmname AS CHARACTER
    INDEX prgmname IS PRIMARY
    prgmname
    .            
        
DEFINE TEMP-TABLE ttDBMS NO-UNDO 
    FIELD iLineNo AS INTEGER 
    FIELD cLine AS CHARACTER 
    INDEX iLine IS PRIMARY 
    iLineNo
    .

DEF TEMP-TABLE tempUser NO-UNDO LIKE _User.
DEF TEMP-TABLE ttUsers
    FIELD ttfuserid AS CHAR
    FIELD ttfdbname AS CHAR
    FIELD ttfalias AS CHAR
    FIELD ttfenvlist AS CHAR
    FIELD ttfdblist AS CHAR
    FIELD ttfmodelist AS CHAR.

DEFINE TEMP-TABLE ttDuplicates
    FIELD cItem       AS CHARACTER
    FIELD cVendor     AS CHARACTER
    FIELD cCustomer   AS CHARACTER 
    FIELD cEstimateNo AS CHARACTER
    FIELD iForm       AS INTEGER
    FIELD iBlank      AS INTEGER
    FIELD dEQty       AS DECIMAL
    FIELD cCompany    AS CHARACTER
    FIELD cItemType   AS CHARACTER
    . 
    
DEF TEMP-TABLE ttTemplateFiles
    FIELD cFileName AS CHAR 
    FIELD cLongName AS CHAR 
    FIELD daModDate AS DATE.    

DEF TEMP-TABLE ttResTemplateFiles
    FIELD cFileName AS CHAR 
    FIELD cLongName AS CHAR 
    FIELD daModDate AS DATE.    

DEF BUFFER bnotes FOR notes.
DEF BUFFER bf-usercomp FOR usercomp.
DEF BUFFER bf-module FOR MODULE.

DEF STREAM outStream.
DEF STREAM logStream.
DEF STREAM iniStream.

DEF VAR cAuditExceptionList AS CHAR NO-UNDO.
DEF VAR cDbDirOnly AS CHAR NO-UNDO.
DEF VAR cBadDirList AS CHAR NO-UNDO.
DEF VAR cfrom AS CHAR.
DEF VAR cMapDrive AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR cMsgStr AS CHAR FORMAT "x(80)" EXTENT 100 NO-UNDO.
DEF VAR connectStatement AS CHAR NO-UNDO.
DEF VAR cPassword AS CHAR FORMAT "x(24)" LABEL "Password" NO-UNDO.
DEF VAR cPatchNo AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR cRunPgm AS CHAR NO-UNDO.
DEF VAR cTemp AS CHAR NO-UNDO.
DEF VAR cTestDir AS CHAR NO-UNDO.
DEF VAR cThisPatch AS CHAR NO-UNDO.
DEF VAR cTo AS CHAR.
DEF VAR cUserID AS CHAR FORMAT "x(8)" LABEL "User ID" NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR cUsrList AS CHAR NO-UNDO.
DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR delCtr AS INT NO-UNDO.
DEF VAR dupCtr AS INT NO-UNDO.
DEF VAR hPreRun AS HANDLE.
DEF VAR giCountCreated    AS INTEGER   NO-UNDO.
DEF VAR giCountDuplicate  AS INTEGER   NO-UNDO.
DEF VAR glUseQtyFrom      AS LOGICAL   NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iCurrVerExt AS INT NO-UNDO.
DEF VAR iDBCurrVer AS INT NO-UNDO.
DEF VAR iDBTgtVer AS INT NO-UNDO.
DEF VAR iExtra AS INT NO-UNDO.
DEF VAR iLastIni AS INT NO-UNDO.
DEF VAR iListEntry AS INT NO-UNDO.
DEF VAR iLockoutTries AS INT NO-UNDO.
DEF VAR iMsgCtr AS INT NO-UNDO.
DEF VAR iNumberChecked AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR iUserCount AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR lAllOK AS LOG NO-UNDO.
DEF VAR lAutorun AS LOG NO-UNDO.
DEF VAR lConnectAudit AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR lNeedUsercontrol AS LOG NO-UNDO.
DEF VAR lSuccess AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lUpdUsr AS LOG NO-UNDO.
DEF VAR lValidDB AS LOG NO-UNDO.
DEF VAR origPropath AS CHAR NO-UNDO.
DEF VAR timestring AS CHAR NO-UNDO.
DEF VAR tslogin-cha AS CHAR NO-UNDO.
DEF VAR v1 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v2 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v3 LIKE lookups.frame_field NO-UNDO.
DEF VAR v4 LIKE lookups.prgmname NO-UNDO.
DEF VAR v5 LIKE lookups.rec_key NO-UNDO.
DEF VAR xDbDir AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-6 tbBackupDBs tbUserControl ~
tbUserCleanup tbDelBadData tbUpdateMaster tbRunDataFix tbUpdateNK1s ~
fiLicensedUsers tbReftableConv bProcess tbLoadMenus tbRelNotes eStatus ~
tbInstallFiles tbUpdateIni 
&Scoped-Define DISPLAYED-OBJECTS fiSiteName fiOptions fiHostname ~
tbBackupDBs tbUserControl fiEnvironment tbUserCleanup fiAsiDbName ~
fiAudDbName tbDelBadData fiAsiPortNo fiAudPortNo tbUpdateMaster fiFromVer ~
tbRunDataFix fiToVer tbUpdateNK1s fiLicensedUsers tbUpdateFileLocs ~
tbReftableConv tbLoadMenus tbRelNotes eStatus tbBackupFiles tbInstallFiles ~
tbUpdateIni 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFixYear C-Win 
FUNCTION fFixYear RETURNS DATE
  (INPUT daDate AS DATE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD formatForCSV C-Win 
FUNCTION FormatForCSV RETURNS CHARACTER 
    (ipcValue AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bProcess 
     LABEL "Start Update" 
     SIZE 46 BY 1.43
     FONT 6.

DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 3.81 NO-UNDO.

DEFINE VARIABLE fiAsiDbName AS CHARACTER FORMAT "X(256)":U 
     LABEL "and databases (ASI)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiAsiPortNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "running on ports" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiAudDbName AS CHARACTER FORMAT "X(256)":U 
     LABEL "(Audit)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiAudPortNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiEnvironment AS CHARACTER FORMAT "X(256)":U 
     LABEL "Upgrading environment" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromVer AS CHARACTER FORMAT "X(256)":U 
     LABEL "from ASI version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiHostname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Server Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiLicensedUsers AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Licensed User count" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "Options:" 
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

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 14.76.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.43
     FGCOLOR 3 .

DEFINE RECTANGLE rStatusBar
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 10 BY 1.43
     BGCOLOR 3 FGCOLOR 3 .

DEFINE VARIABLE tbBackupDBs AS LOGICAL INITIAL no 
     LABEL "Backup Databases" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbBackupFiles AS LOGICAL INITIAL no 
     LABEL "Backup System Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbDelBadData AS LOGICAL INITIAL no 
     LABEL "Remove deprecated records" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbInstallFiles AS LOGICAL INITIAL no 
     LABEL "Install new System Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbLoadMenus AS LOGICAL INITIAL no 
     LABEL "Load new Menu files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbReftableConv AS LOGICAL INITIAL no 
     LABEL "Convert Reftable elements" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbRelNotes AS LOGICAL INITIAL no 
     LABEL "Copy Release Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbRunDataFix AS LOGICAL INITIAL no 
     LABEL "Run Data Fix programs" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateFileLocs AS LOGICAL INITIAL no 
     LABEL "Update hardcoded file locations" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateIni AS LOGICAL INITIAL no 
     LABEL "Update advantzware.ini file" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateMaster AS LOGICAL INITIAL no 
     LABEL "Update Master records" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateNK1s AS LOGICAL INITIAL no 
     LABEL "Update NK1 records" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUserCleanup AS LOGICAL INITIAL no 
     LABEL "Cleanup user files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUserControl AS LOGICAL INITIAL no 
     LABEL "Create/Update User Control" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSiteName AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 68
     fiOptions AT ROW 1.24 COL 87 COLON-ALIGNED NO-LABEL
     fiHostname AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 36
     tbBackupDBs AT ROW 2.43 COL 90 WIDGET-ID 384
     tbUserControl AT ROW 3.38 COL 90 WIDGET-ID 370
     fiEnvironment AT ROW 3.86 COL 29 COLON-ALIGNED
     tbUserCleanup AT ROW 4.33 COL 90 WIDGET-ID 368
     fiAsiDbName AT ROW 4.81 COL 29 COLON-ALIGNED
     fiAudDbName AT ROW 4.81 COL 56 COLON-ALIGNED
     tbDelBadData AT ROW 5.29 COL 90 WIDGET-ID 374
     fiAsiPortNo AT ROW 5.76 COL 29 COLON-ALIGNED
     fiAudPortNo AT ROW 5.76 COL 56 COLON-ALIGNED
     tbUpdateMaster AT ROW 6.24 COL 90 WIDGET-ID 376
     fiFromVer AT ROW 6.71 COL 29 COLON-ALIGNED
     tbRunDataFix AT ROW 7.19 COL 90 WIDGET-ID 400
     fiToVer AT ROW 7.67 COL 29 COLON-ALIGNED WIDGET-ID 46
     tbUpdateNK1s AT ROW 8.14 COL 90 WIDGET-ID 396
     fiLicensedUsers AT ROW 8.62 COL 29 COLON-ALIGNED WIDGET-ID 440
     tbUpdateFileLocs AT ROW 9.1 COL 90 WIDGET-ID 398
     tbReftableConv AT ROW 10.05 COL 90 WIDGET-ID 504
     bProcess AT ROW 10.29 COL 15 WIDGET-ID 404
     tbLoadMenus AT ROW 11 COL 90 WIDGET-ID 378
     tbRelNotes AT ROW 11.95 COL 90 WIDGET-ID 382
     eStatus AT ROW 12.43 COL 3 NO-LABEL
     tbBackupFiles AT ROW 12.91 COL 90 WIDGET-ID 386
     tbInstallFiles AT ROW 13.86 COL 90 WIDGET-ID 388
     tbUpdateIni AT ROW 14.81 COL 90 WIDGET-ID 450
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.71 COL 3 WIDGET-ID 54
     RECT-5 AT ROW 1.48 COL 87
     rStatusBar AT ROW 16.71 COL 3
     RECT-6 AT ROW 16.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130 BY 17.86 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Advantzware Update - Programs/Data"
         COLUMN             = 5
         ROW                = 5
         HEIGHT             = 17.91
         WIDTH              = 130
         MAX-HEIGHT         = 34.29
         MAX-WIDTH          = 166.2
         VIRTUAL-HEIGHT     = 34.29
         VIRTUAL-WIDTH      = 166.2
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
/* SETTINGS FOR FILL-IN fiAsiDbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAsiPortNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAudDbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAudPortNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEnvironment IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFromVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHostname IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOptions IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSiteName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rStatusBar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbBackupFiles IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateFileLocs IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Advantzware Update - Programs/Data */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    IF USERID(LDBNAME(1)) EQ "" THEN RETURN.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Advantzware Update - Programs/Data */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON LEAVE OF FRAME DEFAULT-FRAME
ANYWHERE DO:
    RUN ipUpdateTTIniFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess C-Win
ON CHOOSE OF bProcess IN FRAME DEFAULT-FRAME /* Start Update */
DO:
    RUN ipProcessAll.

    IF CONNECTED(LDBNAME(2)) THEN
        DISCONNECT VALUE(LDBNAME(2)).
    IF CONNECTED(LDBNAME(1)) THEN
        DISCONNECT VALUE(LDBNAME(1)).

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

    ASSIGN 
        fiEnvironment:{&SV} = ipcEnv
        fiFromVer:{&SV}     = ipcFromVer
        fiToVer:{&SV}       = ipcToVer
        fiAsiDbName:{&SV}   = ipcName
        fiAsiPortNo:{&SV}   = ipcPort
        rStatusBar:WIDTH    = MIN(75,(iopiStatus / 100) * 75).

    RUN ipFindIniFile ("..\advantzware.ini",
                       OUTPUT cIniLoc).
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipExpandVarNames.
    RUN ipSetDispVars.
    
    ASSIGN 
        iListEntry          = LOOKUP(fiAsiDbName:{&SV},cDbList)
        fiAudDbName:{&SV}   = ENTRY(iListEntry,cAudDbList)
        fiAudPortNo:{&SV}   = ENTRY(iListEntry,cAudPortList).
    
    RUN ipValidateDB (OUTPUT lValidDB).
    IF NOT lValidDB THEN RETURN.

    IF ipiLevel GT 10 THEN ASSIGN
        fiLicensedUsers:SENSITIVE = TRUE
        tbUserControl:SENSITIVE = TRUE.

    FIND FIRST usercontrol NO-LOCK NO-ERROR.
    IF AVAIL usercontrol THEN ASSIGN
        fiLicensedUsers:{&SV} = STRING(usercontrol.numLicensedUsers)
        lNeedUsercontrol = FALSE.
    ELSE ASSIGN
        lNeedUsercontrol = TRUE.
        
    ASSIGN
        tbBackupDBs:CHECKED = iplNeedBackup
        tbUserControl:CHECKED = TRUE
        tbUserCleanup:CHECKED = TRUE
        tbDelBadData:CHECKED = TRUE
        tbUpdateMaster:CHECKED = TRUE
        tbLoadMenus:CHECKED = TRUE
        tbRunDataFix:CHECKED = TRUE
        tbUpdateNK1s:CHECKED = TRUE
        tbUpdateFileLocs:CHECKED = TRUE
        tbRefTableConv:CHECKED = TRUE
        tbRelNotes:CHECKED = TRUE
        tbBackupFiles:CHECKED = FALSE
        tbInstallFiles:CHECKED = TRUE
        tbUpdateINI:CHECKED = TRUE
        .
        
    IF ipiLevel LT 10 THEN DO:
        ASSIGN
            c-Win:WIDTH = 79.8
            bProcess:LABEL = "No User Action Required"
            lAutorun = TRUE.
        DISABLE ALL EXCEPT bProcess eStatus WITH FRAME {&FRAME-NAME}.
        APPLY 'choose' to bProcess.
    END.

    RETURN.
    
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
  DISPLAY fiSiteName fiOptions fiHostname tbBackupDBs tbUserControl 
          fiEnvironment tbUserCleanup fiAsiDbName fiAudDbName tbDelBadData 
          fiAsiPortNo fiAudPortNo tbUpdateMaster fiFromVer tbRunDataFix fiToVer 
          tbUpdateNK1s fiLicensedUsers tbUpdateFileLocs tbReftableConv 
          tbLoadMenus tbRelNotes eStatus tbBackupFiles tbInstallFiles 
          tbUpdateIni 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 RECT-6 tbBackupDBs tbUserControl tbUserCleanup tbDelBadData 
         tbUpdateMaster tbRunDataFix tbUpdateNK1s fiLicensedUsers 
         tbReftableConv bProcess tbLoadMenus tbRelNotes eStatus tbInstallFiles 
         tbUpdateIni 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipActivateParent C-Win 
PROCEDURE ipActivateParent :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER prgrms FOR prgrms.
    
    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmName EQ ipcPrgmName
         NO-ERROR.
    IF AVAILABLE prgrms THEN DO:
        FIND FIRST xUserMenu
             WHERE xUserMenu.user_id  EQ ipcUserID
               AND xUserMenu.prgmName EQ ipcPrgmName
             NO-ERROR.
        IF AVAILABLE xUserMenu THEN
        DELETE xUserMenu.
        IF prgrms.itemParent NE "" THEN
        RUN ipActivateParent (prgrms.itemParent, ipcUserID).
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAddDbmsFonts C-Win 
PROCEDURE ipAddDbmsFonts :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cFileStream AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.
    DEF VAR iThisLine AS INT NO-UNDO.
    DEF VAR cLineText AS CHAR NO-UNDO.
    DEF VAR iNextLine AS INT NO-UNDO.
    
    RUN ipStatus ("    Fixing dbms.ini files").
    
    ASSIGN 
        cTgtEnv = cEnvAdmin.

    INPUT FROM OS-DIR (cEnvAdmin).

    REPEAT:
        IMPORT cFileStream.
        FILE-INFO:FILE-NAME = cFileStream.

        IF SUBSTRING(FILE-INFO:FILE-NAME,LENGTH(FILE-INFO:FILE-NAME),1) EQ "." THEN DO:
            NEXT.
        END.
        ELSE IF FILE-INFO:FILE-TYPE BEGINS "F" 
        AND INDEX(FILE-INFO:FILE-NAME,"dbms") NE 0 
        AND INDEX(FILE-INFO:FILE-NAME,".ini") NE 0 THEN DO:
            EMPTY TEMP-TABLE ttDbms.
            ASSIGN 
                iThisLine = 100.
            
            INPUT STREAM s1 FROM VALUE(FILE-INFO:FULL-PATHNAME).
            REPEAT:
                IMPORT STREAM s1 UNFORMATTED cLineText.
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iThisLine
                    ttDbms.cLine = cLineText
                    iThisLine = iThisLine + 100.
            END.
            INPUT STREAM s1 CLOSE.
            
            FIND FIRST ttDbms NO-LOCK WHERE 
                ttDbms.cLine BEGINS "font32"
                NO-ERROR.
            IF NOT AVAIL ttDbms THEN DO:
                FIND FIRST ttDbms NO-LOCK WHERE 
                    ttDbms.cLine BEGINS "font31"
                    NO-ERROR.
                ASSIGN 
                    iNextLine = ttDbms.iLineNo.
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iNextLine + 1
                    ttDbms.cLine = "font32=Tahoma, size=8".
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iNextLine + 2
                    ttDbms.cLine = "font33=Tahoma, size=8, bold".
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iNextLine + 3
                    ttDbms.cLine = "font34=Tahoma, size=10".
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iNextLine + 4
                    ttDbms.cLine = "font35=Tahoma, size=10, bold".
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iNextLine + 5
                    ttDbms.cLine = "font36=Tahoma, size=12".
                CREATE ttDbms.
                ASSIGN 
                    ttDbms.iLineNo = iNextLine + 6
                    ttDbms.cLine = "font37=Tahoma, size=12, bold".

                OUTPUT STREAM s2 TO VALUE(FILE-INFO:FULL-PATHNAME).
                FOR EACH ttDbms BY ttDbms.iLineNo:
                    PUT STREAM s2 UNFORMATTED ttDbms.cLine + CHR(10).
                END.

            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAddJobMchSeq C-Win
PROCEDURE ipAddJobMchSeq:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF job-mch.
    FOR EACH job-mch BY RECID(job-mch):
        ASSIGN  
            job-mch.job-mchID = NEXT-VALUE(job-mch_seq).
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAddLocationData C-Win 
PROCEDURE ipAddLocationData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF addrPhone.
    DISABLE TRIGGERS FOR LOAD OF location.
    DISABLE TRIGGERS FOR LOAD OF loc.
    
    FOR EACH loc:
        IF NOT CAN-FIND (FIRST location WHERE
            location.locationCode EQ loc.loc AND  
            location.rec_key EQ loc.addrRecKey) THEN DO:
            CREATE location.
            ASSIGN
                location.locationCode = loc.loc
                location.rec_key      = STRING(YEAR(TODAY),"9999")
                    + STRING(MONTH(TODAY),"99")
                    + STRING(DAY(TODAY),"99")
                    + STRING(TIME,"99999")
                    + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
                loc.addrRecKey        = location.rec_key.
        END. 
    END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAddSuppUserRecords C-Win 
PROCEDURE ipAddSuppUserRecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Adding Supplemental User Records").

    DEF INPUT PARAMETER ipcUserID AS CHAR NO-UNDO.
    DEF VAR lv-default-comp AS CHAR NO-UNDO.
    DEF VAR lv-default-loc AS CHAR NO-UNDO.
    DEF VAR cCurrentDir AS CHAR NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF usercomp.
    DISABLE TRIGGERS FOR LOAD OF usr.

    /* Add default usercomp records for new user */
    FIND FIRST bf-usercomp NO-LOCK WHERE 
        bf-usercomp.USER_id = "ASI" AND
        bf-usercomp.company_default EQ TRUE
        NO-ERROR.
    IF NOT AVAIL bf-usercomp THEN FIND FIRST bf-usercomp NO-LOCK WHERE 
        bf-usercomp.company_default EQ TRUE
        NO-ERROR.
    
    ASSIGN 
        lv-default-comp = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001".
    FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.USER_id = ipcUserID AND 
        usercomp.company = lv-default-comp AND
        usercomp.loc = ""
        NO-ERROR.
    IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN 
            usercomp.user_id = ipcUserID
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = ""
            usercomp.company_default = YES.
    END.

    FIND FIRST bf-usercomp NO-LOCK WHERE
        bf-usercomp.user_id = "ASI" AND
        bf-usercomp.loc_default EQ TRUE
        NO-ERROR.
    IF NOT AVAIL bf-usercomp THEN FIND FIRST bf-usercomp NO-LOCK WHERE
        bf-usercomp.loc_default 
        NO-ERROR.
    ASSIGN 
        lv-default-loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".
    FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.user_id = ipcUserID AND 
        usercomp.company = lv-default-comp AND
        usercomp.loc = lv-default-loc 
        NO-ERROR.
    IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN 
            usercomp.user_id = ipcUserID
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
            usercomp.loc_DEFAULT = YES.
    END.
    
    FIND FIRST usr WHERE 
        usr.uid EQ ipcUserID 
        NO-ERROR.
    IF NOT AVAIL usr THEN DO:
        CREATE usr.
        ASSIGN
            usr.uid = ipcUserID
            usr.usr-lang = "English"
            usr.last-chg = today.
    END.
    ELSE DO:
        IF usr.usr-lang = "EN" THEN ASSIGN
            usr.usr-lang = "English".
    END.

    /* Ensure folder available for custom menus */
    ASSIGN
        cCurrentDir = cDrive + "\" + 
                      cTopDir + "\" +
                      cEnvDir + "\" +
                      fiEnvironment:{&SV} + "\" +
                      "UserMenu\" + ipcUserID.
    OS-CREATE-DIR VALUE(cCurrentDir).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipArchiveFiles C-Win 
PROCEDURE ipArchiveFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Archiving ASI System Files").

    DEF VAR cCmdZip AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lProdOverExists AS LOG NO-UNDO.
    
    FILE-INFO:FILE-NAME = cEnvProdDir.
    ASSIGN
        lSuccess = FALSE 
        lProdExists = FILE-INFO:FULL-PATHNAME NE ?.
        
    IF SEARCH(cUpdProgramDir + "\programs.7z") NE ? THEN DO:
        RUN ipStatus ("Archiving old program files").

        /* Default is to pull from Env\Prod, but if not found, will also search root drive and Rcode */
        IF SEARCH(cEnvProdDir + "\Programs\nosweat.r") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                          cPgmBackup + "\Programs" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cEnvProdDir + "\Programs\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.    
        ELSE IF SEARCH(cMapDir + "\Programs\nosweat.r") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                          cPgmBackup + "\Programs" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cMapDir + "\Programs\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
        ELSE IF SEARCH(cMapDir + "\Rcode\Programs\nosweat.r") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                          cPgmBackup + "\Programs" + 
                          STRING(YEAR(TODAY),"99") +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY)) + ".7z " +
                          cMapDir + "\Rcode\Programs\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
    END.

    IF SEARCH(cUpdProgramDir + "\resources.7z") NE ? THEN DO:
        RUN ipStatus ("Archiving old resource files").

        IF SEARCH(cEnvProdDir + "\Resources\quoter.exe") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                          cResBackup + "\Resources" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cEnvProdDir + "\Resources\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
        ELSE IF SEARCH(cMapDir + "\Resources\quoter.exe") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                          cResBackup + "\Resources" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cMapDir + "\Resources\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
        ELSE IF SEARCH(cMapDir + "\Rcode\Resources\quoter.exe") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                          cResBackup + "\Resources" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cMapDir + "\Rcode\Resources\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
    END.

    FILE-INFO:FILE-NAME = cEnvProdDir + "\Override".
    ASSIGN
        lProdOverExists = FILE-INFO:FULL-PATHNAME NE ?.
    IF lProdOverExists THEN DO:
        RUN ipStatus ("Archiving old hotfix files").
        ASSIGN
            cCmdZip = cUpdProgramDir + "\7z.exe a " + 
                      cPgmBackup + "\Override" + 
                      STRING(YEAR(TODAY)) +
                      STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + ".7z " +
                      cEnvProdDir + "\Override\*".
        OS-COMMAND SILENT VALUE(cCmdZip).
    END.

    ASSIGN 
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAuditSysCtrl C-Win 
PROCEDURE ipAuditSysCtrl :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Turn on sys-ctrl (NK1) auditing").

    FIND FIRST auditTbl EXCLUSIVE WHERE
        auditTbl.auditTable EQ "sys-ctrl"
        NO-ERROR.
    IF AVAIL auditTbl THEN ASSIGN 
        auditTbl.auditCreate = TRUE 
        auditTbl.auditDelete = TRUE 
            auditTbl.auditUpdate = TRUE 
            .

    FIND FIRST auditTbl EXCLUSIVE WHERE
        auditTbl.auditTable EQ "sys-ctrl-shipto"
        NO-ERROR.
    IF AVAIL auditTbl THEN ASSIGN 
            auditTbl.auditCreate = TRUE 
            auditTbl.auditDelete = TRUE 
        auditTbl.auditUpdate = TRUE 
        .

    FIND FIRST auditTbl EXCLUSIVE WHERE
        auditTbl.auditTable EQ "sys-ctrl-shipto"
        NO-ERROR.
    IF AVAIL auditTbl THEN ASSIGN 
            auditTbl.auditCreate = TRUE 
            auditTbl.auditDelete = TRUE 
        auditTbl.auditUpdate = TRUE 
        .
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBackupDataFiles C-Win 
PROCEDURE ipBackupDataFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcType AS CHAR NO-UNDO.
    
    RUN ipStatus ("  Backing up data files").
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl-shipto.

&SCOPED-DEFINE cFile AuditTbl

    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile sys-ctrl
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile sys-ctrl-shipto
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile emailcod
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile lookups
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile module
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgmxref
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgrms
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile translation
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile userlanguage
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile xusermenu
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile cueCard
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile cueCardText
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynParam
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynParamSet
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynParamSetDtl
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynParamValue
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynSubject
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynSubjectColumn
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynSubjectParamSEt
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynSubjectTable
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynSubjectWhere
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile Utilities
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

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
    DEF VAR cCmdLine    AS CHAR NO-UNDO.
    DEF VAR cLocItem    AS CHAR NO-UNDO.
    DEF VAR cLocDir     AS CHAR NO-UNDO.
    DEF VAR cLocName    AS CHAR NO-UNDO.
    DEF VAR cLocPort    AS CHAR NO-UNDO.
    DEF VAR cBackupName AS CHAR NO-UNDO.
    DEF VAR cLockFile   AS CHAR NO-UNDO.  
    DEF VAR cPrefix     AS CHAR NO-UNDO.
    DEF VAR cThisDir    AS CHAR NO-UNDO.
    DEF VAR iCount AS INT NO-UNDO.
    DEF VAR cThisDB AS CHAR NO-UNDO.
    DEF VAR cThisPort AS CHAR NO-UNDO.
    
    ASSIGN 
        lSuccess = FALSE 
        iCount = 1.
        
    DO iCount = 1 TO 2:
        IF iCount = 1 THEN ASSIGN 
            cThisDB = fiAsiDBName:{&SV}.
        ELSE ASSIGN 
            cThisDB = fiAudDBName:{&SV}.
      
        ASSIGN 
            cPrefix = SUBSTRING(cThisDB,1,3).
            
        IF cPrefix = "asi" THEN ASSIGN 
                iListEntry = LOOKUP(cThisDB,cDbList)
                cThisDir   = ENTRY(iListEntry,cDbDirList)
                cThisPort  = ENTRY(iListEntry,cDbPortList).
        ELSE ASSIGN 
                iListEntry = LOOKUP(cThisDB,cAudDbList)
                cThisDir   = ENTRY(iListEntry,cAudDirList)
                cThisPort  = ENTRY(iListEntry,cAudPortList).
        
        ASSIGN
            cLocDir     = cThisDir
            cLocName    = cThisDB
            cLocPort    = cThisDir
            cBackupName = cDbBackup + "\" + cLocName + "_" +
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + "_" +
                       STRING(TIME) + ".bak" 
            cCmdLine    = cDLCDir + "\bin\probkup online " + 
                       cDBDrive + "\" + 
                       cTopDir + "\" + 
                       cDbDirOnly + "\" +
                       cLocDir + "\" +
                       cLocName + " " + 
                       cBackupName
            cLockFile   = cDbDrive + "\" +
                       cTopDir + "\" + cDbDirOnly + "\" + 
                       cThisDir + "\" +
                       cThisDB + ".lk".
        .

        IF SEARCH(cLockFile) EQ ? THEN 
        DO:
            MESSAGE 
                "The " + cThisDB + " database is not currently running." SKIP 
                "This means that it will not be possible to back up the data-" SKIP 
                "base, or to upgrade it with this program.  You should exit" SKIP 
                "this program now, and make sure that the databases are" SKIP 
                "running before you attempt to upgrade the system again."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
        
        RUN ipStatus ("  Backing Up database " + cThisDB).
        
        OS-COMMAND SILENT VALUE(cCmdLine).
        
        IF SEARCH(cBackupName) NE ? THEN 
        DO:
            ASSIGN
                iopiStatus = iopiStatus + 5
                rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75)
                lSuccess = TRUE.
            RUN ipStatus ("    Backup successful").
        END.
        ELSE 
        DO:
            ASSIGN
                lSuccess = FALSE.
            RUN ipStatus ("    Backup FAILED").
            RETURN.
        END.
    END.
    
    ASSIGN 
        lSuccess = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBuildCustFilesTree C-Win 
PROCEDURE ipBuildCustFilesTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating CustFiles Structure").

    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cCmdLine2 AS CHAR NO-UNDO.
    DEF VAR cCmdLine3 AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lTestExists AS LOG NO-UNDO.
    DEF VAR lProdFilesExist AS LOG NO-UNDO.
    DEF VAR lTestFilesExist AS LOG NO-UNDO.
    DEF VAR cTestDir AS CHAR NO-UNDO.
    DEF VAR cBaseDir AS CHAR NO-UNDO.
    DEF VAR iNumPasses AS INT NO-UNDO.
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    
    FILE-INFO:FILE-NAME = cEnvProdDir.
    ASSIGN
        lProdExists = FILE-INFO:FULL-PATHNAME NE ?.
    FILE-INFO:FILE-NAME = cEnvTestDir.
    ASSIGN
        lTestExists = FILE-INFO:FULL-PATHNAME NE ?
        cCmdLine1 = cUpdProgramDir + "\7z.exe x ".
    FILE-INFO:FILE-NAME = cEnvProdDir + "\CustFiles\Logs".
    ASSIGN
        lProdFilesExist = FILE-INFO:FULL-PATHNAME NE ?.
    FILE-INFO:FILE-NAME = cEnvTestDir + "\CustFiles\Logs".
    ASSIGN
        lTestFilesExist = FILE-INFO:FULL-PATHNAME NE ?.
    
    IF lProdFilesExist
    AND lTestFilesExist THEN RETURN.
    
    IF NOT lProdFilesExist
    AND SEARCH(cUpdatesDir + "\" + "Patch" + cPatchNo + "\Deployment\CustFiles.7z") NE ? THEN DO:
        ASSIGN
            cCmdLine2 = cCmdLine1 + cUpdatesDir + "\" + "Patch" + cPatchNo + "\Deployment\CustFiles..7z -y -o".
        IF lProdExists THEN DO:
            OS-CREATE-DIR VALUE(cEnvProdDir + "\CustFiles").
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvProdDir + "\CustFiles".
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBuildDirs C-Win 
PROCEDURE ipBuildDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Building Required Directories").

    DEF VAR cBadDir AS CHAR NO-UNDO.
    DEF VAR lStructOK AS LOG NO-UNDO.
    
    RUN ipTestStructure (OUTPUT lStructOK).
    
    IF cBadDirList NE "" THEN DO i = 1 TO NUM-ENTRIES(cBadDirList):
        ASSIGN
            cBadDir = ENTRY(i,cBadDirList).
        OS-CREATE-DIR VALUE(cBadDir).
    END.
    
    RUN ipBuildCustFilesTree.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCheckPayMaster C-Win 
PROCEDURE ipCheckPayMaster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Verifying payment-type records").

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPaper AS LOGICAL NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF payment-type.

    IF NOT CAN-FIND(FIRST payment-type WHERE 
                    payment-type.company EQ ipcCompany AND 
                    payment-type.type EQ ipcType) THEN DO:
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = ipcType
            payment-type.dscr       = ipcDescription
            payment-type.paperCheck = iplPaper
            .
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCleanBadUserData C-Win 
PROCEDURE ipCleanBadUserData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Clean Bad User Data").

DISABLE TRIGGERS FOR LOAD OF usr.
DISABLE TRIGGERS FOR LOAD OF user-print.
DISABLE TRIGGERS FOR LOAD OF usercomp.
DISABLE TRIGGERS FOR LOAD OF usercust.
DISABLE TRIGGERS FOR LOAD OF userEula.
DISABLE TRIGGERS FOR LOAD OF userlog.
DISABLE TRIGGERS FOR LOAD OF usersman.
DISABLE TRIGGERS FOR LOAD OF uservend.
DISABLE TRIGGERS FOR LOAD OF usr-grp.
DISABLE TRIGGERS FOR LOAD OF usr-menu.
DISABLE TRIGGERS FOR LOAD OF usrx.
DISABLE TRIGGERS FOR LOAD OF reftable.

    RUN ipStatus ("  Removing Old User Records").

    /* Clean up remnant records for any deleted users */
&SCOPED-DEF cFileName usr
&SCOPED-DEF cFieldName uid
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName user-print
&SCOPED-DEF cFieldName user-id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usercomp
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usercust
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName userEula
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName userLog
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usersman
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName userVend
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usr-grp
&SCOPED-DEF cFieldName uid
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usr-menu
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usrx
&SCOPED-DEF cFieldName uid
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

DISABLE TRIGGERS FOR LOAD OF reftable.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.user-docs":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.phone-no":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.fax-no":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.phone-cnty":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.fax-cnty":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCleanTemplates C-Win
PROCEDURE ipCleanTemplates:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
         
    ------------------------------------------------------------------------------*/
    DEF VAR cTgtEnv AS CHAR NO-UNDO.
    DEF VAR icShortName AS CHAR NO-UNDO.
    DEF VAR icLongName AS CHAR NO-UNDO.
    DEF VAR icType AS CHAR NO-UNDO.
    DEF VAR daFileDate AS DATE NO-UNDO.
    DEF VAR cTestFileName AS CHAR NO-UNDO.
    DEF VAR cResDir AS CHAR NO-UNDO.
    
    RUN ipStatus ("  Cleaning /Template directories.").

    ASSIGN 
        cResDir = cEnvDir + "\" + fiEnvironment:{&SV} + "\Resources\Template"
        cTgtEnv = cEnvDir + "\" + fiEnvironment:{&SV}.
    
    /* Build list of files in <root>\Template directory */
    ASSIGN  
        FILE-INFO:FILE-NAME = cTgtEnv + "\Template".
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN RETURN.
    ELSE DO:
        ASSIGN 
            cTgtEnv = FILE-INFO:FULL-PATHNAME.
        INPUT FROM OS-DIR (cTgtEnv).
        REPEAT:
            IMPORT 
                icShortName
                icLongName
                icType.
            ASSIGN 
                FILE-INFO:FILE-NAME = icLongName.
            CREATE ttTemplateFiles.
            ASSIGN 
                ttTemplateFiles.cFileName = icShortName
                ttTemplateFiles.cLongName = icLongName
                ttTemplateFiles.daModDate = FILE-INFO:FILE-MOD-DATE.
        END.
    END.
    
    /* Build list of files in /Resources/Template */
    ASSIGN  
        FILE-INFO:FILE-NAME = cResDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN RETURN.
    ELSE 
    DO:
        ASSIGN 
            cResDir = FILE-INFO:FULL-PATHNAME.
        INPUT FROM OS-DIR (cResDir).
        REPEAT:
            IMPORT 
                icShortName
                icLongName
                icType.
            ASSIGN 
                FILE-INFO:FILE-NAME = icLongName.
            CREATE ttResTemplateFiles.
            ASSIGN 
                ttResTemplateFiles.cFileName = icShortName
                ttResTemplateFiles.cLongName = icLongName
                ttResTemplateFiles.daModDate = FILE-INFO:FILE-MOD-DATE.
        END.
    END.
    
    /* Now compare the lists and remove the files in <root>\Template that have a match in /Resources */
    FOR EACH ttTemplateFiles:
        FIND FIRST ttResTemplateFiles NO-LOCK WHERE 
            ttResTemplateFiles.cFileName EQ ttTemplateFiles.cFileName
            NO-ERROR.
        IF AVAIL ttResTemplateFiles 
        AND ttResTemplateFiles.daModDate GE ttTemplateFiles.daModDate THEN DO:
            OS-DELETE VALUE(ttTemplateFiles.cLongName).
            DELETE ttTemplateFiles.
        END.
    END.
    
    /* Anything left are custom to this customer, and need to go to CustFiles */
    OS-CREATE-DIR VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\Template").
    
    /* Note: try to clean up any NK1s that might reference the file while we're here */
    FOR EACH ttTemplateFiles:
        FIND FIRST sys-ctrl EXCLUSIVE WHERE 
            sys-ctrl.char-fld EQ ttTemplateFiles.cLongName
            NO-ERROR.
        IF AVAIL sys-ctrl THEN DO:
            ASSIGN 
                sys-ctrl.char-fld = ".\CustFiles\Template\" + ttTemplateFiles.cFileName.
        END.
                     
        OS-COPY VALUE(ttTemplateFiles.cLongName) VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\Template"). 
        OS-DELETE VALUE(ttTemplateFiles.cLongName).
        DELETE ttTemplateFiles.
     END.
     
     /* Finally, remove the directory */
     OS-DELETE VALUE(cTgtEnv) RECURSIVE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConfirmAdminUser C-Win 
PROCEDURE ipConfirmAdminUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Validating ADMIN user entries").

    DISABLE TRIGGERS FOR LOAD OF users.

    FIND users EXCLUSIVE WHERE 
        users.user_id = "admin"
        NO-ERROR.
    ASSIGN
        users.developer = false
        users.isActive = true
        users.isLocked = false
        users.securityLevel = IF users.securityLevel LT 900 THEN 900 ELSE users.securityLevel
        users.updateDate = today
        users.updateTime = time
        users.updateUser = USERID(LDBNAME(1))
        users.userType = "Administrator"
        users.user_language = IF users.user_language = "" THEN "English" ELSE users.user_language
        users.user_name = IF users.user_name = "" THEN "Local System Admin" ELSE users.user_name
        users.use_colors = false
        users.use_ctrl_keys = false
        users.use_fonts = false
        users.widget_bgc = 0
        users.widget_fgc = 0
        users.widget_font = 0
        . 
    
    RUN ipSetAdminPwd IN THIS-PROCEDURE.
    RUN ipAddSuppUserRecords IN THIS-PROCEDURE (INPUT users.user_id).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConfirmASIUser C-Win 
PROCEDURE ipConfirmASIUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Validating ASI User Entries").

    DISABLE TRIGGERS FOR LOAD OF users.

    FIND users EXCLUSIVE WHERE 
        users.user_id = "asi"
        NO-ERROR.
    ASSIGN
        users.developer = false
        users.fax = ""
        users.fax-cnty = ""
        users.image_filename = "asiHelp@advantzware.com"
        users.isActive = true
        users.isLocked = false
        users.phone = "2153697800"
        users.phone-cnty = "1"
        users.securityLevel = 1000
        users.showOnAck = false
        users.showOnBol = false
        users.showOnInv = false
        users.showOnPO = false
        users.showOnQuote = false
        users.track_usage = true
        users.updateDate = today
        users.updateTime = time
        users.updateUser = "asi"
        users.userType = "Administrator"
        users.user_language = "English"
        users.user_name = "Advantzware System Admin"
        users.user_program[1] = ""
        users.user_program[2] = ""
        users.user_program[3] = ""
        users.use_colors = false
        users.use_ctrl_keys = false
        users.use_fonts = false
        users.widget_bgc = 0
        users.widget_fgc = 0
        users.widget_font = 0
        . 
    
    RUN ipSetAsiPwd IN THIS-PROCEDURE.
    RUN ipAddSuppUserRecords IN THIS-PROCEDURE (INPUT users.user_id).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertModule C-Win 
PROCEDURE ipConvertModule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFrom AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTo AS CHAR NO-UNDO.
    
    FIND FIRST module NO-LOCK WHERE 
        module.module = ipcfrom 
        NO-ERROR.
    IF NOT AVAIL module THEN RETURN.
  
    RUN ipStatus ("    Converting module " + ipcFrom + " to " + ipcTo).

    FIND FIRST bf-module EXCLUSIVE WHERE 
        ROWID(bf-module) = ROWID(module) NO-ERROR.
    IF AVAIL bf-module THEN ASSIGN
        bf-module.module = ipcTo.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertPrepItems C-Win
PROCEDURE ipConvertPrepItems:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Converting advantzware.usr file...").

    DISABLE TRIGGERS FOR LOAD OF prep.
    
    FOR EACH oe-ctrl NO-LOCK:
        FOR EACH prep EXCLUSIVE WHERE 
            prep.company EQ oe-ctrl.company:
            ASSIGN 
                prep.taxable = oe-ctrl.prep-chrg
                prep.commissionable = oe-ctrl.prep-comm.
        END.
    END.
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertUsrFile C-Win 
PROCEDURE ipConvertUsrFile :
/*---------------------------------------------------------------------------*/
/*  File:           admin\envadmin\convusr.p                                 */
/*  Copyright:      (c)2018 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Convert old-style advantzware.usr file to new style      */
/*                                                                           */
/*  Included files:     none                                                 */
/*                                                                           */
/*  External RUN/CALL:  none                                                 */
/*                                                                           */
/*  External files:     READ admin/advantzware.usr                           */
/*                      COPY admin/advantzware.usr.old                       */
/*                      WRITE admin/advantzware.usr                          */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      06/14/18    MYT     xxxxx   Original Version         */
/*                      06/15/18    MYT             Moved to upgrade process */
/*---------------------------------------------------------------------------*/

    RUN ipStatus ("    Converting advantzware.usr file...").

DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR cOutline AS CHAR NO-UNDO.
DEF VAR cuserlist AS CHAR NO-UNDO.
DEF VAR cenvlist AS CHAR NO-UNDO.
DEF VAR cdblist AS CHAR NO-UNDO.
DEF VAR calias AS CHAR NO-UNDO.
DEF VAR cenv AS CHAR NO-UNDO.
DEF VAR cdb AS CHAR NO-UNDO.
DEF VAR cmode AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO. 
DEF VAR j AS INT NO-UNDO. 
DEF VAR k AS INT NO-UNDO. 
DEF VAR ctr AS INT NO-UNDO.
DEF VAR lContinue AS LOG INITIAL TRUE NO-UNDO.

INPUT FROM VALUE(cAdminDir + "\advantzware.usr").
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF ENTRY(2,cLine,"|") EQ "*" THEN DO:
        ASSIGN
            lContinue = FALSE.
        RETURN.
    END.
    
    CREATE ttUsers.
    ASSIGN
        ttfUserid = ENTRY(3,cLine,"|")
        ttfdbname = ENTRY(1,cLine,"|")
        ttfalias = ENTRY(2,cLine,"|")
        ttfenvlist = ENTRY(4,cLine,"|")
        ttfdblist = ENTRY(5,cLine,"|")
        ttfmodelist = ENTRY(6,cLine,"|").
END.
INPUT CLOSE.

IF lContinue EQ FALSE THEN RETURN.

OS-COPY VALUE(cAdminDir + "\advantzware.usr") VALUE(cAdminDir + "\advantzware.usr.old").

FOR EACH ttUsers:
    IF INDEX(cuserlist,ttfuserid) EQ 0 THEN ASSIGN
        cuserlist = cuserlist + ttfuserid + ",".
    DO i = 1 TO NUM-ENTRIES(ttfenvlist):
        IF INDEX(cenvlist,ENTRY(i,ttfenvlist)) EQ 0 THEN ASSIGN
            cenvlist = cenvlist + ENTRY(i,ttfenvlist) + ",".
    END.
    IF INDEX(cdblist,ttfDbName) EQ 0 THEN ASSIGN
        cdblist = cdblist + ttfDbName + ",".
END.

ASSIGN
    cuserlist = TRIM(cuserlist,",")
    cdblist = TRIM(cdblist,",")
    cenvlist = TRIM(cenvlist,",").

OUTPUT TO VALUE(cAdminDir + "\advantzware.usr").
DO i = 1 TO NUM-ENTRIES(cuserlist):
    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfalias NE ""
        NO-ERROR.
    ASSIGN
        calias = IF AVAIL ttusers THEN ttfalias ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfenvlist NE ""
        NO-ERROR.
    ASSIGN
        cenv = IF AVAIL ttusers THEN ttfenvlist ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfdblist NE ""
        NO-ERROR.
    ASSIGN
        cdb = IF AVAIL ttusers THEN ttfdblist ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfmodelist NE ""
        NO-ERROR.
    ASSIGN
        cmode = IF AVAIL ttusers THEN ttfmodelist ELSE "".

    ASSIGN
        cOutline = ENTRY(i,cuserlist) + "|*|" +
                   calias + "|" +
                   cenv + "|" +
                   cdb + "|".
    PUT UNFORMATTED coutline + CHR(10).
END.
    
DO j = 1 TO NUM-ENTRIES(cdblist):
    ASSIGN
        cdb = ENTRY(j,cdblist).
    DO i = 1 TO NUM-ENTRIES(cuserlist):
        FIND FIRST ttusers WHERE
            ttfuserid = ENTRY(i,cuserlist) AND
            ttfmodelist NE ""
            NO-ERROR.
        ASSIGN
            cmode = IF AVAIL ttusers THEN ttfmodelist ELSE ""
            cOutline = ENTRY(i,cuserlist) + "|" +
                       cdb + "|" +
                       "" + "|" +
                       "" + "|" +
                       "" + "|" +
                       cmode.
        PUT unformatted coutline + CHR(10).
    END.
END.
OUTPUT CLOSE.  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertVendorCosts C-Win
PROCEDURE ipConvertVendorCosts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("    Converting vendor cost records").

    DEFINE VARIABLE iVendCostItemID AS INT64 NO-UNDO.

    FOR EACH e-item-vend NO-LOCK, 
        FIRST e-item NO-LOCK 
        WHERE e-item.company EQ e-item-vend.company
        AND e-item.i-no EQ e-item-vend.i-no:
        RUN pCreateVendItemCostFromEItemVend(BUFFER e-item-vend, BUFFER e-item, OUTPUT iVendCostItemID).
    END. /*each e-item-vend*/
    
    FOR EACH e-itemfg NO-LOCK, 
        EACH e-itemfg-vend NO-LOCK 
        WHERE e-itemfg-vend.company EQ e-itemfg.company
        AND e-itemfg-vend.i-no EQ e-itemfg.i-no:
        RUN pCreateVendItemCostFromEItemFgVend(BUFFER e-itemfg-vend, BUFFER e-itemfg, OUTPUT iVendCostItemID).
    END. /*each e-item-vend*/

    RUN TempTableToCSV(TEMP-TABLE ttDuplicates:HANDLE, "C:\tmp\DuplicateVendCosts.csv", TRUE /* Export Header */).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvQtyPerSet C-Win 
PROCEDURE ipConvQtyPerSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Converting QtyPerSet records...").

    DEF VAR cOrigPropath AS CHAR NO-UNDO.
    DEF VAR cNewPropath AS CHAR NO-UNDO.
    DEF VAR cThisElement AS CHAR NO-UNDO.
    DEF VAR dQtyPerSet AS DECIMAL NO-UNDO.
    DEF VAR iCount AS INTEGER NO-UNDO.
    DEF VAR iCountProcessed AS INTEGER NO-UNDO.
    DEF VAR iCountInitialized AS INTEGER NO-UNDO.
    DEF VAR iCountSets AS INTEGER NO-UNDO.
    DEF VAR iCountFGSets AS INTEGER NO-UNDO.
    DEF VAR iCountFGSetsProcessed AS INTEGER NO-UNDO.
    DEF VAR iCountFGSetsInitialized AS INTEGER NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF eb.
    DISABLE TRIGGERS FOR LOAD OF fg-set.
    
    FOR EACH company NO-LOCK, 
        EACH est NO-LOCK WHERE 
        est.company EQ company.company,
        EACH eb OF est EXCLUSIVE-LOCK:
        ASSIGN
            iCount = iCount + 1.
        CASE est.est-type:
            WHEN 5 OR WHEN 6 THEN DO:
                IF eb.quantityPerSet EQ 0 THEN DO: 
                    ASSIGN 
                        iCountProcessed = iCountProcessed + 1 
                        dQtyPerSet = eb.yld-qty
                        .
                    IF dQtyPerSet LT 0 THEN dQtyPerSet = -1 / dQtyPerSet.
                    IF dQtyPerSet EQ 0 THEN dQtyPerSet = 1.
                    eb.quantityPerSet = dQtyPerSet.   
                END.
            END.
  
            /*Folding carton uses %-cust - out of scope for ticket 25146*/     
/*          WHEN 1 OR WHEN 2 THEN      */
/*                dQtyPerSet = eb.cust-%.*/
/*                                       */
        END CASE.
        IF eb.quantityPerSet EQ 0 THEN ASSIGN 
            eb.quantityPerSet = 1
            iCountInitialized = iCountInitialized + 1.
    END.

    FOR EACH company NO-LOCK, 
        EACH fg-set EXCLUSIVE-LOCK WHERE 
            fg-set.company EQ company.company:
        ASSIGN
            iCountFGSets = iCountFGSets + 1.
        IF fg-set.qtyPerSet EQ 0 AND fg-set.part-qty NE 0 THEN ASSIGN 
            iCountFGSetsProcessed = iCountFGSetsProcessed + 1 
            fg-set.qtyPerSet = fg-set.part-qty.
        IF fg-set.qtyPerSet EQ 0 THEN ASSIGN 
            iCountFGSetsInitialized = iCountFGSetsInitialized + 1 
            fg-set.qtyPerSet = 1.
    END.     

    RUN ipStatus ("       Total Estimates: "  + STRING(iCount)).
    RUN ipStatus ("       Converted from .yld-qty: " + STRING(iCountProcessed)).
    RUN ipStatus ("       Initialized to 1: " + STRING(iCountInitialized)).
    RUN ipStatus ("       Total Sets: " + STRING(iCountFGSets)).
    RUN ipStatus ("       Sets Converted from .part-qty to .qtyPerSet: " + STRING(iCountFGSetsProcessed)).
    RUN ipStatus ("       Sets Initialized to 1: " + STRING(iCountFGSetsInitialized )).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyDirs C-Win 
PROCEDURE ipCopyDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTgtDir AS CHAR NO-UNDO.
    DEF VAR cFileStream AS CHAR NO-UNDO.

    RUN ipStatus ("Copying " + ipcDir + " to " + ipcTgtDir).

    INPUT FROM OS-DIR (ipcDir).

    REPEAT:
        IMPORT cFileStream.
        FILE-INFO:FILE-NAME = ipcDir + "\" + cFileStream.
        IF SUBSTRING(FILE-INFO:FILE-NAME,LENGTH(FILE-INFO:FILE-NAME),1) EQ "." THEN DO:
            NEXT.
        END.
        ELSE IF FILE-INFO:FILE-TYPE BEGINS "F" THEN DO:
            OS-COPY VALUE(FILE-INFO:FILE-NAME) VALUE(ipcTgtDir).
        END.
        ELSE DO:
            OS-CREATE-DIR VALUE(ipcTgtDir + "\" + cFileStream).
            RUN ipCopyDirs IN THIS-PROCEDURE (FILE-INFO:FILE-NAME,ipcTgtDir + "\" + cFileStream).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyRelNotes C-Win 
PROCEDURE ipCopyRelNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Copying Release Notes").

    DEF VAR cCommandLine AS CHAR NO-UNDO.
    
    ASSIGN 
        lSuccess = FALSE.
    
    FILE-INFO:FILE-NAME = cDocDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        OS-CREATE-DIR VALUE(cDocDir).
    OS-CREATE-DIR VALUE(cDocDir + "\ReleaseNotes").

    ASSIGN
        cCommandLine = "COPY " + cUpdRelNotesDir + "\*.* " + cDocDir + "\ReleaseNotes".
    OS-COMMAND SILENT VALUE(cCommandLine).
    
    ASSIGN 
        lSuccess = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyStartup C-Win 
PROCEDURE ipCopyStartup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lAdminExists AS LOG NO-UNDO.
    DEF VAR lDesktopExists AS LOG NO-UNDO.
    
    RUN ipStatus ("Installing Startup Files").

    ASSIGN
        FILE-INFO:FILE-NAME = cDeskDir
        lDesktopExists = FILE-INFO:FULL-PATHNAME NE ?
        FILE-INFO:FILE-NAME = cAdminDir
        lAdminExists = FILE-INFO:FULL-PATHNAME NE ?.
    
    IF NOT lDesktopExists 
    OR SEARCH(cDeskDir + "\advantzware.ico") = ? THEN
        OS-COPY VALUE(cUpdDeskDir) VALUE(cDeskDir).

    OS-COMMAND SILENT VALUE("COPY " + cUpdAdminDir + " " + cAdminDir).
    IF SEARCH(cAdminDir + "\advantzware.usr") = ? THEN
        OS-COMMAND SILENT VALUE("COPY " + cUpdDataDir + "\advantzware.usr " + cAdminDir). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateAdminUser C-Win 
PROCEDURE ipCreateAdminUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating ADMIN User").

    DISABLE TRIGGERS FOR LOAD OF users.
    
    CREATE users.
    ASSIGN
        users.user_id = "ADMIN"
        users.createDate = today
        users.createTime = time
        users.createUser = USERID(LDBNAME(1)).
        
    RUN ipConfirmAdminUser IN THIS-PROCEDURE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateASIUser C-Win 
PROCEDURE ipCreateASIUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating ASI User").

    DISABLE TRIGGERS FOR LOAD OF users.

    CREATE users.
    ASSIGN
        users.user_id = "asi"
        users.createDate = today
        users.createTime = time
        users.createUser = USERID(LDBNAME(1)).
        
    RUN ipConfirmAsiUser IN THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix C-Win 
PROCEDURE ipDataFix :
/*------------------------------------------------------------------------------
  Purpose:     Master Procedure for Data Fix Processes
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.

    RUN ipStatus ("Starting Data Fixes - from version " + fiFromVer:{&SV}).

    ASSIGN 
        cThisEntry = fiFromVer:{&SV}
        lSuccess = FALSE.

    IF fIntVer(cThisEntry) LT 16001000 THEN
        RUN ipDataFix160001.
    IF fIntVer(cThisEntry) LT 16014000 THEN
        RUN ipDataFix160104.
    IF fIntVer(cThisEntry) LT 16020000 THEN
        RUN ipDataFix160200.
    IF fIntVer(cThisEntry) LT 16060000 THEN
        RUN ipDataFixConfig.
    IF fIntVer(cThisEntry) LT 16069000 THEN
        RUN ipDataFix160609.
    IF fIntVer(cThisEntry) LT 16070000 THEN 
        RUN ipDataFix160700.
    IF fIntVer(cThisEntry) LT 16074000 THEN
        RUN ipDataFix160704.
    IF fIntVer(cThisEntry) LT 16078000 THEN
        RUN ipDataFix160708.
    IF fIntVer(cThisEntry) LT 16071200 THEN
        RUN ipDataFix160712.
    IF fIntVer(cThisEntry) LT 16080000 THEN
        RUN ipDataFix160800.
    IF fIntVer(cThisEntry) LT 16084000 THEN
        RUN ipDataFix160840.
    IF fIntVer(cThisEntry) LT 16085000 THEN
        RUN ipDataFix160850.
    IF fIntVer(cThisEntry) LT 16085100 THEN
        RUN ipDataFix160851.
    IF fIntVer(cThisEntry) LT 16086000 THEN
        RUN ipDataFix160860.
    IF fIntVer(cThisEntry) LT 16088000 THEN 
        RUN ipDataFix160880.
    IF fIntVer(cThisEntry) LT 16089000 THEN 
        RUN ipDataFix160890.
    IF fIntVer(cThisEntry) LT 16100000 THEN
        RUN ipDataFix161000.
    IF fIntVer(cThisEntry) LT 16120000 THEN 
        RUN ipDataFix161200.
    IF fIntVer(cThisEntry) LT 16130000 THEN 
        RUN ipDataFix161300.
    IF fIntVer(cThisEntry) LT 99999999 THEN
        RUN ipDataFix999999.

RUN ipStatus ("Completed Data Fixes").
    
    ASSIGN 
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160001 C-Win 
PROCEDURE ipDataFix160001 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-reftable FOR reftable.
    DEF VAR cnt AS INT NO-UNDO.
    DISABLE TRIGGERS FOR LOAD OF bf-reftable.

    RUN ipStatus ("  Data Fix 160001...").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160104 C-Win 
PROCEDURE ipDataFix160104 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cPrimComp AS CHAR NO-UNDO.
    DEF VAR iNextNum AS INT NO-UNDO.
    DEF VAR li-nxt-r-no AS INT NO-UNDO.
    DEF BUFFER bf-rctd FOR rm-rctd.
 
    RUN ipStatus ("  Data Fix 160104...").

    FOR EACH company NO-LOCK:
        FIND LAST po-ord NO-LOCK WHERE 
            po-ord.company EQ company.company 
            NO-ERROR.
        IF AVAIL po-ord THEN
            DYNAMIC-CURRENT-VALUE("po_Seq" + company.spare-char-1, "ASI") = po-ord.po-no + 1.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160200 C-Win 
PROCEDURE ipDataFix160200 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF vend.
    
    RUN ipStatus ("  Data Fix 160200...").

    FOR EACH company NO-LOCK:
        /*Set Payment Type Defaults*/
        RUN ipCheckPayMaster (company.company,"Check","Paper Check",YES).
        RUN ipCheckPayMaster (company.company,"Bill Pay","Online Bill Pay",NO).
        RUN ipCheckPayMaster (company.company,"Credit Card","Credit Card",NO).
        RUN ipCheckPayMaster (company.company,"ACH","ACH Electronic Transfer",NO).
        
        /*Convert spare-int values to payment-type*/
        FOR EACH vend EXCLUSIVE WHERE 
            vend.company EQ company.company:
            IF vend.spare-int-1 = 1 THEN ASSIGN 
                vend.payment-type = "ACH".
            ELSE IF vend.spare-int-2 = 1 THEN ASSIGN 
                vend.payment-type = "Bill Pay".
            ELSE ASSIGN 
                vend.payment-type = "Check".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160600 C-Win 
PROCEDURE ipDataFix160600 :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160600...").

    RUN ipDelDupeNotes.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160609 C-Win 
PROCEDURE ipDataFix160609 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160609...").

    RUN ipInvRnoSeq.
    RUN ipRelRnoSeq.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160700 C-Win 
PROCEDURE ipDataFix160700 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160700...").

    /* 24948 - Must ALWAYS be set to "BOL" (true) */
    DISABLE TRIGGERS FOR LOAD OF oe-ctrl.
    FOR EACH oe-ctrl:
        ASSIGN
            oe-ctrl.u-inv = TRUE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160704 C-Win 
PROCEDURE ipDataFix160704 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF job-code.
    DISABLE TRIGGERS FOR LOAD OF oe-rel.
       
    RUN ipStatus ("  Data Fix 160704...").

    /* Ensure jobCode sequence is GT 100 */
    IF CURRENT-VALUE(jobCodeDMIseq) LT 100 THEN ASSIGN
        CURRENT-VALUE(jobCodeDMIseq) = CURRENT-VALUE(jobCodeDMIseq) + 100.
        
    /* If any empty job-codes, assign dmiID */
    FOR EACH job-code WHERE 
        job-code.dmiID EQ 0:
        ASSIGN
            job-code.dmiID = NEXT-VALUE(jobCodeDMIseq).
    END. 
    
    /* If job-codes had been built with ID LT 100, fix them */
    IF CAN-FIND(FIRST job-code WHERE job-code.dmiID LT 100) THEN FOR EACH job-code EXCLUSIVE:
        ASSIGN
            job-code.dmiID = job-code.dmiID + 100.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160708 C-Win 
PROCEDURE ipDataFix160708 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN ipStatus ("  Data Fix 160708...").

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160712 C-Win 
PROCEDURE ipDataFix160712 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF oe-rel.

    RUN ipStatus ("  Data Fix 160712...").

    RUN ipConvertUsrFile.
    RUN ipTurnOffUserColors.
    RUN ipFixPoEdiDirs.
    
    /* Ticket 32053 */
    FOR EACH reftable1 WHERE reftable1.reftable = "oe-rel.lot-no"
        NO-LOCK:
        DO TRANSACTION:    
            FIND FIRST oe-rel WHERE oe-rel.r-no = int(reftable1.company)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN    
            DO: 
                IF oe-rel.lot-no EQ "" THEN ASSIGN oe-rel.lot-no = reftable1.code.
                IF oe-rel.frt-pay EQ "" THEN ASSIGN oe-rel.frt-pay = reftable1.code2.
                IF oe-rel.fob-code EQ "" THEN ASSIGN oe-rel.fob-code = reftable1.dscr.
            END.   
            FIND CURRENT oe-rel NO-LOCK NO-ERROR.    
            RELEASE oe-rel. 
        END.  
    END.             
    
    RUN ipConvQtyPerSet.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160800 C-Win 
PROCEDURE ipDataFix160800 :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160800...").

    RUN ipRemoveUserAddon.
    RUN ipMoveUserMenusToDatabase.
    RUN ipAddLocationData.
    RUN ipVendorMaxValue.
    RUN ipSetImageFiles.
    RUN ipTrackUsage.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160840 C-Win 
PROCEDURE ipDataFix160840 :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160840...").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160850 C-Win 
PROCEDURE ipDataFix160850 :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160850...").

    RUN ipRemoveUserMenu.
    RUN ipFixUserPrint.
    RUN ipAddDbmsFonts.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160851 C-Win 
PROCEDURE ipDataFix160851 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160851...").

    RUN ipFixBadYears.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160860 C-Win 
PROCEDURE ipDataFix160860 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160860...").

        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160880 C-Win 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160880 C-Win 
PROCEDURE ipDataFix160880 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160880...").
    
/*    Deprecated                */
/*    RUN ipUpdateAdvantzwarePf.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE ipDataFix160890 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160890...").
    
    RUN ipConvertPrepItems.  
    RUN ipFixFrtPay.
    RUN ipFgcatStatusActive.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160899 C-Win 
PROCEDURE ipDataFix161000 :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161000...").

    RUN ipAddJobMchSeq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix161200 C-Win
PROCEDURE ipDataFix161200:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161200...").

    RUN ipLoadAPIData.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix1613010 C-Win
PROCEDURE ipDataFix161300:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161300...").
    
    RUN ipConvertVendorCosts.
    RUN ipLoadOEAutoApproveNK1s.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix999999 C-Win 
PROCEDURE ipDataFix999999 :
    /*------------------------------------------------------------------------------
     Purpose:   These procedures should run on every update
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 999999...").

    RUN ipUseOldNK1.
    RUN ipAuditSysCtrl.
    RUN ipLoadJasperData.
    RUN ipSetCueCards.
    RUN ipDeleteAudit.
    RUN ipCleanTemplates.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix161100 C-Win 
PROCEDURE ipDataFix161100 :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161100...").

    RUN ipFixBlankOrdlShipIDs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFixConfig C-Win 
PROCEDURE ipDataFixConfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH config EXCLUSIVE:
        ASSIGN
            config.audit_dir = ".\CustFiles\Logs\AuditFiles"
            config.logs_dir = ".\CustFiles\Logs"
            config.spool_dir = ".\CustFiles\Logs\Spool".
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelBadData C-Win 
PROCEDURE ipDelBadData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Removing Deprecated File Entries").

    ASSIGN 
        lSuccess = FALSE.

    DISABLE TRIGGERS FOR LOAD OF parmfile.
    DISABLE TRIGGERS FOR LOAD OF userlog.
    DISABLE TRIGGERS FOR LOAD OF mfdata.
    
    /* Remove parmfile database connections */
    FOR EACH parmfile:
        DELETE parmfile.
    END.

    /* Remove bad UDF entries */
    FOR EACH mfdata:
        IF INDEX(mfdata.mfgroup_data,"General Class") <> 0 THEN
            DELETE mfdata.
    END.

    /* Remove userlogs for proper user control counting */
    IF lNeedUsercontrol THEN FOR EACH userlog:
        DELETE userlog.
    END.

    ASSIGN 
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelDupeNotes C-Win 
PROCEDURE ipDelDupeNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Don't need to do this for modern installs */
    IF cCurrVer GE "16.6" THEN DO:
        RUN ipStatus ("Version doesn't require notes de-dupe").
        RETURN.
    END.
        
    DISABLE TRIGGERS FOR LOAD OF notes.
    
    RUN ipStatus ("Deleting duplicate notes").

    ASSIGN
        delCtr = 0
        dupCtr = 0.
        
    FOR EACH notes NO-LOCK:
        ASSIGN jCtr = jCtr + 1.
    END.
    
    FOR EACH notes NO-LOCK:
        ASSIGN
            iCtr = iCtr + 1.
    
        IF ictr MODULO 1000 EQ 0 THEN 
            RUN ipStatus ("Deleting duplicate notes - Reviewing " + STRING(iCtr,">>>>>>>9") + " of " + STRING(jCtr,">>>>>>>9") + " records.").
    
        /* Remove exact duplicates */
        FIND bnotes WHERE
            ROWID(bnotes) <> ROWID(notes) AND
            bnotes.note_code = notes.note_code AND
            bnotes.note_date = notes.note_date AND
            bnotes.note_form_no = notes.note_form_no AND
            bnotes.note_group = notes.note_group AND
            bnotes.note_text = notes.note_text AND
            bnotes.note_time = notes.note_time AND
            bnotes.note_title = notes.note_title AND
            bnotes.note_type = notes.note_type AND
            bnotes.rec_key = notes.rec_key AND
            bnotes.user_id = notes.user_id
            EXCLUSIVE NO-ERROR.
        IF AVAIL bnotes THEN DO:
            ASSIGN 
                delCtr = delCtr + 1.
            DELETE bnotes.
        END.
        
        /* Write possible duplicates to file */
        FIND bnotes WHERE
            ROWID(bnotes) <> ROWID(notes) AND
            bnotes.note_code = notes.note_code AND
            bnotes.note_date = notes.note_date AND
            bnotes.note_form_no = notes.note_form_no AND
            bnotes.note_group = notes.note_group AND
            bnotes.note_time = notes.note_time AND
            bnotes.note_title = notes.note_title AND
            bnotes.note_type = notes.note_type AND
            bnotes.rec_key = notes.rec_key AND
            bnotes.user_id = notes.user_id
            EXCLUSIVE NO-ERROR.
        IF AVAIL bnotes THEN DO:
            ASSIGN 
                dupCtr = dupCtr + 1.
            IF dupCtr = 1 THEN DO:
                ASSIGN
                    FILE-INFO:FILE-NAME = "c:\tmp\."
                    cTemp = IF FILE-INFO:FULL-PATHNAME <> ? THEN "c:\tmp" ELSE "".
                IF cTemp = "" THEN DO:
                    ASSIGN
                        FILE-INFO:FILE-NAME = "c:\temp\."
                        cTemp = IF FILE-INFO:FULL-PATHNAME <> ? THEN "c:\temp" ELSE "".
                END.
                IF cTemp = "" THEN ASSIGN
                    cTemp = OS-GETENV("TEMP").
                OUTPUT STREAM outstream TO VALUE(cTemp + "\dupNotes.txt").
                PUT STREAM outstream UNFORMATTED
                    "List of possible duplicate notes detected on " + STRING(TODAY,"99/99/99") + CHR(10) + 
                    "-----------------------------------------------------------------------------------------" + CHR(10) + 
                    "Note Code Group     Type      Form      Date      Time      UserID       Text Begins     " + CHR(10) +
                    "-----------------------------------------------------------------------------------------".
            END.
            PUT STREAM outstream   
                bnotes.note_code                    AT 1
                bnotes.note_group                   AT 11
                bnotes.note_type                    AT 21
                STRING(bnotes.note_form_no)         AT 31
                STRING(bnotes.note_date,"99/99/99") AT 41
                STRING(bnotes.note_time,"HH:MM:SS") AT 51
                bnotes.USER_id                      AT 61
                bnotes.note_text FORMAT "x(30)"     AT 74.
        END.
    END.
    
    IF dupCtr > 0 
    OR delCtr > 0 THEN DO:
        OUTPUT STREAM outstream CLOSE.
    END.

    /* Deprecated - do not use
    /* Add additional field data - from WK populateNotesFields */
    FOR EACH notes EXCLUSIVE WHERE
        notes.createDate = ?:
        ASSIGN 
            notes.updateDate   = notes.note_date  
            notes.updateTime   = notes.note_time 
            notes.updateUser   = notes.user_id
            notes.createDate   = notes.note_date  
            notes.createTime   = notes.note_time 
            notes.createUser   = notes.user_id         
            .
    END.
    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDeleteAudit C-Win 
PROCEDURE ipDeleteAudit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR lAuditLicensed AS LOG NO-UNDO INITIAL TRUE.
    DEF VAR iElapsed AS INT NO-UNDO.
    DEF VAR iDelCount AS INT NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF auditHdr.
    DISABLE TRIGGERS FOR LOAD OF auditDtl.
    DISABLE TRIGGERS FOR LOAD OF auditStack.
    DISABLE TRIGGERS FOR LOAD OF auditTbl.
    
    FIND FIRST module NO-LOCK WHERE 
        module.module EQ "Audit." OR
        module.module EQ "Audit"
        NO-ERROR.
    IF NOT AVAIL module
    OR module.is-used EQ FALSE THEN ASSIGN 
        lAuditLicensed = FALSE.
        
    ASSIGN
        iElapsed = etime(TRUE).
        
    IF NOT lAuditLicensed THEN DO:
        RUN ipStatus ("    Deleting audit records (unlicensed)...").
        RUN ipStatus ("      (30 minute limit on this process)").
        RUN ipStatus ("      Deleting audit headers and details...").
        FOR EACH AuditHdr TABLE-SCAN:
            FOR EACH AuditDtl OF auditHdr:
                DELETE AuditDtl.
                ASSIGN
                    iDelCount = iDelCount + 1.
            END.
            DELETE AuditHdr.
            ASSIGN
                iDelCount = iDelCount + 1.
            IF etime GT 1800000 THEN 
                LEAVE.
        END.
        RUN ipStatus ("      Deleting audit stack...").
        FOR EACH AuditStack TABLE-SCAN:
            DELETE AuditStack.
            ASSIGN
                iDelCount = iDelCount + 1.
            IF etime GT 1800000 THEN 
                LEAVE.
        END.
        FOR EACH AuditTbl:
            ASSIGN
                AuditTbl.AuditCreate = NO
                AuditTbl.AuditDelete = NO
                AuditTbl.AuditUpdate = NO
                AuditTbl.AuditStack  = NO.
        END.
    END.
    ELSE DO:
        RUN ipStatus ("    Deleting audit records older than 180 days...").
        RUN ipStatus ("      (30 minute limit on this process)").
        FOR EACH AuditHdr WHERE 
            DATE(auditHdr.auditDateTime) LT TODAY - 180:
            FOR EACH AuditDtl OF auditHdr:
                DELETE AuditDtl.
                ASSIGN
                    iDelCount = iDelCount + 1.
            END.
            DELETE AuditHdr.
            ASSIGN
                iDelCount = iDelCount + 1.
            IF etime GT 1800000 THEN 
                LEAVE.
        END.
    END.
    RUN ipStatus ("      Deleted " + STRING(iDelCount,">,>>>,>>>,>>9") + " audit records in " + STRING(INTEGER(eTime / 1000)) + " seconds.").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpandFiles C-Win 
PROCEDURE ipExpandFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cCmdLine2 AS CHAR NO-UNDO.
    DEF VAR cCmdLine3 AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lTestExists AS LOG NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cNewEntry AS CHAR NO-UNDO.
    DEF VAR cListItems AS CHAR NO-UNDO.
    DEF VAR cScreenValue AS CHAR NO-UNDO.
    
    RUN ipStatus ("Installing New ASI System Files").

    ASSIGN 
        lSuccess = FALSE 
        cThisEntry = fiEnvironment:{&SV}
        cTgtEnv = cEnvDir + "\" + fiEnvironment:{&SV}.

    RUN ipStatus ("  Expanding files...").

    ASSIGN
        cCmdLine1 = cUpdProgramDir + "\7z.exe x " + cUpdProgramDir + "\Override.7z -y -o" + cUpdProgramDir + "\Override"
        cCmdLine2 = cUpdProgramDir + "\7z.exe x " + cUpdProgramDir + "\Programs.7z -y -o" + cUpdProgramDir + "\Programs"
        cCmdLine3 = cUpdProgramDir + "\7z.exe x " + cUpdProgramDir + "\Resources.7z -y -o" + cUpdProgramDir + "\Resources".
    
    IF SEARCH(cUpdProgramDir + "\Override.7z") NE ? THEN DO:
        OS-COMMAND SILENT VALUE(cCmdLine1).
    END.
    OS-COMMAND SILENT VALUE(cCmdLine2).
    OS-COMMAND SILENT VALUE(cCmdLine3).

    /* Skip the copy part, just MOVE the files  */
    RUN ipStatus ("  Moving expanded files from ").
    RUN ipStatus ("    " + cUpdProgramDir + " to").
    RUN ipStatus ("    " + cTgtEnv).
    OS-RENAME VALUE(cUpdProgramDir + "\Override") VALUE(cTgtEnv + "\OverrideN").
    OS-RENAME VALUE(cUpdProgramDir + "\Programs") VALUE(cTgtEnv + "\ProgramsN").
    OS-RENAME VALUE(cUpdProgramDir + "\Resources") VALUE(cTgtEnv + "\ResourcesN").

    /* Rename the old files to "O", then new files from "N", then delete "O" */
    RUN ipStatus ("  Renaming old " + cThisEntry + " directories").
    OS-RENAME VALUE(cTgtEnv + "\Override") VALUE(cTgtEnv + "\OverrideO").
    OS-RENAME VALUE(cTgtEnv + "\Programs") VALUE(cTgtEnv + "\ProgramsO").
    OS-RENAME VALUE(cTgtEnv + "\Resources") VALUE(cTgtEnv + "\ResourcesO").

    RUN ipStatus ("  Moving temp directories in " + cThisEntry + " to permanents").
    OS-RENAME VALUE(cTgtEnv + "\OverrideN") VALUE(cTgtEnv + "\Override").
    OS-RENAME VALUE(cTgtEnv + "\ProgramsN") VALUE(cTgtEnv + "\Programs").
    OS-RENAME VALUE(cTgtEnv + "\ResourcesN") VALUE(cTgtEnv + "\Resources").

    RUN ipStatus ("  Deleting old files from " + cThisEntry + " directories").
    OS-DELETE VALUE(cTgtEnv + "\OverrideO") RECURSIVE.
    OS-DELETE VALUE(cTgtEnv + "\ProgramsO") RECURSIVE.
    OS-DELETE VALUE(cTgtEnv + "\ResourcesO") RECURSIVE.

    RUN ipStatus ("Installation of new system files complete").
    
    ASSIGN 
        lSuccess = TRUE.

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
        cDbDirOnly = cDbDir
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFgcatStatusActive C-Win
PROCEDURE ipFgcatStatusActive:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF fgcat.
    
    RUN ipStatus("   Set fgcat.lActive = TRUE").

    FOR EACH fgcat:
        ASSIGN 
            lActive = TRUE.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixBadYears C-Win 
PROCEDURE ipFixBadYears :
/*------------------------------------------------------------------------------
 Purpose:   ensure year values are in 20th or 21st century
 Notes:     from ticket 41037
------------------------------------------------------------------------------*/
    RUN ipStatus("   Fix year values not in this century (41037)").

    DISABLE TRIGGERS FOR LOAD OF oe-ord.
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    DISABLE TRIGGERS FOR LOAD OF oe-rel.
    DISABLE TRIGGERS FOR LOAD OF oe-relh.
    DISABLE TRIGGERS FOR LOAD OF oe-rell.
    
    FOR EACH oe-ord EXCLUSIVE WHERE (oe-ord.ord-date GT 09/01/2018 
                           OR oe-ord.ord-date LT 12/31/0100):
        /* Note: do in multiple assigns, else function only evaluates once */
        ASSIGN oe-ord.ord-date = fFixYear(oe-ord.ord-date).
        ASSIGN oe-ord.prod-date = fFixYear(oe-ord.prod-date).
        ASSIGN oe-ord.due-date = fFixYear(oe-ord.due-date).
        ASSIGN oe-ord.last-date = fFixYear(oe-ord.last-date).
        ASSIGN oe-ord.inv-date = fFixYear(oe-ord.inv-date).
        ASSIGN oe-ord.upd-date = fFixYear(oe-ord.upd-date).
        ASSIGN oe-ord.approved-date = fFixYear(oe-ord.approved-date).
        ASSIGN oe-ord.entered-date = fFixYear(oe-ord.entered-date).
        ASSIGN oe-ord.updated-date = fFixYear(oe-ord.updated-date).
        ASSIGN oe-ord.closedate = fFixYear(oe-ord.closedate).
        FOR EACH oe-ordl EXCLUSIVE OF oe-ord:
            ASSIGN oe-ordl.req-date = fFixYear(oe-ordl.req-date).
            ASSIGN oe-ordl.prom-date = fFixYear(oe-ordl.prom-date).
            ASSIGN oe-ordl.upd-date = fFixYear(oe-ordl.upd-date).
            ASSIGN oe-ordl.job-start-date = fFixYear(oe-ordl.job-start-date).
        END.
        FOR EACH oe-rel EXCLUSIVE OF oe-ord:
            ASSIGN oe-rel.rel-date = fFixYear(oe-rel.rel-date).
            ASSIGN oe-rel.ship-date = fFixYear(oe-rel.ship-date).
            ASSIGN oe-rel.upd-date = fFixYear(oe-rel.upd-date).
            FOR EACH oe-relh EXCLUSIVE OF oe-rel:
                ASSIGN oe-relh.rel-date = IF NOT oe-relh.rel-date EQ ? THEN fFixYear(oe-relh.rel-date) ELSE ?.
                ASSIGN oe-relh.upd-date = IF NOT oe-relh.upd-date EQ ? THEN fFixYear(oe-relh.upd-date) ELSE ?.
                ASSIGN oe-relh.prt-date = IF NOT oe-relh.prt-date EQ ? THEN fFixYear(oe-relh.prt-date) ELSE ?.
            END.
            FOR EACH oe-rell EXCLUSIVE OF oe-rel:
                ASSIGN oe-relh.upd-date = fFixYear(oe-rell.upd-date).
            END.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixBlankOrdlShipIDs C-Win
PROCEDURE ipFixBlankOrdlShipIDs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    RUN ipStatus("   Fix blank oe-ordl ship-ids").
    FOR EACH oe-ordl EXCLUSIVE-LOCK
        WHERE oe-ordl.ship-id EQ ""
        :
        FIND FIRST oe-ord NO-LOCK WHERE 
            oe-ord.company EQ oe-ordl.company AND 
            oe-ord.ord-no EQ oe-ordl.ord-no
            NO-ERROR.
        IF AVAIL oe-ord THEN ASSIGN 
            oe-ordl.ship-id = oe-ord.ship-id.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixFrtPay C-Win
PROCEDURE ipFixFrtPay:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF oe-rel.
    DISABLE TRIGGERS FOR LOAD OF oe-rell.
    RUN ipStatus("   Fix release frt pay unknowns").
    FOR EACH oe-rel EXCLUSIVE-LOCK
        WHERE oe-rel.frt-pay eq ? OR oe-rel.fob-code EQ ?
        :
        IF oe-rel.frt-pay EQ ? THEN oe-rel.frt-pay = "".
        IF oe-rel.fob-code EQ ? THEN oe-rel.fob-code = "".
    END.
    FOR EACH oe-rell EXCLUSIVE-LOCK 
        WHERE oe-rell.frt-pay EQ ? OR oe-rell.fob-code eq ?
        :
        IF oe-rell.frt-pay EQ ? THEN oe-rell.frt-pay = "".
        IF oe-rell.fob-code EQ ? THEN oe-rell.fob-code = "".
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixPoEdiDirs C-Win 
PROCEDURE ipFixPoEdiDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTestLoc AS CHAR NO-UNDO.
    
    RUN ipStatus("   Fix file locations for PO EDI").
    
    /* The correct target for this dir is <env>\CustFiles\EDIfiles\PO */
    /* Is it already correct? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\EDIfiles\POs\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN
        RETURN.        
        
    /* Is it in /Customers folder? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + fiEnvironment:{&SV} + "\Customer\PO\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN DO:
        RUN ipCopyDirs (cEnvDir + "\" + fiEnvironment:{&SV} + "\Customer\PO",
                        cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\EDIfiles\POs").
        RETURN.
    END.
    
    /* Is it in /PO? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + fiEnvironment:{&SV} + "\PO\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN 
        RUN ipCopyDirs (cEnvDir + "\" + fiEnvironment:{&SV} + "\PO",
                        cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\EDIfiles\POs").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixUserPrint C-Win 
PROCEDURE ipFixUserPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Change 'asi' userprint records to 'admin'").

    DISABLE TRIGGERS FOR LOAD OF user-print.
    
    FOR EACH user-print EXCLUSIVE WHERE
        user-print.user-id EQ "asi":
        ASSIGN 
            user-print.user-id = "admin".
    END. 
         


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixUsers C-Win 
PROCEDURE ipFixUsers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Validating Required Users").

    ASSIGN 
        lSuccess = FALSE.
        
    IF NOT CAN-FIND (FIRST users WHERE
                     users.user_id = "asi") THEN
        RUN ipCreateAsiUser IN THIS-PROCEDURE.
    ELSE 
        RUN ipConfirmAsiUser IN THIS-PROCEDURE.

    IF NOT CAN-FIND (FIRST users WHERE
                     users.user_id = "admin") THEN
        RUN ipCreateAdminUser IN THIS-PROCEDURE.
    ELSE 
        RUN ipConfirmAdminUser IN THIS-PROCEDURE.

    RUN ipLoadNewUserData IN THIS-PROCEDURE.
    RUN ipCleanBadUserData IN THIS-PROCEDURE.
    
    ASSIGN 
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInvRnoSeq C-Win 
PROCEDURE ipInvRnoSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*  File:           util\InvRnoSeq.p                                         */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Utility to assign inv_r_no_seq                           */
/*                  values from raw data. Apply to versions 16.6.8 and below.*/
/*                  (non-destructive if applied twice)                       */
/*                                                                           */
/*  Included files:     none                                                 */
/*  External RUN/CALL:  none                                                 */
/*  External files:     READ inv-head                                        */                   
/*                      READ company                                         */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      12/01/17    wfk     24853   Original Version         */
/*---------------------------------------------------------------------------*/

    DEF VAR iCurrVal AS INT NO-UNDO.
    DEF VAR iLastDataValue AS INT NO-UNDO.
    DEF VAR iTries AS INT NO-UNDO.
    DEF VAR cCompSuffix AS CHAR NO-UNDO.
    
    RUN ipStatus ("    Data Fix InvRnoSeq...").

    /* Remove orphaned inv-line records */
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    OUTPUT TO C:\tmp\OrphanInvLineRecs.d APPEND.
    FOR EACH inv-line WHERE
        NOT CAN-FIND (FIRST inv-head OF inv-line):
        EXPORT inv-line.
        DELETE inv-line.
    END.
    OUTPUT CLOSE.
    
    /* Create inv_r_no_seq from last inv-head by r-no */
    ASSIGN
        iTries = 0
        iCurrVal = 0
        CURRENT-VALUE(inv_r_no_seq) = 0.
    INVHEAD_RNO:
    DO WHILE iCurrVal EQ 0:
    
        FIND LAST inv-head NO-LOCK
          USE-INDEX r-no 
          NO-ERROR.
        
        IF AVAIL inv-head THEN DO:
            ASSIGN
                iLastDataValue = IF AVAIL inv-head THEN inv-head.r-no ELSE 0.
            
            /* If the record is in ambiguous state or otherwise returns 0, keep trying */
            IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
                PAUSE 1 before-hide.
                FIND LAST inv-head NO-LOCK
                  USE-INDEX r-no 
                  NO-ERROR.
                ASSIGN
                    iLastDataValue = IF AVAIL inv-head THEN inv-head.r-no ELSE 0
                    iTries = iTries + 1.
                IF iTries GT 60 THEN DO: /* Try for 1 minute, then quit */
                    MESSAGE
                        "Unable to set sequence value for inv-head.r-no" SKIP
                        "Please contact Advantzware Support for assistance."
                        VIEW-AS ALERT-BOX ERROR.
                    LEAVE INVHEAD_RNO.
                END.
            END.
        
            ASSIGN
                CURRENT-VALUE(inv_r_no_seq) = iLastDataValue
                iCurrVal = CURRENT-VALUE(inv_r_no_seq).       
        END.  
        ELSE DO:
            LEAVE INVHEAD_RNO.
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadAPIData C-Win
PROCEDURE ipLoadAPIData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    RUN ipStatus ("  Loading API Data Files").

    DISABLE TRIGGERS FOR LOAD OF APIInbound.
    DISABLE TRIGGERS FOR LOAD OF APIInboundDetail.
    DISABLE TRIGGERS FOR LOAD OF APIInboundEvent.
    DISABLE TRIGGERS FOR LOAD OF APIOutbound.
    DISABLE TRIGGERS FOR LOAD OF APIOutboundDetail.
    DISABLE TRIGGERS FOR LOAD OF APIOutboundEvent.
    DISABLE TRIGGERS FOR LOAD OF APIOutboundTrigger.

&SCOPED-DEFINE tablename APIInbound
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename APIInboundDetail
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename APIInboundEvent
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename APIOutbound
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename APIOutboundDetail
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename APIOutboundEvent
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename APIOutboundTrigger
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadAuditRecs C-Win 
PROCEDURE ipLoadAuditRecs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading AuditTbl Records").

    &SCOPED-DEFINE tablename audittbl
    
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    ASSIGN 
        cAuditExceptionList = "dynParamValue,report,tag,Task,taskEmail,taskResult,user-print".
    
    /* First section reads the .d and creates records that aren't already there 
        (unless they're in the exception lis) */
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        /* If it's in the exception list, remove the base record and the tt record */
        IF CAN-DO(cAuditExceptionList,tt{&tablename}.auditTable) THEN DO:
            FIND {&tablename} EXCLUSIVE WHERE 
                {&tablename}.auditTable EQ tt{&tablename}.auditTable
                NO-ERROR.
            IF AVAIL {&tablename} THEN 
                DELETE {&tablename}.
            DELETE tt{&tablename}.
        END.
        /* Otherwise create the base record from the .d */
        ELSE DO:
            FIND {&tablename} EXCLUSIVE WHERE 
                {&tablename}.auditTable EQ tt{&tablename}.auditTable
                NO-ERROR.
            IF NOT AVAIL {&tablename} THEN DO:
                CREATE {&tablename}.
                BUFFER-COPY tt{&tablename} TO {&tablename}.
            END.
        END.
    END.
    INPUT CLOSE.

    /* Now remove any base records that are not in the tt */
    FOR EACH {&tablename} EXCLUSIVE WHERE
        NOT CAN-FIND(FIRST tt{&tablename} WHERE tt{&tablename}.auditTable = {&tablename}.auditTable ):
        DELETE {&tablename}.
    END.
    
    /* Finally, read the DB and ensure ALL (non-exception) tables in asi DB are referenced in audittbl file
        with all audit options off - Tkt #39728*/
    FOR EACH _file NO-LOCK WHERE _file._Tbl-type EQ "T":
        /* check if already exists */
        IF CAN-FIND(FIRST AuditTbl WHERE 
            AuditTbl.AuditTable EQ _file._file-name) THEN NEXT.
        /* ensure field rec_key exists */
        IF CAN-FIND(FIRST _field OF _file WHERE 
            ASI._field._field-name EQ "rec_key") EQ NO THEN NEXT.
        /* ensure trigger code exists */
        IF CAN-FIND(FIRST _file-trig WHERE 
            _file-trig._file-recid EQ recid(_file) AND 
            _file-trig._event EQ "create") EQ NO THEN NEXT.
        /* check if in exception list */
        IF CAN-DO(cAuditExceptionList,_file._file-name) THEN NEXT.
        /* add table to Audit Table */
        CREATE AuditTbl.
        ASSIGN
            AuditTbl.AuditTable  = _file._file-name
            AuditTbl.AuditCreate = NO
            AuditTbl.AuditDelete = NO
            AuditTbl.AuditUpdate = NO
            AuditTbl.AuditStack  = NO
            .
    END.
            
    EMPTY TEMP-TABLE tt{&tablename}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadCueCard C-Win 
PROCEDURE ipLoadCueCard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Cue Cards").

    &SCOPED-DEFINE tablename cueCard

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.cuePrgmName EQ tt{&tablename}.cuePrgmName AND 
            {&tablename}.isActive EQ tt{&tablename}.isActive AND 
            {&tablename}.cueID EQ tt{&tablename}.cueID 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadCueCardText C-Win 
PROCEDURE ipLoadCueCardText :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Cue Card Text").

    &SCOPED-DEFINE tablename cueCardText

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.cueID EQ tt{&tablename}.cueID AND 
            {&tablename}.isActive EQ tt{&tablename}.isActive AND 
            {&tablename}.cueOrder EQ tt{&tablename}.cueOrder 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadEmailCodes C-Win 
PROCEDURE ipLoadEmailCodes :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Email codes").

    &SCOPED-DEFINE tablename emailcod

    DEFINE BUFFER bemaildtl FOR emaildtl.
    
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    DISABLE TRIGGERS FOR LOAD OF bemaildtl.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.emailcod EQ tt{&tablename}.emailcod
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.
    
    /* 54067 Upon upgrade, add e-code for BOL to the new e-code for COC such that they are the same*/
    FOR EACH emaildtl NO-LOCK WHERE 
        emaildtl.emailcod = "r-bolprt.":
        FIND FIRST bemaildtl NO-LOCK WHERE
            bemaildtl.emailcod = "r-bolcert." AND 
            bemaildtl.table_rec_key EQ emaildtl.table_rec_key
            NO-ERROR.
        IF NOT AVAIL bemaildtl THEN 
        DO:
            CREATE bemaildtl.
            ASSIGN 
                bemaildtl.emailcod = "r-bolcert."
                bemaildtl.table_rec_key = emaildtl.table_rec_key
                bemaildtl.rec_key = STRING(YEAR(TODAY),"9999")
                                    + STRING(MONTH(TODAY),"99")
                                    + STRING(DAY(TODAY),"99")
                                    + STRING(TIME,"99999")
                                    + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999").
        END.
    END.      
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadJasperData C-Win 
PROCEDURE ipLoadJasperData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Loading Dynamic subjects...").
    
    DISABLE TRIGGERS FOR LOAD OF dynSubject.
    DISABLE TRIGGERS FOR LOAD OF dynSubjectTable.
    DISABLE TRIGGERS FOR LOAD OF dynSubjectWhere.
    DISABLE TRIGGERS FOR LOAD OF dynSubjectColumn.
    DISABLE TRIGGERS FOR LOAD OF dynSubjectParamSet.
    DISABLE TRIGGERS FOR LOAD OF dynParam.
    DISABLE TRIGGERS FOR LOAD OF dynParamSet.
    DISABLE TRIGGERS FOR LOAD OF dynParamValue.
    DISABLE TRIGGERS FOR LOAD OF dynParamSetDtl.
    
    /* Remove all records that we plan to replace */
    FOR EACH dynSubject EXCLUSIVE WHERE 
        dynSubject.subjecttype EQ "system" AND 
        dynSubject.user-id EQ "_default"
        BY dynSubject.subjectid:
        FOR EACH dynSubjectTable OF dynSubject EXCLUSIVE:
            DELETE dynSubjectTable.
        END.
        FOR EACH dynSubjectWhere OF dynSubject EXCLUSIVE:
            DELETE dynSubjectWhere.
        END.
        FOR EACH dynSubjectColumn OF dynSubject EXCLUSIVE:
            DELETE dynSubjectColumn.
        END.
        FOR EACH dynSubjectParamSet OF dynSubject EXCLUSIVE:
            DELETE dynSubjectParamSet.
        END.
        DELETE dynSubject.
    END. 
    
    FOR EACH dynParam EXCLUSIVE WHERE 
        dynParam.paramType EQ "system":
        DELETE dynParam.
    END.
    
    FOR EACH dynParamSet EXCLUSIVE WHERE 
        dynParamSet.paramSetType EQ "system":
        FOR EACH dynParamSetDtl OF dynParamSet EXCLUSIVE:
            DELETE dynParamSetDtl.
        END.
        DELETE dynParamSet.
    END.
    
    FOR EACH dynParamValue EXCLUSIVE WHERE 
        dynParamValue.user-id EQ "_default":
        DELETE dynParamValue.
    END.

&SCOPED-DEFINE tablename dynSubject
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynSubjectTable
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynSubjectWhere
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynSubjectColumn
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynSubjectParamSet
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynParam
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynParamSet
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynParamSetDtl
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynParamValue
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadLookups C-Win 
PROCEDURE ipLoadLookups :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Lookups").

    &SCOPED-DEFINE tablename lookups

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename}.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadMenus C-Win 
PROCEDURE ipLoadMenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Deprecated in 16.8.5
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTgtDir AS CHAR NO-UNDO.
    DEF VAR cFileStream AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.

    RUN ipStatus ("Loading New Menus").
    
    ASSIGN 
        cTgtEnv = cEnvDir + "\" + fiEnvironment:{&SV}.

    INPUT FROM OS-DIR (ipcDir).

    REPEAT:
        IMPORT cFileStream.
        FILE-INFO:FILE-NAME = ipcDir + "\" + cFileStream.
        IF SUBSTRING(FILE-INFO:FILE-NAME,LENGTH(FILE-INFO:FILE-NAME),1) EQ "." THEN DO:
            NEXT.
        END.
        ELSE IF FILE-INFO:FILE-TYPE BEGINS "F" THEN DO:
            OS-COPY VALUE(FILE-INFO:FILE-NAME) VALUE(cTgtEnv).
        END.
        ELSE DO:
            OS-CREATE-DIR VALUE(cTgtEnv + "\" + cFileStream).
            RUN ipLoadMenus IN THIS-PROCEDURE (FILE-INFO:FILE-NAME,cTgtEnv + "\Addon").
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadModules C-Win 
PROCEDURE ipLoadModules :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Module Records").

    &SCOPED-DEFINE tablename module
    
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.db-name EQ tt{&tablename}.db-name AND 
            {&tablename}.module EQ tt{&tablename}.module
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
            ASSIGN 
                {&tablename}.is-Used = FALSE.
        END.
    END.
    INPUT CLOSE.

    /* Delete records no longer used */
    FOR EACH {&tablename} EXCLUSIVE WHERE 
        NOT CAN-FIND(FIRST tt{&tablename} WHERE 
                    tt{&tablename}.db-name EQ {&tablename}.db-name AND
                    tt{&tablename}.module EQ {&tablename}.module):
        DELETE {&tablename}.
    END.
        
    EMPTY TEMP-TABLE tt{&tablename}.

    /* From Wade's convertModule.p in ticket 23532 */
    RUN ipConvertModule ("m2.", "outboundProcess.").
    RUN ipConvertModule ("m31.", "eddoc.").
    RUN ipConvertModule ("m33.", "edivtran.").
    RUN ipConvertModule ("m34.", "wedshtr.").
    RUN ipConvertModule ("m36.", "edcat.").
    RUN ipConvertModule ("m41.", "edmast.").
    RUN ipConvertModule ("m42.", "edcode.").
    RUN ipConvertModule ("m43.", "edShipto.").
    RUN ipConvertModule ("m44.", "edICXRef.").
    RUN ipConvertModule ("m45.", "edshipvia.").
    RUN ipConvertModule ("m46.", "edco.").
    RUN ipConvertModule ("m47.", "edSetID.").
    RUN ipConvertModule ("m48.", "edPartnerGrp.").
    RUN ipConvertModule ("m49.", "edPartnerSegment.").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadNewUserData C-Win 
PROCEDURE ipLoadNewUserData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF users.
    DISABLE TRIGGERS FOR LOAD OF usr.
    DISABLE TRIGGERS FOR LOAD OF reftable.

    RUN ipStatus ("Updating User Table Fields").

    /* Add/convert data for new users table fields */
    FOR EACH users EXCLUSIVE:

        ASSIGN
            users.userImage[1] = IF users.userImage[1] = "" THEN "Graphics\32x32\user.png" ELSE users.userImage[1]
            users.showMnemonic = IF users.showMnemonic = "" THEN "All" ELSE users.showMnemonic
            users.positionMnemonic = IF users.positionMnemonic = "" THEN "Begin" ELSE users.positionMnemonic
            users.use_colors = FALSE
            users.use_fonts = FALSE.

        IF users.userType = "" OR users.userType = ? THEN DO:
            CASE users.user_id:
                WHEN "ASI" OR
                WHEN "Administrator" OR
                WHEN "Admin"THEN ASSIGN users.userType = "Administrator".
                OTHERWISE ASSIGN users.userType = "Full User".
            END CASE.
        END.
        IF users.securityLevel = 0 THEN DO:
            CASE users.user_id:
                WHEN "ASI" THEN ASSIGN users.securityLevel = 1000.
                WHEN "Administrator" OR
                WHEN "Admin"THEN ASSIGN users.securityLevel = 900.
                OTHERWISE ASSIGN users.securityLevel = 100.
            END CASE.
        END.
        
        FOR EACH reftable EXCLUSIVE WHERE 
            reftable.reftable EQ "users.user-docs" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.showOnPO = IF users.showOnPO = TRUE OR reftable.val[1] = 1 THEN TRUE ELSE FALSE
                users.showOnBOL = IF users.showOnBOL = TRUE OR reftable.val[2] = 1 THEN TRUE ELSE FALSE
                users.showOnInv = IF users.showOnInv = TRUE OR reftable.val[3] = 1 THEN TRUE ELSE FALSE
                users.showOnAck = IF users.showOnAck = TRUE OR reftable.val[4] = 1 THEN TRUE ELSE FALSE
                users.showOnQuote = IF users.showOnQuote = TRUE OR reftable.val[5] = 1 THEN TRUE ELSE FALSE
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.phone-no" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.phone = IF users.phone = "" AND reftable.CODE = "" THEN reftable.CODE ELSE users.phone
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.fax-no" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.fax = IF users.fax = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.phone-cnty" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.phone-cnty = IF users.phone-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.phone-cnty
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.fax-cnty" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.fax-cnty = IF users.fax-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax-cnty
                .
            DELETE reftable.
        END.
    END. /* each users */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadOEAutoApproveNK1s C-Win
PROCEDURE ipLoadOEAutoApproveNK1s:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading OEAutoApprove NK1s").

    &SCOPED-DEFINE tablename sys-ctrl

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    DISABLE TRIGGERS FOR DUMP OF {&tablename}.

    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        IF tt{&tablename}.module NE "val" THEN 
            DELETE tt{&tablename}.
    END.
    
    FOR EACH tt{&tablename}:
        FOR EACH company:
            FIND FIRST {&tablename} EXCLUSIVE WHERE 
                {&tablename}.company EQ tt{&tablename}.company AND  
                {&tablename}.name EQ tt{&tablename}.name  
                NO-ERROR.
            IF NOT AVAIL {&tablename} THEN 
            DO:
                CREATE {&tablename}.
                BUFFER-COPY tt{&tablename} EXCEPT company TO {&tablename}
                ASSIGN
                    {&tablename}.company = company.company 
                    {&tablename}.log-fld = FALSE.
            END.
        END.
    END.
    INPUT CLOSE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadPrograms C-Win 
PROCEDURE ipLoadPrograms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Program Master Records").

    &SCOPED-DEFINE tablename prgrms

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.

        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.prgmname EQ tt{&tablename}.prgmname 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}
            ASSIGN 
                {&tablename}.can_run = '*'
                {&tablename}.can_create = '*'
                {&tablename}.can_update = '*'
                {&tablename}.can_delete = '*'.
        END.
        ELSE DO:
            BUFFER-COPY tt{&tablename} EXCEPT
                tt{&tablename}.can_run
                tt{&tablename}.can_create
                tt{&tablename}.can_update
                tt{&tablename}.can_delete
                tt{&tablename}.subjectID
                TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    /* Delete records no longer used */
    FOR EACH {&tablename} EXCLUSIVE:
        FIND FIRST tt{&tablename} WHERE
            tt{&tablename}.prgmname = {&tablename}.prgmname 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN ASSIGN
            {&tablename}.prgmname = "x" + {&tablename}.prgmname
            {&tablename}.menu_item = false
            {&tablename}.securityLevelDefault = 9999
            {&tablename}.securityLevelUser = 9999
            {&tablename}.mnemonic = "".
    END.
    
    EMPTY TEMP-TABLE tt{&tablename}.

    /* Fix "about." prgrms record description */
    FIND FIRST {&tablename} EXCLUSIVE-LOCK WHERE
        {&tablename}.prgmname EQ "about." 
        NO-ERROR.
    IF NOT AVAILABLE {&tablename} THEN DO:
        CREATE {&tablename}.
        ASSIGN
            {&tablename}.prgmname = "about."
            {&tablename}.dir_group = "nosweat"
            {&tablename}.run_persistent = YES
            {&tablename}.menu_item = YES
            .
    END.
    ASSIGN
        {&tablename}.prgtitle = "About". 
        
    /* Fix "w-head." program master (Help Maint) regardless of existing entry */
    FIND FIRST {&tablename} EXCLUSIVE-LOCK WHERE
        {&tablename}.prgmname EQ "w-head." 
        NO-ERROR.
    IF AVAILABLE {&tablename} THEN ASSIGN
        {&tablename}.securityLevelUser = 1000
        {&tablename}.securityLevelDefault = 1000
        .
        
    /* Fix "audit." program master regardless of existing entry */
    FIND FIRST {&tablename} EXCLUSIVE-LOCK WHERE
        {&tablename}.prgmname EQ "audit." 
        NO-ERROR.
    IF NOT AVAILABLE {&tablename} THEN
        CREATE {&tablename}.
    ASSIGN
        {&tablename}.dir_group = "system"
        {&tablename}.run_persistent = YES
        {&tablename}.menu_item = YES
        .

    /* Ensure 'admin' user has same privileges as 'asi' */
    /* This is better handled with new security, but eliminates some access issues */
    /* See ticket 27968 */
    FOR EACH {&tablename}:
        IF CAN-DO({&tablename}.can_run,"asi") 
        AND NOT CAN-DO({&tablename}.can_run,"admin") THEN ASSIGN
            {&tablename}.can_run = {&tablename}.can_run + ",admin".
        IF CAN-DO({&tablename}.can_create,"asi") 
        AND NOT CAN-DO({&tablename}.can_create,"admin") THEN ASSIGN
            {&tablename}.can_create = {&tablename}.can_create + ",admin".
        IF CAN-DO({&tablename}.can_update,"asi") 
        AND NOT CAN-DO({&tablename}.can_update,"admin") THEN ASSIGN
            {&tablename}.can_update = {&tablename}.can_update + ",admin".
        IF CAN-DO({&tablename}.can_delete,"asi") 
        AND NOT CAN-DO({&tablename}.can_delete,"admin") THEN ASSIGN
            {&tablename}.can_delete = prgrms.can_delete + ",admin".
    END.
    
    /* Added usergrp test per BV request - same ticket */
    DISABLE TRIGGERS FOR LOAD OF usergrps.
    FOR EACH usergrps:
        IF CAN-DO(usergrps.users,"asi") 
        AND NOT CAN-DO(usergrps.users,"admin") THEN ASSIGN
                usergrps.users = usergrps.users + ",admin".
    END.

    /* 35628 - ensure additional field content is loaded */
    INPUT FROM VALUE(cUpdDataDir + "\prgrms.d") NO-ECHO.
    REPEAT:
        CREATE ttPrgrms.
        IMPORT ttPrgrms.
        FIND FIRST prgrms EXCLUSIVE WHERE 
            prgrms.prgmname EQ ttPrgrms.prgmname 
            NO-ERROR.
        IF NOT AVAIL prgrms THEN DO:
            CREATE prgrms.
            BUFFER-COPY ttPrgrms TO prgrms.
        END.
        ELSE DO:
            ASSIGN 
                prgrms.menuOrder = ttPrgrms.menuOrder
                prgrms.menuLevel = ttPrgrms.menuLevel
                prgrms.itemParent = ttPrgrms.itemParent
                prgrms.mnemonic = ttPrgrms.mnemonic
                prgrms.systemType = ttPrgrms.systemType
                prgrms.menuImage = ttPrgrms.menuImage
                prgrms.translation = ttPrgrms.translation.
        END.
        DELETE ttPrgrms.
    END.

    DISABLE TRIGGERS FOR LOAD OF employee.
    FOR EACH employee EXCLUSIVE-LOCK:
        employee.employeeImage[1] = "Graphics\32x32\user.png".
    END. /* each users */

    DISABLE TRIGGERS FOR LOAD OF mach.
    FOR EACH mach EXCLUSIVE-LOCK:
        mach.machineImage[1] = "Graphics\32x32\gearwheels.png".
    END. /* each users */

    DISABLE TRIGGERS FOR LOAD OF users.
    FOR EACH users EXCLUSIVE-LOCK:
        ASSIGN
            users.userImage[1] = if users.userImage[1] = "" then "Graphics\32x32\user.png" else users.userImage[1]
            users.showMnemonic = IF users.showMnemonic = "" then "All" else users.showMnemonic
            users.positionMnemonic = if users.positionMnemonic = "" then "Begin" else users.positionMnemonic.
    END. /* each users */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadProgramXref C-Win 
PROCEDURE ipLoadProgramXref :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Program Master Cross-References").

    &SCOPED-DEFINE tablename prgmxref

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.table_name EQ tt{&tablename}.table_name 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
        ELSE DO:
            ASSIGN 
                {&tablename}.prgmname = tt{&tablename}.prgmname
                {&tablename}.pageno = tt{&tablename}.pageno.
        END.
    END.
    INPUT CLOSE.
        
    /* Delete records no longer used */
    FOR EACH {&tablename} EXCLUSIVE WHERE
        NOT CAN-FIND(FIRST tt{&tablename} WHERE tt{&tablename}.table_name = {&tablename}.table_name ):
        DELETE {&tablename}.
    END.
    
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadTranslation C-Win 
PROCEDURE ipLoadTranslation :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Translation Records").

    &SCOPED-DEFINE tablename translation
    
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.rec_key EQ tt{&tablename}.rec_key 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadUserLanguage C-Win 
PROCEDURE ipLoadUserLanguage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading UserLanguage Records").

    &SCOPED-DEFINE tablename userlanguage

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.rec_key EQ tt{&tablename}.rec_key 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadUtilitiesTable C-Win 
PROCEDURE ipLoadUtilitiesTable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Utilities Records").

    &SCOPED-DEFINE tablename utilities

    DISABLE TRIGGERS FOR LOAD OF reftable.
    DISABLE TRIGGERS FOR LOAD OF notes.
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    
    FOR EACH {&tablename}:
        DELETE {&tablename}.
    END.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename}.
        IF {&tablename}.programName = "module.r" /* Module Maint */
        OR {&tablename}.programName = "w-head.r" /* Help Maint */ THEN ASSIGN 
            {&tablename}.securityLevel = 1000.
        ELSE ASSIGN 
            {&tablename}.securityLevel = 900.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

    /* 25458 - Delete utilities reftables */
    FOR EACH reftable EXCLUSIVE WHERE 
        reftable.reftable EQ 'Utilities':
        FOR EACH notes EXCLUSIVE WHERE 
            notes.rec_key EQ reftable.rec_key:
            DELETE notes.
        END. 
        CREATE reftable1.
        BUFFER-COPY reftable TO reftable1.
        DELETE reftable.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadXuserMenu C-Win 
PROCEDURE ipLoadXuserMenu :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading xusermenu Records").

    &SCOPED-DEFINE tablename xusermenu

    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        IF tt{&tablename}.user_id NE "AddonUsr" THEN DO:
            DELETE tt{&tablename}.
            NEXT.
        END.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.rec_key EQ tt{&tablename}.rec_key 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
    END.
    
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE tt{&tablename}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadZmessage C-Win
PROCEDURE ipLoadZmessage:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading zMessage Records").

    &SCOPED-DEFINE tablename zMessage
    
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.msgID EQ tt{&tablename}.msgID 
            NO-ERROR.
        IF NOT AVAIL {&tablename} THEN 
        DO:
            CREATE {&tablename}.
            BUFFER-COPY tt{&tablename} TO {&tablename}.
        END.
        ELSE DO: /* Update fields except those controlled by user */
            BUFFER-COPY tt{&tablename} EXCEPT 
                currentTitle 
                currMessage 
                userSuppress 
                displayOptions 
                TO {&tablename}.
        END.
    END.
    INPUT CLOSE.

    /* Delete records no longer used */
    FOR EACH {&tablename} EXCLUSIVE WHERE 
        NOT CAN-FIND(FIRST tt{&tablename} WHERE 
                    tt{&tablename}.msgID EQ {&tablename}.msgID):
        DELETE {&tablename}.
    END.
        
    EMPTY TEMP-TABLE tt{&tablename}.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipMoveUserMenusToDatabase C-Win 
PROCEDURE ipMoveUserMenusToDatabase :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSearchDir AS CHARACTER NO-UNDO INITIAL ".\usermenu".
    DEFINE VARIABLE cUserDir   AS CHARACTER NO-UNDO FORMAT "X(60)".
    DEFINE VARIABLE cAttrList  AS CHARACTER NO-UNDO FORMAT "X(4)".
    DEFINE VARIABLE cListUsers AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMenuList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrgmName  AS CHARACTER NO-UNDO.

    DISABLE TRIGGERS FOR LOAD OF xUserMenu.
    
    ASSIGN 
        cSearchDir =  cEnvDir + "/" + fiEnvironment:{&SV} + "/usermenu".
                
    /* get a list of usermenu folders */
    INPUT FROM OS-DIR(cSearchDir) NO-ECHO.
    REPEAT:
        SET cUserDir ^ cAttrList.
        IF cAttrList NE "d" THEN NEXT.
        IF CAN-DO(".,..",cUserDir) THEN NEXT.
        cListUsers = cListUsers + cUserDir + ",".
    END. /* repeat */
    INPUT CLOSE.
    cListUsers = TRIM(cListUsers,",").

    /* check each usermenu folder */
    DO idx = 1 TO NUM-ENTRIES(cListUsers):
        cMenuList = SEARCH(cSearchDir + "/" + ENTRY(idx,cListUsers) + "/menu.lst").
        /* if menu.lst does not exist */
        IF cMenuList EQ ? THEN NEXT.
        /* menu.lst exists, process it */
        EMPTY TEMP-TABLE ttUserMenu.
        INPUT FROM VALUE(cMenuList) NO-ECHO.
        REPEAT:
            IMPORT cPrgmName.
            CREATE ttUserMenu.
            ttUserMenu.prgmname = cPrgmName.
        END. /* repeat */
        INPUT CLOSE.
        /* look for each menu option in the user's custom menu.lst */
        FOR EACH prgrms NO-LOCK
            WHERE prgrms.menu_item EQ YES
              AND prgrms.menuOrder GT 0
              AND prgrms.menuLevel GT 0
              AND prgrms.mnemonic  NE ""
            :
            /* new additions, do not add to user's exceptions */
            IF CAN-DO("r-jcstdN.,translatn.,userLang.",prgrms.prgmname) THEN
            NEXT.
            /* if found, skip to next menu option */
            IF CAN-FIND(FIRST ttUserMenu
                        WHERE ttUserMenu.prgmname EQ prgrms.prgmname) THEN DO:
                IF prgrms.itemParent NE "" THEN
                RUN ipActivateParent (prgrms.itemParent, ENTRY(idx,cListUsers)).
                NEXT.
            END. /* if can-find */
            /* menu option not found in menu.lst, add as an exception */
            CREATE xUserMenu.
            ASSIGN
                xUserMenu.user_id  = ENTRY(idx,cListUsers)
                xUserMenu.prgmname = prgrms.prgmname
                .
        END. /* each prgrms */
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcessAll C-Win 
PROCEDURE ipProcessAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    RUN ipStatus ("Beginning Patch Application").
    ASSIGN
        SELF:LABEL = IF SELF:SENSITIVE THEN "Processing..." ELSE SELF:LABEL 
        SELF:SENSITIVE = FALSE
        lSuccess = TRUE.

    IF tbBackupDBs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipBackupDBs.
        IF lSuccess EQ FALSE THEN RETURN. 
    END.
    IF tbUserControl:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateUserControl.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 1
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 1
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
    
    IF tbUserCleanup:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipFixUsers.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 1
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 1
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbDelBadData:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDelBadData.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 2
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 2
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbUpdateMaster:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateMaster.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 4
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 4
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbRunDataFix:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDataFix.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 10
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 10
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbUpdateNK1s:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateNK1s.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 3
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 3
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbLoadMenus:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        /* Deprecated in 16.8.5 
        RUN ipLoadMenus (cUpdMenuDir,cEnvProdDir).
        */
    END.

    IF tbRelNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipCopyRelNotes.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 2
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 2
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbBackupFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipArchiveFiles.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 10
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 10
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
    
    IF tbInstallFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipExpandFiles.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 20
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 20
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).

    IF tbRefTableConv:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipRefTableConv.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 30
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 30
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
    
    IF tbUpdateIni:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateTTIniFile.
        RUN ipWriteIniFile.
        IF lSuccess EQ TRUE THEN ASSIGN 
            iopiStatus = iopiStatus + 5
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
        ELSE RETURN.
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 5
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
    
    RUN ipBackupDataFiles IN THIS-PROCEDURE ("NEW").
    RUN ipSetNewDbVersion IN THIS-PROCEDURE.
    
    RUN ipStatus ("Patch Application Complete").

    ASSIGN
        SELF:LABEL = "Start Update"
        SELF:SENSITIVE = TRUE
        fiFromVer:{&SV} = fiToVer:{&SV}
        oplSuccess = TRUE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRefTableConv C-Win 
PROCEDURE ipRefTableConv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cOrigPropath AS CHAR NO-UNDO.
    DEF VAR cNewPropath AS CHAR NO-UNDO.
    DEF VAR cThisElement AS CHAR NO-UNDO.
    DISABLE TRIGGERS FOR LOAD OF reftable.
    DISABLE TRIGGERS FOR LOAD OF reftable1.
    DISABLE TRIGGERS FOR LOAD OF oe-rel.
    
    RUN ipStatus ("Converting Reftable records...").

    ASSIGN
        lSuccess = FALSE 
        cOrigPropath = PROPATH
        cNewPropath  = cEnvDir + "\" + fiEnvironment:{&SV} + "\Programs," + PROPATH
        PROPATH = cNewPropath.
    RUN ipStatus ("   ReftableConvert for " + fiEnvironment:{&SV}).
    RUN 
        VALUE(SEARCH("util\dev\RefTableConvert.r")).
    ASSIGN
        PROPATH = cOrigPropath.

    /* Ticket 25507 */
    RUN ipStatus ("   Ticket 25507 reftable = blank").
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "":
        CREATE reftable1.
        BUFFER-COPY reftable TO reftable1.
        DELETE 
            reftable.
    END.

    /* Ticket 27898 */
    RUN ipStatus ("   Ticket 27898 oe-rel.s-code").
    FOR EACH reftable1 EXCLUSIVE WHERE
        reftable1.reftable EQ 'oe-rel.s-code' AND 
        reftable1.val[1] NE 1,
        FIRST oe-rel EXCLUSIVE WHERE
            oe-rel.r-no EQ integer(reftable1.company) AND 
            oe-rel.s-code NE reftable1.code
            USE-INDEX seq-no:
        ASSIGN 
            reftable1.val[1] = 1
            oe-rel.s-code = reftable1.code.
    END.

    /* Ticket 32053 - oe-rel customer lot number */
    RUN ipStatus ("   Ticket 32053 oe-rel.lot-no").
    OUTPUT TO c:\tmp\reftable-oe-rel.txt.
    FOR EACH reftable1 EXCLUSIVE WHERE 
        reftable1.reftable = "oe-rel.lot-no" AND
        reftable1.spare-char-1 NE "1" AND
        reftable1.spare-char-2 NE "1" AND
        reftable1.spare-char-3 NE "1":
        FIND FIRST oe-rel EXCLUSIVE WHERE
            oe-rel.r-no = INT(reftable1.company)
            USE-INDEX seq-no
            NO-ERROR.
        IF AVAILABLE oe-rel THEN DO: 
            IF oe-rel.lot-no EQ "" 
            AND reftable1.code NE "" THEN DO:
                EXPORT oe-rel.
                ASSIGN 
                    oe-rel.lot-no = reftable1.code
                    reftable1.spare-char-1 = "1".
            END.
            IF oe-rel.frt-pay EQ "" 
            AND reftable1.code2 NE "" THEN DO:
                EXPORT oe-rel.
                ASSIGN 
                    oe-rel.frt-pay = reftable1.code2
                    reftable1.spare-char-2 = "1".
            END.
            IF oe-rel.fob-code EQ "" 
            AND reftable1.dscr NE "" THEN DO:
                EXPORT oe-rel.
                ASSIGN 
                    oe-rel.fob-code = reftable1.dscr
                    reftable1.spare-char-3 = "1".
            END.
        END.   
    END.  /*FOR EACH reftable1*/  

    ASSIGN 
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRelRnoSeq C-Win 
PROCEDURE ipRelRnoSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*  File:           DeploymentFiles\DataFixPrograms\oeRelSeq.p               */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Utility to assign oerel_rno_seq and oerel_release_seq    */
/*                  values from raw data. Apply to versions 16.6.8 and below.*/
/*                  (non-destructive if applied twice)                       */
/*                                                                           */
/*  Included files:     none                                                 */
/*  External RUN/CALL:  none                                                 */
/*  External files:     READ oe-rel                                          */
/*                      READ oe-relh                                         */
/*                      READ company                                         */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      12/01/17    MYT     24853   Original Version         */
/*---------------------------------------------------------------------------*/

    DEF VAR iCurrVal AS INT NO-UNDO.
    DEF VAR iLastDataValue AS INT NO-UNDO.
    DEF VAR iTries AS INT NO-UNDO.
    DEF VAR cCompSuffix AS CHAR NO-UNDO.

    RUN ipStatus ("    Data Fix InvRnoSeq...").


    /* Create oerel_rno_seq from last oe-rel by r-no */
    ASSIGN
        iTries = 0
        iCurrVal = 0
        CURRENT-VALUE(oerel_rno_seq) = 0.

    OEREL_RNO:
    DO WHILE iCurrVal EQ 0:
        FIND FIRST oe-rel NO-LOCK 
            USE-INDEX seq-no 
            NO-ERROR.
        ASSIGN
            iLastDataValue = IF AVAIL oe-rel THEN oe-rel.r-no ELSE 0.
        /* If the record is in ambiguous state or otherwise returns 0, keep trying */
        IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
            PAUSE 1 BEFORE-HIDE.
            FIND FIRST oe-rel NO-LOCK 
                USE-INDEX seq-no 
                NO-ERROR.
            ASSIGN
                iTries = iTries + 1
                iLastDataValue = IF AVAIL oe-rel THEN oe-rel.r-no ELSE 0.
            IF iTries GT 5 THEN DO: /* Try for 1 minute, then quit */
                MESSAGE
                    "Unable to set sequence value for oe-rel.r-no" SKIP
                    "Please contact Advantzware Support for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                LEAVE OEREL_RNO.
            END.
        END.
        ASSIGN
            CURRENT-VALUE(oerel_rno_seq) = iLastDataValue
            iCurrVal = CURRENT-VALUE(oerel_rno_seq).
    END.    

    /* Create oerel_release_seq from last oe-relh by r-no (NOT by company) */
    ASSIGN
        iTries = 0
        iCurrVal = 0
        CURRENT-VALUE(oerel_release_seq) = 0.
    OEREL_REL_NOCO:
    DO WHILE iCurrVal EQ 0:
        FIND LAST oe-relh NO-LOCK 
            USE-INDEX r-no 
            NO-ERROR.
        ASSIGN
            iLastDataValue = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0.
        /* If the record is in ambiguous state or otherwise returns 0, keep trying */
        IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
            PAUSE 1 BEFORE-HIDE.
            FIND LAST oe-relh NO-LOCK 
                USE-INDEX r-no 
                NO-ERROR.
            ASSIGN
                iTries = iTries + 1
                iLastDataValue = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0.
            IF iTries GT 5 THEN DO: /* Try for 1 minute, then quit */
                MESSAGE
                    "Unable to set sequence value for oe-relh.r-no" SKIP
                    "Please contact Advantzware Support for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                LEAVE OEREL_REL_NOCO.
            END.
        END.
        ASSIGN
            CURRENT-VALUE(oerel_release_seq) = iLastDataValue
            iCurrVal = CURRENT-VALUE(oerel_release_seq).       
    END.    

    /* Create oerel_release_seq from last oe-relh by r-no BY COMPANY */
    FOR EACH company:
        IF company.spare-char-1 EQ "" 
        OR INT(company.spare-char-1) GT 10 THEN NEXT.
        ASSIGN
            iLastDataValue = 0
            cCompSuffix = company.spare-char-1
            iTries = 0
            iCurrVal = 0
            DYNAMIC-CURRENT-VALUE("oerel_release_seq" + cCompSuffix, "ASI") = 0.
        OEREL_REL_USINGCO:
        DO WHILE iCurrVal EQ 0:
            IF NOT CAN-FIND(FIRST oe-relh NO-LOCK WHERE 
                oe-relh.company EQ company.company) THEN
                LEAVE OEREL_REL_USINGCO.
            FIND LAST oe-relh NO-LOCK WHERE 
                oe-relh.company EQ company.company
                USE-INDEX release# 
                NO-ERROR.
            ASSIGN
                iLastDataValue = IF AVAIL oe-relh THEN oe-relh.release# ELSE 0.
            /* If the record is in ambiguous state or otherwise returns 0, keep trying */
            IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
                PAUSE 1 BEFORE-HIDE.
                FIND LAST oe-relh NO-LOCK WHERE 
                    oe-relh.company EQ company.company
                    USE-INDEX release# 
                    NO-ERROR.
                ASSIGN
                    iTries = iTries + 1
                    iLastDataValue = IF AVAIL oe-relh THEN oe-relh.release# ELSE 0.
                IF iTries GT 2 THEN DO: /* Try for 1 minute, then quit */
                    MESSAGE
                        "Unable to set sequence value for oe-relh.r-no" SKIP
                        "Please contact Advantzware Support for assistance."
                        VIEW-AS ALERT-BOX ERROR.
                    LEAVE OEREL_REL_USINGCO.
                END.
            END.
            DYNAMIC-CURRENT-VALUE("oerel_release_seq" + cCompSuffix, "ASI") = iLastDataValue.
            ASSIGN
                iCurrVal = DYNAMIC-CURRENT-VALUE("oerel_release_seq" + cCompSuffix, "ASI").       
                
        END. 
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRemoveUserAddon C-Win 
PROCEDURE ipRemoveUserAddon :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    RUN ipStatus ("    Removing addon mode from .usr file").

    DEF BUFFER bxUserMenu FOR xUserMenu.
    DISABLE TRIGGERS FOR LOAD OF xUserMenu.
    
    DEF VAR cLine     AS CHAR NO-UNDO.
    DEF VAR cOutline  AS CHAR NO-UNDO.

    INPUT FROM VALUE(cAdminDir + "\advantzware.usr").
    REPEAT:
        IMPORT UNFORMATTED cLine.
        CREATE ttUsers.
        ASSIGN
            ttfUserid   = ENTRY(1,cLine,"|")
            ttfdbname   = ENTRY(2,cLine,"|")
            ttfalias    = ENTRY(3,cLine,"|")
            ttfenvlist  = ENTRY(4,cLine,"|")
            ttfdblist   = ENTRY(5,cLine,"|")
            ttfmodelist = ENTRY(6,cLine,"|").
    END.
    INPUT CLOSE.

    FOR EACH ttUsers WHERE
        ttUsers.ttfDbName EQ ipcName:
       
        /* This condition implies user can access Addon menu, but not Main menu */
        IF INDEX(ttfModeList,"Addon") NE 0 
        AND INDEX(ttfModeList,"Advantzware") EQ 0 THEN DO:
            FOR EACH xUserMenu NO-LOCK WHERE 
                xUserMenu.user_id EQ "AddOnUsr":
                CREATE bxUserMenu.
                BUFFER-COPY xUserMenu EXCEPT user_id TO bxUserMenu
                ASSIGN 
                    bxUserMenu.user_id = ttUsers.ttfUserID.
            END. /* each xusermenu */
        END.
        ASSIGN 
            ttfModeList = REPLACE(ttfModeList,"Addon","") 
            ttfModeList = REPLACE(ttfModeList,",,",",").
    END.

    OUTPUT TO VALUE(cAdminDir + "\advantzware.usr").
    FOR EACH ttUsers:
        ASSIGN
            cOutline = ttfUserID + "|" +
                       ttfdbName + "|" +
                       ttfAlias  + "|" +
                       ttfEnvList + "|" +
                       ttfDbList + "|" +
                       ttfModeList.
        PUT UNFORMATTED cOutline + CHR(10).
    END.
    OUTPUT CLOSE.  

    RUN ipStatus ("    Modifying menu for Prod Floor users").
    FOR EACH users NO-LOCK WHERE 
        users.userType EQ "Production Floor":
        IF CAN-FIND(FIRST xUserMenu WHERE 
                    xUserMenu.user_id EQ users.user_id) THEN NEXT.    
        FOR EACH xUserMenu NO-LOCK WHERE 
            xUserMenu.user_id EQ "AddOnUsr":
            CREATE bxUserMenu.
            BUFFER-COPY xUserMenu EXCEPT user_id TO bxUserMenu
            ASSIGN 
                bxUserMenu.user_id = users.user_id.
        END. /* each xusermenu */
    END. /* each users */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRemoveUserMenu C-Win 
PROCEDURE ipRemoveUserMenu :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
      ------------------------------------------------------------------------------*/
    DEF VAR cUserMenuDir AS CHAR NO-UNDO.
    DEF VAR cAddonUserMenuDir AS CHAR NO-UNDO.
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    RUN ipStatus ("    Removing user menu and menu files").

    ASSIGN 
        cUserMenuDir = cEnvDir + "\" + fiEnvironment:{&SV} + "\UserMenu"
        cAddonUserMenuDir = cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\UserMenu"
        .

    IF SEARCH(cUserMenuDir + "\.") NE ? THEN DO:
        OS-DELETE VALUE(cUserMenuDir) RECURSIVE.
        OS-DELETE VALUE(cAddonUserMenuDir) RECURSIVE.

        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu.cor").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu.fol").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu.lst").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu_addon.cor").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu_addon.fol").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu_addon.lst").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu_plus.cor").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu_plus.fol").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\Addon\menu_plus.lst").

        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu.cor").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu.fol").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu.lst") RECURSIVE.

        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu_addon.cor").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu_addon.fol").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu_addon.lst").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu_plus.cor").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu_plus.fol").
        OS-DELETE VALUE(cEnvDir + "\" + fiEnvironment:{&SV} + "\menu_plus.lst").
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetAdminPwd C-Win 
PROCEDURE ipSetAdminPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Setting ADMIN Password").

    FIND FIRST _User WHERE 
        _User._UserId = "admin" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL (_User) THEN DO: 
        CREATE _User.
        ASSIGN
            _User._UserId = "ADMIN"
            _User._Password = ENCODE("admin").
    END.
    
    RELEASE _user.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetAsiPwd C-Win 
PROCEDURE ipSetAsiPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
    RUN ipStatus ("  Setting ASI Password").

    FIND FIRST _User WHERE 
        _User._UserId = "asi" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL (_User) THEN DO:
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser.
        ASSIGN 
            tempUser._Password = "fcpapdfHaSLfnMbA".
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
    END.
    ELSE DO:
        CREATE _User.
        ASSIGN
            _User._UserId = "asi"
            _User._Password = "fcpapdfHaSLfnMbA".
    END.

    RELEASE _user.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetCueCards C-Win
PROCEDURE ipSetCueCards:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Turn off new cue cards if user.showCueCards = false").

    DISABLE TRIGGERS FOR LOAD OF xCueCard.
    
    FOR EACH users NO-LOCK WHERE 
        users.showCueCards EQ NO:
        FOR EACH CueCardText NO-LOCK,
            FIRST CueCard NO-LOCK
            WHERE CueCard.cueType EQ "System"
            :
            IF CAN-FIND(FIRST xCueCard
                WHERE xCueCard.user_id EQ users.user_id
                AND xCueCard.cueType   EQ CueCard.cueType
                AND xCueCard.cueTextID EQ CueCardText.cueTextID) THEN
                NEXT.
            CREATE xCueCard.
            ASSIGN
                xCueCard.user_id   = users.user_id
                xCueCard.cueType   = CueCard.cueType
                xCueCard.cueTextID = CueCardText.cueTextID
                .
            RELEASE xCueCard.
        END. /* each cuecardtext */
    END. /* each users */

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
                WHEN "hostname" THEN ASSIGN fiHostname:{&SV} = ttIniFile.cVarValue.
            END CASE.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetImageFiles C-Win 
PROCEDURE ipSetImageFiles :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("    Setting Image files").
    
    FOR EACH employee EXCLUSIVE-LOCK:
        employee.employeeImage[1] = "Graphics\32x32\user.png".
    END. /* each users */

    FOR EACH mach EXCLUSIVE-LOCK:
        mach.machineImage[1] = "Graphics\32x32\gearwheels.png".
    END. /* each users */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetNewDbVersion C-Win
PROCEDURE ipSetNewDbVersion:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:     This forces a lock between ENV version and DB version; 
                this was not previously the case
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Set new DB version to " + ipcVer).
    
    FIND FIRST config EXCLUSIVE.
    IF NOT AVAIL config THEN
        CREATE config.
    ASSIGN 
        config.databaseVersion = ipcVer.

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
    DEF VAR cLogFile AS CHAR NO-UNDO.
                
    IF INDEX(ipcStatus,"duplicate") EQ 0 THEN DO:
        ASSIGN
            eStatus:{&SV}       = eStatus:{&SV} + ipcStatus + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        ASSIGN
            cLogFile = cEnvAdmin + "\UpdateLog.txt"
            iMsgCtr = iMsgCtr + 1
            cMsgStr[iMsgCtr] = "  " + ipcStatus.
        
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr[iMsgCtr] FORMAT "x(60)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
    END.
    
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTrackUsage C-Win 
PROCEDURE ipTrackUsage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus("   Turn on user usage tracking").
    
    DISABLE TRIGGERS FOR LOAD OF users.
    
    FOR EACH users:
        ASSIGN
            users.track_usage = TRUE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTurnOffUserColors C-Win 
PROCEDURE ipTurnOffUserColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus("   Turn off user colors/fonts").
    
    DISABLE TRIGGERS FOR LOAD OF users.
    
    FOR EACH users:
        ASSIGN
            users.use_colors = FALSE
            users.use_fonts = FALSE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateAdvantzwarePf C-Win 
PROCEDURE ipUpdateAdvantzwarePf :
/*------------------------------------------------------------------------------
 Purpose: This is broken, but is superseded by copying advantzware.pf during startUpdate
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cFileLine AS CHAR NO-UNDO.
    DEF VAR iLine AS INT NO-UNDO INITIAL 1.
    
    RUN ipStatus ("    Beginning advantzware.pf update").
    
    INPUT FROM VALUE(SEARCH(cEnvAdmin + "\advantzware.pf")).
    REPEAT:
        IMPORT UNFORMATTED cFileLine.
        CREATE ttPfFile.
        ASSIGN 
            ttPfFile.ttfLine = iLine
            ttPfFile.ttfRawLine = cFileLine
            iLine = iLine + 1.
        IF NOT cFileLine BEGINS "#" THEN ASSIGN 
            ttPfFile.ttfParm = ENTRY(1,cFileLine," ").
        IF NUM-ENTRIES(cFileLine," ") GT 1 THEN  
            ttPfFile.ttfValue = ENTRY(2,cFileLine," ").
        IF NUM-ENTRIES(cFileLine,"#") GT 1 THEN 
            ttPfFile.ttfDesc = ENTRY(2,cFileLine,"#").
    END.
 
    FIND FIRST ttPfFile WHERE 
        ttPfFile.ttfParm EQ "-cpstream"
        NO-ERROR.
    IF AVAIL ttPfFile THEN ASSIGN 
        ttPfFile.ttfValue = "ISO8859-1".
        
    OUTPUT TO VALUE(SEARCH(cEnvAdmin + "\advantzware.pf")).
    FOR EACH ttPfFile:
        IF ttPfFile.ttfParm NE "" THEN PUT 
            ttPfFile.ttfParm + " " + ttPfFile.ttfValue AT 1
            "# " + ttPfFile.ttfDesc AT 21 SKIP.
        ELSE put
            ttPfFile.ttfRawLine AT 1 SKIP.
    END.         
    OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateMaster C-Win 
PROCEDURE ipUpdateMaster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Updating Master Records").

    ASSIGN 
        lSuccess = FALSE.

    RUN ipBackupDataFiles IN THIS-PROCEDURE ("OLD").
    
    IF SEARCH(cUpdDataDir + "\prgrms.d") <> ? THEN
        RUN ipLoadPrograms IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\prgmxref.d") <> ? THEN
        RUN ipLoadProgramXref IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\lookups.d") <> ? THEN
        RUN ipLoadLookups IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\emailcod.d") <> ? THEN
        RUN ipLoadEmailCodes IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\module.d") <> ? THEN
        RUN ipLoadModules IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\translation.d") <> ? THEN
        RUN ipLoadTranslation IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\userlanguage.d") <> ? THEN
        RUN ipLoadUserLanguage IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\xusermenu.d") <> ? THEN
        RUN ipLoadXuserMenu IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\utilities.d") <> ? THEN
        RUN ipLoadUtilitiesTable IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\audittbl.d") <> ? THEN
        RUN ipLoadAuditRecs IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\cuecard.d") <> ? THEN
        RUN ipLoadCueCard IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\cuecardtext.d") <> ? THEN
        RUN ipLoadCueCardText IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\zMessage.d") <> ? THEN
        RUN ipLoadZmessage IN THIS-PROCEDURE.

    ASSIGN 
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateNK1s C-Win 
PROCEDURE ipUpdateNK1s :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lIsDir AS LOG NO-UNDO.
    DEF VAR cOrigValue AS CHAR NO-UNDO.
    DEF VAR cTgtDir AS CHAR NO-UNDO.
        
    RUN ipStatus ("Updating NK1 Records").
    
    ASSIGN 
        lSuccess = FALSE.

    DISABLE TRIGGERS FOR LOAD OF sys-ctrl.
    DISABLE TRIGGERS FOR LOAD OF sys-ctrl-shipto.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl-shipto.
    
    /* 01/18/19 - MYT - ensure all these defaults/overrides apply to all companies in DB, not just first record found */
    /* Verify system help WSDL NK1 */
    RUN ipStatus ("  Help Service entries").
    FOR EACH sys-ctrl WHERE
        sys-ctrl.name EQ "AsiHelpService":
        ASSIGN
            sys-ctrl.char-fld = "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'".
    END. 
    FOR EACH sys-ctrl WHERE
        sys-ctrl.name EQ "UpdateService":
        ASSIGN
            sys-ctrl.char-fld = "-WSDL 'http:\\34.203.15.64/updatehelpServices/helpupdate.asmx?WSDL'".
    END.
    
    /* Reports - set LV = true */
    RUN ipStatus ("  REPORTS - set log value to TRUE").
    FOR EACH sys-ctrl WHERE
        sys-ctrl.name EQ "Reports":
        ASSIGN
            sys-ctrl.log-fld = TRUE.
    END. 

    /* RelType - set Default to "B" */
    RUN ipStatus ("  RelType - if empty, set char value to Bill and Ship").
    FOR EACH sys-ctrl WHERE
        sys-ctrl.name EQ "RelType":
        IF sys-ctrl.char-fld EQ "" THEN ASSIGN
            sys-ctrl.char-fld = "Bill and Ship".
    END.

    /* Zoho Support Button */
    RUN ipStatus ("  MenuLinkZoho").
    FOR EACH sys-ctrl WHERE
        sys-ctrl.name EQ "MenuLinkZoho":
        ASSIGN
            sys-ctrl.descrip = "https://desk.zoho.com/portal/advantzware/kb"
            sys-ctrl.char-fld = "Graphics\32x32\question.ico"
            sys-ctrl.log-fld = TRUE.
    END.
    
    /* Upgrade Button */
    RUN ipStatus ("  MenuLinkUpdate").
    FOR EACH  sys-ctrl WHERE
        sys-ctrl.name EQ "MenuLinkUpgrade":
        ASSIGN
            sys-ctrl.descrip = "https://34.203.15.64/patches/asiUpdate.html"
            sys-ctrl.char-fld = "Graphics\32x32\question_and_answer.ico"
            sys-ctrl.log-fld = TRUE
            sys-ctrl.securityLevelUser = 1000.
    END.
        
    /* BusinessFormLogo */
    RUN ipStatus ("  BusinessFormLogo").
    FOR EACH company:
        FIND FIRST sys-ctrl WHERE
            sys-ctrl.company EQ company.company AND 
            sys-ctrl.name EQ "BusinessFormLogo"
            NO-ERROR.
        IF NOT AVAIL sys-ctrl THEN DO: 
            CREATE sys-ctrl.
            ASSIGN 
                sys-ctrl.company = company.company
                sys-ctrl.name = "BusinessFormLogo"
                sys-ctrl.descrip = "Define the path to the logo to be used on the standard business forms"
                sys-ctrl.char-fld = ".\resources\images\nologo.png".
        END.
    END.
    
    /* 44453 - Deprecate MenuImage */
    RUN ipStatus ("  MenuImage").
    FOR EACH  sys-ctrl WHERE
        sys-ctrl.name EQ "MenuImage":
        DELETE sys-ctrl.
    END.
    
    /* 44448 - EnforceUserCount */
    RUN ipStatus ("  EnforceUserCount").
    FOR EACH company:
        FIND FIRST sys-ctrl WHERE
            sys-ctrl.company EQ company.company AND 
            sys-ctrl.name EQ "EnforceUserCount"
            NO-ERROR.
        IF NOT AVAIL sys-ctrl THEN DO: 
            CREATE sys-ctrl.
            ASSIGN 
                sys-ctrl.company = company.company
                sys-ctrl.name = "EnforceUserCount"
                sys-ctrl.descrip = "Enforce the limit on number of user logins?".
        END.
        ASSIGN
            sys-ctrl.log-fld = TRUE
            sys-ctrl.securityLevelUser = 1000.
    END.
    
    /* 48200 - Deprecate KEEPFROZEN */
    RUN ipStatus ("  KEEPFROZEN").
    FOR EACH  sys-ctrl WHERE
        sys-ctrl.name EQ "FGKEEPZEROBIN":
        DELETE sys-ctrl.
    END.
    FOR EACH  sys-ctrl WHERE
        sys-ctrl.name EQ "RMKEEPZEROBIN":
        DELETE sys-ctrl.
    END.
    
    ASSIGN 
        lSuccess = TRUE.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateTTIniFile C-Win 
PROCEDURE ipUpdateTTIniFile :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    ASSIGN 
        lSuccess = FALSE 
        iListEntry = LOOKUP(fiEnvironment:{&SV},cEnvList).    

    FIND ttIniFile WHERE ttIniFile.cVarName = "envVerList" NO-ERROR.
    ASSIGN ENTRY(iListEntry,ttIniFile.cVarValue) = fiToVer:{&SV}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateUserControl C-Win 
PROCEDURE ipUpdateUserControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Create/Update usercontrol record").

    DISABLE TRIGGERS FOR LOAD OF usercontrol.
    
    ASSIGN
        lSuccess = FALSE 
        iUserCount = INTEGER(fiLicensedUsers:{&SV}).
        
    /* Create/update usercontrol record */
    FIND FIRST usercontrol EXCLUSIVE NO-ERROR.
    IF NOT AVAIL usercontrol THEN
        CREATE usercontrol.
    IF usercontrol.numLicensedUsers = 0 
    OR usercontrol.numLicensedUsers = ? 
    OR usercontrol.rec_key = ? THEN DO:
        ASSIGN
            timeString = STRING(time,"HH:MM:SS")
            timeString = REPLACE(timeString,":","") + "00"
            iExtra = MAXIMUM(1,INTEGER(iUserCount / 10))
            usercontrol.numLicensedUsers = iUserCount
            usercontrol.maxAllowedUsers = iUserCount + iExtra
            usercontrol.maxSessionsPerUser = 2
            usercontrol.numUsersOverLimit = iExtra
            usercontrol.rec_key = STRING(year(today),"9999") +
                                 STRING(month(today),"99") +
                                 STRING(day(today),"99") +
                                 timeString.
        FIND FIRST rec_key WHERE
            rec_key.table_name = "usercontrol" AND
            rec_key.rec_key = usercontrol.rec_key
            NO-LOCK NO-ERROR.
        IF NOT AVAIL rec_key THEN DO:
            CREATE rec_key.
            ASSIGN
                rec_key.table_name = "usercontrol"
                rec_key.rec_key = usercontrol.rec_key.
        END.
    END.
    ELSE DO:
        ASSIGN
            usercontrol.numLicensedUsers = iUserCount.
    END.
    
    RELEASE usercontrol.
    ASSIGN 
        lSuccess = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUseOldNK1 C-Win 
PROCEDURE ipUseOldNK1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
    RUN ipStatus ("  Setting NK1 to old view").
    
    DISABLE TRIGGERS FOR LOAD OF prgrms.
    
    FIND FIRST prgrms EXCLUSIVE WHERE
        prgrms.prgmname EQ "sys-ctrl."
        NO-ERROR.
    IF AVAIL prgrms THEN ASSIGN
        prgrms.dir_group = "windows".
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValidateDB C-Win 
PROCEDURE ipValidateDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER oplValidDB AS LOG.
    
    ASSIGN
        oplValidDB = TRUE.
    
    CREATE ALIAS "DICTDB" FOR DATABASE asi.

    IF NOT CAN-FIND(FIRST _file WHERE 
                    _file._file-name EQ "xusermenu") THEN DO:
        ASSIGN
            oplValidDb = FALSE.
        MESSAGE
            "The connected database has not been upgraded to version 16.7." SKIP
            "Please use the database update program provided in folder" SKIP
            cMapDrive + "\Desktop\DBUpdate to correct this issue, then run" SKIP
            "this program again."
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipVendorMaxValue C-Win 
PROCEDURE ipVendorMaxValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Setting e-item-vend max values").

    DISABLE TRIGGERS FOR LOAD OF e-item-vend.

    FOR EACH e-item-vend EXCLUSIVE:
        IF e-item-vend.roll-w[28] EQ 0 THEN ASSIGN 
            e-item-vend.roll-w[28] = 999.000 .
        IF e-item-vend.roll-w[30] EQ 0 THEN ASSIGN 
            e-item-vend.roll-w[30] = 999.000 .
    END.
                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipVerifyNK1Changes C-Win 
PROCEDURE ipVerifyNK1Changes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Verifying NK1 Changes...").

    DEF VAR cLogFile AS CHAR NO-UNDO.
    DEF VAR cString AS CHAR NO-UNDO. 
    DEF VAR cChangeList AS CHAR NO-UNDO.
                   
    ASSIGN
        cLogFile = cUpdatesDir + "\" + "Patch" + fiToVer:{&SV} + "\installLog.txt"
        iMsgCtr = iMsgCtr + 1
        cMsgStr[iMsgCtr] = "Validating NK1 records...".
        
    DO:
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr[iMsgCtr] FORMAT "x(60)" AT 25
            SKIP.
        FOR EACH sys-ctrl NO-LOCK:
            FIND ttSysCtrl NO-LOCK WHERE
                ttSysCtrl.company = sys-ctrl.company AND
                ttSysCtrl.name = sys-ctrl.name
                NO-ERROR.
            IF NOT AVAIL ttSysCtrl THEN DO:
                ASSIGN
                    iMsgCtr = iMsgCtr + 1
                    cString = "ADDED NK1 - Company: " + sys-ctrl.company + " Name: " + sys-ctrl.name.
                PUT STREAM logStream
                    cString FORMAT "x(60)" AT 25.
            END.
            /* ELSE IF buffer-compare */
        END.    
        OUTPUT STREAM logStream CLOSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipWriteIniFile C-Win 
PROCEDURE ipWriteIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT TO VALUE(cIniLoc).
    FOR EACH ttIniFile BY ttIniFile.iPos:
        IF ttIniFile.cVarName BEGINS "#" THEN
            PUT UNFORMATTED ttIniFile.cVarName + CHR(10).
        ELSE IF ttIniFile.cVarName NE "" THEN DO:
            IF ttIniFile.cVarName EQ "modeList" THEN ASSIGN 
                ttIniFile.cVarValue = REPLACE(ttIniFile.cVarValue,"Addon,","").
            IF ttIniFile.cVarName EQ "pgmList" THEN ASSIGN 
                ttIniFile.cVarValue = REPLACE(ttIniFile.cVarValue,"system/addmain.w,","")
                ttIniFile.cVarValue = REPLACE(ttIniFile.cVarValue,"system/addmain2.w,","")
                .
            /* #53853 New 'mode': AutoLogout */
            IF ttIniFile.cVarName EQ "modeList"
            AND LOOKUP("AutoLogout",ttIniFile.cVarValue) EQ 0 THEN ASSIGN 
                ttIniFile.cVarName= ttIniFile.cVarValue + ",AutoLogout". 
            IF ttIniFile.cVarName EQ "pgmList"
            AND LOOKUP("userControl/monitor.w",ttIniFile.cVarValue) EQ 0 THEN ASSIGN 
                ttIniFile.cVarValue = ttIniFile.cVarValue + ",userControl/monitor.w". 
            PUT UNFORMATTED ttIniFile.cVarName + "=" + ttIniFile.cVarValue + CHR(10).
        END.
        ELSE NEXT.
    END.
    OUTPUT CLOSE.
    
    ASSIGN 
        lSuccess = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateVendItemCostFromEItemfgVend C-Win 
PROCEDURE pCreateVendItemCostFromEItemfgVend PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an e-item-vend buffer, create vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE PARAMETER BUFFER ipbf-e-itemfg      FOR e-itemfg.
    DEFINE OUTPUT PARAMETER opiVendItemCostID AS INT64.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE iIndex   AS INTEGER NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF bf-vendItemCost.
    DISABLE TRIGGERS FOR LOAD OF bf-vendItemCostLevel.
    DISABLE TRIGGERS FOR LOAD OF rec_key.
    
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ company.company AND 
        sys-ctrl.name EQ "VendCostMatrix"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN 
        glUseQtyFrom = sys-ctrl.log-fld.
    ELSE ASSIGN 
        glUseQtyFrom = FALSE.
        
    IF CAN-FIND(FIRST bf-vendItemCost
        WHERE bf-vendItemCost.company EQ ipbf-e-itemfg-vend.company
        AND bf-vendItemCost.itemID EQ ipbf-e-itemfg-vend.i-no
        AND bf-vendItemCost.itemType EQ "FG"
        AND bf-vendItemCost.vendorID EQ ipbf-e-itemfg-vend.vend-no
        AND bf-vendItemCost.customerID EQ ipbf-e-itemfg-vend.cust-no
        AND bf-vendItemCost.estimateNo EQ ipbf-e-itemfg-vend.est-no
        AND bf-vendItemCost.formNo EQ ipbf-e-itemfg-vend.form-no
        AND bf-vendItemCost.blankNo EQ ipbf-e-itemfg-vend.blank-no)
        THEN 
    DO:
        giCountDuplicate = giCountDuplicate + 1.
        CREATE ttDuplicates.
        ASSIGN 
            ttDuplicates.cCompany    = ipbf-e-itemfg-vend.company
            ttDuplicates.cItem       = ipbf-e-itemfg-vend.i-no
            ttDuplicates.cItemType   = "FG"
            ttDuplicates.cVendor     = ipbf-e-itemfg-vend.vend-no
            ttDuplicates.cEstimateNo = ipbf-e-itemfg-vend.est-no
            ttDuplicates.iForm       = ipbf-e-itemfg-vend.form-no
            ttDuplicates.iBlank      = ipbf-e-itemfg-vend.blank-no
            ttDuplicates.cCustomer   = ipbf-e-itemfg-vend.cust-no
            ttDuplicates.dEQty       = ipbf-e-itemfg-vend.eqty
            .
    END.
    ELSE 
    DO:
        CREATE bf-vendItemCost.
        ASSIGN  
            giCountCreated                   = giCountCreated + 1
            opiVendItemCostID                = bf-vendItemCost.vendItemCostID
            bf-vendItemCost.company          = ipbf-e-itemfg-vend.company
            bf-vendItemCost.itemID           = ipbf-e-itemfg-vend.i-no
            bf-vendItemCost.itemType         = "FG"
            bf-vendItemCost.vendorID         = ipbf-e-itemfg-vend.vend-no
            bf-vendItemCost.customerID       = ipbf-e-itemfg-vend.cust-no
            bf-vendItemCost.estimateNo       = ipbf-e-itemfg-vend.est-no
            bf-vendItemCost.formNo           = ipbf-e-itemfg-vend.form-no
            bf-vendItemCost.blankNo          = ipbf-e-itemfg-vend.blank-no
            bf-vendItemCost.dimWidthMinimum  = ipbf-e-itemfg-vend.roll-w[27]
            bf-vendItemCost.dimWidthMaximum  = ipbf-e-itemfg-vend.roll-w[28]
            bf-vendItemCost.dimLengthMinimum = ipbf-e-itemfg-vend.roll-w[29]
            bf-vendItemCost.dimLengthMaximum = ipbf-e-itemfg-vend.roll-w[30]
            bf-vendItemCost.dimUOM           = "IN"
            bf-vendItemCost.vendorItemID     = ipbf-e-itemfg-vend.vend-item
            bf-vendItemCost.vendorUOM        = CAPS(ipbf-e-itemfg.std-uom) 
            bf-vendItemCost.useQuantityFrom  = glUseQtyFrom
            /* Assignments from triggers */
            bf-vendItemCost.vendItemCostID = NEXT-VALUE(vendItemCostID_seq,ASI)
            bf-vendItemCost.createdID = USERID('ASI')
            bf-vendItemCost.createdDate = DATE(TODAY)
            bf-vendItemCost.rec_key = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")
                                                                 + STRING(DAY(TODAY),"99")
                                                                 + STRING(TIME,"99999")
                                                                 + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")      
            /* End trigger assignments */
            .
        
        DO iIndex = 1 TO 26:
            bf-vendItemCost.validWidth[iIndex] = IF ipbf-e-itemfg-vend.roll-w[iIndex] NE 0 
                THEN ipbf-e-itemfg-vend.roll-w[iIndex] 
                ELSE ipbf-e-itemfg.roll-w[iIndex].
        END.
        DO iIndex = 1 TO 10:
            IF ipbf-e-itemfg-vend.run-qty[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-itemfg-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-itemfg-vend.run-cost[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-itemfg-vend.setups[iIndex]
                    /* Assignments from triggers */
                    bf-vendItemCostLevel.vendItemCostLevelID = NEXT-VALUE(vendItemCostLevelID_seq,ASI)
                    bf-vendItemCostLevel.createdID = USERID('ASI')
                    bf-vendItemCostLevel.createdDate = DATE(TODAY)
                    bf-vendItemCostLevel.rec_key = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")
                                                                 + STRING(DAY(TODAY),"99")
                                                                 + STRING(TIME,"99999")
                                                                 + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")      
                    /* End trigger assignments */
                    .
            END. /*run-qty ne 0*/
        END.  /*Do loop 1*/              
    END. /*Not duplicate*/
    RELEASE bf-vendItemCost.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateVendItemCostFromEItemVend C-Win 
PROCEDURE pCreateVendItemCostFromEItemVend PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an e-item-vend buffer, create vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-e-item-vend FOR e-item-vend.
    DEFINE PARAMETER BUFFER ipbf-e-item      FOR e-item.
    DEFINE OUTPUT PARAMETER opiVendItemCostID AS INT64.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DISABLE TRIGGERS FOR LOAD OF bf-vendItemCost.
    DISABLE TRIGGERS FOR LOAD OF bf-vendItemCostLevel.
    DISABLE TRIGGERS FOR LOAD OF rec_key.

    DEFINE VARIABLE iIndex   AS INTEGER NO-UNDO.
        
    IF CAN-FIND(FIRST bf-vendItemCost
        WHERE bf-vendItemCost.company EQ ipbf-e-item-vend.company
        AND bf-vendItemCost.itemID EQ ipbf-e-item-vend.i-no
        AND bf-vendItemCost.itemType EQ "RM"
        AND bf-vendItemCost.vendorID EQ ipbf-e-item-vend.vend-no)
        THEN 
    DO:
        giCountDuplicate = giCountDuplicate + 1.
        CREATE ttDuplicates.
        ASSIGN 
            ttDuplicates.cCompany  = ipbf-e-item-vend.company
            ttDuplicates.cItem     = ipbf-e-item-vend.i-no
            ttDuplicates.cItemType = "RM"
            ttDuplicates.cVendor   = ipbf-e-item-vend.vend-no
            .
    END.
    ELSE 
    DO:
        CREATE bf-vendItemCost.
        ASSIGN  
            giCountCreated                       = giCountCreated + 1
            opiVendItemCostID                    = bf-vendItemCost.vendItemCostID
            bf-vendItemCost.company              = ipbf-e-item-vend.company
            bf-vendItemCost.itemID               = ipbf-e-item-vend.i-no
            bf-vendItemCost.itemType             = "RM"
            bf-vendItemCost.vendorID             = ipbf-e-item-vend.vend-no
            bf-vendItemCost.dimWidthMinimum      = ipbf-e-item-vend.roll-w[27]
            bf-vendItemCost.dimWidthMaximum      = ipbf-e-item-vend.roll-w[28]
            bf-vendItemCost.dimLengthMinimum     = ipbf-e-item-vend.roll-w[29]
            bf-vendItemCost.dimLengthMaximum     = ipbf-e-item-vend.roll-w[30]
            bf-vendItemCost.dimWidthUnder        = ipbf-e-item-vend.underWidth
            bf-vendItemCost.dimWidthUnderCharge  = ipbf-e-item-vend.underWidthCost
            bf-vendItemCost.dimLengthUnder       = ipbf-e-item-vend.underLength
            bf-vendItemCost.dimLengthUnderCharge = ipbf-e-item-vend.underLengthCost
            bf-vendItemCost.dimUOM               = "IN"
            bf-vendItemCost.vendorItemID         = ipbf-e-item-vend.vend-item
            bf-vendItemCost.vendorUOM            = CAPS(ipbf-e-item.std-uom) 
            /* Assignments from triggers */
            bf-vendItemCost.vendItemCostID = NEXT-VALUE(vendItemCostID_seq,ASI)
            bf-vendItemCost.createdID = USERID('ASI')
            bf-vendItemCost.createdDate = DATE(TODAY)
            bf-vendItemCost.rec_key = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")
                                                                 + STRING(DAY(TODAY),"99")
                                                                 + STRING(TIME,"99999")
                                                                 + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")      
            /* End trigger assignments */
            .
        DO iIndex = 1 TO 26:
            bf-vendItemCost.validWidth[iIndex] = IF ipbf-e-item-vend.roll-w[iIndex] NE 0 
                THEN ipbf-e-item-vend.roll-w[iIndex] 
                ELSE ipbf-e-item.roll-w[iIndex].
        END.
        DO iIndex = 1 TO 10:
            IF ipbf-e-item-vend.run-qty[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-item-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-item-vend.run-cost[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-item-vend.setups[iIndex]
                    /* Assignments from triggers */
                    bf-vendItemCostLevel.vendItemCostLevelID = NEXT-VALUE(vendItemCostLevelID_seq,ASI)
                    bf-vendItemCostLevel.createdID = USERID('ASI')
                    bf-vendItemCostLevel.createdDate = DATE(TODAY)
                    bf-vendItemCostLevel.rec_key = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")
                                                                 + STRING(DAY(TODAY),"99")
                                                                 + STRING(TIME,"99999")
                                                                 + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")      
                    /* End trigger assignments */
                    .
            END. /*run-qty ne 0*/
        END.  /*Do loop 1*/              
        DO iIndex = 1 TO 10:
            IF ipbf-e-item-vend.runQtyXtra[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-item-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-item-vend.runCostXtra[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-item-vend.setupsXtra[iIndex]
                    /* Assignments from triggers */
                    bf-vendItemCostLevel.vendItemCostLevelID = NEXT-VALUE(vendItemCostLevelID_seq,ASI)
                    bf-vendItemCostLevel.createdID = USERID('ASI')
                    bf-vendItemCostLevel.createdDate = DATE(TODAY)
                    bf-vendItemCostLevel.rec_key = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")
                                                                 + STRING(DAY(TODAY),"99")
                                                                 + STRING(TIME,"99999")
                                                                 + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")      
                    /* End trigger assignments */
                    .
            END. /*runQtyExtra ne 0*/
        END.  /*Do loop 2*/              
    END. /*Not duplicate*/
    RELEASE bf-vendItemCost.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TempTableToCSV C-Win 
PROCEDURE TempTableToCSV:
    /*------------------------------------------------------------------------------ 
     Purpose: Exports the contents of any temp-table into CSV    
     Notes: 
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
  
    DEFINE VARIABLE hQuery  AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE eIndex  AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cTTName AS CHARACTER NO-UNDO. 
        
    ASSIGN
        cTTName = iphTT:NAME
        hBuffer = iphTT:DEFAULT-BUFFER-HANDLE
        .

    IF iplHeader THEN 
    DO:
        OUTPUT STREAM sOutput to VALUE(ipcFileName). 
        DO iIndex = 1 TO hBuffer:NUM-FIELDS: 
            IF hBuffer:BUFFER-FIELD(iIndex):EXTENT GT 0 THEN 
            DO:
                DO eIndex = 1 to hBuffer:BUFFER-FIELD(iIndex):EXTENT:
                    PUT STREAM sOutput UNFORMATTED hBuffer:BUFFER-FIELD(iIndex):COLUMN-LABEL + STRING(eIndex) + 
                        (IF iIndex EQ hBuffer:NUM-FIELDS AND eIndex EQ hBuffer:BUFFER-FIELD(iIndex):EXTENT THEN '' ELSE ',').
                END.
            END.
            ELSE
                PUT STREAM sOutput UNFORMATTED hBuffer:BUFFER-FIELD(iIndex):COLUMN-LABEL + 
                    (IF iIndex NE hBuffer:NUM-FIELDS THEN "," ELSE ""). 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END.
    ELSE 
        OUTPUT STREAM sOutput to VALUE(ipcFileName) APPEND. 
        
    CREATE QUERY hQuery. 
    hQuery:SET-BUFFERS (hBuffer). 
    hQuery:QUERY-PREPARE("FOR EACH " + cTTName). 
    hQuery:QUERY-OPEN().
    REPEAT:   
        hQuery:GET-NEXT().   
        IF hQuery:QUERY-OFF-END THEN LEAVE.   
        DO iIndex = 1 TO hBuffer:NUM-FIELDS: 
            IF hBuffer:BUFFER-FIELD(iIndex):EXTENT GT 0 THEN 
            DO:
                DO eIndex = 1 to hBuffer:BUFFER-FIELD(iIndex):EXTENT:
                    PUT STREAM sOutput UNFORMATTED  
                        '"' FormatForCSV(hBuffer:BUFFER-FIELD(iIndex):BUFFER-VALUE(eIndex)) 
                        (IF iIndex EQ hBuffer:NUM-FIELDS AND eIndex EQ hBuffer:BUFFER-FIELD(iIndex):EXTENT THEN '"' ELSE '",').
                END.
            END.
            ELSE
                PUT STREAM sOutput UNFORMATTED  
                    '"' FormatForCSV(hBuffer:BUFFER-FIELD(iIndex):BUFFER-VALUE) 
                    (IF iIndex NE hBuffer:NUM-FIELDS THEN '",' ELSE '"'). 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END. 
    OUTPUT STREAM sOutput CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFixYear C-Win 
FUNCTION fFixYear RETURNS DATE
  (INPUT daDate AS DATE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE outDate AS DATE NO-UNDO.
    
    IF daDate EQ ? THEN RETURN ?.
    ELSE DO:
        IF YEAR(daDate) LT 2000 
        AND YEAR(daDate) GT 50 THEN ASSIGN 
            outDate = DATE(MONTH(daDate), DAY(daDate), YEAR(daDate + 1900)).  
        ELSE IF YEAR(daDate) LT 2000 THEN ASSIGN 
            outDate = DATE(MONTH(daDate), DAY(daDate), YEAR(daDate + 2000)). 
        ELSE ASSIGN
            outDate = daDate.
        RETURN outDate.
    END. 
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ):
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
        iIntVal[3] = IF LENGTH(cStrVal[3]) EQ 1 THEN INT(cStrVal[3]) * 10 ELSE INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION formatForCSV C-Win 
FUNCTION FormatForCSV RETURNS CHARACTER 
    ( ipcValue AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Fixes the input character value and returns a CSV friendly text
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cInvalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
    DEFINE VARIABLE cReplaceChars AS CHARACTER NO-UNDO INITIAL "'',". 
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    
 
    DO iCount = 1 TO NUM-ENTRIES(cInvalidChars):
        ipcValue = REPLACE(ipcValue,ENTRY(iCount,cInvalidChars),ENTRY(iCount,cReplaceChars)).
    END.
    RETURN ipcValue.   
        
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


