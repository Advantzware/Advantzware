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

DEF TEMP-TABLE ttAuditTbl LIKE AuditTbl.
DEF TEMP-TABLE ttAuditFld LIKE AuditFld.
DEF TEMP-TABLE ttCueCard LIKE cueCard.
DEF TEMP-TABLE ttCueCardText LIKE cueCardText.
DEF TEMP-TABLE ttPrgrms LIKE prgrms.
DEF TEMP-TABLE ttPrgmxref LIKE prgmxref.
DEF TEMP-TABLE ttDynPrgrmsPage LIKE dynPrgrmsPage.
DEF TEMP-TABLE ttEmailcod LIKE emailcod.
DEF TEMP-TABLE ttNotes LIKE notes.
DEF TEMP-TABLE ttModule LIKE module.
DEF TEMP-TABLE ttLookups LIKE lookups.
DEF TEMP-TABLE ttReftable LIKE reftable.
DEF TEMP-TABLE ttSysCtrl LIKE sys-ctrl.
DEF TEMP-TABLE ttSys-Ctrl LIKE sys-ctrl.
DEF TEMP-TABLE ttSys-Ctrl-Shipto LIKE sys-ctrl-shipto.
DEF TEMP-TABLE ttTranslation LIKE translation.
DEF TEMP-TABLE ttUserLanguage LIKE userlanguage.
DEF TEMP-TABLE ttXuserMenu LIKE xuserMenu.
DEF TEMP-TABLE ttUtilities LIKE utilities.
DEF TEMP-TABLE ttZmessage LIKE zMessage.
DEF TEMP-TABLE ttEmailConfig LIKE emailConfig.
DEF TEMP-TABLE ttServerResource LIKE serverResource.

DEF TEMP-TABLE ttAPIOutbound 
    FIELD apiOutboundID AS INT64 
    FIELD username AS CHAR 
    FIELD password AS CHAR 
    FIELD endpoint AS CHAR.        
 
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
DEF VAR hVendCostProcs AS HANDLE NO-UNDO.

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

&SCOPED-DEFINE cFile AuditFld

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

&SCOPED-DEFINE cFile dynSubjectParamSet
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

&SCOPED-DEFINE cFile dynPrgrmsPage
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynLookup
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynValueParam
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynValueColumn
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}." + ipcType) NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile dynValueParamSet
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
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeCostMethod C-Win
PROCEDURE ipChangeCostMethod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Change cost method in IF2").

    
    DEF BUFFER bFg-ctrl FOR fg-ctrl.
    
    DISABLE TRIGGERS FOR LOAD OF fg-ctrl.
    DISABLE TRIGGERS FOR LOAD OF bfg-ctrl.

    FOR EACH bFg-ctrl EXCLUSIVE
        WHERE bFg-ctrl.inv-meth EQ "S":
        ASSIGN 
            bFg-ctrl.inv-meth = "L".
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConfirmMonitorUser C-Win 
PROCEDURE ipConfirmMonitorUser :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Validating Monitor User Entries").

    DISABLE TRIGGERS FOR LOAD OF users.

    FIND users EXCLUSIVE WHERE 
        users.user_id = "monitor"
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
        users.securityLevel = 100
        users.showOnAck = false
        users.showOnBol = false
        users.showOnInv = false
        users.showOnPO = false
        users.showOnQuote = false
        users.track_usage = true
        users.updateDate = today
        users.updateTime = time
        users.updateUser = "monitor"
        users.userType = "Administrator"
        users.user_language = "English"
        users.user_name = "ASI Monitor Runner Account"
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
    
    RUN ipSetMonitorPwd IN THIS-PROCEDURE.
    RUN ipAddSuppUserRecords IN THIS-PROCEDURE (INPUT users.user_id).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertDynParam C-Win 
PROCEDURE ipConvertDynParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Converting dynValue tables...").

    DISABLE TRIGGERS FOR LOAD OF dynValueParam.
    DISABLE TRIGGERS FOR LOAD OF dynValueParamSet.
    DISABLE TRIGGERS FOR LOAD OF dynValueColumn.

    DEFINE VARIABLE hSession AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER NO-UNDO.

    FOR EACH dynValueParam EXCLUSIVE-LOCK:
        DELETE dynValueParam.
    END. /* each dynValueParam */

    FOR EACH dynValueParamSet EXCLUSIVE-LOCK:
        DELETE dynValueParamSet.
    END. /* each dynValueParam */

    FOR EACH dynValueColumn EXCLUSIVE-LOCK:
        DELETE dynValueColumn.
    END. /* each dynValueParam */

    FOR EACH dynParamValue NO-LOCK:
        DO idx = 1 TO EXTENT(dynParamValue.paramSetID):
            IF dynParamValue.paramSetID[idx] EQ 0 THEN LEAVE.
            CREATE dynValueParamSet.
            ASSIGN
                dynValueParamSet.subjectID    = dynParamValue.subjectID
                dynValueParamSet.user-id      = dynParamValue.user-id
                dynValueParamSet.prgmName     = dynParamValue.prgmName
                dynValueParamSet.paramValueID = dynParamValue.paramValueID
                dynValueParamSet.sortOrder    = idx
                dynValueParamSet.paramSetID   = dynParamValue.paramSetID[idx]
                dynValueParamSet.isVisible    = dynParamValue.isVisible[idx]
                .
        END. /* do idx */
        DO idx = 1 TO EXTENT(dynParamValue.paramName):
            IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
            CREATE dynValueParam.
            ASSIGN
                dynValueParam.subjectID    = dynParamValue.subjectID
                dynValueParam.user-id      = dynParamValue.user-id
                dynValueParam.prgmName     = dynParamValue.prgmName
                dynValueParam.paramValueID = dynParamValue.paramValueID
                dynValueParam.sortOrder    = idx
                dynValueParam.paramName    = dynParamValue.paramName[idx]
                dynValueParam.paramLabel   = dynParamValue.paramLabel[idx]
                dynValueParam.paramValue   = dynParamValue.paramValue[idx]
                dynValueParam.dataType     = dynParamValue.paramDataType[idx]
                dynValueParam.paramFormat  = dynParamValue.paramFormat[idx]
                .
        END. /* do idx */
        DO idx = 1 TO EXTENT(dynParamValue.colName):
            IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
            CREATE dynValueColumn.
            ASSIGN
                dynValueColumn.subjectID      = dynParamValue.subjectID
                dynValueColumn.user-id        = dynParamValue.user-id
                dynValueColumn.prgmName       = dynParamValue.prgmName
                dynValueColumn.paramValueID   = dynParamValue.paramValueID
                dynValueColumn.sortOrder      = idx
                dynValueColumn.isActive       = dynParamValue.isActive[idx]
                dynValueColumn.colName        = dynParamValue.colName[idx]
                dynValueColumn.colLabel       = dynParamValue.colLabel[idx]
                dynValueColumn.colFormat      = dynParamValue.colFormat[idx]
                dynValueColumn.columnSize     = dynParamValue.columnSize[idx]
                dynValueColumn.dataType       = dynParamValue.dataType[idx]
                dynValueColumn.sortCol        = dynParamValue.sortCol[idx]
                dynValueColumn.sortDescending = dynParamValue.sortDescending[idx]
                dynValueColumn.isGroup        = dynParamValue.isGroup[idx]
                dynValueColumn.isReturnValue  = dynParamValue.isReturnValue[idx]
                dynValueColumn.isSearchable   = dynParamValue.isSearchable[idx]
                dynValueColumn.isSortable     = dynParamValue.isSortable[idx]
                dynValueColumn.groupLabel     = dynParamValue.groupLabel[idx]
                dynValueColumn.groupCalc      = dynParamValue.groupCalc[idx]
                dynValueColumn.isCalcField    = dynParamValue.isCalcField[idx]
                dynValueColumn.calcProc       = dynParamValue.calcProc[idx]
                dynValueColumn.calcParam      = dynParamValue.calcParam[idx]
                dynValueColumn.calcFormula    = dynParamValue.calcFormula[idx]
                .
        END. /* do idx */
    END. /* each dynParamValue */

    OUTPUT TO c:\tmp\dynParamValue.save.d.
    FOR EACH dynParamValue NO-LOCK:
        EXPORT dynParamValue.
    END. /* each dynparamvalue */
    OUTPUT CLOSE.
    
    RELEASE dynValueParamSet.
    RELEASE dynValueParam.
    RELEASE dynValueColumn.
    RELEASE dynParamValue.

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

    DEF VAR iVendCostItemID AS INT64 NO-UNDO.
    DEF VAR cOrigPropath AS CHAR NO-UNDO.
    DEF VAR cNewPropath AS CHAR NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
    DEFINE VARIABLE hTags AS HANDLE NO-UNDO.
    DEFINE VARIABLE hCommonProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hCreditProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hPurgeProcs AS HANDLE NO-UNDO.

    ASSIGN
        cOrigPropath = PROPATH
        cNewPropath  = cEnvDir + "\" + fiEnvironment:{&SV} + "\Programs," + PROPATH
        PROPATH = cNewPropath.
    
        IF NOT VALID-HANDLE(hSession) THEN DO:
            RUN system/session.p PERSISTENT SET hSession.
            SESSION:ADD-SUPER-PROCEDURE (hSession).
        END. 
        IF NOT VALID-HANDLE(hTags) THEN DO: 
            RUN system/TagProcs.p PERSISTENT SET hTags.
            SESSION:ADD-SUPER-PROCEDURE (hTags).
        END.
        IF NOT VALID-HANDLE(hCommonProcs) THEN DO: 
            RUN system/commonProcs.p PERSISTENT SET hCommonProcs.
            SESSION:ADD-SUPER-PROCEDURE (hCommonProcs).
        END.
        IF NOT VALID-HANDLE(hCreditProcs) THEN DO:
            RUN system/creditProcs.p PERSISTENT SET hCreditProcs.
            SESSION:ADD-SUPER-PROCEDURE (hCreditProcs).
        END.
        IF NOT VALID-HANDLE(hPurgeProcs) THEN DO:
            RUN system/purgeProcs.p PERSISTENT SET hPurgeProcs.
            SESSION:ADD-SUPER-PROCEDURE (hPurgeProcs).
        END.

    RUN util/dev/VendorCostConvProcs PERSISTENT SET hVendCostProcs.
    FOR EACH company NO-LOCK:
        RUN ConvertLegacyToNew IN hVendCostProcs (company.company,
                            TRUE, /* Convert Farm */
                            TRUE, /* Convert RM */
                            TRUE, /* Convert FG */
                            FALSE, /* Include Inactive FG */
                            FALSE, /* Include Inactive Vend */
                            FALSE, /* Include Inactive Cust */
                            "c:\tmp\VendCostConv.txt", 
                            OUTPUT lError, 
                            OUTPUT cMessage).
    END.
    
    ASSIGN 
        PROPATH = cOrigPropath.
    DELETE OBJECT hVendCostProcs.        
    
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateDataLoader C-Win
PROCEDURE ipCreateDataLoader:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("    Create DataLoader folder and files...").

    DEF VAR cCurrentDir AS CHAR NO-UNDO.
    
    /* Ensure folder available for custom menus */
    ASSIGN
        cCurrentDir = cEnvDir + "\" +
                      fiEnvironment:{&SV} + "\CustFiles\DataLoader".
    OS-CREATE-DIR VALUE(cCurrentDir).
    OS-COPY VALUE(cUpdDataDir + "\TestFile.txt") VALUE(cCurrentDir).
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateMonitorUser C-Win 
PROCEDURE ipCreateMonitorUser :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating Monitor User").

    DISABLE TRIGGERS FOR LOAD OF users.

    CREATE users.
    ASSIGN
        users.user_id = "monitor"
        users.createDate = today
        users.createTime = time
        users.createUser = USERID(LDBNAME(1)).
        
    RUN ipConfirmMonitorUser IN THIS-PROCEDURE.

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
    IF fIntVer(cThisEntry) LT 16140000 THEN 
        RUN ipDataFix161400.
    IF fIntVer(cThisEntry) LT 16140100 THEN  
        RUN ipDataFix161401.
    IF fIntVer(cThisEntry) LT 16150000 THEN  
        RUN ipDataFix161500.
    IF fIntVer(cThisEntry) LT 20010000 THEN  
        RUN ipDataFix200100.
    IF fIntVer(cThisEntry) LT 20011000 THEN  
        RUN ipDataFix200110.
    IF fIntVer(cThisEntry) LT 20020000 THEN  
        RUN ipDataFix200200.
    IF fIntVer(cThisEntry) LT 20020200 THEN 
        RUN ipDataFix200202.
    IF fIntVer(cThisEntry) LT 20030300 THEN 
        RUN ipDataFix200303.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160890 C-Win 
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

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix161300 C-Win
PROCEDURE ipDataFix161300:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161300...").
    
    RUN ipLoadOEAutoApproveNK1s.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix161400 C-Win
PROCEDURE ipDataFix161400:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161400...").
    
    RUN ipCreateDataLoader.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix161401 C-Win
PROCEDURE ipDataFix161401:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161401...").
    
    RUN ipConvertVendorCosts.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix161500 C-Win
PROCEDURE ipDataFix161500:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 161500...").
    

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix200100 C-Win
PROCEDURE ipDataFix200100:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 200100...").

    DISABLE TRIGGERS FOR LOAD OF sys-ctrl.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
    DISABLE TRIGGERS FOR LOAD OF userPwdHist.
    DISABLE TRIGGERS FOR LOAD OF inventoryStatusType.

/*  64833 Set by default to output Estimate data */
    FOR EACH sys-ctrl WHERE 
        sys-ctrl.name EQ "CECostSave":
        ASSIGN 
            sys-ctrl.log-fld = TRUE
            sys-ctrl.int-fld = 0.
    END. 
     
    /* 64504 Credit hold - Remove any change that turns off Credit Hold upon upgrade */
    FOR EACH sys-ctrl WHERE 
        sys-ctrl.name EQ "CreditHold":
        ASSIGN 
            sys-ctrl.log-fld = TRUE.
    END. 
    
    /* 64876 - Encrypt passwords in userPwdHist */
    FOR EACH userPwdHist:
        IF userPwdHist.pwd NE ENCODE(userPwdHist.pwd) THEN ASSIGN 
            userPwdHist.pwd = ENCODE(userPwdHist.pwd).
    END.
    
    /* 64885 - Load inventoryStatusType data */
    INPUT FROM VALUE(cUpdDataDir + "\inventoryStatusType.d") NO-ECHO.
    REPEAT:
        CREATE inventoryStatusType.
        IMPORT inventoryStatusType.
    END.
    
            
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix200110 C-Win
PROCEDURE ipDataFix200110:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 200110...").

    /* Ticket #68616 Additional security to API Inbound and Outbound */
    &SCOPED-DEFINE cTable apiinbound
    FIND LAST {&cTable} NO-LOCK NO-ERROR.
    IF AVAIL {&cTable} THEN DO:
        IF {&cTable}.{&cTable}ID LT 5000 THEN ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = 5000.
        ELSE ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = {&cTable}.{&cTable}ID.
    END.
    ELSE ASSIGN 
        CURRENT-VALUE({&cTable}ID_seq) = 5000.
            
    &SCOPED-DEFINE cTable apiinbounddetail
    FIND LAST {&cTable} NO-LOCK NO-ERROR.
    IF AVAIL {&cTable} THEN DO:
        IF {&cTable}.{&cTable}ID LT 5000 THEN ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = 5000.
        ELSE ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = {&cTable}.{&cTable}ID.
    END.
    ELSE ASSIGN 
        CURRENT-VALUE({&cTable}ID_seq) = 5000.
            
    &SCOPED-DEFINE cTable apioutbound
    FIND LAST {&cTable} NO-LOCK NO-ERROR.
    IF AVAIL {&cTable} THEN DO:
        IF {&cTable}.{&cTable}ID LT 5000 THEN ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = 5000.
        ELSE ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = {&cTable}.{&cTable}ID.
    END.
    ELSE ASSIGN 
        CURRENT-VALUE({&cTable}ID_seq) = 5000.
            
    &SCOPED-DEFINE cTable apioutbounddetail
    FIND LAST {&cTable} NO-LOCK NO-ERROR.
    IF AVAIL {&cTable} THEN DO:
        IF {&cTable}.{&cTable}ID LT 5000 THEN ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = 5000.
        ELSE ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = {&cTable}.{&cTable}ID.
    END.
    ELSE ASSIGN 
        CURRENT-VALUE({&cTable}ID_seq) = 5000.
            
    &SCOPED-DEFINE cTable apioutboundtrigger
    FIND LAST {&cTable} NO-LOCK NO-ERROR.
    IF AVAIL {&cTable} THEN DO:
        IF {&cTable}.{&cTable}ID LT 5000 THEN ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = 5000.
        ELSE ASSIGN 
            CURRENT-VALUE({&cTable}ID_seq) = {&cTable}.{&cTable}ID.
    END.
    ELSE ASSIGN 
        CURRENT-VALUE({&cTable}ID_seq) = 5000.
            
    RUN ipConvertDynParam.
            
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix200200 C-Win
PROCEDURE ipDataFix200200:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 200200...").

    /* Conversion program for ticket #69261 */
    /* The field stax.tax-rate[5] value will be moved to new field 
       stax.taxableLimit, when the stax.tax-dscr1[5] field's value is "Dollor Limit" */
    DISABLE TRIGGERS FOR LOAD OF stax.
    DO TRANSACTION:
        FOR EACH stax EXCLUSIVE-LOCK:
            IF stax.tax-dscr1[5] EQ "Dollar Limit" THEN
                ASSIGN
                    stax.taxableLimit = stax.tax-rate1[5]
                    stax.tax-rate1[5] = 0
                    stax.tax-dscr1[5] = ""
                    stax.tax-code1[5] = ""
                    stax.tax-frt1[5]  = FALSE
                    .
        END.
    END.
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix200202 C-Win
PROCEDURE ipDataFix200202:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 200202...").

    /* 85663 - Sales/Commission Reporting Definitions */
    DISABLE TRIGGERS FOR LOAD OF account.
    FOR EACH account EXCLUSIVE WHERE
        account.type EQ "R": /* Revenue accounts only */
        ASSIGN 
            account.salesReport = TRUE 
            account.commReport = TRUE.
    END. 
     
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix200303 C-Win
PROCEDURE ipDataFix200303:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 200303...").

  /* from Jay's known issues list */
    /* Deactivate estimate type filter in DAOA */
    DISABLE TRIGGERS FOR LOAD OF dynSubject.
    FIND FIRST dynSubject EXCLUSIVE WHERE
        dynSubject.subjectID EQ 99
        NO-ERROR.
    IF AVAIL dynSubject THEN ASSIGN 
        dynSubject.isActive = FALSE. 
        
    /* Remove new SharpShooter menu */
    DISABLE TRIGGERS FOR LOAD OF prgrms.
    FIND FIRST prgrms EXCLUSIVE WHERE 
        prgrms.prgmname = "ssMenu."
        NO-ERROR.
    IF AVAIL prgrms THEN DELETE prgrms.
    
    /* Set oe-ctrl flag for jobs on hold */
    DISABLE TRIGGERS FOR LOAD OF oe-ctrl.
    FOR EACH oe-ctrl EXCLUSIVE:
        ASSIGN 
            oe-ctrl.p-job = TRUE.
    END.
    /* Set default Gain/Loss accounts in currency records */    
    RUN ipSetCurrencyAccounts.
     
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
    RUN ipLoadDAOAData.
    RUN ipLoadAPIConfigData.
    RUN ipLoadAPIData.
    RUN ipSetCueCards.
/*    RUN ipDeleteAudit.*/
    RUN ipCleanTemplates.
    RUN ipLoadEstCostData.
    RUN ipChangeCostMethod.
    
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
    DISABLE TRIGGERS FOR LOAD OF auditFld.
    
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
        FOR EACH AuditFld:
            ASSIGN
                AuditFld.Audit = NO.
        END.
    END.
    ELSE DO:
        RUN ipStatus ("    Deleting audit records older than 180 days...").
        RUN ipStatus ("      (30 minute limit on this process)").
        FOR EACH AuditHdr NO-LOCK WHERE 
            DATE(auditHdr.auditDateTime) LT TODAY - 180:
            FOR EACH AuditDtl OF auditHdr NO-LOCK:
                FIND CURRENT AuditDtl EXCLUSIVE-LOCK NO-WAIT.
                IF AVAIL AuditDtl THEN DO:
                    DELETE AuditDtl.
                    ASSIGN
                        iDelCount = iDelCount + 1.
                END.
            END.
            FIND CURRENT AuditHdr EXCLUSIVE-LOCK NO-WAIT.
            IF AVAIL auditHdr THEN DO:
                DELETE AuditHdr.
                ASSIGN
                    iDelCount = iDelCount + 1.
            END.
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

    /* Copy DataDigger saved files to CustFiles\DDBackups */
    OS-CREATE-DIR VALUE(cTgtEnv + "\CustFiles\DDBackups").
    OS-CREATE-DIR VALUE(cTgtEnv + "\CustFiles\DDBackups\Cache").
    OS-COPY VALUE(cTgtEnv + "\Programs\DataDigger\Datadigger-*.ini") VALUE(cTgtEnv + "\CustFiles\DDBackups").
    OS-COPY VALUE(cTgtEnv + "\Programs\DataDigger\Cache\*.*") VALUE(cTgtEnv + "\CustFiles\DDBackups\Cache").
    
    /* Unzip/move breaks for any number of security reasons; just copy  */
    RUN ipStatus ("  Copying system files from ").
    RUN ipStatus ("    " + cUpdProgramDir + " to").
    RUN ipStatus ("    " + cTgtEnv).

    OS-COPY VALUE(cUpdProgramDir + "\Override\*.*") VALUE(cTgtEnv + "\Override").
    OS-COPY VALUE(cUpdProgramDir + "\Resources\*.*") VALUE(cTgtEnv + "\Resources").
    OS-COPY VALUE(cUpdProgramDir + "\Programs\*.*") VALUE(cTgtEnv + "\Programs").

    /* Now restore DD files from backed up copies and remove Backup dirs */
    OS-COPY VALUE(cTgtEnv + "\CustFiles\DDBackups\*.*") VALUE(cTgtEnv + "\Programs\DataDigger").
    OS-COPY VALUE(cTgtEnv + "\CustFiles\DDBackups\Cache\*.*") VALUE(cTgtEnv + "\\Programs\DataDigger\Cache").
    OS-DELETE VALUE(cTgtEnv + "\CustFiles\DDBackups") RECURSIVE.
    
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

    IF NOT CAN-FIND (FIRST users WHERE
        users.user_id = "monitor") THEN
        RUN ipCreateMonitorUser IN THIS-PROCEDURE.
    ELSE 
        RUN ipConfirmMonitorUser IN THIS-PROCEDURE.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadAPIConfigData C-Win
PROCEDURE ipLoadAPIConfigData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading API Config Data").

    /* Only load any new records created on DEVEL */
    &SCOPED-DEFINE tablename serverResource
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
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

    &SCOPED-DEFINE tablename emailConfig
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\APIData\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.configID EQ tt{&tablename}.configID 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadAPIData C-Win
PROCEDURE ipLoadAPIData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    RUN ipStatus ("  Loading API Data Files").

    DISABLE TRIGGERS FOR LOAD OF APIInbound.
    DISABLE TRIGGERS FOR LOAD OF APIInboundDetail.
    DISABLE TRIGGERS FOR LOAD OF APIOutbound.
    DISABLE TRIGGERS FOR LOAD OF APIOutboundDetail.
    DISABLE TRIGGERS FOR LOAD OF APIOutboundTrigger.
    DISABLE TRIGGERS FOR LOAD OF APIClient.
    DISABLE TRIGGERS FOR LOAD OF APIClientXref.

&SCOPED-DEFINE tablename APIInbound
    FOR EACH {&tablename} WHERE {&tablename}.{&tablename}ID LT 5000:
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
    FOR EACH {&tablename} WHERE {&tablename}.{&tablename}ID LT 5000:
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
    FOR EACH {&tablename} WHERE {&tablename}.{&tablename}ID LT 5000:
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
    FOR EACH {&tablename} WHERE {&tablename}.{&tablename}ID LT 5000:
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
    FOR EACH {&tablename} WHERE {&tablename}.{&tablename}ID LT 5000:
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

&SCOPED-DEFINE tablename APIClient
    FOR EACH {&tablename} WHERE {&tableName}.clientID BEGINS "_default":
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

&SCOPED-DEFINE tablename APIClientXref
    FOR EACH {&tablename} WHERE {&tableName}.clientID BEGINS "_default":
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
            /* Ensure OUR defaults are set */
            ASSIGN 
                {&tableName}.auditCreateDefault = tt{&tableName}.auditCreateDefault
                {&tableName}.auditUpdateDefault = tt{&tableName}.auditUpdateDefault
                {&tableName}.auditDeleteDefault = tt{&tableName}.auditDeleteDefault
                {&tableName}.auditStackDefault = tt{&tableName}.auditStackDefault
                {&tableName}.ExpireDaysDefault = tt{&tableName}.ExpireDaysDefault
                .
            /* and make sure THEIR activation is AT LEAST the default */
            ASSIGN 
                {&tableName}.auditCreate = IF {&tableName}.auditCreateDefault THEN TRUE ELSE {&tableName}.auditCreate
                {&tableName}.auditUpdate = IF {&tableName}.auditUpdateDefault THEN TRUE ELSE {&tableName}.auditUpdate
                {&tableName}.auditDelete = IF {&tableName}.auditDeleteDefault THEN TRUE ELSE {&tableName}.auditDelete
                {&tableName}.auditStack = IF {&tableName}.auditStackDefault THEN TRUE ELSE {&tableName}.auditStack
                {&tableName}.expireDays = IF {&tableName}.ExpireDaysDefault NE 0 THEN {&tableName}.ExpireDaysDefault ELSE {&tableName}.ExpireDays
                . 
            
        END.
    END.
    INPUT CLOSE.

    /* Now remove any base records that are not in the tt */
    FOR EACH {&tablename} EXCLUSIVE WHERE
        NOT CAN-FIND(FIRST tt{&tablename} WHERE tt{&tablename}.auditTable = {&tablename}.auditTable ):
        DELETE {&tablename}.
    END.
    
            
    EMPTY TEMP-TABLE tt{&tablename}.
    
    RUN ipStatus ("  Loading AuditFld Records").

    &SCOPED-DEFINE tablename auditfld
    
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    
    /* First section reads the .d and creates records that aren't already there 
        (unless they're in the exception lis) */
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE tt{&tablename}.
        IMPORT tt{&tablename}.
        /* Otherwise create the base record from the .d */
        DO:
            FIND {&tablename} EXCLUSIVE WHERE 
                {&tablename}.auditTable EQ tt{&tablename}.auditTable AND
                {&tablename}.auditField EQ tt{&tablename}.auditField
                NO-ERROR.
            IF NOT AVAIL {&tablename} THEN DO:
                CREATE {&tablename}.
                BUFFER-COPY tt{&tablename} TO {&tablename}.
            END.
            /* Ensure OUR defaults are set */
            ASSIGN 
                {&tableName}.auditDefault = tt{&tableName}.auditDefault
                .
        END.
    END.
    INPUT CLOSE.

    /* Now remove any base records that are not in the tt */
    FOR EACH {&tablename} EXCLUSIVE WHERE
        NOT CAN-FIND(FIRST tt{&tablename} WHERE 
                        tt{&tablename}.auditTable = {&tablename}.auditTable AND
                        tt{&tablename}.auditField = {&tablename}.auditField):
        DELETE {&tablename}.
    END.
    
    EMPTY TEMP-TABLE tt{&tablename}.

    /* Finally, if an earlier iteration of auditFld, if auditTbl says audit updates, turn on all fields */
    IF fIntVer(fiFromVer:{&SV}) LE 20020500 THEN DO:
        FOR EACH AuditTbl NO-LOCK WHERE 
            AuditTbl.AuditUpdate EQ YES:
            FOR EACH AuditFld EXCLUSIVE-LOCK WHERE 
                AuditFld.AuditTable EQ AuditTbl.AuditTable:
                ASSIGN
                    AuditFld.Audit = YES.
            END.
        END.
    END. 
 
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadEstCostData C-Win 
PROCEDURE ipLoadEstCostData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading EstCostData").

    &SCOPED-DEFINE tablename estCostCategory
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    IF NOT CAN-FIND (FIRST {&tablename}) THEN DO:
        INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
        REPEAT:
            CREATE {&tablename}.
            IMPORT {&tablename}.
        END.
        INPUT CLOSE.
    END.
        
    &SCOPED-DEFINE tablename estCostGroup
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    IF NOT CAN-FIND (FIRST {&tablename}) THEN DO:
        INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
        REPEAT:
            CREATE {&tablename}.
            IMPORT {&tablename}.
        END.
        INPUT CLOSE.
    END.

    &SCOPED-DEFINE tablename estCostGroupLevel
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    IF NOT CAN-FIND (FIRST {&tablename}) THEN DO:
        INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
        REPEAT:
            CREATE {&tablename}.
            IMPORT {&tablename}.
        END.
        INPUT CLOSE.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadDAOAData C-Win 
PROCEDURE ipLoadDAOAData :
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
    DISABLE TRIGGERS FOR LOAD OF dynPrgrmsPage.
    DISABLE TRIGGERS FOR LOAD OF dynLookup.
    DISABLE TRIGGERS FOR LOAD OF dynValueParam.
    DISABLE TRIGGERS FOR LOAD OF dynValueColumn.
    DISABLE TRIGGERS FOR LOAD OF dynValueParamSet.
    
    /* Remove all records that we plan to replace */
    FOR EACH dynSubject EXCLUSIVE WHERE 
        dynSubject.subjectid LT 5000:
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

    FOR EACH dynPrgrmsPage EXCLUSIVE:
        DELETE dynPrgrmsPage.
    END.

    FOR EACH dynLookup:
        DELETE dynLookup.
    END.

    FOR EACH dynValueParam EXCLUSIVE WHERE 
        dynValueParam.user-id EQ "_default" AND
        dynValueParam.subjectID LT 5000:
        DELETE dynValueParam.
    END.

    FOR EACH dynValueColumn EXCLUSIVE WHERE 
        dynValueColumn.user-id EQ "_default" AND
        dynValueColumn.subjectID LT 5000:
        DELETE dynValueColumn.
    END.

    FOR EACH dynValueParamSet EXCLUSIVE WHERE 
        dynValueParamSet.user-id EQ "_default" AND
        dynValueParamSet.subjectID LT 5000:
        DELETE dynValueParamSet.
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

&SCOPED-DEFINE tablename dynPrgrmsPage
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynLookup
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynValueParam
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynValueColumn
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.

&SCOPED-DEFINE tablename dynValueParamSet
    DISABLE TRIGGERS FOR LOAD OF {&tablename}.
    INPUT FROM VALUE(cUpdDataDir + "\{&tablename}.d") NO-ECHO.
    REPEAT:
        CREATE {&tablename}.
        IMPORT {&tablename} NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            DELETE {&tablename}.
    END.
    INPUT CLOSE.
    
/*  #92831 Configure DAOA for Altex*/
    DISABLE TRIGGERS FOR LOAD OF emailConfig.
    FIND FIRST emailConfig NO-LOCK WHERE 
        emailConfig.configID EQ 1
        NO-ERROR.
    IF NOT AVAIL emailConfig THEN DO:
        CREATE emailConfig.
        ASSIGN 
            emailConfig.configID = 1
            emailConfig.description = "DAOA Report Config".
    END.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadNaicsData C-Win
PROCEDURE ipLoadNaicsData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading NAICS Records").

    &SCOPED-DEFINE tablename naics

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
            users.use_fonts = FALSE
            users.track_usage = TRUE.

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

    EMPTY TEMP-TABLE tt{&tablename}.
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
                /* 64504 Remove any change that turns off CreditHold */
                IF {&tablename}.name EQ "CreditHold" 
                AND {&tablename}.log-fld EQ FALSE THEN ASSIGN 
                    {&tablename}.log-fld = TRUE.
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
                TO {&tablename}.
        END.
    END.
    INPUT CLOSE.
        
    /* Delete records no longer used */
    FOR EACH {&tablename} EXCLUSIVE:
        FIND FIRST tt{&tablename} WHERE
            tt{&tablename}.prgmname = {&tablename}.prgmname 
            NO-ERROR.
        IF NOT AVAIL tt{&tablename} THEN 
            DELETE {&tablename}.
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
                WHERE ttUserMenu.prgmname EQ prgrms.prgmname) THEN 
            DO:
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
        ASSIGN 
            iopiStatus = iopiStatus + 5
            rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
    END.
    ELSE ASSIGN 
        iopiStatus = iopiStatus + 5
        rStatusBar:WIDTH = MIN(75,(iopiStatus / 100) * 75).
    
    RUN ipUpdateSQLSettings IN THIS-PROCEDURE.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipResetCostGroups C-Win
PROCEDURE ipResetCostGroups:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("    Reset Cost Groups and Categories").

    DEF VAR cOrigPropath AS CHAR NO-UNDO.
    DEF VAR cNewPropath AS CHAR NO-UNDO.

    DISABLE TRIGGERS FOR LOAD OF estCostCategory.
    DISABLE TRIGGERS FOR LOAD OF estCostGroup.
    DISABLE TRIGGERS FOR LOAD OF estCostGroupLevel.
    
    FOR EACH estCostCategory:
        DELETE estCostCategory.
    END.
    FOR EACH estCostGroup:
        DELETE estCostGroup.
    END.
    FOR EACH estCostGroupLevel:
        DELETE estCostGroupLevel.
    END.
    
    ASSIGN
        cOrigPropath = PROPATH
        cNewPropath  = cEnvDir + "\" + fiEnvironment:{&SV} + "\Programs," + PROPATH
        PROPATH = cNewPropath.
        
    RUN est/ResetCostGroupsAndCategories.p.   
    
    ASSIGN 
        PROPATH = cOrigPropath.     


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
            tempUser._Password = "McjlwjaffvkbBCti".
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
    END.
    ELSE DO:
        CREATE _User.
        ASSIGN
            _User._UserId = "asi"
            _User._Password = "McjlwjaffvkbBCti".
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




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetCurrencyAccounts C-Win
PROCEDURE ipSetCurrencyAccounts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("    Setting Currency Accounts").

    DEF BUFFER bcurrency FOR currency.
    
    FOR EACH bcurrency EXCLUSIVE WHERE 
        bcurrency.ar-ast-acct EQ "":
        FIND ar-ctrl NO-LOCK WHERE 
            ar-ctrl.company = bcurrency.company.
        IF AVAIL ar-ctrl THEN ASSIGN 
            bcurrency.ar-ast-acct = ar-ctrl.sales.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetMonitorPwd C-Win 
PROCEDURE ipSetMonitorPwd :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
        
    RUN ipStatus ("  Setting Monitor Password").

    FIND FIRST _User WHERE 
        _User._UserId = "monitor" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL (_User) THEN 
    DO:
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser.
        ASSIGN 
            tempUser._Password = "laaEbPjiXlakhcql".
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
    END.
    ELSE 
    DO:
        CREATE _User.
        ASSIGN
            _User._UserId = "monitor"
            _User._Password = "laaEbPjiXlakhcql".
    END.

    RELEASE _user.
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
        FIND FIRST ttUpdateHist NO-LOCK NO-ERROR.
        IF AVAIL ttUpdateHist THEN ASSIGN 
                ttUpdateHist.updLog = ttUpdateHist.updLog + STRING(TODAY,"99/99/99") + "  " + STRING(TIME,"HH:MM:SS") + "  " + cMsgStr[iMsgCtr] + CHR(10)
                ttUpdateHist.endTimeInt = INT(TIME)
                ttUpdateHist.endTime = STRING(time,"HH:MM:SS AM")        
                ttUpdateHist.success = lSuccess.        
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
    IF SEARCH(cUpdDataDir + "\naics.d") <> ? THEN
        RUN ipLoadNaicsData IN THIS-PROCEDURE.

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
            sys-ctrl.char-fld = "-WSDL 'http:\\helpsvr.advantzware.com/asihelpServices/helpmaintenance.asmx?WSDL'".
    END. 
    FOR EACH sys-ctrl WHERE
        sys-ctrl.name EQ "UpdateService":
        ASSIGN
            sys-ctrl.char-fld = "-WSDL 'http:\\helpsvr.advantzware.com/updatehelpServices/helpupdate.asmx?WSDL'".
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
            sys-ctrl.descrip = "https://helpsvr.advantzware.com/patches/asiUpdate.html"
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
    
    /* 94653 Passwords Displayed in Plain Text */
    RUN ipStatus ("  NK1 password field").
    INPUT FROM VALUE(cUpdDataDir + "\sys-ctrl.d") NO-ECHO.
    REPEAT:
        CREATE ttsys-ctrl.
        IMPORT ttsys-ctrl.
    END.
    INPUT CLOSE.
    INPUT FROM VALUE(cUpdDataDir + "\sys-ctrl-shipto.d") NO-ECHO.
    REPEAT:
        CREATE ttsys-ctrl-shipto.
        IMPORT ttsys-ctrl-shipto.
    END.
    INPUT CLOSE.
    FOR EACH sys-ctrl EXCLUSIVE:
        FIND FIRST ttsys-ctrl WHERE
            ttsys-ctrl.company EQ "001" AND 
            ttsys-ctrl.name EQ sys-ctrl.name
            NO-ERROR.
        IF AVAIL ttsys-ctrl THEN ASSIGN 
                sys-ctrl.isPassword = ttsys-ctrl.isPassword.
    END.
    FOR EACH sys-ctrl-shipto EXCLUSIVE:
        FIND FIRST ttsys-ctrl-shipto WHERE
            ttsys-ctrl-shipto.company EQ "001" AND 
            ttsys-ctrl-shipto.name EQ sys-ctrl-shipto.name 
            NO-ERROR.
        IF AVAIL ttsys-ctrl-shipto THEN ASSIGN 
            sys-ctrl-shipto.isPassword = ttsys-ctrl-shipto.isPassword.
    END.
    
    ASSIGN 
        lSuccess = TRUE.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateSQLSettings C-Win
PROCEDURE ipUpdateSQLSettings:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Just an overview:
            Read the conmgr.properties file to see if there is a "special" port for SQL server
            If not, use the "regular" DB port
            Output some values to a file /Admin/SQLParms.txt. This contains
                dbname=<dbname>
                dbport=<port>
                cusername=asi
                cpassword=Boxco2020
            This file will be used later by the batch file to ensure SQL permissions are given to all files
            It will then be deleted as part of the normal upgrade
            
            DB name is from input variable ipcName, normal port is from ipcPort
------------------------------------------------------------------------------*/
    DEF VAR cRaw AS CHAR NO-UNDO.
    DEF VAR cTestString AS CHAR NO-UNDO.
    DEF VAR cSQLDbPort AS CHAR NO-UNDO.
    DEF VAR lInSection AS LOG.
    INPUT FROM VALUE(cDLCDir + "\properties\conmgr.properties").
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        IF INDEX(cRaw,"servergroup." + ipcName + ".defaultConfiguration") NE 0 
        AND INDEX(cRaw,"sql") NE 0 THEN ASSIGN
            lInSection = TRUE.
        IF lInSection
        AND INDEX(TRIM(cRaw),"port=") NE 0 THEN ASSIGN
            cTestString = TRIM(cRaw)
            cSQLDbPort = ENTRY(2,cTestString,"=")
            cSQLDbPort = TRIM(cSQLDbPort).
        IF cSQLDbPort NE "" THEN LEAVE.
    END. 
    INPUT CLOSE.
    IF cSQLDbPort EQ "" THEN ASSIGN 
        cSQLDbPort = ipcPort.
    OUTPUT TO VALUE(cAdminDir + "\SQLParms.txt").
    PUT UNFORMATTED "dbname=" + ipcName + CHR(10).         
    PUT UNFORMATTED "dbport=" + cSQLDbPort + CHR(10).         
    PUT UNFORMATTED "cusername=asi" + CHR(10).         
    PUT UNFORMATTED "cpassword=Boxco2020" + CHR(10).         
    OUTPUT CLOSE.    
    
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
    
    FIND ttIniFile WHERE ttIniFile.cVarName EQ "modeList" NO-ERROR.
    IF AVAIL ttIniFile 
    AND INDEX(ttIniFile.cVarValue,"API Monitor") = 0 THEN ASSIGN 
        ttIniFile.cVarValue = ttIniFile.cVarValue + ",API Monitor".
    FIND ttIniFile WHERE ttIniFile.cVarName EQ "pgmList" NO-ERROR.
    IF AVAIL ttIniFile 
    AND INDEX(ttIniFile.cVarValue,"api/Monitor.w") = 0 THEN ASSIGN 
        ttIniFile.cVarValue = ttIniFile.cVarValue + ",api/Monitor.w".
        
    ASSIGN
        lSuccess = TRUE.
    
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
        iIntVal[3] = INT(cStrVal[3])
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


