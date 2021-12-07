&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: sysCtrlU.w

  Description: Show User sys-ctrl Usage

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.7.2018

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

DEFINE VARIABLE lShowSysCtrlUsage AS LOGICAL NO-UNDO INITIAL YES.

{methods/defines/globdefs.i}
{system/ttPermissions.i}
{system/ttSetting.i}
{system/ttSysCtrlUsage.i}
{system/ttSessionParam.i}
{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE bttSysCtrlUsage NO-UNDO LIKE ttSysCtrlUsage.

SESSION:SET-WAIT-STATE ("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME API

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES APIOutboundTrigger APIOutbound ttPermissions ~
ttSettingUsage ttSysCtrlUsage ttSessionParam

/* Definitions for BROWSE API                                           */
&Scoped-define FIELDS-IN-QUERY-API APIOutboundTrigger.apiID ~
APIOutboundTrigger.clientID APIOutboundTrigger.triggerID ~
APIOutboundTrigger.description APIOutboundTrigger.createTime ~
APIOutboundTrigger.createBy 
&Scoped-define ENABLED-FIELDS-IN-QUERY-API 
&Scoped-define QUERY-STRING-API FOR EACH APIOutboundTrigger ~
      WHERE APIOutboundTrigger.company EQ g_company AND ~
APIOutboundTrigger.apiOutboundID GE 5000 AND ~
APIOutboundTrigger.inActive EQ NO NO-LOCK, ~
      FIRST APIOutbound WHERE APIOutbound.company EQ APIOutboundTrigger.company AND ~
APIOutbound.apiOutboundID EQ APIOutboundTrigger.apiOutboundID ~
      AND APIOutbound.inActive EQ NO NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-API OPEN QUERY API FOR EACH APIOutboundTrigger ~
      WHERE APIOutboundTrigger.company EQ g_company AND ~
APIOutboundTrigger.apiOutboundID GE 5000 AND ~
APIOutboundTrigger.inActive EQ NO NO-LOCK, ~
      FIRST APIOutbound WHERE APIOutbound.company EQ APIOutboundTrigger.company AND ~
APIOutbound.apiOutboundID EQ APIOutboundTrigger.apiOutboundID ~
      AND APIOutbound.inActive EQ NO NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-API APIOutboundTrigger APIOutbound
&Scoped-define FIRST-TABLE-IN-QUERY-API APIOutboundTrigger
&Scoped-define SECOND-TABLE-IN-QUERY-API APIOutbound


/* Definitions for BROWSE Permissions                                   */
&Scoped-define FIELDS-IN-QUERY-Permissions ttPermissions.mnemonic ttPermissions.prgmName ttPermissions.prgTitle ttPermissions.can_run ttPermissions.can_create ttPermissions.can_delete ttPermissions.can_update   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Permissions   
&Scoped-define SELF-NAME Permissions
&Scoped-define QUERY-STRING-Permissions FOR EACH ttPermissions  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Permissions OPEN QUERY {&SELF-NAME} FOR EACH ttPermissions  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Permissions ttPermissions
&Scoped-define FIRST-TABLE-IN-QUERY-Permissions ttPermissions


/* Definitions for BROWSE sessionParams                                  */
&Scoped-define FIELDS-IN-QUERY-sessionParams ttSessionParam.sessionParam ttSessionParam.sessionValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-sessionParams   
&Scoped-define SELF-NAME sessionParams
&Scoped-define QUERY-STRING-sessionParams FOR EACH ttSessionParam  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-sessionParams OPEN QUERY {&SELF-NAME} FOR EACH ttSessionParam  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-sessionParams ttSessionParam
&Scoped-define FIRST-TABLE-IN-QUERY-sessionParams ttSessionParam

/* Definitions for BROWSE settingUsage                                  */
&Scoped-define FIELDS-IN-QUERY-settingUsage ttSettingUsage.settingName ttSettingUsage.description ttSettingUsage.settingValue ttSettingUsage.scopeTable ttSettingUsage.scopeField1 ttSettingUsage.scopeField2 ttSettingUsage.scopeField3 ttSettingUsage.categoryTags ttSettingUsage.defaultValue ttSettingUsage.validValues   
&Scoped-define ENABLED-FIELDS-IN-QUERY-settingUsage   
&Scoped-define SELF-NAME settingUsage
&Scoped-define QUERY-STRING-settingUsage FOR EACH ttSettingUsage  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-settingUsage OPEN QUERY {&SELF-NAME} FOR EACH ttSettingUsage  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-settingUsage ttSettingUsage
&Scoped-define FIRST-TABLE-IN-QUERY-settingUsage ttSettingUsage


/* Definitions for BROWSE sysCtrlUsage                                  */
&Scoped-define FIELDS-IN-QUERY-sysCtrlUsage ttSysCtrlUsage.company ttSysCtrlUsage.module ttSysCtrlUsage.name ttSysCtrlUsage.char-fld ttSysCtrlUsage.date-fld ttSysCtrlUsage.dec-fld ttSysCtrlUsage.int-fld ttSysCtrlUsage.log-fld ttSysCtrlUsage.descrip ttSysCtrlUsage.usageNow ttSysCtrlUsage.category ttSysCtrlUsage.cust-vend ttSysCtrlUsage.cust-vend-no ttSysCtrlUsage.seqNo ttSysCtrlUsage.ship-id ttSysCtrlUsage.subCategory ttSysCtrlUsage.sysCtrlID ttSysCtrlUsage.typeCode   
&Scoped-define ENABLED-FIELDS-IN-QUERY-sysCtrlUsage   
&Scoped-define SELF-NAME sysCtrlUsage
&Scoped-define QUERY-STRING-sysCtrlUsage FOR EACH ttSysCtrlUsage  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-sysCtrlUsage OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrlUsage  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-sysCtrlUsage ttSysCtrlUsage
&Scoped-define FIRST-TABLE-IN-QUERY-sysCtrlUsage ttSysCtrlUsage


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-API}~
    ~{&OPEN-QUERY-Permissions}~
    ~{&OPEN-QUERY-settingUsage}~
    ~{&OPEN-QUERY-sysCtrlUsage}~
    ~{&OPEN-QUERY-sessionParams}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS showBrowse btnStackTrace btnClear ~
sysCtrlUsage settingUsage sessionParams Permissions API 
&Scoped-Define DISPLAYED-OBJECTS showBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClear 
     LABEL "Clear" 
     SIZE 8 BY 1.

DEFINE BUTTON btnStackTrace 
     LABEL "View Stack Trace" 
     SIZE 20 BY 1.

DEFINE VARIABLE showBrowse AS CHARACTER INITIAL "SysCtrl" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "API Settings", "API",
"Permissions", "Permissions",
"SysCtrl Usage", "SysCtrl",
"Configuration Settings", "Settings",
"Session Parameters", "SessionParams"
     SIZE 118 BY .91 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY API FOR 
      APIOutboundTrigger, 
      APIOutbound SCROLLING.

DEFINE QUERY Permissions FOR 
      ttPermissions SCROLLING.

DEFINE QUERY sessionParams FOR 
      ttSessionParam SCROLLING.

DEFINE QUERY settingUsage FOR 
      ttSettingUsage SCROLLING.

DEFINE QUERY sysCtrlUsage FOR 
      ttSysCtrlUsage SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE API
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS API C-Win _STRUCTURED
  QUERY API NO-LOCK DISPLAY
      APIOutboundTrigger.apiID FORMAT "x(32)":U WIDTH 21.8
      APIOutboundTrigger.clientID FORMAT "x(32)":U WIDTH 21.8
      APIOutboundTrigger.triggerID FORMAT "x(32)":U WIDTH 21.8
      APIOutboundTrigger.description FORMAT "x(100)":U WIDTH 46.8
      APIOutboundTrigger.createTime FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 24.2
      APIOutboundTrigger.createBy FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.62 FIT-LAST-COLUMN.

DEFINE BROWSE Permissions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Permissions C-Win _FREEFORM
  QUERY Permissions NO-LOCK DISPLAY
      ttPermissions.mnemonic LABEL-BGCOLOR 14
ttPermissions.prgmName LABEL-BGCOLOR 14
ttPermissions.prgTitle LABEL-BGCOLOR 14
ttPermissions.can_run
ttPermissions.can_create
ttPermissions.can_delete
ttPermissions.can_update
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.62.

DEFINE BROWSE sessionParams
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sessionParams C-Win _FREEFORM
  QUERY sessionParams DISPLAY
      ttSessionParam.sessionParam LABEL-BGCOLOR 14
ttSessionParam.sessionValue LABEL-BGCOLOR 14 FORMAT "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.62 ROW-HEIGHT-CHARS .62.

DEFINE BROWSE settingUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS settingUsage C-Win _FREEFORM
  QUERY settingUsage DISPLAY
      ttSettingUsage.settingName LABEL-BGCOLOR 14
ttSettingUsage.description LABEL-BGCOLOR 14 FORMAT "x(50)"
ttSettingUsage.settingValue LABEL-BGCOLOR 14 FORMAT "x(50)"
ttSettingUsage.scopeTable LABEL-BGCOLOR 14
ttSettingUsage.scopeField1 LABEL-BGCOLOR 14
ttSettingUsage.scopeField2 LABEL-BGCOLOR 14
ttSettingUsage.scopeField3 LABEL-BGCOLOR 14
ttSettingUsage.categoryTags LABEL-BGCOLOR 14 FORMAT "x(50)"
ttSettingUsage.defaultValue LABEL-BGCOLOR 14 FORMAT "x(50)"
ttSettingUsage.validValues
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.62 ROW-HEIGHT-CHARS .62.

DEFINE BROWSE sysCtrlUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sysCtrlUsage C-Win _FREEFORM
  QUERY sysCtrlUsage DISPLAY
      ttSysCtrlUsage.company LABEL-BGCOLOR 14
ttSysCtrlUsage.module LABEL-BGCOLOR 14
ttSysCtrlUsage.name LABEL-BGCOLOR 14
ttSysCtrlUsage.char-fld LABEL-BGCOLOR 14
ttSysCtrlUsage.date-fld LABEL-BGCOLOR 14
ttSysCtrlUsage.dec-fld LABEL-BGCOLOR 14
ttSysCtrlUsage.int-fld LABEL-BGCOLOR 14
ttSysCtrlUsage.log-fld LABEL-BGCOLOR 14
ttSysCtrlUsage.descrip LABEL-BGCOLOR 14
ttSysCtrlUsage.usageNow LABEL-BGCOLOR 14
ttSysCtrlUsage.category LABEL-BGCOLOR 14
ttSysCtrlUsage.cust-vend
ttSysCtrlUsage.cust-vend-no
ttSysCtrlUsage.seqNo
ttSysCtrlUsage.ship-id
ttSysCtrlUsage.subCategory
ttSysCtrlUsage.sysCtrlID
ttSysCtrlUsage.typeCode
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.62 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     showBrowse AT ROW 1 COL 9 NO-LABEL WIDGET-ID 10
     btnStackTrace AT ROW 1 COL 128 WIDGET-ID 4
     btnClear AT ROW 1 COL 149 WIDGET-ID 2
     sysCtrlUsage AT ROW 1.95 COL 1 WIDGET-ID 200
     settingUsage AT ROW 1.95 COL 1
     sessionParams AT ROW 1.95 COL 1
     Permissions AT ROW 1.95 COL 1 WIDGET-ID 300
     API AT ROW 1.95 COL 1 WIDGET-ID 400
     "Show:" VIEW-AS TEXT
          SIZE 7 BY .91 AT ROW 1 COL 2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57 WIDGET-ID 100.


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
         TITLE              = "User Usage"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
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
/* BROWSE-TAB sysCtrlUsage btnClear DEFAULT-FRAME */
/* BROWSE-TAB sessionParams sysCtrlUsage DEFAULT-FRAME */
/* BROWSE-TAB settingUsage sysCtrlUsage DEFAULT-FRAME */
/* BROWSE-TAB Permissions settingUsage DEFAULT-FRAME */
/* BROWSE-TAB API Permissions DEFAULT-FRAME */
ASSIGN 
       Permissions:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       Permissions:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       sessionParams:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       sessionParams:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       settingUsage:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       settingUsage:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       sysCtrlUsage:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE API
/* Query rebuild information for BROWSE API
     _TblList          = "ASI.APIOutboundTrigger,ASI.APIOutbound WHERE ASI.APIOutboundTrigger ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "APIOutboundTrigger.company EQ g_company AND
APIOutboundTrigger.apiOutboundID GE 5000 AND
APIOutboundTrigger.inActive EQ NO"
     _JoinCode[2]      = "APIOutbound.company EQ APIOutboundTrigger.company AND
APIOutbound.apiOutboundID EQ APIOutboundTrigger.apiOutboundID"
     _Where[2]         = "APIOutbound.inActive EQ NO"
     _FldNameList[1]   > ASI.APIOutboundTrigger.apiID
"APIOutboundTrigger.apiID" ? ? "character" ? ? ? ? ? ? no ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.APIOutboundTrigger.clientID
"APIOutboundTrigger.clientID" ? ? "character" ? ? ? ? ? ? no ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.APIOutboundTrigger.triggerID
"APIOutboundTrigger.triggerID" ? ? "character" ? ? ? ? ? ? no ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.APIOutboundTrigger.description
"APIOutboundTrigger.description" ? ? "character" ? ? ? ? ? ? no ? no no "46.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.APIOutboundTrigger.createTime
"APIOutboundTrigger.createTime" ? "99/99/9999 HH:MM:SS" "datetime" ? ? ? ? ? ? no ? no no "24.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = ASI.APIOutboundTrigger.createBy
     _Query            is OPENED
*/  /* BROWSE API */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Permissions
/* Query rebuild information for BROWSE Permissions
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPermissions
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Query            is OPENED
*/  /* BROWSE Permissions */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sessionParams
/* Query rebuild information for BROWSE sessionParams
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSessionParam
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE sessionParams */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE settingUsage
/* Query rebuild information for BROWSE settingUsage
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSettingUsage
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE settingUsage */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sysCtrlUsage
/* Query rebuild information for BROWSE sysCtrlUsage
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrlUsage
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE sysCtrlUsage */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User Usage */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Usage */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* User Usage */
DO:
    RUN pReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME DEFAULT-FRAME /* Clear */
DO:
    DYNAMIC-FUNCTION("sfClearUsage").
    RUN pGetTtPermissions.
    RUN pGetSysCtrlUsage.
    RUN pGetSettingUsage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStackTrace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStackTrace C-Win
ON CHOOSE OF btnStackTrace IN FRAME DEFAULT-FRAME /* View Stack Trace */
DO:
    CASE showBrowse:
        WHEN "Settings" THEN
            IF AVAILABLE ttSettingUsage THEN
            RUN system/stackTrace.w (ttSettingUsage.stackTrace).
        WHEN "SysCtrl" THEN
            IF AVAILABLE ttSysCtrlUsage THEN
            RUN system/stackTrace.w (ttSysCtrlUsage.stackTrace).
        WHEN "SessionParams" THEN
            IF AVAILABLE ttSessionParam THEN
            RUN system/stackTrace.w (ttSessionParam.stackTrace).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Permissions
&Scoped-define SELF-NAME Permissions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Permissions C-Win
ON START-SEARCH OF Permissions IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME settingUsage
&Scoped-define SELF-NAME settingUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL settingUsage C-Win
ON DEFAULT-ACTION OF settingUsage IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnStackTrace.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sessionParams C-Win
ON START-SEARCH OF sessionParams IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL settingUsage C-Win
ON START-SEARCH OF settingUsage IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME showBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showBrowse C-Win
ON VALUE-CHANGED OF showBrowse IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        {&SELF-NAME}
        btnClear:SENSITIVE          = CAN-DO("Permissions,SysCtrl,Settings",{&SELF-NAME})
        btnStackTrace:SENSITIVE     = CAN-DO("Settings,SysCtrl,SessionParams",{&SELF-NAME})
        BROWSE API:HIDDEN           = {&SELF-NAME} NE "API"
        BROWSE Permissions:HIDDEN   = {&SELF-NAME} NE "Permissions"
        BROWSE sysCtrlUsage:HIDDEN  = {&SELF-NAME} NE "SysCtrl"
        BROWSE settingUsage:HIDDEN  = {&SELF-NAME} NE "Settings"
        BROWSE sessionParams:HIDDEN = {&SELF-NAME} NE "SessionParams"
        .
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME sysCtrlUsage
&Scoped-define SELF-NAME sysCtrlUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlUsage C-Win
ON DEFAULT-ACTION OF sysCtrlUsage IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnStackTrace.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlUsage C-Win
ON START-SEARCH OF sysCtrlUsage IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME API
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

&Scoped-define sdBrowseName API
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName Permissions
{methods/template/brwcustom2.i 2}
&Scoped-define sdBrowseName sysCtrlUsage
{methods/template/brwcustom2.i 3}
&Scoped-define sdBrowseName settingUsage
{methods/template/brwcustom2.i 4}
&Scoped-define sdBrowseName sessionParams
{methods/template/brwcustom2.i 5}

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
  DYNAMIC-FUNCTION("sfSetSysCtrlUsageHandle", THIS-PROCEDURE).
  RUN pGetTtPermissions.
  RUN pGetSysCtrlUsage.
  RUN pGetSettingUsage.
  RUN pGetSessionParams.
  RUN enable_UI.
  RUN pToggleAPISettingsStatus.
  APPLY "VALUE-CHANGED":U TO showBrowse.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName sysCtrlUsage
{methods/sortByProc.i "pByCompany" "ttSysCtrlUsage.company"}
{methods/sortByProc.i "pByModule" "ttSysCtrlUsage.module"}
{methods/sortByProc.i "pByName" "ttSysCtrlUsage.name"}
{methods/sortByProc.i "pByCharFld" "ttSysCtrlUsage.char-fld"}
{methods/sortByProc.i "pByDateFld" "ttSysCtrlUsage.date-fld"}
{methods/sortByProc.i "pByDecFld" "ttSysCtrlUsage.dec-fld"}
{methods/sortByProc.i "pByIntFld" "ttSysCtrlUsage.int-fld"}
{methods/sortByProc.i "pByLogFld" "ttSysCtrlUsage.log-fld"}
{methods/sortByProc.i "pByDescrip" "ttSysCtrlUsage.descrip"}
{methods/sortByProc.i "pByUsageNow" "ttSysCtrlUsage.usageNow"}
{methods/sortByProc.i "pByCategory" "ttSysCtrlUsage.category"}

&Scoped-define sdBrowseName Permissions
{methods/sortByProc.i "pByMnemonic" "ttPermissions.mnemonic"}
{methods/sortByProc.i "pByPrgmName" "ttPermissions.prgmName"}
{methods/sortByProc.i "pByPrgTitle" "ttPermissions.prgTitle"}

&Scoped-define sdBrowseName settingUsage
{methods/sortByProc.i "pBySettingName" "ttSettingUsage.settingName"}
{methods/sortByProc.i "pByDescription" "ttSettingUsage.description"}
{methods/sortByProc.i "pBySettingValue" "ttSettingUsage.settingValue"}
{methods/sortByProc.i "pByScopeTable" "ttSettingUsage.scopeTable"}
{methods/sortByProc.i "pByScopeField1" "ttSettingUsage.scopeField1"}
{methods/sortByProc.i "pByScopeField2" "ttSettingUsage.scopeField2"}
{methods/sortByProc.i "pByScopeField3" "ttSettingUsage.scopeField3"}
{methods/sortByProc.i "pByCategoryTags" "ttSettingUsage.categoryTags"}
{methods/sortByProc.i "pByDefaultValue" "ttSettingUsage.defaultValue"}

&Scoped-define sdBrowseName sessionParams
{methods/sortByProc.i "pBySessionParam" "ttSessionParam.sessionParam"}
{methods/sortByProc.i "pBySessionValue" "ttSessionParam.sessionValue"}

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
  DISPLAY showBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE showBrowse btnStackTrace btnClear sysCtrlUsage settingUsage 
         sessionParams Permissions API 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSessionParams C-Win
PROCEDURE pGetSessionParams:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN spGetSessionParams (OUTPUT TABLE ttSessionParam).
    FOR EACH ttSessionParam
        WHERE ttSessionParam.sessionParam EQ "Password"
        :
        ttSessionParam.sessionValue = FILL("*",LENGTH(ttSessionParam.sessionValue)).
    END. // each ttsessionparam
    {&OPEN-QUERY-sessionParams}

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettingUsage C-Win 
PROCEDURE pGetSettingUsage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hSettingUsage AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE hQuery        AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx           AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttSettingUsage.
    
    ASSIGN
        hSettingUsage[1] = DYNAMIC-FUNCTION("sfGetTtSettingUsageHandle")
        hSettingUsage[1] = hSettingUsage[1]:DEFAULT-BUFFER-HANDLE
        hSettingUsage[2] = TEMP-TABLE ttSettingUsage:HANDLE
        hSettingUsage[2] = hSettingUsage[2]:DEFAULT-BUFFER-HANDLE
        .    
    /* scroll returned temp-table records */
    CREATE QUERY hQuery[1].
    hQuery[1]:SET-BUFFERS(hSettingUsage[1]:HANDLE).
    hQuery[1]:QUERY-PREPARE("FOR EACH " + hSettingUsage[1]:NAME).
    hQuery[1]:QUERY-OPEN.

    CREATE QUERY hQuery[2].
    hQuery[2]:SET-BUFFERS(hSettingUsage[2]:HANDLE).

    REPEAT:
        hQuery[1]:GET-NEXT().
        IF hQuery[1]:QUERY-OFF-END THEN LEAVE.
        CREATE ttSettingUsage.
        DO idx = 1 TO hSettingUsage[1]:NUM-FIELDS:
            hSettingUsage[2]:BUFFER-FIELD(idx):BUFFER-VALUE() = hSettingUsage[1]:BUFFER-FIELD(idx):BUFFER-VALUE(). 
        END. /* do idx */
    END. /* repeat */
    hQuery[1]:QUERY-CLOSE().
    DELETE OBJECT hQuery[1].
    
    {&OPEN-QUERY-settingUsage}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSysCtrlUsage C-Win 
PROCEDURE pGetSysCtrlUsage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hSysCtrlUsage AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE hQuery        AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx           AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttSysCtrlUsage.
    
    ASSIGN
        hSysCtrlUsage[1] = DYNAMIC-FUNCTION("sfGetTtSysCtrlUsageHandle")
        hSysCtrlUsage[1] = hSysCtrlUsage[1]:DEFAULT-BUFFER-HANDLE
        hSysCtrlUsage[2] = TEMP-TABLE ttSysCtrlUsage:HANDLE
        hSysCtrlUsage[2] = hSysCtrlUsage[2]:DEFAULT-BUFFER-HANDLE
        .    
    /* scroll returned temp-table records */
    CREATE QUERY hQuery[1].
    hQuery[1]:SET-BUFFERS(hSysCtrlUsage[1]:HANDLE).
    hQuery[1]:QUERY-PREPARE("FOR EACH " + hSysCtrlUsage[1]:NAME).
    hQuery[1]:QUERY-OPEN.

    CREATE QUERY hQuery[2].
    hQuery[2]:SET-BUFFERS(hSysCtrlUsage[2]:HANDLE).

    REPEAT:
        hQuery[1]:GET-NEXT().
        IF hQuery[1]:QUERY-OFF-END THEN LEAVE.
        CREATE ttSysCtrlUsage.
        DO idx = 1 TO hSysCtrlUsage[1]:NUM-FIELDS:
            hSysCtrlUsage[2]:BUFFER-FIELD(idx):BUFFER-VALUE() = hSysCtrlUsage[1]:BUFFER-FIELD(idx):BUFFER-VALUE(). 
        END. /* do idx */
    END. /* repeat */
    hQuery[1]:QUERY-CLOSE().
    DELETE OBJECT hQuery[1].
    
    {&OPEN-QUERY-sysCtrlUsage}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTtPermissions C-Win 
PROCEDURE pGetTtPermissions :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hPermissions AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE hQuery        AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx           AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttPermissions.
    
    ASSIGN
        hPermissions[1] = DYNAMIC-FUNCTION("sfGetttPermissionsHandle")
        hPermissions[1] = hPermissions[1]:DEFAULT-BUFFER-HANDLE
        hPermissions[2] = TEMP-TABLE ttPermissions:HANDLE
        hPermissions[2] = hPermissions[2]:DEFAULT-BUFFER-HANDLE
        .    
    /* scroll returned temp-table records */
    CREATE QUERY hQuery[1].
    hQuery[1]:SET-BUFFERS(hPermissions[1]:HANDLE).
    hQuery[1]:QUERY-PREPARE("FOR EACH " + hPermissions[1]:NAME).
    hQuery[1]:QUERY-OPEN.

    CREATE QUERY hQuery[2].
    hQuery[2]:SET-BUFFERS(hPermissions[2]:HANDLE).

    REPEAT:
        hQuery[1]:GET-NEXT().
        IF hQuery[1]:QUERY-OFF-END THEN LEAVE.
        CREATE ttPermissions.
        DO idx = 1 TO hPermissions[1]:NUM-FIELDS:
            hPermissions[2]:BUFFER-FIELD(idx):BUFFER-VALUE() = hPermissions[1]:BUFFER-FIELD(idx):BUFFER-VALUE(). 
        END. /* do idx */
    END. /* repeat */
    hQuery[1]:QUERY-CLOSE().
    DELETE OBJECT hQuery[1].
    
    {&OPEN-QUERY-Permissions}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "company" THEN
        RUN pByCompany.
        WHEN "module" THEN
        RUN pByModule.
        WHEN "name" THEN
        RUN pByName.
        WHEN "char-fld" THEN
        RUN pByCharFld.
        WHEN "date-fld" THEN
        RUN pByDateFld.
        WHEN "dec-fld" THEN
        RUN pByDecFld.
        WHEN "int-fld" THEN
        RUN pByIntFld.
        WHEN "log-fld" THEN
        RUN pByLogFld.
        WHEN "descrip" THEN
        RUN pByDescrip.
        WHEN "usageNow" THEN
        RUN pByUsageNow.
        WHEN "category" THEN
        RUN pByCategory.
        WHEN "mnemonic" THEN
        RUN pByMnemonic.
        WHEN "prgmName" THEN
        RUN pByPrgmName.
        WHEN "prgTitle" THEN
        RUN pByPrgTitle.
        WHEN "settingName" THEN
        RUN pBySettingName.
        WHEN "description" THEN
        RUN pByDescription.
        WHEN "settingValue" THEN
        RUN pBySettingValue.
        WHEN "scopeTable" THEN
        RUN pByScopeTable.
        WHEN "scopeField1" THEN
        RUN pByScopeField1.
        WHEN "scopeField2" THEN
        RUN pByScopeField2.
        WHEN "scopeField3" THEN
        RUN pByScopeField3.
        WHEN "categoryTags" THEN
        RUN pByCategoryTags.
        WHEN "defaultValue" THEN
        RUN pByDefaultValue.
        WHEN "sessionParam" THEN
        RUN pBySessionParam.
        WHEN "sessionValue" THEN
        RUN pBySessionValue.
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReSize C-Win 
PROCEDURE pReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FRAME {&FRAME-NAME}:HIDDEN = YES.
    IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
    {&WINDOW-NAME}:HEIGHT = 28.57.
    IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
    {&WINDOW-NAME}:WIDTH  = 160.
    ASSIGN
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:WIDTH   = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:HEIGHT  = {&WINDOW-NAME}:HEIGHT
        BROWSE API:WIDTH            = FRAME {&FRAME-NAME}:WIDTH
        BROWSE API:HEIGHT           = FRAME {&FRAME-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW
        BROWSE Permissions:WIDTH    = FRAME {&FRAME-NAME}:WIDTH
        BROWSE Permissions:HEIGHT   = FRAME {&FRAME-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW
        BROWSE sysCtrlUsage:WIDTH   = FRAME {&FRAME-NAME}:WIDTH
        BROWSE sysCtrlUsage:HEIGHT  = FRAME {&FRAME-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW
        BROWSE settingUsage:WIDTH   = FRAME {&FRAME-NAME}:WIDTH
        BROWSE settingUsage:HEIGHT  = FRAME {&FRAME-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW
        BROWSE sessionParams:WIDTH  = FRAME {&FRAME-NAME}:WIDTH
        BROWSE sessionParams:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW
        .
    FRAME {&FRAME-NAME}:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pToggleAPISettingsStatus C-Win 
PROCEDURE pToggleAPISettingsStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
        
    RUN sys/ref/nk1look.p (
        g_company, "APIConfig", "L", FALSE, FALSE, "", "",
        OUTPUT cReturnValue, OUTPUT lRecFound
        ).    
    IF NOT lRecFound THEN
    showBrowse:DELETE("API Settings") IN FRAME {&FRAME-NAME}.
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

