&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/users.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters: <none>

  Output Parameters: <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{methods/defines/hndlset.i}
{methods/defines/noreckey.i}
{custom/resizdef.i}

ASSIGN
    cocode = g_company
    locode = g_loc
    .

&SCOPED-DEFINE proc-enable proc-enable
&SCOPED-DEFINE users-rowavail proc-rowavail

DEFINE SHARED VAR cIniLoc AS CHAR NO-UNDO.
DEFINE SHARED VAR cUsrLoc AS CHAR NO-UNDO.

/*DEFINE SHARED VARIABLE h_users AS HANDLE NO-UNDO.*/

DEFINE BUFFER zUsers FOR users.
DEFINE BUFFER lUsers FOR users.

DEFINE VARIABLE createLabelPath AS LOG       NO-UNDO.
DEFINE VARIABLE cOldUserID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEnvList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbList         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cModeList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEnvSelList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbSelList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cModeSelList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAliasFromFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE correct-error   AS LOG       NO-UNDO.
DEFINE VARIABLE copy-record     AS LOG       NO-UNDO.
DEFINE VARIABLE rThisUser       AS ROWID     NO-UNDO.
DEFINE VARIABLE lAdd            AS LOG       NO-UNDO.
DEFINE VARIABLE lCopy           AS LOG       NO-UNDO.
DEFINE VARIABLE lPwdChanged     AS LOG       NO-UNDO.
DEFINE VARIABLE cOldPwd         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPgmMstrSecur   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSuperAdmin     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAdmin          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMenuChanges    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hColorWidget    AS HANDLE    NO-UNDO.
DEFINE VARIABLE iFGColor        AS INTEGER   NO-UNDO EXTENT 3.
DEFINE VARIABLE iBGColor        AS INTEGER   NO-UNDO EXTENT 3.
    
DEFINE TEMP-TABLE tempUser NO-UNDO LIKE _User.

DEFINE TEMP-TABLE ttUsers
    FIELD ttfPdbname AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfUserAlias AS CHAR
    FIELD ttfEnvList AS CHAR
    FIELD ttfDbList AS CHAR
    FIELD ttfModeList AS CHAR
        INDEX iUserID   IS UNIQUE ttfUserID  ttfPdbName
        INDEX iDatabase IS UNIQUE ttfPdbName ttfUserID
        .
DEFINE BUFFER bttUsers FOR ttUsers.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES users usr
&Scoped-define FIRST-EXTERNAL-TABLE users


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR users, usr.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS users.AMPM users.user_name users.phone-cnty ~
users.isManager users.isLocked users.mobile users.image_filename ~
users.user_program[1] users.user_program[2] users.user_program[3] ~
users.userImage[1] users.use_colors users.use_fonts users.menuSize ~
users.userLanguage users.showMenuImages users.showCueCards ~
users.showMnemonic users.positionMnemonic 
&Scoped-define ENABLED-TABLES users
&Scoped-define FIRST-ENABLED-TABLE users
&Scoped-Define DISPLAYED-FIELDS users.AMPM users.user_id users.user_name ~
users.userAlias users.department users.phone-cnty users.phone ~
users.securityLevel users.isActive users.isManager users.isLocked ~
users.mobile users.fax users.image_filename users.user_program[1] ~
users.user_program[2] users.user_program[3] users.userImage[1] ~
users.use_colors users.manager users.use_fonts users.menuSize ~
users.purchaseLimit users.userLanguage users.showMenuImages ~
users.showCueCards users.showMnemonic users.positionMnemonic ~
users.sessionLimit 
&Scoped-define DISPLAYED-TABLES users
&Scoped-define FIRST-DISPLAYED-TABLE users
&Scoped-Define DISPLAYED-OBJECTS fiPassword cbUserType fi_phone-area ~
lv-phone-num slModes slEnvironments slDatabases 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELDS,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS users.user_id users.department ~
users.manager 
&Scoped-define ADM-ASSIGN-FIELDS users.phone-cnty fi_phone-area ~
lv-phone-num users.mobile users.image_filename 
&Scoped-define DISPLAY-FIELDS users.phone-cnty fi_phone-area lv-phone-num ~
users.mobile users.image_filename 
&Scoped-define F1 colorChoice-0 colorChoice-1 colorChoice-2 colorChoice-3 ~
colorChoice-4 colorChoice-5 colorChoice-6 colorChoice-7 colorChoice-8 ~
colorChoice-9 colorChoice-10 colorChoice-11 colorChoice-12 colorChoice-13 ~
colorChoice-14 colorChoice-15 colorChoice-default FGColor-1 FGColor-2 ~
FGColor-3 BGColor-1 BGColor-2 BGColor-3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bAll1 
     LABEL "All" 
     SIZE 10 BY .71.

DEFINE BUTTON bAll2 
     LABEL "All" 
     SIZE 10 BY .71.

DEFINE BUTTON bAll3 
     LABEL "All" 
     SIZE 10 BY .71.

DEFINE BUTTON bChgPwd 
     LABEL "Change" 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE BUTTON bDefaults 
     LABEL "Use Defaults" 
     SIZE 18 BY .95
     FONT 1.

DEFINE BUTTON bNone1 
     LABEL "None" 
     SIZE 10 BY .71.

DEFINE BUTTON bNone2 
     LABEL "None" 
     SIZE 10 BY .71.

DEFINE BUTTON bNone3 
     LABEL "None" 
     SIZE 10 BY .71.

DEFINE VARIABLE cbUserType AS CHARACTER FORMAT "X(256)":U 
     LABEL "User Type" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "Full User","Production Floor","Administrator","Portal User" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(999)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_phone-area AS CHARACTER FORMAT "xxx":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE lv-phone-num AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE IMAGE cUserImage
     FILENAME "adeicon/blank":U TRANSPARENT
     SIZE 11 BY 2.62.

DEFINE RECTANGLE BGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE BGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE BGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE colorChoice-0
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 0 .

DEFINE RECTANGLE colorChoice-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 1 .

DEFINE RECTANGLE colorChoice-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 10 .

DEFINE RECTANGLE colorChoice-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 11 .

DEFINE RECTANGLE colorChoice-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 12 .

DEFINE RECTANGLE colorChoice-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 13 .

DEFINE RECTANGLE colorChoice-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 14 .

DEFINE RECTANGLE colorChoice-15
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE colorChoice-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 2 .

DEFINE RECTANGLE colorChoice-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 3 .

DEFINE RECTANGLE colorChoice-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 4 .

DEFINE RECTANGLE colorChoice-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 5 .

DEFINE RECTANGLE colorChoice-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 6 .

DEFINE RECTANGLE colorChoice-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 7 .

DEFINE RECTANGLE colorChoice-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE colorChoice-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 9 .

DEFINE RECTANGLE colorChoice-default
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE FGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE FGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE FGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 100 BY 3.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 142 BY 20.71.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 53 BY 12.38.

DEFINE VARIABLE slDatabases AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 30 BY 2.14
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE slEnvironments AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 30 BY 2.14
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE slModes AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Advantzware","Addon","Case Labels","Loadtags","RM Loadtags","Sharpshooter","Touchscreen","xxx" 
     SIZE 30 BY 4.05
     BGCOLOR 15 FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     users.AMPM AT ROW 19.1 COL 23 WIDGET-ID 534
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     users.user_id AT ROW 1.24 COL 21 COLON-ALIGNED
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     users.user_name AT ROW 1.24 COL 46 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
          BGCOLOR 15 FONT 4
     users.userAlias AT ROW 1.24 COL 99 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 15 FONT 4
     fiPassword AT ROW 2.43 COL 21 COLON-ALIGNED WIDGET-ID 80 PASSWORD-FIELD 
     users.department AT ROW 2.43 COL 80 COLON-ALIGNED
          LABEL "Dept"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 
     cbUserType AT ROW 2.43 COL 99 COLON-ALIGNED WIDGET-ID 48
     bChgPwd AT ROW 2.43 COL 61 WIDGET-ID 82 NO-TAB-STOP 
     users.phone-cnty AT ROW 3.62 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     fi_phone-area AT ROW 3.62 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     users.phone AT ROW 3.62 COL 77 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 84 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 15  NO-TAB-STOP 
     lv-phone-num AT ROW 3.62 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     users.securityLevel AT ROW 3.62 COL 99 COLON-ALIGNED WIDGET-ID 44
          LABEL "Security Level" FORMAT ">999"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     users.isActive AT ROW 3.62 COL 112 WIDGET-ID 90
          LABEL "Active?"
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     users.isManager AT ROW 3.62 COL 112 WIDGET-ID 90
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     users.isLocked AT ROW 3.62 COL 128 WIDGET-ID 88
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     users.mobile AT ROW 4.81 COL 21 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 18 FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
          BGCOLOR 15 FONT 4
     users.fax AT ROW 4.81 COL 77 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 86 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 15  NO-TAB-STOP 
     users.image_filename AT ROW 6 COL 21 COLON-ALIGNED HELP
          "Enter Main Menu Image File Name (fully qualified path)" WIDGET-ID 38
          LABEL "Email" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          BGCOLOR 15 FONT 4
     users.user_program[1] AT ROW 7.19 COL 21 COLON-ALIGNED
          LABEL "Image Viewer" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          BGCOLOR 15 FONT 4
     users.user_program[2] AT ROW 8.38 COL 21 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Report Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          BGCOLOR 15 FONT 4
     users.user_program[3] AT ROW 9.57 COL 21 COLON-ALIGNED WIDGET-ID 36
          LABEL "Document Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          BGCOLOR 15 FONT 4
     users.userImage[1] AT ROW 10.76 COL 21 COLON-ALIGNED WIDGET-ID 116
          LABEL "User Image" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     users.use_colors AT ROW 11.91 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY 1
     users.manager AT ROW 13.38 COL 65 COLON-ALIGNED
          LABEL "Manager"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     users.use_fonts AT ROW 12.86 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 26.2 BY 1
     users.menuSize AT ROW 14.57 COL 65 COLON-ALIGNED WIDGET-ID 112
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEMS "Small","Medium","Large" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     users.purchaseLimit AT ROW 15.71 COL 21 COLON-ALIGNED
          LABEL "PO Limit"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 
     users.userLanguage AT ROW 15.76 COL 61 COLON-ALIGNED WIDGET-ID 114
          VIEW-AS COMBO-BOX INNER-LINES 10
          LIST-ITEM-PAIRS "Englist","EN"
          DROP-DOWN-LIST
          SIZE 20 BY 1
     users.showMenuImages AT ROW 13.76 COL 23 WIDGET-ID 528
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY 1
     users.showCueCards AT ROW 14.71 COL 23 WIDGET-ID 532
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
     users.showMnemonic AT ROW 16.71 COL 23 NO-LABEL WIDGET-ID 120
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "None", "None":U,
"All", "All":U,
"Programs Only", "Program":U
          SIZE 34 BY 1
     users.positionMnemonic AT ROW 17.91 COL 23 NO-LABEL WIDGET-ID 126
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Begin", "Begin":U,
"End", "End":U
          SIZE 17 BY 1
     bDefaults AT ROW 5.52 COL 118 WIDGET-ID 104
     bAll3 AT ROW 7.43 COL 97 WIDGET-ID 72
     bNone3 AT ROW 8.14 COL 97 WIDGET-ID 74
     slModes AT ROW 6.95 COL 108 NO-LABEL WIDGET-ID 54
     bAll1 AT ROW 11.95 COL 97 WIDGET-ID 64
     bNone1 AT ROW 12.67 COL 97 WIDGET-ID 66
     slEnvironments AT ROW 11.71 COL 108 NO-LABEL WIDGET-ID 50
     bAll2 AT ROW 14.81 COL 97 WIDGET-ID 68
     bNone2 AT ROW 15.52 COL 97 WIDGET-ID 70
     slDatabases AT ROW 14.33 COL 108 NO-LABEL WIDGET-ID 52
     users.sessionLimit AT ROW 20.29 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1 TOOLTIP "~"0~" reverts to NK5 setting"
     "Environments:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 11.24 COL 91 WIDGET-ID 58
          FONT 4
     "BG Color:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 20.05 COL 43 WIDGET-ID 516
     "FG Color:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 18.86 COL 43 WIDGET-ID 520
     "Phone: (Country)" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 3.62 COL 15 WIDGET-ID 92
     "?" VIEW-AS TEXT
          SIZE 2 BY .76 AT ROW 19.57 COL 79 WIDGET-ID 522
          FGCOLOR 0 FONT 6
     "Mobile:" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 4.81 COL 15.6 WIDGET-ID 94
     "Time Display Use" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 19.1 COL 6 WIDGET-ID 536
     " At Login User Can Select:" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 4.81 COL 91 WIDGET-ID 56
          FONT 4
     "(Area)" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 3.62 COL 40 WIDGET-ID 96
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "HotKey (Mnemonic):" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 16.71 COL 3 WIDGET-ID 124
     "Options:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.14 COL 14 WIDGET-ID 42
     "HotKey Position:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 17.91 COL 6 WIDGET-ID 130
     "Databases:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 14.1 COL 94 WIDGET-ID 60
          FONT 4
     "Modes:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 6.71 COL 99 WIDGET-ID 62
          FONT 4
     "(#)" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 3.62 COL 56 WIDGET-ID 106
          BGCOLOR 15 
     "3" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 18.14 COL 69 WIDGET-ID 512
     "(Use CTRL-click to select multiple items)" VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 16.48 COL 98 WIDGET-ID 76
          FONT 1
     "2" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 18.14 COL 62 WIDGET-ID 514
     "Menu Level 1" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 18.14 COL 43 WIDGET-ID 518
     RECT-5 AT ROW 5.05 COL 88 WIDGET-ID 78
     cUserImage AT ROW 10.76 COL 72 WIDGET-ID 118
     colorChoice-0 AT ROW 18.86 COL 84 WIDGET-ID 472
     colorChoice-1 AT ROW 18.86 COL 91 WIDGET-ID 474
     colorChoice-2 AT ROW 18.86 COL 98 WIDGET-ID 488
     colorChoice-3 AT ROW 18.86 COL 105 WIDGET-ID 490
     colorChoice-4 AT ROW 18.86 COL 112 WIDGET-ID 492
     colorChoice-5 AT ROW 18.86 COL 119 WIDGET-ID 494
     colorChoice-6 AT ROW 18.86 COL 126 WIDGET-ID 496
     colorChoice-7 AT ROW 18.86 COL 133 WIDGET-ID 498
     colorChoice-8 AT ROW 20.05 COL 84 WIDGET-ID 500
     colorChoice-9 AT ROW 20.05 COL 91 WIDGET-ID 502
     colorChoice-10 AT ROW 20.05 COL 98 WIDGET-ID 476
     colorChoice-11 AT ROW 20.05 COL 105 WIDGET-ID 478
     colorChoice-12 AT ROW 20.05 COL 112 WIDGET-ID 480
     colorChoice-13 AT ROW 20.05 COL 119 WIDGET-ID 482
     colorChoice-14 AT ROW 20.05 COL 126 WIDGET-ID 484
     colorChoice-15 AT ROW 20.05 COL 133 WIDGET-ID 486
     colorChoice-default AT ROW 19.33 COL 77 WIDGET-ID 504
     FGColor-1 AT ROW 18.86 COL 53 WIDGET-ID 506
     FGColor-2 AT ROW 18.86 COL 60 WIDGET-ID 508
     FGColor-3 AT ROW 18.86 COL 67 WIDGET-ID 510
     BGColor-1 AT ROW 20.05 COL 53 WIDGET-ID 466
     BGColor-2 AT ROW 20.05 COL 60 WIDGET-ID 468
     BGColor-3 AT ROW 20.05 COL 67 WIDGET-ID 470
     RECT-1 AT ROW 17.91 COL 41 WIDGET-ID 524
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 526
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.users,asi.usr
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 20.71
         WIDTH              = 142.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bAll1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bAll2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bAll3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bChgPwd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bDefaults IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE BGColor-1 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       BGColor-1:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE BGColor-2 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       BGColor-2:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE BGColor-3 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       BGColor-3:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR BUTTON bNone1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bNone2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bNone3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbUserType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE colorChoice-0 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-0:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-1 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-1:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-10 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-10:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-11 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-11:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-12 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-12:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-13 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-13:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-14 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-14:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-15 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-15:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-2 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-2:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-3 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-3:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-4 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-4:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-5 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-5:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-6 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-6:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-7 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-7:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-8 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-8:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-9 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-9:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-default IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-default:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR IMAGE cUserImage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.department IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN users.fax IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       users.fax:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RECTANGLE FGColor-1 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       FGColor-1:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE FGColor-2 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       FGColor-2:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR RECTANGLE FGColor-3 IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       FGColor-3:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fiPassword IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_phone-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.image_filename IN FRAME F-Main
   2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR TOGGLE-BOX users.isActive IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       users.isActive:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN lv-phone-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.manager IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN users.mobile IN FRAME F-Main
   2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN users.phone IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       users.phone:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN users.phone-cnty IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN users.purchaseLimit IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.securityLevel IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN users.sessionLimit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slDatabases IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slEnvironments IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slModes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.userAlias IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.userImage[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN users.user_id IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN users.user_name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN users.user_program[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN users.user_program[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN users.user_program[3] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bAll1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bAll1 V-table-Win
ON CHOOSE OF bAll1 IN FRAME F-Main /* All */
OR CHOOSE OF bAll2
OR CHOOSE OF bAll3
OR CHOOSE OF bNone1
OR CHOOSE OF bNone2
OR CHOOSE OF bNone3
DO:
    CASE SELF:NAME:
        WHEN "bAll1" THEN ASSIGN slEnvironments:SCREEN-VALUE IN FRAME {&FRAME-NAME} = slEnvironments:LIST-ITEMS.
        WHEN "bAll2" THEN ASSIGN slDatabases:SCREEN-VALUE IN FRAME {&FRAME-NAME} = slDatabases:LIST-ITEMS.
        WHEN "bAll3" THEN ASSIGN slModes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = slModes:LIST-ITEMS.
        WHEN "bNone1" THEN ASSIGN slEnvironments:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
        WHEN "bNone2" THEN ASSIGN slDatabases:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
        WHEN "bNone3" THEN ASSIGN slModes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bChgPwd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bChgPwd V-table-Win
ON CHOOSE OF bChgPwd IN FRAME F-Main /* Change */
DO:
    DEFINE VARIABLE lPwdOK AS LOGICAL NO-UNDO.
    /* Verify password restrictions and display */
    IF bChgPwd:LABEL EQ "Change" 
    AND (usercontrol.minLC > 0 
    OR usercontrol.minUC > 0 
    OR usercontrol.minNC > 0 
    OR usercontrol.minSC > 0 
    OR usercontrol.minPasswordLen > 0) THEN DO:
        MESSAGE
            "Note: Passwords have the following restrictions:" SKIP
            "Minimum Length = " + STRING(usercontrol.minPasswordLen) SKIP
            "Minimum lower case = " + STRING(usercontrol.minLC) SKIP
            "Minimum UPPER case = " + STRING(usercontrol.minUC) SKIP
            "Minimum numeric chars = " + STRING(usercontrol.minNC) SKIP
            "Minimum special chars = " + STRING(usercontrol.minSC)
            VIEW-AS ALERT-BOX.
    END.
    
    IF bChgPwd:LABEL EQ "Change" AND NOT lAdd THEN
    DO: 
    
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
      RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("disable-all").
       
      ASSIGN
        fiPassword:SCREEN-VALUE = ""
        fiPassword:SENSITIVE = TRUE
        SELF:LABEL = "Save" .
        APPLY 'entry' TO fiPassword.
        RETURN NO-APPLY.
    END.
     IF bChgPwd:LABEL EQ "Save" AND NOT lAdd THEN 
     DO:
        RUN ipCheckPwd (INPUT-OUTPUT lPwdOK).
        IF NOT lPwdOK THEN DO:
            ASSIGN 
                fiPassword:SCREEN-VALUE = ""
                fiPassword:SENSITIVE = TRUE 
                SELF:LABEL = "Change"
                SELF:SENSITIVE = TRUE.          
            RETURN NO-APPLY.
        END.
        SELF:LABEL = "Change".          
        RUN ipChangePassword (fiPassword:SCREEN-VALUE).
        
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
        RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("initial").
        MESSAGE "Password changed. " VIEW-AS ALERT-BOX INFO.
     END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bDefaults V-table-Win
ON CHOOSE OF bDefaults IN FRAME F-Main /* Use Defaults */
DO:
    ASSIGN
        slModes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        slEnvironments:SCREEN-VALUE = ""
        slDatabases:SCREEN-VALUE = ""
        slModes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
            "Advantzware,Addon,CaseLabel,Loadtags,RM Loadtag,Sharpshooter,Touchscreen"
        slEnvironments:SCREEN-VALUE = ENTRY(1,slEnvironments:LIST-ITEMS)
        slDatabases:SCREEN-VALUE = ENTRY(1,slDatabases:LIST-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbUserType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUserType V-table-Win
ON VALUE-CHANGED OF cbUserType IN FRAME F-Main /* User Type */
DO:
    CASE SELF:SCREEN-VALUE:
        WHEN "Portal User" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "10".
        WHEN "Production Floor" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "50".
        WHEN "Full User" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "100".
        WHEN "Administrator" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "900".
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorChoice-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorChoice-0 V-table-Win
ON SELECTION OF colorChoice-0 IN FRAME F-Main
,colorChoice-1,colorChoice-2,colorChoice-3,colorChoice-4,colorChoice-5
,colorChoice-6,colorChoice-7,colorChoice-8,colorChoice-9,colorChoice-10
,colorChoice-11,colorChoice-12,colorChoice-13,colorChoice-14,colorChoice-15
,colorChoice-default
DO:
    IF VALID-HANDLE(hColorWidget) THEN
    ASSIGN
        hColorWidget:BGCOLOR = SELF:BGCOLOR
        hColorWidget:FILLED  = TRUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.department
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.department V-table-Win
ON HELP OF users.department IN FRAME F-Main /* Dept */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        g_company, 
        "dept", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN 
    DO:
        users.department:SCREEN-VALUE = lookupField.
    
        APPLY "LEAVE" TO SELF.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FGColor-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FGColor-1 V-table-Win
ON SELECTION OF FGColor-1 IN FRAME F-Main
,FGColor-2,FGColor-3,BGColor-1,BGColor-2,BGColor-3
DO:
    hColorWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword V-table-Win
ON END-ERROR OF fiPassword IN FRAME F-Main /* Password */
DO:
    ASSIGN  
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = _user._password
        fiPassword:SENSITIVE = FALSE
        bChgPwd:SENSITIVE = TRUE.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword V-table-Win
ON LEAVE OF fiPassword IN FRAME F-Main /* Password */
OR RETURN OF fiPassword
DO:
    DEF VAR lPwdOK AS LOG NO-UNDO.
    
    IF LASTKEY EQ -1 
        THEN RETURN.
    
    RUN ipCheckPwd (INPUT-OUTPUT lPwdOK).
    IF NOT lPwdOK THEN DO:
        ASSIGN 
            SELF:SCREEN-VALUE = ""
            SELF:SENSITIVE = TRUE .
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
    END.
    IF NOT lAdd THEN 
    DO: 
       bChgPwd:LABEL = "Change".         
       RUN ipChangePassword (SELF:SCREEN-VALUE).
       MESSAGE "Password changed. " VIEW-AS ALERT-BOX INFO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.manager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.manager V-table-Win
ON HELP OF users.manager IN FRAME F-Main /* Manager */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        g_company, 
        "users-manager", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN 
    DO:
        users.manager:SCREEN-VALUE = lookupField.
    
        APPLY "LEAVE" TO SELF.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.menuSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.menuSize V-table-Win
ON VALUE-CHANGED OF users.menuSize IN FRAME F-Main /* Menu Size */
DO:
    lMenuChanges = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.positionMnemonic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.positionMnemonic V-table-Win
ON VALUE-CHANGED OF users.positionMnemonic IN FRAME F-Main /* Position Mnemonic */
DO:
    lMenuChanges = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.securityLevel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.securityLevel V-table-Win
ON LEAVE OF users.securityLevel IN FRAME F-Main /* Security Level */
DO:
    IF (zUsers.securityLevel < 1000 OR users.user_id:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "asi")
    AND INTEGER(SELF:SCREEN-VALUE) GE 1000 THEN DO:
        ASSIGN
            SELF:SCREEN-VALUE = "700".
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.showMnemonic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.showMnemonic V-table-Win
ON VALUE-CHANGED OF users.showMnemonic IN FRAME F-Main /* Show Mnemonic */
DO:
    ASSIGN
        users.positionMnemonic:SENSITIVE = SELF:SCREEN-VALUE NE "None"
        lMenuChanges = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.userAlias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.userAlias V-table-Win
ON LEAVE OF users.userAlias IN FRAME F-Main /* Login Alias */
DO:
    IF SELF:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST ttUsers NO-LOCK WHERE 
            ttUsers.ttfuserAlias = SELF:SCREEN-VALUE AND
            ttUsers.ttfpdbname = "*" AND
            ttUsers.ttfuserid NE users.user_id:SCREEN-VALUE IN FRAME f-main
            NO-ERROR.
        IF AVAIL ttUsers THEN DO:
            MESSAGE
                "Duplicate alias detected with user " + ttUsers.ttfuserid + ". Please enter a different value."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN
                SELF:SCREEN-VALUE = "".
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.userImage[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.userImage[1] V-table-Win
ON HELP OF users.userImage[1] IN FRAME F-Main /* User Image */
DO:
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitDir   AS CHARACTER NO-UNDO INITIAL ".\".
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.

    SYSTEM-DIALOG GET-FILE cImageFile 
        TITLE "Select Image File"
        FILTERS "PNG Files    (*.png)" "*.png",
                "Bitmap files (*.bmp)" "*.bmp",
                "ICO Files    (*.ico)" "*.ico",
                "JPG Files    (*.jpg)" "*.jpg",                 
                "JPEG Files   (*.jpeg)" "*.jpeg",
                "All Files    (*.*) " "*.*"
        INITIAL-DIR cInitDir
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK
        .
    IF lOK THEN DO:
        cUserImage:LOAD-IMAGE(cImageFile).
        SELF:SCREEN-VALUE = REPLACE(cImageFile,cInitDir,"").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.userImage[1] V-table-Win
ON LEAVE OF users.userImage[1] IN FRAME F-Main /* User Image */
DO:
    IF SELF:SCREEN-VALUE NE "" THEN 
    cUserImage:LOAD-IMAGE(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.userLanguage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.userLanguage V-table-Win
ON VALUE-CHANGED OF users.userLanguage IN FRAME F-Main /* Language */
DO:
    lMenuChanges = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.user_id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_id V-table-Win
ON LEAVE OF users.user_id IN FRAME F-Main /* User ID */
DO:
    IF SELF:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST lUsers NO-LOCK WHERE 
            lUsers.userAlias = SELF:SCREEN-VALUE
            NO-ERROR.
        IF AVAIL lUsers THEN DO:
            MESSAGE
                "Duplicate User ID detected. Please enter a different value."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN
                SELF:SCREEN-VALUE = "".
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
        END.
        IF INDEX(users.user_id:SCREEN-VALUE,",") NE 0 THEN
           users.user_id:SCREEN-VALUE = REPLACE(users.user_id:SCREEN-VALUE,",","") .
        users.user_id:SCREEN-VALUE = TRIM(users.user_id:SCREEN-VALUE) .
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.user_program[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[1] V-table-Win
ON HELP OF users.user_program[1] IN FRAME F-Main /* Image Viewer */
DO:
    DEF VAR ls-filename AS CHAR NO-UNDO.
    DEF VAR ll-ok AS LOG NO-UNDO.

    SYSTEM-DIALOG GET-FILE ls-filename 
        TITLE "Select Image Viewer File to insert"
        FILTERS "Application Files    (*.exe)" "*.exe",
                "All Files    (*.*) " "*.*"
        INITIAL-DIR '"c:\program files\"'
        MUST-EXIST
        USE-FILENAME
        UPDATE ll-ok.

     IF INDEX(ls-filename,"/") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(ls-filename,R-INDEX(ls-filename,"/") + 1).
     ELSE IF INDEX(ls-filename,"\") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(ls-filename,R-INDEX(ls-filename,"\") + 1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[1] V-table-Win
ON LEAVE OF users.user_program[1] IN FRAME F-Main /* Image Viewer */
DO:

    IF INDEX(SELF:SCREEN-VALUE,"/") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,R-INDEX(SELF:SCREEN-VALUE,"/") + 1).
    ELSE IF INDEX(SELF:SCREEN-VALUE,"\") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,R-INDEX(SELF:SCREEN-VALUE,"\") + 1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.user_program[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[3] V-table-Win
ON HELP OF users.user_program[3] IN FRAME F-Main /* Document Path */
DO:
    DEF VAR ls-filename AS CHAR NO-UNDO.
    DEF VAR ll-ok AS LOG NO-UNDO.

    SYSTEM-DIALOG GET-DIR ls-filename 
        TITLE "Select Path to save"
        INITIAL-DIR users.USER_program[3]
        UPDATE ll-ok.

    IF ll-ok THEN ASSIGN
        SELF:SCREEN-VALUE = ls-filename.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.user_program[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[2] V-table-Win
ON HELP OF users.user_program[2] IN FRAME F-Main /* Document Path */
DO:
    DEF VAR ls-filename AS CHAR NO-UNDO.
    DEF VAR ll-ok AS LOG NO-UNDO.

    SYSTEM-DIALOG GET-DIR ls-filename 
        TITLE "Select Path to save"
        INITIAL-DIR users.USER_program[2]
        UPDATE ll-ok.

    IF ll-ok THEN ASSIGN
        SELF:SCREEN-VALUE = ls-filename.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
    {sys/inc/f3help.i}
    {custom/gcompany.i}
    {custom/getcmpny.i}

    DO TRANSACTION:
        {sys/inc/webroot.i}
    END.
    
    FIND FIRST usercontrol NO-LOCK NO-ERROR.
    IF NOT AVAIL usercontrol THEN DO:
        MESSAGE
            "There is no usercontrol record defined." SKIP
            "Please contact your System Administrator."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    FIND zUsers NO-LOCK WHERE
        zUsers.user_id = USERID(LDBNAME(1))
        NO-ERROR.
    RUN ipReadIniFile.
    RUN ipReadUsrFile.
    
    IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
    RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
    IF VALID-HANDLE(hPgmMstrSecur) THEN DO:
        RUN epCanAccess IN hPgmMstrSecur (
            "viewers/users.w",
            "SuperAdmin",
            OUTPUT lSuperAdmin 
            ).
        RUN epCanAccess IN hPgmMstrSecur (
            "viewers/users.w",
            "Admin",
            OUTPUT lAdmin 
            ).
    END. /* if valid-handle */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF       
  
    ASSIGN
        slEnvironments:LIST-ITEMS = cEnvList
        slDatabases:LIST-ITEMS = cDbList
        slModes:LIST-ITEMS = cModeList
        /* Future development
        users.isActive:SENSITIVE = if zUsers.securityLevel > 899 THEN TRUE ELSE FALSE
        */
        .  

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "users"}
  {src/adm/template/row-list.i "usr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "users"}
  {src/adm/template/row-find.i "usr"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangePassword V-table-Win 
PROCEDURE ipChangePassword :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cNewPassword AS CHARACTER NO-UNDO INITIAL 'jill'.

    FIND FIRST _User WHERE 
        _User._UserId = users.user_id 
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL (_User) 
    AND cNewPassword NE _User._password THEN DO: /* IF displayed value is already encoded password, don't change */
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser
            ASSIGN 
                tempUser._Password = ENCODE(cNewPassword).
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
        FIND FIRST userPwdHist EXCLUSIVE WHERE
            userPwdHist.user_id = _user._userID AND
            userPwdHist.pwd = ENCODE(cNewPassword) AND
            userPwdHist.pwdDate = today NO-ERROR.
        IF NOT AVAIL userPwdHist THEN DO:
            CREATE userPwdHist.
            ASSIGN
                userPwdHist.user_id = _user._userID
                userPwdHist.pwdDate = TODAY 
                userPwdHist.createDate = TODAY
                userPwdHist.createTime = TIME
                userPwdHist.createUser = USERID(LDBNAME(1)).
        END.
        ASSIGN
            userPwdHist.pwd = ENCODE(cNewPassword)
            userPwdHist.updateDate = TODAY
            userPwdHist.updateTime = TIME
            userPwdHist.updateUser = USERID(LDBNAME(1))
            .
    END.

    ASSIGN  
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = _user._password
        fiPassword:SENSITIVE = FALSE
        bChgPwd:SENSITIVE = TRUE
        fiPassword:MODIFIED = FALSE.          
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCheckPwd V-table-Win 
PROCEDURE ipCheckPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT-OUTPUT PARAMETER lPwdOK AS LOG NO-UNDO.
    
    DEF VAR iHasLC AS INT NO-UNDO.
    DEF VAR iHasUC AS INT NO-UNDO.
    DEF VAR iHasNC AS INT NO-UNDO.
    DEF VAR iHasSC AS INT NO-UNDO.
    DEF VAR iHasLen AS INT NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR cMsg AS CHAR NO-UNDO.
    
    ASSIGN
        lPwdOK = YES.
        
    DO iCtr = 1 TO LENGTH(fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
        IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 31 
        OR ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) EQ 34 /* dbl-quote */
        OR ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) EQ 39 /* apostrophe */
        OR ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) EQ 40 /* left paren */
        OR ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) EQ 41 /* right paren */
        THEN DO:
            ASSIGN
                lPwdOK = FALSE.
            MESSAGE
                "Your Password contains invalid characters.  Please limit your password to" skip
                "letters (A-Z,a-z), numbers (0-9), or the following valid characters:" skip
                "!,@,#,$,%,^,*,-,_,+,=,[,],/,\,<,>,?,|,;,:,comma,period" skip
                "(NOTE - apostrophe, dbl-quote marks and parentheses are NOT allowed.)"
                VIEW-AS ALERT-BOX.
            RETURN.
        END.
    END.

    /* Verify password restrictions and display */
    IF usercontrol.minLC > 0 
    OR usercontrol.minUC > 0 
    OR usercontrol.minNC > 0 
    OR usercontrol.minSC > 0 
    OR usercontrol.minPasswordLen > 0 THEN DO:
        DO iCtr = 1 TO LENGTH(fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 97
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 122 THEN ASSIGN
                iHasLC = iHasLC + 1.
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 65
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 90 THEN ASSIGN
                iHasUC = iHasUC + 1.
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 48
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 57 THEN ASSIGN
                iHasNC = iHasNC + 1.
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 33
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 47 THEN ASSIGN
                iHasSC = iHasSC + 1.
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 58
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 64 THEN ASSIGN
                iHasSC = iHasSC + 1.
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 91
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 96 THEN ASSIGN
                iHasSC = iHasSC + 1.
            IF ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) GE 123
            AND ASC(SUBSTRING(fiPassword:SCREEN-VALUE,iCtr,1)) LE 126 THEN ASSIGN
                iHasSC = iHasSC + 1.
        END.
        IF ENCODE(fiPassword:SCREEN-VALUE) = cOldPwd THEN DO:
            MESSAGE
                "The password you entered is the same as the existing" SKIP
                "password. This is not allowed in this environment."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN
                fiPassword:SCREEN-VALUE = cOldPwd
                fiPassword:SENSITIVE = FALSE
                lPwdOK = NO
                lPwdChanged = FALSE
                bChgPwd:SENSITIVE = TRUE.
            RETURN.
        END.
    END.
    ASSIGN
        iHasLen = LENGTH(fiPassword:SCREEN-VALUE).
    IF iHasLen LT usercontrol.minPasswordLen
    OR iHasLC LT usercontrol.minLC
    OR iHasUC LT usercontrol.minUC
    OR iHasNC LT usercontrol.minNC
    OR iHasSC LT usercontrol.minSC THEN DO:
        ASSIGN
            lPwdOK = NO
            cMsg = "The password you entered does not meet requirements." + CHR(10) + 
                    (IF (iHasLen LT usercontrol.minPasswordLen) THEN ("Minimum Length: " + STRING(usercontrol.minPasswordLen) + " - Yours has " + STRING(iHasLen) + CHR(10)) ELSE "") +
                    (IF (iHasLC LT usercontrol.minLC) THEN ("Minimum lower case: " + STRING(usercontrol.minLC) + " - Yours has " + STRING(iHasLC) + CHR(10)) ELSE "") +
                    (IF (iHasUC LT usercontrol.minUC) THEN ("Minimum UPPER case: " + STRING(usercontrol.minUC) + " - Yours has " + STRING(iHasUC) + CHR(10)) ELSE "") +
                    (IF (iHasNC LT usercontrol.minNC) THEN ("Minimum numeric chars: " + STRING(usercontrol.minNC) + " - Yours has " + STRING(iHasNC) + CHR(10)) ELSE "") +
                    (IF (iHasSC LT usercontrol.minSC) THEN ("Minimum special chars: " + STRING(usercontrol.minSC) + " - Yours has " + STRING(iHasSC) + CHR(10)) ELSE "") +
                    "Please try again." 
                    .
        MESSAGE
            cMsg
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    ASSIGN
        lPwdChanged = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadIniFile V-table-Win 
PROCEDURE ipReadIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cIniLine AS CHAR NO-UNDO.
    
    IF SEARCH(cIniLoc) NE ? THEN do:
        INPUT FROM VALUE(SEARCH(cIniLoc)) .
        REPEAT:
            IMPORT UNFORMATTED cIniLine.
            IF cIniLine BEGINS "envList" THEN ASSIGN
                cEnvList = ENTRY(2,cIniLine,"=").
            ELSE IF cIniLine BEGINS "dbList" THEN ASSIGN
                cDbList = ENTRY(2,cIniLine,"=").
            IF cIniLine BEGINS "modeList" THEN ASSIGN
                cModeList = ENTRY(2,cIniLine,"=").
            ELSE NEXT.
        END.
        INPUT CLOSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadUsrFile V-table-Win 
PROCEDURE ipReadUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cUsrLine AS CHAR NO-UNDO.
    
    IF SEARCH(cUsrLoc) NE ? THEN do:
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
                    .
            END.
        END.
        INPUT CLOSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipWriteUsrFile V-table-Win 
PROCEDURE ipWriteUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cOutString AS CHAR.
    
    OUTPUT TO VALUE(cUsrLoc).
    FOR EACH ttUsers BY ttUsers.ttfPdbname by ttUsers.ttfUserID:
        ASSIGN cOutString = 
                ttUsers.ttfUserID + "|" + 
                ttUsers.ttfPdbName + "|" +
                ttUsers.ttfUserAlias + "|" + 
                ttUsers.ttfEnvList + "|" +
                ttUsers.ttfDbList + "|" +
                ttUsers.ttfModeList.
        PUT UNFORMATTED cOutString + CHR(10).
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    {methods/template/local/create.i}

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
    FIND FIRST userControl NO-LOCK NO-ERROR. 
  
    ASSIGN
        lAdd = TRUE
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fiPassword:SENSITIVE = TRUE
        fi_phone-area:SCREEN-VALUE = ""
        lv-phone-num:SCREEN-VALUE = ""
        cbUserType:SCREEN-VALUE = "Full User"
        users.securityLevel:SCREEN-VALUE = "100".
        
    APPLY 'entry' to users.user_id IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* {methods/viewers/assign/{&FIRST-EXTERNAL-TABLE}.i}  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
  
    DISABLE 
        users.user_id
        users.user_name
        users.manager
        users.department
        users.userAlias
        users.securityLevel
        fiPassword
        fi_phone-area 
        lv-phone-num 
        cbUserType
        slEnvironments
        slDatabases
        slModes
        bAll1
        bNone1
        bAll2
        bNone2
        bAll3
        bNone3
        bDefaults
            WITH FRAME {&FRAME-NAME}.
        lAdd = FALSE .     
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    {methods/template/local/create.i}
    ASSIGN
        lAdd = TRUE
        lCopy = TRUE
        cOldUserID = users.user_id:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    {methods/template/local/copy.i}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF user-print.
    DISABLE TRIGGERS FOR LOAD OF usercomp.
    DISABLE TRIGGERS FOR LOAD OF userEula.
    DISABLE TRIGGERS FOR LOAD OF userlog.
    DISABLE TRIGGERS FOR LOAD OF usersman.
    DISABLE TRIGGERS FOR LOAD OF uservend.
    DISABLE TRIGGERS FOR LOAD OF usr.
    DISABLE TRIGGERS FOR LOAD OF usr-grp.
    DISABLE TRIGGERS FOR LOAD OF usr-menu.
    DISABLE TRIGGERS FOR LOAD OF usrx.
    DISABLE TRIGGERS FOR LOAD OF reftable.
    DISABLE TRIGGERS FOR LOAD OF xUserMenu.
    DISABLE TRIGGERS FOR LOAD OF cueCardText.

    {methods/template/local/delete.i}

    FOR EACH user-print EXCLUSIVE WHERE
        user-print.user-id = users.user_id:
        DELETE user-print.
    END.
    FOR EACH usercomp EXCLUSIVE WHERE 
        usercomp.USER_id = users.USER_id:
        DELETE usercomp.
    END.
    FOR EACH usercust EXCLUSIVE WHERE 
        usercust.user_id EQ users.user_id:
        DELETE usercust.
    END.     
    FOR EACH userEula EXCLUSIVE WHERE 
        userEula.user_id EQ users.user_id:
        DELETE userEula.
    END.     
    FOR EACH userLog EXCLUSIVE WHERE 
        userLog.user_id EQ users.user_id:
        DELETE userLog.
    END.     
    FOR EACH usersman EXCLUSIVE WHERE 
        usersman.user_id EQ users.user_id:
        DELETE usersman.
    END.     
    FOR EACH uservend EXCLUSIVE WHERE 
        uservend.user_id EQ users.user_id:
        DELETE uservend.
    END.     
    FOR EACH usr EXCLUSIVE WHERE 
        usr.uid EQ users.user_id:
        DELETE usr.
    END.
    FOR EACH usr-grp EXCLUSIVE WHERE 
        usr-grp.uid EQ users.user_id:
        DELETE usr-grp.
    END.     
    FOR EACH usr-menu EXCLUSIVE WHERE 
        usr-menu.user_id EQ users.user_id:
        DELETE usr-menu.
    END.     
    FOR EACH usrx EXCLUSIVE WHERE 
        usrx.uid EQ users.user_id:
        DELETE usrx.
    END.
    FOR EACH xUserMenu EXCLUSIVE WHERE 
        xUserMenu.user_id EQ users.user_id:
        DELETE xUserMenu.
    END.
    FOR EACH cueCardText EXCLUSIVE WHERE 
        cueCardText.createdFor EQ users.user_id:
        DELETE cueCardText.
    END.
   
    FIND ttUsers EXCLUSIVE WHERE
        ttUsers.ttfPdbname = "*" AND
        ttUsers.ttfUserID = users.user_id:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-ERROR.
    IF AVAIL ttUsers THEN DO:
        DELETE ttUsers.
        RUN ipWriteUsrFile.
        EMPTY TEMP-TABLE ttUsers.
        RUN ipReadUsrFile.
    END.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    {methods/template/local/deleteAfter.i}

    {methods/run_link.i "RECORD-SOURCE" "ipGoBack"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  {methods/template/local/disable.i}
  {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    IF AVAIL users THEN DO WITH FRAME {&frame-name}:
        /* Most elements come from the 'generic' ttUser (ttfPdbname = '*') */
        FIND FIRST ttUsers WHERE
            ttUsers.ttfPdbName = "*" AND
            ttUsers.ttfUserID = users.user_id
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfPdbName = "*"
                ttUsers.ttfUserID = users.user_id
                .
/* 39245 - User MODE does not save - 12/10/18 - MYT - remove references to db-based fields; use .usr file only */
/*                IF ttUsers.ttfEnvlist EQ "" THEN                                                                  */
/*                  ttUsers.ttfEnvList = IF users.envList GT "" THEN REPLACE(users.envList, "|", ",") ELSE "".      */
/*                IF ttUsers.ttfDbList EQ "" THEN                                                                   */
/*                  ttUsers.ttfDbList = IF users.dbList GT "" THEN REPLACE(users.dbList, "|", ",") ELSE "".         */
/*                IF ttUsers.ttfUserAlias EQ "" THEN                                                                */
/*                  ttUsers.ttfUserAlias = IF users.userAlias GT "" THEN REPLACE(users.userAlias, "|", ",") ELSE "".*/
/*                IF ttUsers.ttfModeList EQ "" THEN                                                                 */
/*                  ttUsers.ttfModeList = IF users.modeList GT "" THEN REPLACE(users.modeList, "|", ",") ELSE "".   */
            
        END.

        ASSIGN 
            fi_phone-area:screen-value = substring(users.phone,1,3)
            lv-phone-num:screen-value = substring(users.phone,4)
            cbUserType:screen-value = users.userType
            slEnvironments:screen-value = if ttUsers.ttfEnvList <> "" THEN ttUsers.ttfEnvList else slEnvironments:list-items
            slDatabases:screen-value = if ttUsers.ttfDbList <> "" THEN ttUsers.ttfDbList else slDatabases:list-items
            users.userAlias:SCREEN-VALUE = users.userAlias
            users.userAlias:modified = FALSE
            FGColor-1:BGCOLOR = users.menuFGColor[1]
            FGColor-2:BGCOLOR = users.menuFGColor[2]
            FGColor-3:BGCOLOR = users.menuFGColor[3]
            BGColor-1:BGCOLOR = users.menuBGColor[1]
            BGColor-2:BGCOLOR = users.menuBGColor[2]
            BGColor-3:BGCOLOR = users.menuBGColor[3]
            .

        /* But mode-list has a by-db component (ttfPdbname = pdbname(1)) */
        FIND FIRST ttUsers WHERE
            ttUsers.ttfPdbName = PDBNAME(1) AND
            ttUsers.ttfUserID = users.user_id
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfPdbName = PDBNAME(1)
                ttUsers.ttfUserID = users.user_id
                .
/* 39245 - User MODE does not save - 12/10/18 - MYT - remove references to db-based fields; use .usr file only */
/*            IF ttUsers.ttfModeList EQ "" THEN                                                                                */
/*                ttUsers.ttfModeList = IF users.modeList GT "" THEN REPLACE(users.modeList, "|", ",") ELSE slModes:list-items.*/
        END.
        slModes:SCREEN-VALUE = IF ttUsers.ttfModeList NE "" THEN ttUsers.ttfModeList ELSE slModes:LIST-ITEMS.

        FIND _user NO-LOCK WHERE 
            _user._userid = users.user_id
            NO-ERROR.
        IF AVAIL _user THEN
        ASSIGN
            fiPassword:SCREEN-VALUE = _user._password
            cOldPwd = _user._password
            .
        ASSIGN
            bChgPwd:SENSITIVE = IF users.user_id = zusers.user_id OR zusers.securityLevel > 699 THEN TRUE ELSE FALSE
            fiPassword:SENSITIVE = FALSE
            .
        IF users.userImage[1]:SCREEN-VALUE NE "" THEN 
        cUserImage:LOAD-IMAGE(users.userImage[1]:SCREEN-VALUE).
        
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    {methods/template/local/update.i}

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    {methods/template/local/enable.i}
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'reset-record':U ) .
  {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .  

    {methods/viewers/rowavail.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-usercust FOR usercust .
    DEFINE BUFFER bf-usrx     FOR usrx .
    DEFINE BUFFER bf-usercomp FOR usercomp.
    
    DEFINE VARIABLE lv-default-comp     AS CHARACTER                NO-UNDO.
    DEFINE VARIABLE lv-default-loc      AS CHARACTER                NO-UNDO.
    DEFINE VARIABLE ll-ans              AS LOGICAL                  NO-UNDO.
    DEFINE VARIABLE ll-dummy            AS LOGICAL                  NO-UNDO.
    DEFINE VARIABLE v-old-pass          AS CHARACTER FORM "x(30)"   NO-UNDO.
    DEFINE VARIABLE v-new-pass          AS CHARACTER FORM "x(30)"   NO-UNDO.
    DEFINE VARIABLE cNewPwd             AS CHARACTER                NO-UNDO.
    DEFINE VARIABLE lPwdOK              AS LOGICAL                  NO-UNDO.
    DEFINE VARIABLE cCurrentDir         AS CHARACTER                NO-UNDO.
    DEFINE VARIABLE iStat               AS INTEGER                  NO-UNDO.
    DEFINE VARIABLE hMainMenu           AS HANDLE                   NO-UNDO.
    DEFINE VARIABLE iSecLevel           AS INTEGER                  NO-UNDO.
    DEFINE VARIABLE cFilePath           AS CHARACTER                NO-UNDO.
    DEFINE VARIABLE lCreated            AS LOGICAL                  NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER                NO-UNDO.
    
    ASSIGN 
        cOldUserID = users.user_id
        .

    RUN validate-userid NO-ERROR.
    IF error-status:error THEN RETURN.

    IF users.user_program[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
        FILE-INFO:FILE-NAME = users.USER_program[1].
        IF FILE-INFO:FILE-type eq ? then do:
            MESSAGE 
                "Image Viewer file does not exist. Please change it to an existing file." 
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO users.user_program[1].
            RETURN.
        END.
    END.

    IF users.user_program[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
        IF SUBSTRING(users.user_program[2]:SCREEN-VALUE,LENGTH(users.user_program[2]:SCREEN-VALUE),1) EQ "\" 
        OR SUBSTRING(users.user_program[2]:SCREEN-VALUE,LENGTH(users.user_program[2]:SCREEN-VALUE),1) EQ "/" THEN DO:
            ASSIGN
                users.user_program[2]:SCREEN-VALUE = SUBSTRING(users.user_program[2]:SCREEN-VALUE,1,LENGTH(users.user_program[2]:SCREEN-VALUE) - 1).
        END.

        FILE-INFO:FILE-NAME = users.user_program[2]:SCREEN-VALUE.
        cFilePath = FILE-INFO:FILE-NAME.
        IF FILE-INFO:FILE-type eq ? then do:
            MESSAGE 
                "Document Path does not exist. Do you want to create it?" 
                VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
            IF v-ans THEN DO:
              RUN FileSys_CreateDirectory(
              INPUT  cFilePath,
              OUTPUT lCreated,
              OUTPUT cMessage
              ) NO-ERROR.
              IF NOT lCreated THEN DO:
                  MESSAGE "Unable to find report path '" + cFilePath + "' to export report file"
                      VIEW-AS ALERT-BOX ERROR.
                  RETURN.
              END. /* IF NOT lCreated */
            END.  /* IF v-ans */
        END.   /* IF FILE-INFO:FILE-type */
    END.     /* IF users.user_program[2] */

    IF users.user_program[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
        IF SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "\" 
        OR SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "/" THEN DO:
            ASSIGN
                users.user_program[3]:SCREEN-VALUE = SUBSTRING(users.user_program[3]:SCREEN-VALUE,1,LENGTH(users.user_program[3]:SCREEN-VALUE) - 1).
        END.
         
        FILE-INFO:FILE-NAME = users.USER_program[3]:SCREEN-VALUE.
        cFilePath = FILE-INFO:FILE-NAME.
        IF FILE-INFO:FILE-type eq ? then do:
            MESSAGE 
                "Document Path does not exist. Do you want to create it?" 
                VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE v-ans2 AS LOG.
            IF v-ans2 THEN DO:
              RUN FileSys_CreateDirectory(
              INPUT  cFilePath,
              OUTPUT lCreated,
              OUTPUT cMessage
              ) NO-ERROR.
              IF NOT lCreated THEN DO:
                  MESSAGE "Unable to find report path '" + cFilePath + "' to export report file"
                      VIEW-AS ALERT-BOX ERROR.
                  RETURN.
              END. /* IF NOT lCreated */
            END.   /* IF v-ans2 */
        END.     /* IF FILE-INFO:FILE-type */
    END.        /* IF users.user_program[3] */

    IF fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        MESSAGE
            "You have not created a password for this" SKIP
            "user. Would you like to do so now?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lNewPwd AS LOG.
        IF lNewPwd THEN DO:
            APPLY 'entry' to fiPassword.
            RETURN.
        END.
        ELSE DO:
            RUN ipCheckPwd (INPUT-OUTPUT lPwdOK).
            IF NOT lPwdOK THEN DO:
                ASSIGN fiPassword:SCREEN-VALUE = "".
                RETURN.
            END.
        END.            
    END.
    /*
    IF fiPassword:MODIFIED THEN DO:
        RUN ipCheckPwd (INPUT-OUTPUT lPwdOK).
        IF NOT lPwdOK THEN 
        DO:
            ASSIGN 
                fiPassword:SCREEN-VALUE = "".
            RETURN NO-APPLY.
        END.
        IF NOT lAdd THEN 
            RUN ipChangePassword (fiPassword:SCREEN-VALUE).
    END.
    */                
    IF lAdd THEN DO:
        /* Add _user record */
        FIND FIRST _user EXCLUSIVE WHERE
            _user._userid = users.user_id:SCREEN-VALUE
            NO-ERROR.
        IF NOT AVAIL _user THEN DO:
            CREATE _user.
            ASSIGN
                _user._userid = users.user_id:SCREEN-VALUE
                _user._password = ENCODE(fiPassword:SCREEN-VALUE)
                _user._user-name = users.user_name:SCREEN-VALUE.
            ASSIGN
                fiPassword:SCREEN-VALUE = _user._password.
        END.
        
        /* Add default usercomp records for new user */
        FIND FIRST bf-usercomp NO-LOCK WHERE 
            bf-usercomp.USER_id = "ASI" AND
            bf-usercomp.company_default 
            NO-ERROR.
        ASSIGN 
            lv-default-comp = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001".
        FIND FIRST usercomp NO-LOCK WHERE 
            usercomp.USER_id = users.user_id:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND 
            usercomp.company = lv-default-comp AND
            usercomp.loc = ""
            NO-ERROR.
        IF NOT AVAIL usercomp THEN DO:
            CREATE usercomp.
            ASSIGN 
                usercomp.user_id = users.user_id:SCREEN-VALUE
                usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
                usercomp.loc = ""
                usercomp.company_default = YES.
        END.
        FIND FIRST bf-usercomp NO-LOCK WHERE
            bf-usercomp.user_id = "ASI" AND
            bf-usercomp.loc_default 
            NO-ERROR.
        ASSIGN 
            lv-default-loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".
        FIND FIRST usercomp NO-LOCK WHERE 
            usercomp.user_id = users.user_id:SCREEN-VALUE AND 
            usercomp.company = lv-default-comp AND
            usercomp.loc = lv-default-loc 
            NO-ERROR.
        IF NOT AVAIL usercomp THEN DO:
            CREATE usercomp.
            ASSIGN 
                usercomp.user_id = users.user_id:SCREEN-VALUE
                usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
                usercomp.loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
                usercomp.loc_DEFAULT = YES.
        END.
        
        
        IF lCopy THEN DO:
            FOR EACH usercust NO-LOCK WHERE 
                usercust.user_id EQ cOldUserID AND 
                usercust.company EQ cocode  , 
                FIRST cust WHERE 
                    cust.company EQ usercust.company AND 
                    cust.cust-no EQ usercust.cust-no NO-LOCK  :
    
                CREATE bf-usercust .
                BUFFER-COPY usercust EXCEPT rec_key user_id TO bf-usercust
                    ASSIGN
                        bf-usercust.user_id = users.user_id:SCREEN-VALUE.
            END.
            FOR EACH usrx NO-LOCK WHERE 
                usrx.uid = cOldUserID AND 
                usrx.company = cocode AND 
                usrx.loc NE "", 
                EACH loc OF usrx NO-LOCK:
    
                CREATE bf-usrx .
                BUFFER-COPY usrx EXCEPT rec_key uid TO bf-usrx
                    ASSIGN
                        bf-usrx.uid = users.user_id:SCREEN-VALUE.
            END.

            FOR EACH prgrms :
                IF LOOKUP(cOldUserID,prgrms.can_run) > 0 
                AND LOOKUP(users.user_id:SCREEN-VALUE IN FRAME {&FRAME-NAME},prgrms.can_run) <= 0 THEN ASSIGN
                    prgrms.can_run = prgrms.can_run + "," + users.user_id:SCREEN-VALUE.
                IF LOOKUP(cOldUserID,prgrms.can_create) > 0 
                AND LOOKUP(users.user_id:SCREEN-VALUE,prgrms.can_create) <= 0 THEN ASSIGN
                    prgrms.can_create = prgrms.can_create + "," + users.user_id:SCREEN-VALUE.
                IF LOOKUP(cOldUserID,prgrms.can_update) > 0 
                AND LOOKUP(users.user_id:SCREEN-VALUE,prgrms.can_update) <= 0 THEN ASSIGN
                    prgrms.can_update = prgrms.can_update + "," + users.user_id:SCREEN-VALUE.
                IF LOOKUP(cOldUserID,prgrms.can_delete) > 0 
                AND LOOKUP(users.user_id:SCREEN-VALUE,prgrms.can_delete) <= 0 THEN ASSIGN
                    prgrms.can_delete = prgrms.can_delete + "," + users.user_id:SCREEN-VALUE.
            END.
        END. /* lCopy */
    END. /* lAdd */

        /* Add usr record for new user */
        FIND FIRST usr WHERE usr.uid EQ users.user_id:SCREEN-VALUE NO-ERROR.
        IF NOT AVAIL usr THEN DO:
            CREATE usr.
            ASSIGN
                usr.uid = users.user_id:SCREEN-VALUE
                usr.usr-lang = "English".
            IF lPwdChanged THEN ASSIGN
                usr.last-chg = today.
        END.
        ELSE DO:
            IF usr.usr-lang = "EN" THEN ASSIGN
                usr.usr-lang = "English".
            IF lPwdChanged THEN ASSIGN
                usr.last-chg = today.
        END.

        /* Most elements come from the 'generic' ttUser (ttfPdbname = '*') */
        FIND ttUsers WHERE
            ttUsers.ttfPdbName = "*" AND
            ttUsers.ttfUserID = users.user_id:SCREEN-VALUE
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfPdbName = "*"
                ttUsers.ttfUserID = users.user_id:SCREEN-VALUE.
        END.
        ASSIGN
            ttUsers.ttfUserAlias = users.userAlias:SCREEN-VALUE
            ttUsers.ttfEnvList = if slEnvironments:SCREEN-VALUE <> slEnvironments:list-items then slEnvironments:SCREEN-VALUE else ""
            ttUsers.ttfDbList = if slDatabases:SCREEN-VALUE <> slDatabases:list-items then slDatabases:SCREEN-VALUE else ""
            ttUsers.ttfModeList = ""
            .

        /* But mode-list has a by-db component (ttfPdbname = pdbname(1)) */
        FIND ttUsers WHERE
            ttUsers.ttfPdbName = PDBNAME(1) AND
            ttUsers.ttfUserID = users.user_id:SCREEN-VALUE
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfPdbName = PDBNAME(1)
                ttUsers.ttfUserID = users.user_id:SCREEN-VALUE.
        END.
        ASSIGN
            ttUsers.ttfModeList = if slModes:SCREEN-VALUE <> slModes:list-items then slModes:SCREEN-VALUE else ""
            ttUsers.ttfEnvList = ""
            ttUsers.ttfDbList = ""
            ttUsers.ttfUserAlias = ""
            .
         ASSIGN iSecLevel = INTEGER(users.securityLevel:SCREEN-VALUE) . 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    ASSIGN
        rThisUser = ?.
        
    FIND users EXCLUSIVE WHERE 
        users.user_id = users.user_id:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL users THEN DO:
        ASSIGN 
            users.userAlias = users.userAlias:screen-value in frame {&frame-name}
            users.securityLevel = iSecLevel 
            users.sessionLimit = INTEGER(users.sessionLimit:SCREEN-VALUE)
            users.purchaseLimit = DECIMAL(users.purchaseLimit:SCREEN-VALUE)
            users.userType = cbUserType:SCREEN-VALUE IN FRAME {&FRAME-NAME}
/* 39245 - User MODE does not save - 12/10/18 - MYT - remove references to db-based fields; use .usr file only */
/*            users.envList = slEnvironments:SCREEN-VALUE*/
/*            users.dbList = slDatabases:SCREEN-VALUE    */
/*            users.modeList = slModes:SCREEN-VALUE      */
            users.phone = fi_phone-area:screen-value + lv-phone-num
            /*users.fax = fi_fax-area:screen-value + lv-fax-num*/
            rThisUser = ROWID(users)
            users.menuFGColor[1] = FGColor-1:BGCOLOR
            users.menuFGColor[2] = FGColor-2:BGCOLOR
            users.menuFGColor[3] = FGColor-3:BGCOLOR
            users.menuBGColor[1] = BGColor-1:BGCOLOR
            users.menuBGColor[2] = BGColor-2:BGCOLOR
            users.menuBGColor[3] = BGColor-3:BGCOLOR
            .
    END.
    
/*    /* future development */                     */
/*    IF users.isActive EQ NO THEN DO:             */
/*        /* delete dAOA user specific subjects, */*/
/*        /* and remove email references in dAOA */*/
/*        {methods/delete.trg/removeUser.i}        */
/*    END. /* if not active */                     */
    
    IF NOT lCopy THEN
    CASE users.showCueCard:
        WHEN NO THEN
        RUN spInactivateCueCards ("System", users.user_id).
        WHEN YES THEN
        RUN spActivateCueCards (users.user_id).
    END CASE.

    RUN ipWriteUsrFile.
    EMPTY TEMP-TABLE ttUsers.
    RUN ipReadUsrFile.
   
    DISABLE 
        fiPassword
        fi_phone-area 
        lv-phone-num 
        cbUserType
        users.userAlias
        users.securityLevel
        users.sessionLImit
        users.purchaseLimit
        slEnvironments
        slDatabases
        slModes
        bAll1
        bNone1
        bAll2
        bNone2
        bAll3
        bNone3
        bDefaults
        WITH FRAME {&FRAME-NAME}.
    
    IF lAdd THEN DO:
       {methods/run_link.i "RECORD-SOURCE" "DISPATCH" "('open-query')"}
    END.
    ASSIGN
        lAdd = FALSE
        lCopy = FALSE.
    
    {methods/run_link.i "RECORD-SOURCE" "ipReposition" "(rThisUser)"}
    RUN local-display-fields.
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    IF users.user_id EQ USERID("ASI") AND lMenuChanges AND fSuperRunning("session.") THEN DO:
        hMainMenu = DYNAMIC-FUNCTION("sfGetMainMenuHandle").
        IF VALID-HANDLE(hMainMenu) THEN DO:
            MESSAGE 
                "You have changes to values effecting the Main Menu." SKIP(1)
                "Do you wish to apply the changes and rebuild the Main Menu?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lMenuChanges.
            RUN pResetByUser IN hMainMenu (lMenuChanges).
            lMenuChanges = NO.
        END. /* if valid-handle */
    END. /* if super running */ 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view V-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    users.userLanguage:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
    FOR EACH userLanguage NO-LOCK:
        users.userLanguage:ADD-LAST(userLanguage.langDescription,userLanguage.userLanguage).
    END. /* each userlanguage */

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

    {methods/template/local/setvalue.i}

    &IF DEFINED(Translation) NE 0 &THEN
        {{&translationInclude}}
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE max-widget V-table-Win 
PROCEDURE max-widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {custom/resizmx2.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        users.use_colors:SENSITIVE IN FRAME {&frame-name} = lAdmin
        users.use_fonts:SENSITIVE     = lAdmin          
        users.userAlias:SENSITIVE     = lAdmin
        users.securityLevel:SENSITIVE = lAdmin
        users.sessionLimit:SENSITIVE  = lAdmin
        users.isManager:SENSITIVE     = lAdmin
        users.manager:SENSITIVE       = lAdmin
        users.department:SENSITIVE    = lAdmin
        users.isLocked:SENSITIVE      = lAdmin
        cbUserType:SENSITIVE          = lAdmin
        slEnvironments:SENSITIVE      = lAdmin
        slDatabases:SENSITIVE         = lAdmin
        slModes:SENSITIVE             = lAdmin
        bAll1:SENSITIVE               = lAdmin
        bNone1:SENSITIVE              = lAdmin
        bAll2:SENSITIVE               = lAdmin
        bNone2:SENSITIVE              = lAdmin
        bAll3:SENSITIVE               = lAdmin
        bNone3:SENSITIVE              = lAdmin
        bDefaults:SENSITIVE           = lAdmin
        users.purchaseLimit:SENSITIVE = lAdmin
        cbUserType:SCREEN-VALUE       = users.userType
        slEnvironments:SCREEN-VALUE   = IF ttUsers.ttfEnvList NE "" THEN ttUsers.ttfEnvList ELSE slEnvironments:LIST-ITEMS 
        slDatabases:SCREEN-VALUE      = IF ttUsers.ttfDbList NE "" THEN ttUsers.ttfDbList ELSE slDatabases:LIST-ITEMS 
        slModes:SCREEN-VALUE          = IF ttUsers.ttfModeList NE "" THEN ttUsers.ttfModeList ELSE slModes:LIST-ITEMS 
        .
    ENABLE 
        bChgPwd
        fi_phone-area 
        lv-phone-num 
            WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-rowavail V-table-Win 
PROCEDURE proc-rowavail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restore-widget V-table-Win 
PROCEDURE restore-widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {custom/resizrs2.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "users"}
  {src/adm/template/snd-list.i "usr"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-user-id V-table-Win 
PROCEDURE valid-user-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-user-id AS cha NO-UNDO.
    DEF BUFFER bf-users FOR users.

    {methods/lValidateError.i YES}
    FIND FIRST bf-users WHERE 
        bf-users.USER_id = ip-user-id 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-users 
    AND NOT adm-new-record THEN DO:
        MESSAGE 
            "Invalid User ID. " 
            VIEW-AS ALERT-BOX ERROR.     
        RETURN ERROR.
    END.

    {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-userid V-table-Win 
PROCEDURE validate-userid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/lValidateError.i YES}
  
    IF users.USER_id:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        MESSAGE 
            "User Id must be entered. " 
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO users.USER_id.
        RETURN ERROR.
    END.
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

