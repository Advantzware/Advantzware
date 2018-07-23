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

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
{sys/inc/VAR.i NEW SHARED}
{methods/defines/hndlset.i}
{methods/defines/noreckey.i}
{custom/resizdef.i}

ASSIGN cocode = g_company
       locode = g_loc.

&SCOPED-DEFINE proc-enable proc-enable
&SCOPED-DEFINE users-rowavail proc-rowavail

DEFINE SHARED VAR cIniLoc AS CHAR NO-UNDO.
DEFINE SHARED VAR cUsrLoc AS CHAR NO-UNDO.

DEFINE SHARED VARIABLE h_users AS HANDLE NO-UNDO.

DEF BUFFER zUsers FOR users.
DEF BUFFER lUsers FOR users.
DEF VAR createLabelPath AS LOG NO-UNDO.
DEF VAR cOldUserID AS CHAR NO-UNDO.
DEF VAR cEnvList AS CHAR NO-UNDO.
DEF VAR cDbList AS CHAR NO-UNDO.
DEF VAR cModeList AS CHAR NO-UNDO.
DEF VAR cEnvSelList AS CHAR NO-UNDO.
DEF VAR cDbSelList AS CHAR NO-UNDO.
DEF VAR cModeSelList AS CHAR NO-UNDO.
DEF VAR cAliasFromFile AS CHAR NO-UNDO.
DEF VAR correct-error AS LOG no-undo.
DEF VAR copy-record AS LOG NO-UNDO.
DEF VAR rThisUser AS ROWID NO-UNDO.
DEF VAR lAdd AS LOG NO-UNDO.
DEF VAR lCopy AS LOG NO-UNDO.
DEF VAR lPwdChanged AS LOG NO-UNDO.
DEF VAR cOldPwd AS CHAR NO-UNDO.

DEF TEMP-TABLE tempUser NO-UNDO LIKE _User.

DEF TEMP-TABLE ttUsers
    FIELD ttfPdbname AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfUserAlias AS CHAR
    FIELD ttfEnvList AS CHAR
    FIELD ttfDbList AS CHAR
    FIELD ttfModeList AS CHAR
    INDEX iUserID IS UNIQUE ttfUserID ttfPdbName
    INDEX iDatabase IS UNIQUE ttfPdbName ttfUserID.
DEF BUFFER bttUsers FOR ttUsers.

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
&Scoped-Define ENABLED-FIELDS users.user_name users.phone-cnty ~
users.fax-cnty users.image_filename users.user_program[1] ~
users.user_program[2] users.user_program[3] users.track_usage ~
users.use_colors users.use_fonts users.developer users.isLocked 
&Scoped-define ENABLED-TABLES users
&Scoped-define FIRST-ENABLED-TABLE users
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS users.user_id users.user_name ~
users.userAlias users.phone-cnty users.phone users.fax-cnty users.fax ~
users.image_filename users.user_program[1] users.user_program[2] ~
users.user_program[3] users.track_usage users.use_colors users.use_fonts ~
users.developer users.securityLevel users.isActive users.isLocked 
&Scoped-define DISPLAYED-TABLES users
&Scoped-define FIRST-DISPLAYED-TABLE users
&Scoped-Define DISPLAYED-OBJECTS fiPassword cbUserType fi_phone-area ~
lv-phone-num fi_fax-area lv-fax-num slEnvironments slDatabases slModes 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELDS,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS users.user_id 
&Scoped-define ADM-ASSIGN-FIELDS users.phone-cnty fi_phone-area ~
lv-phone-num users.fax-cnty fi_fax-area lv-fax-num users.image_filename 
&Scoped-define DISPLAY-FIELDS users.phone-cnty fi_phone-area lv-phone-num ~
users.fax-cnty fi_fax-area lv-fax-num users.image_filename 

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
     SIZE 12 BY 1.14.

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
     FONT 4 NO-UNDO.

DEFINE VARIABLE fi_fax-area AS CHARACTER FORMAT "xxx":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE fi_phone-area AS CHARACTER FORMAT "xxx":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE lv-fax-num AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE lv-phone-num AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 12.38.

DEFINE VARIABLE slDatabases AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 30 BY 2.14
     FONT 1 NO-UNDO.

DEFINE VARIABLE slEnvironments AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 30 BY 2.14
     FONT 1 NO-UNDO.

DEFINE VARIABLE slModes AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Advantzware","Addon","Case Labels","Loadtags","RM Loadtags","Sharpshooter","Touchscreen","xxx" 
     SIZE 30 BY 4.05
     FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     users.user_id AT ROW 1.24 COL 19 COLON-ALIGNED
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     users.user_name AT ROW 1.24 COL 43 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
          BGCOLOR 15 FONT 4
     users.userAlias AT ROW 1.24 COL 99 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FONT 4
     fiPassword AT ROW 2.67 COL 19 COLON-ALIGNED WIDGET-ID 80
     cbUserType AT ROW 2.43 COL 99 COLON-ALIGNED WIDGET-ID 48
     bChgPwd AT ROW 2.67 COL 59 WIDGET-ID 82 NO-TAB-STOP 
     users.phone-cnty AT ROW 4.1 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          FONT 4
     fi_phone-area AT ROW 4.1 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     users.phone AT ROW 4.1 COL 75 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 84 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     lv-phone-num AT ROW 4.1 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     users.fax-cnty AT ROW 5.29 COL 28 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 18 FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          FONT 4
     fi_fax-area AT ROW 5.29 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     lv-fax-num AT ROW 5.29 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     users.fax AT ROW 5.29 COL 75 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 86 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     users.image_filename AT ROW 6.48 COL 19 COLON-ALIGNED HELP
          "Enter Main Menu Image File Name (fully qualified path)" WIDGET-ID 38
          LABEL "Email" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          FONT 4
     users.user_program[1] AT ROW 7.91 COL 19 COLON-ALIGNED
          LABEL "Image Viewer" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          FONT 4
     users.user_program[2] AT ROW 9.1 COL 19 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Report Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          FONT 4
     users.user_program[3] AT ROW 10.29 COL 2.4 WIDGET-ID 36
          LABEL "Document Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          FONT 4
     users.track_usage AT ROW 11.48 COL 21
          VIEW-AS TOGGLE-BOX
          SIZE 19.8 BY 1
     users.use_colors AT ROW 12.43 COL 21
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY 1
     users.use_fonts AT ROW 13.38 COL 21
          VIEW-AS TOGGLE-BOX
          SIZE 26.2 BY 1
     users.developer AT ROW 14.33 COL 21
          VIEW-AS TOGGLE-BOX
          SIZE 16.8 BY 1
     users.securityLevel AT ROW 3.62 COL 99 COLON-ALIGNED WIDGET-ID 44
          LABEL "Security Level" FORMAT ">999"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          FONT 4
     users.isActive AT ROW 3.62 COL 112 WIDGET-ID 90
          LABEL "Active?"
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     users.isLocked AT ROW 3.62 COL 128 WIDGET-ID 88
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     bDefaults AT ROW 5.52 COL 118 WIDGET-ID 104
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     slEnvironments AT ROW 11.71 COL 108 NO-LABEL WIDGET-ID 50
     bAll1 AT ROW 11.95 COL 97 WIDGET-ID 64
     bNone1 AT ROW 12.67 COL 97 WIDGET-ID 66
     slDatabases AT ROW 14.33 COL 108 NO-LABEL WIDGET-ID 52
     bAll2 AT ROW 14.81 COL 97 WIDGET-ID 68
     bNone2 AT ROW 15.52 COL 97 WIDGET-ID 70
     slModes AT ROW 6.95 COL 108 NO-LABEL WIDGET-ID 54
     bAll3 AT ROW 7.43 COL 97 WIDGET-ID 72
     bNone3 AT ROW 8.14 COL 97 WIDGET-ID 74
     "Environments:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 11.24 COL 91 WIDGET-ID 58
          FONT 4
     "(#)" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 4.1 COL 54 WIDGET-ID 106
     "(Area)" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 5.29 COL 38 WIDGET-ID 108
     "(#)" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 5.29 COL 54 WIDGET-ID 110
     "(Use CTRL-click to select multiple items)" VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 16.48 COL 98 WIDGET-ID 76
          FONT 1
     "Phone: (Country)" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 4.1 COL 10 WIDGET-ID 92
     "FAX: (Country)" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 5.29 COL 13 WIDGET-ID 94
     "(Area)" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 4.1 COL 38 WIDGET-ID 96
     " At Login User Can Select:" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 4.81 COL 91 WIDGET-ID 56
          FONT 4
     "Options:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 11.71 COL 10 WIDGET-ID 42
     "Modes:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 6.71 COL 99 WIDGET-ID 62
          FONT 4
     "Databases:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 14.1 COL 94 WIDGET-ID 60
          FONT 4
     RECT-5 AT ROW 5.05 COL 88 WIDGET-ID 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


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
         HEIGHT             = 16.43
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
/* SETTINGS FOR BUTTON bNone1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bNone2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bNone3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbUserType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.fax IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       users.fax:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN users.fax-cnty IN FRAME F-Main
   2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN fiPassword IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_fax-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_phone-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.image_filename IN FRAME F-Main
   2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR TOGGLE-BOX users.isActive IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       users.isActive:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN lv-fax-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN lv-phone-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.phone IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       users.phone:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN users.phone-cnty IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN users.securityLevel IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR SELECTION-LIST slDatabases IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slEnvironments IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST slModes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.userAlias IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users.user_id IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN users.user_name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN users.user_program[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN users.user_program[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN users.user_program[3] IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT                                         */
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
    /* Verify password restrictions and display */
    IF usercontrol.minLC > 0 
    OR usercontrol.minUC > 0 
    OR usercontrol.minNC > 0 
    OR usercontrol.minSC > 0 
    OR usercontrol.minPasswordLen > 0 THEN DO:
        MESSAGE
            "Note: Passwords have the following restrictions:" SKIP
            "Minimum Length = " + STRING(usercontrol.minPasswordLen) SKIP
            "Minimum lower case = " + STRING(usercontrol.minLC) SKIP
            "Minimum UPPER case = " + STRING(usercontrol.minUC) SKIP
            "Minimum numeric chars = " + STRING(usercontrol.minNC) SKIP
            "Minimum special chars = " + STRING(usercontrol.minSC)
            VIEW-AS ALERT-BOX.
    END.
    
    ASSIGN
        fiPassword:SCREEN-VALUE = ""
        fiPassword:SENSITIVE = TRUE
        SELF:SENSITIVE = FALSE.
    APPLY 'entry' TO fiPassword.
    RETURN NO-APPLY.
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
    RUN ipCheckPwd (INPUT-OUTPUT lPwdOK).
    IF NOT lPwdOK THEN DO:
        ASSIGN SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
    IF SELF:SCREEN-VALUE = "" THEN DO:
        MESSAGE
            "You are setting this user's password" SKIP
            "to BLANKS. Is this correct?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lBlanks AS LOG.
        IF NOT lBlanks AND NOT lAdd THEN DO:
            ASSIGN
                SELF:SCREEN-VALUE = _user._password.
            RETURN NO-APPLY.
        END.
    END.
    IF NOT lAdd THEN 
        RUN ipChangePassword (SELF:SCREEN-VALUE).
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


&Scoped-define SELF-NAME users.userAlias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.userAlias V-table-Win
ON LEAVE OF users.userAlias IN FRAME F-Main /* Login Alias */
DO:
    IF SELF:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST ttUsers NO-LOCK WHERE 
            ttUsers.ttfuserAlias = SELF:SCREEN-VALUE AND
            ttUsers.ttfpdbname = "*" AND
            ttUsers.ttfuserid NE users.user_id
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

    IF AVAIL (_User) THEN DO:
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser.
        ASSIGN 
            tempUser._Password = ENCODE(cNewPassword).
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
        FIND FIRST userPwdHist EXCLUSIVE WHERE
            userPwdHist.user_id = _user._userID AND
            userPwdHist.pwd = cNewPassword AND
            userPwdHist.pwdDate = today NO-ERROR.
        IF NOT AVAIL userPwdHist THEN DO:
            CREATE userPwdHist.
            ASSIGN
                userPwdHist.user_id = _user._userID
                userPwdHist.pwdDate = today
                userPwdHist.createDate = TODAY
                userPwdHist.createTime = TIME
                userPwdHist.createUser = USERID(LDBNAME(1)).
        END.
        ASSIGN
            userPwdHist.pwd = cNewPassword
            userPwdHist.updateDate = TODAY
            userPwdHist.updateTime = TIME
            userPwdHist.updateUser = USERID(LDBNAME(1))
            .
    END.
    ELSE MESSAGE 
        "This Userid does not exist"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN  
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = _user._password
        fiPassword:SENSITIVE = FALSE
        bChgPwd:SENSITIVE = TRUE.          
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
            lPwdOK = NO.
        MESSAGE
            "The password you entered does not meet requirements." SKIP
            "Minimum Length = " + STRING(usercontrol.minPasswordLen) + " - Yours has " + STRING(iHasLen) SKIP
            "Minimum lower case = " + STRING(usercontrol.minLC) + " - Yours has " + STRING(iHasLC)  SKIP
            "Minimum UPPER case = " + STRING(usercontrol.minUC) + " - Yours has " + STRING(iHasUC)  SKIP
            "Minimum numeric chars = " + STRING(usercontrol.minNC) + " - Yours has " + STRING(iHasNC)  SKIP
            "Minimum special chars = " + STRING(usercontrol.minSC) + " - Yours has " + STRING(iHasSC) SKIP
            "Please try again." 
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
  
    ASSIGN
        lAdd = TRUE
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fiPassword:SENSITIVE = TRUE
        fi_phone-area:SCREEN-VALUE = ""
        lv-phone-num:SCREEN-VALUE = ""
        fi_fax-area:SCREEN-VALUE = ""
        lv-fax-num:SCREEN-VALUE = ""
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
        users.userAlias
        users.securityLevel
        fiPassword
        fi_phone-area 
        lv-phone-num 
        fi_fax-area 
        lv-fax-num 
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
        usrx.uid = users.user_id:
        DELETE usrx.
    END.
   
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.phone-no" AND
        reftable.company EQ users.user_id:
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.fax-no" AND
        reftable.company EQ users.user_id:
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.phone-cnty" AND
        reftable.company EQ users.user_id:
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.fax-cnty" AND
        reftable.company EQ users.user_id:
        DELETE reftable.
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

    RUN ipGoBack IN h_Users.

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
    
    IF AVAIL users THEN DO:
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
                ttUsers.ttfEnvList = slEnvironments:list-items in FRAME {&FRAME-NAME}
                ttUsers.ttfDbList = slDatabases:list-items
                ttUsers.ttfUserAlias = users.userAlias:SCREEN-VALUE.
        END.

        ASSIGN 
            fi_phone-area:screen-value = substring(users.phone,1,3)
            lv-phone-num:screen-value = substring(users.phone,4)
            fi_fax-area:screen-value = substring(users.fax,1,3)
            lv-fax-num:screen-value = substring(users.fax,4)
            cbUserType:screen-value = users.userType
            slEnvironments:screen-value = if ttUsers.ttfEnvList <> "" THEN ttUsers.ttfEnvList else slEnvironments:list-items
            slDatabases:screen-value = if ttUsers.ttfDbList <> "" THEN ttUsers.ttfDbList else slDatabases:list-items
            users.userAlias:SCREEN-VALUE = ttUsers.ttfUserAlias
            users.userAlias:modified = false
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
                ttUsers.ttfModeList = slModes:list-items.
        END.
        ASSIGN 
            slModes:screen-value = if ttUsers.ttfModeList <> "" THEN ttUsers.ttfModeList else slModes:list-items
            .

        FIND _user NO-LOCK WHERE 
            _user._userid = users.user_id
            NO-ERROR.
        IF AVAIL _user THEN ASSIGN
            fiPassword:SCREEN-VALUE = _user._password
            cOldPwd = _user._password.

        ASSIGN
            bChgPwd:SENSITIVE = IF users.user_id = zusers.user_id OR zusers.securityLevel > 699 THEN TRUE ELSE FALSE
            fiPassword:SENSITIVE = FALSE.
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
    DEF BUFFER bf-usercust FOR usercust .
    DEF BUFFER bf-usrx FOR usrx .
    DEF BUFFER bf-usercomp FOR usercomp.
    
    DEF VAR lv-default-comp AS cha NO-UNDO.
    DEF VAR lv-default-loc AS cha NO-UNDO.
    DEF VAR ll-ans AS LOG NO-UNDO.
    DEF VAR ll-dummy AS LOG NO-UNDO.
    DEF VAR v-old-pass AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-new-pass AS cha FORM "x(30)" NO-UNDO.
    DEF VAR cNewPwd AS CHAR NO-UNDO.
    DEF VAR lPwdOK AS LOG NO-UNDO.
    DEF VAR cCurrentDir AS CHAR NO-UNDO.
    DEF VAR iStat AS INT NO-UNDO.
  
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

        FILE-INFO:FILE-NAME = users.USER_program[2].
        IF FILE-INFO:FILE-type eq ? then do:
            MESSAGE 
                "Document Path does not exist. Do you want to create it?" 
                VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
            IF v-ans THEN OS-CREATE-DIR VALUE(file-info:file-name).
        END.
    END.

    IF users.user_program[3]:SCREEN-VALUE NE "" THEN DO:
        IF SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "\" 
        OR SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "/" THEN DO:
            ASSIGN
                users.user_program[3]:SCREEN-VALUE = SUBSTRING(users.user_program[3]:SCREEN-VALUE,1,LENGTH(users.user_program[3]:SCREEN-VALUE) - 1).
        END.
         
        FILE-INFO:FILE-NAME = users.USER_program[3].
        IF FILE-INFO:FILE-type eq ? then do:
            MESSAGE 
                "Document Path does not exist. Do you want to create it?" 
                VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE v-ans2 AS LOG.
            IF v-ans2 THEN OS-CREATE-DIR VALUE(file-info:file-name).
        END.
    END.  

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
                BUFFER-COPY usercust EXCEPT rec_key user_id TO bf-usercust.
                ASSIGN
                    bf-usercust.user_id = users.USER_id .
            END.
            FOR EACH usrx NO-LOCK WHERE 
                usrx.uid = cOldUserID AND 
                usrx.company = cocode AND 
                usrx.loc NE "", 
                EACH loc OF usrx NO-LOCK:
    
                CREATE bf-usrx .
                BUFFER-COPY usrx EXCEPT rec_key uid TO bf-usrx.
                ASSIGN
                    bf-usrx.uid = users.USER_id .
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
        
        /* Ensure folder available for custom menus */
        ASSIGN
            FILE-INFO:FILE-NAME = ".".
            cCurrentDir = FILE-INFO:FULL-PATHNAME.
            cCurrentDir = cCurrentDir + "\UserMenu\" + users.user_id:SCREEN-VALUE.
        OS-CREATE-DIR VALUE(cCurrentDir).
        ASSIGN
            iStat = OS-ERROR.
        IF iStat NE 0 THEN DO:
            MESSAGE
                "Unable to create directory " + cCurrentDir + "." SKIP
                "Be sure to create this directory before customizing menus."
                VIEW-AS ALERT-BOX ERROR.
        END.
        
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
            users.securityLevel = INTEGER(users.securityLevel:SCREEN-VALUE)
            users.userType = cbUserType:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            users.envList = slEnvironments:SCREEN-VALUE
            users.dbList = slDatabases:SCREEN-VALUE
            users.modeList = slModes:SCREEN-VALUE
            users.phone = fi_phone-area:screen-value + lv-phone-num
            users.fax = fi_fax-area:screen-value + lv-fax-num
            rThisUser = ROWID(users).
    END.

        /* Most elements come from the 'generic' ttUser (ttfPdbname = '*') */
        FIND ttUsers WHERE
            ttUsers.ttfPdbName = "*" AND
            ttUsers.ttfUserID = users.user_id
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfPdbName = "*"
                ttUsers.ttfUserID = users.user_id.
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
            ttUsers.ttfUserID = users.user_id
            NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                ttUsers.ttfPdbName = PDBNAME(1)
                ttUsers.ttfUserID = users.user_id.
        END.
        ASSIGN
            ttUsers.ttfModeList = if slModes:SCREEN-VALUE <> slModes:list-items then slModes:SCREEN-VALUE else ""
            ttUsers.ttfEnvList = ""
            ttUsers.ttfDbList = ""
            ttUsers.ttfUserAlias = ""
            .

    RUN ipWriteUsrFile.
    EMPTY TEMP-TABLE ttUsers.
    RUN ipReadUsrFile.
   
    DISABLE 
        fiPassword
        fi_phone-area 
        lv-phone-num 
        fi_fax-area 
        lv-fax-num 
        cbUserType
        users.userAlias
        users.securityLevel
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
       RUN DISPATCH IN h_Users ('open-query'). 
    END.
    ASSIGN
        lAdd = FALSE
        lCopy = FALSE.
    
    RUN ipReposition IN h_Users (rThisUser).
    RUN local-display-fields.
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view V-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

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
    DEFINE VARIABLE lSecurityLevel AS LOGICAL NO-UNDO.
    
    ASSIGN 
        lSecurityLevel = zUsers.securityLevel GE 900
        users.track_usage:SENSITIVE IN FRAME {&FRAME-NAME} = lSecurityLevel
        users.use_colors:SENSITIVE = lSecurityLevel
        users.use_fonts:SENSITIVE = lSecurityLevel
        users.developer:SENSITIVE = zUsers.securityLevel GT 999
        users.developer:SENSITIVE = FALSE 
        users.userAlias:SENSITIVE = lSecurityLevel
        users.securityLevel:SENSITIVE = lSecurityLevel
        users.isLocked:SENSITIVE = lSecurityLevel
        cbUserType:SENSITIVE = lSecurityLevel
        slEnvironments:SENSITIVE = lSecurityLevel
        slDatabases:SENSITIVE = lSecurityLevel
        slModes:SENSITIVE = lSecurityLevel
        bAll1:SENSITIVE = lSecurityLevel
        bNone1:SENSITIVE = lSecurityLevel
        bAll2:SENSITIVE = lSecurityLevel
        bNone2:SENSITIVE = lSecurityLevel
        bAll3:SENSITIVE = lSecurityLevel
        bNone3:SENSITIVE = lSecurityLevel
        bDefaults:SENSITIVE = lSecurityLevel
        cbUserType:screen-value = users.userType
        slEnvironments:screen-value = IF ttUsers.ttfEnvList NE "" THEN ttUsers.ttfEnvList ELSE slEnvironments:list-items
        slDatabases:screen-value = IF ttUsers.ttfDbList NE "" THEN ttUsers.ttfDbList ELSE slDatabases:list-items
        slModes:screen-value = IF ttUsers.ttfModeList NE "" THEN ttUsers.ttfModeList ELSE slModes:list-items
        .
    ENABLE 
        bChgPwd
        fi_phone-area 
        lv-phone-num 
        fi_fax-area 
        lv-fax-num 
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

