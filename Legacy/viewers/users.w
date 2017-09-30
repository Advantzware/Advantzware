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

DEFINE NEW GLOBAL SHARED VAR cIniLoc AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR cUsrLoc AS CHAR NO-UNDO.

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


DEF TEMP-TABLE tempUser NO-UNDO LIKE _User.

DEF TEMP-TABLE ttUsers
    FIELD ttfPdbname AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfUserAlias AS CHAR
    FIELD ttfEnvList AS CHAR
    FIELD ttfDbList AS CHAR
    FIELD ttfModeList AS CHAR.

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
&Scoped-Define ENABLED-FIELDS users.user_name users.user_program[1] ~
users.user_program[2] users.user_program[3] users.track_usage ~
users.use_colors users.use_fonts users.use_ctrl_keys users.developer 
&Scoped-define ENABLED-TABLES users
&Scoped-define FIRST-ENABLED-TABLE users
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS users.fax users.isActive users.isLocked ~
users.phone users.user_id users.user_name users.userAlias users.phone-cnty ~
users.image_filename users.user_program[1] users.user_program[2] ~
users.user_program[3] users.track_usage users.use_colors users.use_fonts ~
users.use_ctrl_keys users.developer users.showOnQuote users.showOnAck ~
users.showOnBol users.showOnInv users.showOnPO users.securityLevel 
&Scoped-define DISPLAYED-TABLES users
&Scoped-define FIRST-DISPLAYED-TABLE users
&Scoped-Define DISPLAYED-OBJECTS fiPassword fi_phone-area lv-phone-num ~
fi_fax-country fi_fax-area lv-fax-num cbUserType slEnvironments slDatabases ~
slModes 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELDS,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS users.user_id 
&Scoped-define ADM-ASSIGN-FIELDS users.phone-cnty fi_phone-area ~
lv-phone-num fi_fax-country fi_fax-area lv-fax-num users.image_filename ~
users.showOnQuote users.showOnAck users.showOnBol users.showOnInv ~
users.showOnPO 
&Scoped-define DISPLAY-FIELDS users.phone-cnty fi_phone-area lv-phone-num ~
fi_fax-country fi_fax-area lv-fax-num users.image_filename ~
users.showOnQuote users.showOnAck users.showOnBol users.showOnInv ~
users.showOnPO 

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
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE fi_fax-area AS CHARACTER FORMAT "(xxx)":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_fax-country AS CHARACTER FORMAT "X(8)":U 
     LABEL "FAX +" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_phone-area AS CHARACTER FORMAT "(xxx)":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-fax-num AS CHARACTER FORMAT "xxx-xxxx":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-phone-num AS CHARACTER FORMAT "xxx-xxxx":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 12.38.

DEFINE VARIABLE slDatabases AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 30 BY 2.62
     FONT 1 NO-UNDO.

DEFINE VARIABLE slEnvironments AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 30 BY 2.62
     FONT 1 NO-UNDO.

DEFINE VARIABLE slModes AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Advantzware","Addon","Case Labels","Loadtags","Sharpshooter","Touchscreen" 
     SIZE 30 BY 5.24
     FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     users.fax AT ROW 5.29 COL 60 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 86 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     users.isActive AT ROW 3.62 COL 112 WIDGET-ID 90
          LABEL "Active?"
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     users.isLocked AT ROW 3.62 COL 128 WIDGET-ID 88
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     users.phone AT ROW 4.1 COL 60 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 84 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     bChgPwd AT ROW 2.67 COL 59 WIDGET-ID 82
     fiPassword AT ROW 2.67 COL 19 COLON-ALIGNED WIDGET-ID 80 PASSWORD-FIELD 
     bAll1 AT ROW 11.71 COL 97 WIDGET-ID 64
     users.user_id AT ROW 1.24 COL 12 COLON-ALIGNED
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     users.user_name AT ROW 1.24 COL 37 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
          BGCOLOR 15 FONT 4
     users.userAlias AT ROW 1.24 COL 99 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     users.phone-cnty AT ROW 4.1 COL 19 COLON-ALIGNED WIDGET-ID 12
          LABEL "Phone +"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     fi_phone-area AT ROW 4.1 COL 29 COLON-ALIGNED WIDGET-ID 10
     lv-phone-num AT ROW 4.1 COL 39 COLON-ALIGNED WIDGET-ID 14
     fi_fax-country AT ROW 5.29 COL 19 COLON-ALIGNED WIDGET-ID 18
     fi_fax-area AT ROW 5.29 COL 29 COLON-ALIGNED WIDGET-ID 16
     lv-fax-num AT ROW 5.29 COL 39 COLON-ALIGNED WIDGET-ID 20
     users.image_filename AT ROW 6.48 COL 19 COLON-ALIGNED HELP
          "Enter Main Menu Image File Name (fully qualified path)" WIDGET-ID 38
          LABEL "Email" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     users.user_program[1] AT ROW 7.91 COL 19 COLON-ALIGNED
          LABEL "Image Viewer" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     users.user_program[2] AT ROW 9.1 COL 19 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Report Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     users.user_program[3] AT ROW 10.29 COL 2.4 WIDGET-ID 36
          LABEL "Document Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     users.track_usage AT ROW 12.43 COL 10
          VIEW-AS TOGGLE-BOX
          SIZE 19.8 BY 1
     users.use_colors AT ROW 13.38 COL 10
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY 1
     users.use_fonts AT ROW 14.33 COL 10
          VIEW-AS TOGGLE-BOX
          SIZE 26.2 BY 1
     users.use_ctrl_keys AT ROW 15.29 COL 10
          VIEW-AS TOGGLE-BOX
          SIZE 38.4 BY 1
     users.developer AT ROW 16.24 COL 10
          VIEW-AS TOGGLE-BOX
          SIZE 16.8 BY 1
     users.showOnQuote AT ROW 12.43 COL 53 WIDGET-ID 34
          LABEL "Quote"
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     users.showOnAck AT ROW 13.38 COL 53 WIDGET-ID 26
          LABEL "Ack"
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     users.showOnBol AT ROW 14.33 COL 53 WIDGET-ID 28
          LABEL "BoL"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     users.showOnInv AT ROW 15.29 COL 53 WIDGET-ID 30
          LABEL "Invoice"
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .81
     users.showOnPO AT ROW 16.24 COL 53 WIDGET-ID 32
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     users.securityLevel AT ROW 3.62 COL 99 COLON-ALIGNED WIDGET-ID 44
          LABEL "Security Level" FORMAT ">999"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     cbUserType AT ROW 2.43 COL 99 COLON-ALIGNED WIDGET-ID 48
     slEnvironments AT ROW 11 COL 108 NO-LABEL WIDGET-ID 50
     slDatabases AT ROW 13.86 COL 108 NO-LABEL WIDGET-ID 52
     slModes AT ROW 5.52 COL 108 NO-LABEL WIDGET-ID 54
     bNone1 AT ROW 12.43 COL 97 WIDGET-ID 66
     bAll2 AT ROW 14.57 COL 97 WIDGET-ID 68
     bNone2 AT ROW 15.29 COL 97 WIDGET-ID 70
     bAll3 AT ROW 6.24 COL 97 WIDGET-ID 72
     bNone3 AT ROW 6.95 COL 97 WIDGET-ID 74
     "Environments:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 11 COL 91 WIDGET-ID 58
     " At Login User Can Select:" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 4.81 COL 93 WIDGET-ID 56
     "Options:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 11.71 COL 8 WIDGET-ID 42
     "Modes:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.52 COL 99 WIDGET-ID 62
     "Databases:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 13.86 COL 94 WIDGET-ID 60
     "Phone/Fax Appear on:" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 11.71 COL 51 WIDGET-ID 24
     "(Use CTRL-click to select multiple items)" VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 16.48 COL 98 WIDGET-ID 76
          FONT 1
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

/* SETTINGS FOR FILL-IN fiPassword IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_fax-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_fax-country IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_phone-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.image_filename IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR TOGGLE-BOX users.isActive IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       users.isActive:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX users.isLocked IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       users.isLocked:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN lv-fax-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN lv-phone-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.phone IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       users.phone:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN users.phone-cnty IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN users.securityLevel IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR TOGGLE-BOX users.showOnAck IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX users.showOnBol IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX users.showOnInv IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX users.showOnPO IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX users.showOnQuote IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
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
    ASSIGN
        fiPassword:SCREEN-VALUE = ""
        fiPassword:SENSITIVE = TRUE
        SELF:SENSITIVE = FALSE.
    APPLY 'entry' TO fiPassword.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbUserType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUserType V-table-Win
ON VALUE-CHANGED OF cbUserType IN FRAME F-Main /* User Type */
DO:
    CASE SELF:SCREEN-VALUE:
        WHEN "Full User" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "100".
        WHEN "Production Floor" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "50".
        WHEN "Administrator" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "700".
        WHEN "Portal User" THEN ASSIGN users.securityLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "10".
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword V-table-Win
ON LEAVE OF fiPassword IN FRAME F-Main /* Password */
OR RETURN OF fiPassword
DO:
    IF SELF:SCREEN-VALUE = "" THEN DO:
        MESSAGE
            "You are setting this user's password" SKIP
            "to BLANKS. Is this correct?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lBlanks AS LOG.
        IF NOT lBlanks THEN RETURN NO-APPLY.
    END.
    
    RUN ipChangePassword (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.securityLevel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.securityLevel V-table-Win
ON LEAVE OF users.securityLevel IN FRAME F-Main /* Security Level */
DO:
    IF zUsers.securityLevel < 1000 
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
        FIND FIRST lUsers NO-LOCK WHERE 
            lUsers.userAlias = SELF:SCREEN-VALUE
            NO-ERROR.
        IF AVAIL lUsers THEN DO:
            MESSAGE
                "Duplicate alias detected. Please enter a different value."
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

    {custom/gcompany.i}
    {custom/getcmpny.i}

    DO TRANSACTION:
        {sys/inc/webroot.i}
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
        fiPassword:password-field = if zUsers.user_id = "ASI" then false else true
        slEnvironments:LIST-ITEMS = cEnvList
        slDatabases:LIST-ITEMS = cDbList
        slModes:LIST-ITEMS = cModeList
        bChgPwd:SENSITIVE = if zUsers.securityLevel > 699 THEN TRUE ELSE FALSE
        /* Future development
        users.isActive:SENSITIVE = if zUsers.securityLevel > 699 THEN TRUE ELSE FALSE
        users.isLocked:SENSITIVE = if zUsers.securityLevel > 699 THEN TRUE ELSE FALSE
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
    END.
    ELSE MESSAGE 
        "This Userid does not exist"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN  
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = _user._password
        fiPassword:SENSITIVE = FALSE
        bChgPwd:SENSITIVE = TRUE.          

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
    
    INPUT FROM VALUE(SEARCH(cIniLoc)).
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
    INPUT FROM VALUE(SEARCH(cUsrLoc)).
    REPEAT:
        IMPORT UNFORMATTED cUsrLine.
        IF INDEX(cUsrLine,"|") = 0 THEN NEXT.
        CREATE ttUsers.
        ASSIGN
            ttUsers.ttfPdbname = ENTRY(1,cUsrLine,"|")
            ttUsers.ttfUserAlias = ENTRY(2,cUsrLine,"|")
            ttUsers.ttfUserID = ENTRY(3,cUsrLine,"|")
            ttUsers.ttfEnvList = ENTRY(4,cUsrLine,"|")
            ttUsers.ttfDbList = ENTRY(5,cUsrLine,"|")
            ttUsers.ttfModeList = ENTRY(6,cUsrLine,"|")
            .
    END.
    INPUT CLOSE.

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
    OUTPUT TO VALUE(cUsrLoc).
    FOR EACH ttUsers:
        PUT UNFORMATTED
            ttUsers.ttfPdbname + "|" +
            ttUsers.ttfUserAlias + "|" +
            ttUsers.ttfUserID + "|" +
            ttUsers.ttfEnvList + "|" +
            ttUsers.ttfDbList + "|" +
            ttUsers.ttfModeList + CHR(10).
    END.
    INPUT CLOSE.

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
        fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
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
    DEF BUFFER bf-usercust FOR usercust .
    DEF BUFFER bf-usrx FOR usrx .
    DEF BUFFER bf-usercomp FOR usercomp.
    
    DEF VAR lv-default-comp AS cha NO-UNDO.
    DEF VAR lv-default-loc AS cha NO-UNDO.
    DEF VAR ll-ans AS LOG NO-UNDO.
    DEF VAR ll-dummy AS LOG NO-UNDO.
    DEF VAR v-old-pass AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-new-pass AS cha FORM "x(30)" NO-UNDO.
    DEF VAR cOldUserID AS CHARACTER FORM "x(30)" NO-UNDO.
    DEF VAR cNewPwd AS CHAR NO-UNDO.
  
    ASSIGN 
        cOldUserID = users.user_id.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* {methods/viewers/assign/{&FIRST-EXTERNAL-TABLE}.i} */

    IF adm-new-record THEN DO:
        FIND FIRST bf-usercomp NO-LOCK WHERE 
            bf-usercomp.USER_id = "ASI" AND
            bf-usercomp.company_default 
            NO-ERROR.
        ASSIGN 
            lv-default-comp = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001".

        FIND FIRST usercomp NO-LOCK WHERE 
            usercomp.USER_id = users.USER_id AND 
            usercomp.company = lv-default-comp AND
            usercomp.loc = ""
            NO-ERROR.
        IF NOT AVAIL usercomp THEN DO:
            CREATE usercomp.
            ASSIGN 
                usercomp.user_id = users.USER_id
                usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
                usercomp.loc = ""
                usercomp.company_default = YES.
        END.
     
        FIND FIRST bf-usercomp NO-LOCK WHERE
            bf-usercomp.USER_id = "ASI" AND
            bf-usercomp.loc_default 
            NO-ERROR.
        ASSIGN 
            lv-default-loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".

        FIND FIRST usercomp NO-LOCK WHERE 
            usercomp.USER_id = users.USER_id AND 
            usercomp.company = lv-default-comp AND
            usercomp.loc = lv-default-loc 
            NO-ERROR.

        IF NOT AVAIL usercomp THEN DO:
            CREATE usercomp.
            ASSIGN 
                usercomp.user_id = users.USER_id
                usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
                usercomp.loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
                usercomp.loc_DEFAULT = YES.
        END.
        
        FIND FIRST usr WHERE usr.uid EQ users.user_id NO-ERROR.
        IF NOT AVAIL usr THEN DO:
            CREATE usr.
            ASSIGN
                usr.uid = users.user_id
                usr.usr-lang = "English".
        END.
        
        IF fiPassword:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
            MESSAGE
                "You have not created a password for this" SKIP
                "new user. Would you like to do so now?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lNewPwd AS LOG.
            IF lNewPwd THEN DO:
                UPDATE
                    cNewPwd LABEL "Password"
                    WITH FRAME c VIEW-AS DIALOG-BOX THREE-D SIDE-LABELS.
                ASSIGN
                    fiPassword:SCREEN-VALUE = cNewPwd.
            END.
        END.
                    
        CREATE _user.
        ASSIGN
            _user._userid = users.user_id
            _user._password = ENCODE(fiPassword:SCREEN-VALUE)
            _user._user-name = users.user_name:SCREEN-VALUE.
    END.

    IF adm-new-record AND NOT adm-adding-record THEN DO:  /* copy */
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
    END.

    SESSION:SET-WAIT-STATE("general").
     
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
  
    SESSION:SET-WAIT-STATE("").

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
        reftable.reftable EQ "users.user-docs" AND
        reftable.company EQ users.user_id:
        DELETE reftable.
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

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    {methods/template/local/deleteAfter.i}
    
    RUN local-open-query in h_users.

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
    FIND FIRST ttUsers WHERE
        ttUsers.ttfPdbName = PDBNAME(1) AND
        ttUsers.ttfUserID = users.user_id
        NO-ERROR.
    IF NOT AVAIL ttUsers THEN DO:
        CREATE ttUsers.
        ASSIGN
            ttUsers.ttfPdbName = PDBNAME(1)
            ttUsers.ttfUserID = users.user_id
            ttUsers.ttfEnvList = slEnvironments:list-items in FRAME {&FRAME-NAME}
            ttUsers.ttfDbList = slDatabases:list-items
            ttUsers.ttfModeList = slModes:list-items.
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    ASSIGN 
        cbUserType:screen-value = users.userType
        slEnvironments:screen-value = if ttUsers.ttfEnvList <> "" THEN ttUsers.ttfEnvList else slEnvironments:list-items
        slDatabases:screen-value = if ttUsers.ttfDbList <> "" THEN ttUsers.ttfDbList else slDatabases:list-items
        slModes:screen-value = if ttUsers.ttfModeList <> "" THEN ttUsers.ttfModeList else slModes:list-items.

    IF users.userAlias:SCREEN-VALUE NE ttUsers.ttfUserAlias THEN ASSIGN
        users.userAlias:SCREEN-VALUE = ttUsers.ttfUserAlias.
        
    FIND _user NO-LOCK WHERE 
        _user._userid = users.user_id
        NO-ERROR.
    IF AVAIL _user THEN ASSIGN
        fiPassword:SCREEN-VALUE = _user._password.

    ASSIGN
        bChgPwd:SENSITIVE = IF users.user_id = zusers.user_id OR zusers.securityLevel > 699 THEN TRUE ELSE FALSE.

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

    RUN validate-userid NO-ERROR.
    IF error-status:error THEN RETURN.

    IF users.user_program[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
        IF SUBSTRING(users.user_program[2]:SCREEN-VALUE,LENGTH(users.user_program[2]:SCREEN-VALUE),1) EQ "\" 
        OR SUBSTRING(users.user_program[2]:SCREEN-VALUE,LENGTH(users.user_program[2]:SCREEN-VALUE),1) EQ "/" THEN DO:
            MESSAGE 
                "Report Path cannot end in / or \." 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO users.user_program[2] IN FRAME {&FRAME-NAME}.
            RETURN.
        END.
    END.

    IF users.user_program[3]:SCREEN-VALUE NE "" THEN DO:
        {&methods/lValidateError.i YES}
        IF SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "\" 
        OR SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "/" THEN DO:
            MESSAGE 
                "Document Path cannot end in / or \." 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO users.user_program[3] IN FRAME {&FRAME-NAME}.
            RETURN.
        END.
     
        FILE-INFO:FILE-NAME = users.USER_program[3].
        IF FILE-INFO:FILE-type eq ? then do:
            MESSAGE 
                "Document Path does not exist. Do you want to create it?" 
                VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
            IF v-ans THEN OS-CREATE-DIR VALUE(file-info:file-name).
        END.
        {&methods/lValidateError.i NO}
    END.  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    FIND CURRENT users EXCLUSIVE.
    ASSIGN 
        users.userAlias = users.userAlias:screen-value in frame {&frame-name}
        users.securityLevel = INTEGER(users.securityLevel:SCREEN-VALUE)
        users.userType = cbUserType:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        users.envList = slEnvironments:SCREEN-VALUE
        users.dbList = slDatabases:SCREEN-VALUE
        users.modeList = slModes:SCREEN-VALUE.
    
    ASSIGN
        ttUsers.ttfUserAlias = users.userAlias
        ttUsers.ttfEnvList = if users.envList <> slEnvironments:list-items then users.envList else ""
        ttUsers.ttfDbList = if users.dbList <> slDatabases:list-items then users.dbList else ""
        ttUsers.ttfModeList = if users.modeList <> slModes:list-items then users.modeList else "".
    RUN ipWriteUsrFile.
    
    DISABLE 
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
        WITH FRAME {&FRAME-NAME}.
    
    RUN local-open-query in h_users.
    RUN ipReposition in h_users (ROWID(users)).
    
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
    IF zUsers.securityLevel > 699 THEN ASSIGN 
        users.track_usage:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
        users.use_colors:SENSITIVE = YES
        users.use_fonts:SENSITIVE = YES
        users.use_ctrl_keys:SENSITIVE = YES
        users.developer:SENSITIVE = YES
        users.userAlias:SENSITIVE = TRUE
        users.securityLevel:SENSITIVE = TRUE
        cbUserType:SENSITIVE = TRUE
        slEnvironments:SENSITIVE = TRUE
        slDatabases:SENSITIVE = TRUE
        slModes:SENSITIVE = TRUE
        cbUserType:screen-value = ""
        slEnvironments:screen-value = ""
        slDatabases:screen-value = ""
        slModes:screen-value = ""
        bAll1:SENSITIVE = TRUE
        bNone1:SENSITIVE = TRUE
        bAll2:SENSITIVE = TRUE
        bNone2:SENSITIVE = TRUE
        bAll3:SENSITIVE = TRUE
        bNone3:SENSITIVE = TRUE
        .
    ELSE ASSIGN 
        users.track_usage:SENSITIVE = NO 
        users.use_colors:SENSITIVE = NO
        users.use_fonts:SENSITIVE = NO
        users.use_ctrl_keys:SENSITIVE = NO
        users.developer:SENSITIVE = NO
        users.userAlias:SENSITIVE = FALSE
        users.securityLevel:SENSITIVE = FALSE
        cbUserType:SENSITIVE = FALSE
        slEnvironments:SENSITIVE = FALSE
        slDatabases:SENSITIVE = FALSE
        slModes:SENSITIVE = FALSE
        bAll1:SENSITIVE = FALSE
        bNone1:SENSITIVE = FALSE
        bAll2:SENSITIVE = FALSE
        bNone2:SENSITIVE = FALSE
        bAll3:SENSITIVE = FALSE
        bNone3:SENSITIVE = FALSE
        .
    ASSIGN 
        cbUserType:screen-value = users.userType
        slEnvironments:screen-value = if ttUsers.ttfEnvList <> "" then ttUsers.ttfEnvList else slEnvironments:list-items
        slDatabases:screen-value = if ttUsers.ttfDbList <> "" then ttUsers.ttfDbList else slDatabases:list-items
        slModes:screen-value = if ttUsers.ttfModeList <> "" then ttUsers.ttfModeList else slModes:list-items.


    ENABLE 
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

