&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/prgrms.w

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

&Scoped-define ENHANCE no

DEFINE VARIABLE hSubjectID    AS HANDLE    NO-UNDO EXTENT 20.
DEFINE VARIABLE hPgmMstrSecur AS HANDLE    NO-UNDO.
DEFINE VARIABLE iPageNo       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSubjectID    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSuperAdmin   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE saveParents   AS CHARACTER NO-UNDO.

DEFINE BUFFER bPrgrms FOR prgrms.

/* check if able to set menu fields */
IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
IF VALID-HANDLE(hPgmMstrSecur) THEN DO:
    RUN epCanAccess IN hPgmMstrSecur (
        "viewers/prgrms.w",
        "SuperAdmin",
        OUTPUT lSuperAdmin 
        ).
    DELETE OBJECT hPgmMstrSecur.
END.

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
&Scoped-define EXTERNAL-TABLES prgrms
&Scoped-define FIRST-EXTERNAL-TABLE prgrms


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prgrms.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS prgrms.prgtitle prgrms.dir_group ~
prgrms.securityLevelUser prgrms.module prgrms.run_persistent ~
prgrms.track_usage prgrms.popup prgrms.can_run prgrms.can_create ~
prgrms.can_update prgrms.can_delete prgrms.mfgroup prgrms.subjectID ~
prgrms.pageSubjectID[1] prgrms.pageSubjectID[2] prgrms.pageSubjectID[3] ~
prgrms.pageSubjectID[4] prgrms.pageSubjectID[5] prgrms.pageSubjectID[6] ~
prgrms.pageSubjectID[7] prgrms.pageSubjectID[8] prgrms.pageSubjectID[9] ~
prgrms.pageSubjectID[10] prgrms.pageSubjectID[11] prgrms.pageSubjectID[12] ~
prgrms.pageSubjectID[13] prgrms.pageSubjectID[14] prgrms.pageSubjectID[15] ~
prgrms.pageSubjectID[16] prgrms.pageSubjectID[17] prgrms.pageSubjectID[18] ~
prgrms.pageSubjectID[19] prgrms.pageSubjectID[20] prgrms.menu_item ~
prgrms.securityLevelDefault prgrms.mnemonic prgrms.systemType ~
prgrms.menuOrder prgrms.menuLevel prgrms.menuImage[1] prgrms.itemParent ~
prgrms.customMenuTitle
&Scoped-define ENABLED-TABLES prgrms
&Scoped-define FIRST-ENABLED-TABLE prgrms
&Scoped-Define DISPLAYED-FIELDS prgrms.prgmname prgrms.prgtitle ~
prgrms.dir_group prgrms.securityLevelUser prgrms.module ~
prgrms.run_persistent prgrms.track_usage prgrms.popup prgrms.can_run ~
prgrms.can_create prgrms.can_update prgrms.can_delete prgrms.mfgroup ~
prgrms.subjectID prgrms.pageSubjectID[1] prgrms.pageSubjectID[2] ~
prgrms.pageSubjectID[3] prgrms.pageSubjectID[4] prgrms.pageSubjectID[5] ~
prgrms.pageSubjectID[6] prgrms.pageSubjectID[7] prgrms.pageSubjectID[8] ~
prgrms.pageSubjectID[9] prgrms.pageSubjectID[10] prgrms.pageSubjectID[11] ~
prgrms.pageSubjectID[12] prgrms.pageSubjectID[13] prgrms.pageSubjectID[14] ~
prgrms.pageSubjectID[15] prgrms.pageSubjectID[16] prgrms.pageSubjectID[17] ~
prgrms.pageSubjectID[18] prgrms.pageSubjectID[19] prgrms.pageSubjectID[20] ~
prgrms.menu_item prgrms.securityLevelDefault prgrms.mnemonic ~
prgrms.systemType prgrms.menuOrder prgrms.menuLevel prgrms.menuImage[1] ~
prgrms.itemParent prgrms.customMenuTitle
&Scoped-define DISPLAYED-TABLES prgrms
&Scoped-define FIRST-DISPLAYED-TABLE prgrms
&Scoped-Define DISPLAYED-OBJECTS F1 F-2 parentPrgTitle 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,MENU-FIELDS,F1 */
&Scoped-define ADM-CREATE-FIELDS prgrms.prgmname 
&Scoped-define DISPLAY-FIELD prgrms.itemParent parentPrgTitle 
&Scoped-define MENU-FIELDS prgrms.menu_item prgrms.securityLevelDefault ~
prgrms.mnemonic prgrms.systemType prgrms.menuOrder prgrms.menuLevel ~
prgrms.menuImage[1] prgrms.itemParent 
&Scoped-define F1 F1 F-2 

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
DEFINE BUTTON btnParam-0  NO-FOCUS
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-1  NO-FOCUS
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-10 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-11 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-12 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-13 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-14 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-15 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-16 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-17 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-18 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-19 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-2 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-3 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-4 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-5 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-6 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-7 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-8 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE BUTTON btnParam-9 
     LABEL "Param" 
     SIZE 8 BY 1.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE parentPrgTitle AS CHARACTER FORMAT "X(256)":U INITIAL "Program Not Found" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE cMenuImage
     FILENAME "adeicon/blank":U TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 158 BY 22.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 128 BY 4.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnParam-0 AT ROW 3.38 COL 150 WIDGET-ID 214
     btnParam-1 AT ROW 4.38 COL 150 WIDGET-ID 216
     prgrms.prgmname AT ROW 1.24 COL 10 COLON-ALIGNED FORMAT "X(32)"
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
          BGCOLOR 15 FONT 4
     prgrms.prgtitle AT ROW 1.24 COL 55 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
          BGCOLOR 15 FONT 4          
     prgrms.dir_group AT ROW 2.43 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 FONT 4
     prgrms.securityLevelUser AT ROW 2.43 COL 55 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 
     prgrms.module AT ROW 2.43 COL 70 COLON-ALIGNED WIDGET-ID 160
          VIEW-AS COMBO-BOX INNER-LINES 20
          LIST-ITEMS "","AP","AR","DC","EQ","FG","GL","HS","JC","NS","OE","PO","RM","SB","SS","TS" 
          DROP-DOWN-LIST
          SIZE 8 BY 1
          BGCOLOR 15 
     prgrms.run_persistent AT ROW 2.43 COL 81
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY 1
     prgrms.track_usage AT ROW 2.43 COL 99
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY 1
     prgrms.popup AT ROW 2.43 COL 115
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     prgrms.can_run AT ROW 3.62 COL 12 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 118 BY 2.9
          BGCOLOR 15 
     prgrms.can_create AT ROW 6.52 COL 12 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 118 BY 2.9
          BGCOLOR 15 
     prgrms.can_update AT ROW 9.43 COL 12 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 118 BY 2.9
          BGCOLOR 15 
     prgrms.can_delete AT ROW 12.33 COL 12 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 118 BY 2.9
          BGCOLOR 15 
     prgrms.mfgroup AT ROW 15.24 COL 12 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 118 BY 2.9
          BGCOLOR 15 
     prgrms.subjectID AT ROW 1.24 COL 133 COLON-ALIGNED WIDGET-ID 32
          LABEL "Menu Subject ID"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     prgrms.pageSubjectID[1] AT ROW 3.38 COL 133 COLON-ALIGNED WIDGET-ID 186
          LABEL "0"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     prgrms.pageSubjectID[2] AT ROW 4.38 COL 133 COLON-ALIGNED WIDGET-ID 190
          LABEL "1"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     prgrms.pageSubjectID[3] AT ROW 5.38 COL 133 COLON-ALIGNED WIDGET-ID 192
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-2 AT ROW 5.38 COL 150 WIDGET-ID 218
     prgrms.pageSubjectID[4] AT ROW 6.38 COL 133 COLON-ALIGNED WIDGET-ID 194
          LABEL "3"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-3 AT ROW 6.38 COL 150 WIDGET-ID 220
     prgrms.pageSubjectID[5] AT ROW 7.38 COL 133 COLON-ALIGNED WIDGET-ID 196
          LABEL "4"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-4 AT ROW 7.38 COL 150 WIDGET-ID 222
     prgrms.pageSubjectID[6] AT ROW 8.38 COL 133 COLON-ALIGNED WIDGET-ID 198
          LABEL "5"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     btnParam-5 AT ROW 8.38 COL 150 WIDGET-ID 224
     prgrms.pageSubjectID[7] AT ROW 9.38 COL 133 COLON-ALIGNED WIDGET-ID 200
          LABEL "6"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-6 AT ROW 9.38 COL 150 WIDGET-ID 226
     prgrms.pageSubjectID[8] AT ROW 10.38 COL 133 COLON-ALIGNED WIDGET-ID 202
          LABEL "7"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-7 AT ROW 10.38 COL 150 WIDGET-ID 228
     prgrms.pageSubjectID[9] AT ROW 11.38 COL 133 COLON-ALIGNED WIDGET-ID 204
          LABEL "8"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-8 AT ROW 11.38 COL 150 WIDGET-ID 230
     prgrms.pageSubjectID[10] AT ROW 12.38 COL 133 COLON-ALIGNED WIDGET-ID 166
          LABEL "9"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-9 AT ROW 12.38 COL 150 WIDGET-ID 232
     prgrms.pageSubjectID[11] AT ROW 13.38 COL 133 COLON-ALIGNED WIDGET-ID 168
          LABEL "10"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-10 AT ROW 13.38 COL 150 WIDGET-ID 234
     prgrms.pageSubjectID[12] AT ROW 14.38 COL 133 COLON-ALIGNED WIDGET-ID 170
          LABEL "11"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-11 AT ROW 14.38 COL 150 WIDGET-ID 236
     prgrms.pageSubjectID[13] AT ROW 15.38 COL 133 COLON-ALIGNED WIDGET-ID 172
          LABEL "12"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-12 AT ROW 15.38 COL 150 WIDGET-ID 238
     prgrms.pageSubjectID[14] AT ROW 16.38 COL 133 COLON-ALIGNED WIDGET-ID 174
          LABEL "13"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-13 AT ROW 16.38 COL 150 WIDGET-ID 240
     prgrms.pageSubjectID[15] AT ROW 17.38 COL 133 COLON-ALIGNED WIDGET-ID 176
          LABEL "14"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-14 AT ROW 17.38 COL 150 WIDGET-ID 242
     prgrms.pageSubjectID[16] AT ROW 18.38 COL 133 COLON-ALIGNED WIDGET-ID 178
          LABEL "15"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     F1 AT ROW 2.43 COL 38 NO-LABEL
     F-2 AT ROW 22.19 COL 32 NO-LABEL WIDGET-ID 12
     btnParam-15 AT ROW 18.38 COL 150 WIDGET-ID 244
     prgrms.pageSubjectID[17] AT ROW 19.38 COL 133 COLON-ALIGNED WIDGET-ID 206
          LABEL "16"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-16 AT ROW 19.38 COL 150 WIDGET-ID 246
     prgrms.pageSubjectID[18] AT ROW 20.38 COL 133 COLON-ALIGNED WIDGET-ID 208
          LABEL "17"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-17 AT ROW 20.38 COL 150 WIDGET-ID 248
     prgrms.pageSubjectID[19] AT ROW 21.38 COL 133 COLON-ALIGNED WIDGET-ID 210
          LABEL "18"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     btnParam-18 AT ROW 21.38 COL 150 WIDGET-ID 250
     prgrms.pageSubjectID[20] AT ROW 22.38 COL 133 COLON-ALIGNED WIDGET-ID 212
          LABEL "19"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnParam-19 AT ROW 22.38 COL 150 WIDGET-ID 252
     prgrms.customMenuTitle AT ROW 18.81 COL 14 COLON-ALIGNED FORMAT "X(32)"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          BGCOLOR 15 FONT 4 
     prgrms.menu_item AT ROW 19.81 COL 16
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY 1
     prgrms.securityLevelDefault AT ROW 19.81 COL 65 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 
     prgrms.mnemonic AT ROW 19.81 COL 98 COLON-ALIGNED WIDGET-ID 6
          LABEL "HotKey (Mnemonic)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     prgrms.systemType AT ROW 19.81 COL 115 COLON-ALIGNED WIDGET-ID 2
          LABEL "Type"
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEMS "","Both","Corrware","Foldware" 
          DROP-DOWN-LIST
          SIZE 12 BY 1
          BGCOLOR 15 
     prgrms.menuOrder AT ROW 21 COL 14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 
     prgrms.menuLevel AT ROW 21 COL 34 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     prgrms.menuImage[1] AT ROW 21 COL 65 COLON-ALIGNED WIDGET-ID 8
          LABEL "Menu Image"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     prgrms.itemParent AT ROW 22.19 COL 14 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     parentPrgTitle AT ROW 22.19 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     "Delete:" VIEW-AS TEXT
          SIZE 7.6 BY .62 AT ROW 12.91 COL 4
     "Add:" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 6.71 COL 7
     "Update:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.81 COL 4
     "Parent(s):" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 16 COL 2
     "View:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 3.62 COL 6
     " Menu Fields" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 18.19 COL 3 WIDGET-ID 28
     "Page |   Subject ID   | Initialize" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 2.43 COL 129 WIDGET-ID 164
     RECT-1 AT ROW 1 COL 1
     cMenuImage AT ROW 21 COL 100 WIDGET-ID 18
     RECT-2 AT ROW 18.52 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.prgrms
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
         HEIGHT             = 22.62
         WIDTH              = 158.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

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

/* SETTINGS FOR BUTTON btnParam-0 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-11 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-12 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-13 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-14 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-15 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-16 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-17 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-18 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-19 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnParam-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE cMenuImage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN prgrms.itemParent IN FRAME F-Main
   4 5                                                                  */
/* SETTINGS FOR FILL-IN prgrms.menuImage[1] IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prgrms.menuLevel IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN prgrms.menuOrder IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR TOGGLE-BOX prgrms.menu_item IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN prgrms.mnemonic IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[10] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[11] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[12] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[13] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[14] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[15] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[16] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[17] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[18] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[19] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[20] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[8] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prgrms.pageSubjectID[9] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN parentPrgTitle IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN prgrms.prgmname IN FRAME F-Main
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN prgrms.prgtitle IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN prgrms.securityLevelDefault IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN prgrms.subjectID IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX prgrms.systemType IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN customMenuTitle IN FRAME F-Main
   EXP-FORMAT                                                           */   
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

&Scoped-define SELF-NAME btnParam-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-0 V-table-Win
ON CHOOSE OF btnParam-0 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-1 V-table-Win
ON CHOOSE OF btnParam-1 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-10 V-table-Win
ON CHOOSE OF btnParam-10 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-11 V-table-Win
ON CHOOSE OF btnParam-11 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-12 V-table-Win
ON CHOOSE OF btnParam-12 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-13 V-table-Win
ON CHOOSE OF btnParam-13 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-14 V-table-Win
ON CHOOSE OF btnParam-14 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-15 V-table-Win
ON CHOOSE OF btnParam-15 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-16 V-table-Win
ON CHOOSE OF btnParam-16 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-17 V-table-Win
ON CHOOSE OF btnParam-17 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-18 V-table-Win
ON CHOOSE OF btnParam-18 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-19 V-table-Win
ON CHOOSE OF btnParam-19 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-2 V-table-Win
ON CHOOSE OF btnParam-2 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-3 V-table-Win
ON CHOOSE OF btnParam-3 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-4 V-table-Win
ON CHOOSE OF btnParam-4 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-5 V-table-Win
ON CHOOSE OF btnParam-5 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-6 V-table-Win
ON CHOOSE OF btnParam-6 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-7 V-table-Win
ON CHOOSE OF btnParam-7 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-8 V-table-Win
ON CHOOSE OF btnParam-8 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam-9 V-table-Win
ON CHOOSE OF btnParam-9 IN FRAME F-Main /* Param */
DO:
    {AOA/includes/dynPageParam.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.can_create
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.can_create V-table-Win
ON ENTRY OF prgrms.can_create IN FRAME F-Main /* Create ID's */
DO:
  RUN get-attribute ("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "NO" THEN DO:
     DISABLE {&SELF-name} WITH FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.can_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.can_delete V-table-Win
ON ENTRY OF prgrms.can_delete IN FRAME F-Main /* Delete ID's */
DO:
  RUN get-attribute ("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "NO" THEN DO:
     DISABLE {&SELF-name} WITH FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.can_run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.can_run V-table-Win
ON ENTRY OF prgrms.can_run IN FRAME F-Main /* Run ID's */
DO:
  RUN get-attribute ("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "NO" THEN DO:
     DISABLE {&SELF-name} WITH FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.can_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.can_update V-table-Win
ON ENTRY OF prgrms.can_update IN FRAME F-Main /* Update ID's */
DO:
  RUN get-attribute ("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "NO" THEN DO:
     DISABLE {&SELF-name} WITH FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.dir_group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.dir_group V-table-Win
ON HELP OF prgrms.dir_group IN FRAME F-Main /* Directory */
DO:
  DEFINE VARIABLE m-lookup-var AS CHARACTER NO-UNDO.
  
  RUN "lookups/dir_lkup.p" (INPUT-OUTPUT m-lookup-var).
  IF m-lookup-var NE "" THEN
  {&SELF-NAME}:SCREEN-VALUE = m-lookup-var.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.itemParent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.itemParent V-table-Win
ON LEAVE OF prgrms.itemParent IN FRAME F-Main /* Parent */
DO:
    {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.menuImage[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.menuImage[1] V-table-Win
ON HELP OF prgrms.menuImage[1] IN FRAME F-Main /* Menu Image */
DO:
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitDir   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.

    ASSIGN
        cInitDir = SEARCH("Graphics\32x32\add.png")
        cInitDir = REPLACE(cInitDir,"add.png","")
        .
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
        cMenuImage:LOAD-IMAGE(cImageFile).
        SELF:SCREEN-VALUE = REPLACE(cImageFile,cInitDir,"").
    END. /* if ok */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.menuImage[1] V-table-Win
ON LEAVE OF prgrms.menuImage[1] IN FRAME F-Main /* Menu Image */
DO:
    cMenuImage:LOAD-IMAGE("Graphics\32x32\" + SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.mfgroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.mfgroup V-table-Win
ON ENTRY OF prgrms.mfgroup IN FRAME F-Main /* Misc. Fields Group */
DO:
  IF NOT CAN-DO('asi,nosweat',USERID('nosweat')) THEN DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
  END.
  RUN get-attribute ("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "NO" THEN DO:
     DISABLE {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.mnemonic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.mnemonic V-table-Win
ON LEAVE OF prgrms.mnemonic IN FRAME F-Main /* HotKey (Mnemonic) */
DO:
    IF SELF:SCREEN-VALUE NE "" AND
       CAN-FIND(FIRST prgrms
                WHERE prgrms.mnemonic EQ SELF:SCREEN-VALUE
                  AND prgrms.prgmName NE prgrms.prgmname:SCREEN-VALUE) THEN DO:
        MESSAGE
            "Hotkey:" SELF:SCREEN-VALUE "already exists, please retry."
        VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = prgrms.mnemonic.
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if can-find */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[10] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[10] IN FRAME F-Main /* 9 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[11]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[11] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[11] IN FRAME F-Main /* 10 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[12]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[12] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[12] IN FRAME F-Main /* 11 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[13]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[13] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[13] IN FRAME F-Main /* 12 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[14]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[14] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[14] IN FRAME F-Main /* 13 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[15]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[15] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[15] IN FRAME F-Main /* 14 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[16]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[16] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[16] IN FRAME F-Main /* 15 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[17]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[17] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[17] IN FRAME F-Main /* 16 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[18]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[18] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[18] IN FRAME F-Main /* 17 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[19]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[19] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[19] IN FRAME F-Main /* 18 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[1] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[1] IN FRAME F-Main /* 0 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[20]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[20] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[20] IN FRAME F-Main /* 19 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[2] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[2] IN FRAME F-Main /* 1 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[3] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[3] IN FRAME F-Main /* 2 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[4] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[4] IN FRAME F-Main /* 3 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[5] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[5] IN FRAME F-Main /* 4 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[6] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[6] IN FRAME F-Main /* 5 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[7] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[7] IN FRAME F-Main /* 6 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[8] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[8] IN FRAME F-Main /* 7 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.pageSubjectID[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.pageSubjectID[9] V-table-Win
ON LEAVE OF prgrms.pageSubjectID[9] IN FRAME F-Main /* 8 */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.securityLevelUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.securityLevelUser V-table-Win
ON LEAVE OF prgrms.securityLevelUser IN FRAME F-Main /* User Sec. Lev. */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) GT INTEGER(prgrms.securityLevelDefault:SCREEN-VALUE) THEN
    SELF:SCREEN-VALUE = prgrms.securityLevelDefault:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prgrms.subjectID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.subjectID V-table-Win
ON LEAVE OF prgrms.subjectID IN FRAME F-Main /* Menu Subject ID */
DO:
    {methods/entryerr.i
        &can-find="FIRST dynSubject WHERE dynSubject.subjectID EQ INT(SELF:SCREEN-VALUE)"
        &error-message="Invalid Subject ID, Please Try Again..."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
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
  {src/adm/template/row-list.i "prgrms"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prgrms"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  prgrms.module:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE
      prgrms.can_run
      prgrms.can_create
      prgrms.can_update
      prgrms.can_delete
      prgrms.mfgroup
          WITH FRAME {&FRAME-NAME}.
  cImageFile = SEARCH("Graphics\32x32\" + prgrms.menuImage[1]).
  IF cImageFile NE ? THEN
  cMenuImage:LOAD-IMAGE(cImageFile).
  ELSE
  cMenuImage:LOAD-IMAGE("Graphics\32x32\sign_forbidden.png").
  APPLY "entry" TO FRAME {&FRAME-NAME}.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF VALID-HANDLE(prgrms.pageSubjectID[1]:HANDLE) THEN
      ASSIGN
          hSubjectID[1]  = prgrms.pageSubjectID[1]:HANDLE
          hSubjectID[2]  = prgrms.pageSubjectID[2]:HANDLE
          hSubjectID[3]  = prgrms.pageSubjectID[3]:HANDLE
          hSubjectID[4]  = prgrms.pageSubjectID[4]:HANDLE
          hSubjectID[5]  = prgrms.pageSubjectID[5]:HANDLE
          hSubjectID[6]  = prgrms.pageSubjectID[6]:HANDLE
          hSubjectID[7]  = prgrms.pageSubjectID[7]:HANDLE
          hSubjectID[8]  = prgrms.pageSubjectID[8]:HANDLE
          hSubjectID[9]  = prgrms.pageSubjectID[9]:HANDLE
          hSubjectID[10] = prgrms.pageSubjectID[10]:HANDLE
          hSubjectID[11] = prgrms.pageSubjectID[11]:HANDLE
          hSubjectID[12] = prgrms.pageSubjectID[12]:HANDLE
          hSubjectID[13] = prgrms.pageSubjectID[13]:HANDLE
          hSubjectID[14] = prgrms.pageSubjectID[14]:HANDLE
          hSubjectID[15] = prgrms.pageSubjectID[15]:HANDLE
          hSubjectID[16] = prgrms.pageSubjectID[16]:HANDLE
          hSubjectID[17] = prgrms.pageSubjectID[17]:HANDLE
          hSubjectID[18] = prgrms.pageSubjectID[18]:HANDLE
          hSubjectID[19] = prgrms.pageSubjectID[19]:HANDLE
          hSubjectID[20] = prgrms.pageSubjectID[20]:HANDLE
          .
  END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bPrgrms FOR prgrms.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lSuperAdmin THEN DO:
    IF prgrms.menu_item EQ NO AND
       CAN-FIND(FIRST bPrgrms
                WHERE bPrgrms.itemParent EQ prgrms.prgmname
                  AND bPrgrms.menu_item  EQ YES) THEN DO:
        MESSAGE
            "Child Menu Items exist for this Menu Item,"
            "cannot inactivate this Menu Item unless Child"
            "Menu Items also inactivated." SKIP(1)
            "Inactivate Child Menu Items?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE lContinue AS LOGICAL.
        IF lContinue THEN DO:
            FOR EACH bPrgrms EXCLUSIVE-LOCK
                WHERE bPrgrms.itemParent EQ prgrms.prgmname
                  AND bPrgrms.menu_item  EQ YES 
                :
                bPrgrms.menu_item = NO.
            END. /* each prgrms */
        END. /* if lcontinue */
        ELSE
        ASSIGN
            prgrms.menu_item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
            prgrms.menu_item
            .
    END.

    IF prgrms.menu_item EQ YES AND
       prgrms.menuOrder GT 0   AND
       CAN-FIND(FIRST bPrgrms
                WHERE bPrgrms.menuOrder EQ prgrms.menuOrder
                  AND ROWID(bPrgrms)    NE ROWID(prgrms)) THEN DO:
      FOR EACH bPrgrms EXCLUSIVE-LOCK
          WHERE bPrgrms.menuOrder GE prgrms.menuOrder
            AND ROWID(bPrgrms)    NE ROWID(prgrms)
             BY bPrgrms.prgmname
          :
          bPrgrms.menuOrder = bPrgrms.menuOrder + 1.
      END. /* each bprgrms */
    END. /* if menu item */
    RUN pRebuildMenuTree.
  END. /* if super admin */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildMenuTree V-table-Win 
PROCEDURE pRebuildMenuTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
    
    IF lSuperAdmin THEN DO:
        MESSAGE
            "If changes to any Menu related values were done,"
            "you can rebuild the Menu Tree now, otherwise the"
            "changes will be applied after logging out and back in." SKIP(1)
            "Rebuild Menu Tree?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Rebuild?"
        UPDATE lRebuild AS LOGICAL.
        IF lRebuild THEN DO:
            pHandle = DYNAMIC-FUNCTION("sfGetMainMenuHandle").
            IF VALID-HANDLE(pHandle) THEN
            RUN pRebuildMenuTree IN pHandle.
        END. /* if rebuild */
    END. /* if super admin */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateSubjectID V-table-Win 
PROCEDURE pValidateSubjectID :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphSubjectID AS HANDLE NO-UNDO.
    
    IF NOT CAN-FIND(FIRST dynSubject
                    WHERE dynSubject.subjectID EQ INTEGER(iphSubjectID:SCREEN-VALUE)) THEN DO:
        MESSAGE 
            "Invalid Subject ID, Please Try Again..."
        VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END. /* if not can-find */

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
  {src/adm/template/snd-list.i "prgrms"}

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

