&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: listparm.w

  Description: List Parameters Create/Update

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 02/16/98

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

{methods/defines/hndldefs.i}

DEFINE VARIABLE listparm AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE item_list AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS db-name tablename dbfield-1 beginfld-1 ~
begin-default-1 sort-order-1 endfld-1 end-default-1 dbfield-2 beginfld-2 ~
begin-default-2 sort-order-2 endfld-2 end-default-2 dbfield-3 beginfld-3 ~
begin-default-3 sort-order-3 endfld-3 end-default-3 dbfield-4 beginfld-4 ~
begin-default-4 sort-order-4 endfld-4 end-default-4 dbfield-5 beginfld-5 ~
begin-default-5 sort-order-5 endfld-5 end-default-5 RECT-3 RECT-4 addfld-1 ~
add-default-1 addfld-2 add-default-2 shownotes addfld-3 add-default-3 ~
showmiscflds addfld-4 add-default-4 showaddr addfld-5 add-default-5 ~
showphones addfld-6 add-default-6 query-default Btn_Open Btn_Delete ~
Btn_Save save-name Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS db-name tablename dbfield-1 beginfld-1 ~
begin-default-1 sort-order-1 endfld-1 end-default-1 dbfield-2 beginfld-2 ~
begin-default-2 sort-order-2 endfld-2 end-default-2 dbfield-3 beginfld-3 ~
begin-default-3 sort-order-3 endfld-3 end-default-3 dbfield-4 beginfld-4 ~
begin-default-4 sort-order-4 endfld-4 end-default-4 dbfield-5 beginfld-5 ~
begin-default-5 sort-order-5 endfld-5 end-default-5 addfld-1 add-default-1 ~
addfld-2 add-default-2 shownotes addfld-3 add-default-3 showmiscflds ~
addfld-4 add-default-4 showaddr addfld-5 add-default-5 showphones addfld-6 ~
add-default-6 query-default save-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "&OK" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Open 
     LABEL "&Open" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Save 
     LABEL "&Save" 
     SIZE 14 BY 1.14.

DEFINE VARIABLE db-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "None" 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE dbfield-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field 1" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS " "
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE dbfield-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field 2" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS " "
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE dbfield-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field 3" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS " "
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE dbfield-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field 4" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS " "
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE dbfield-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field 5" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS " "
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tablename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "None" 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE addfld-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Add Fld 1" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE addfld-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Add Fld 2" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE addfld-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Add Fld 3" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE addfld-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Add Fld 4" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE addfld-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Add Fld 5" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE addfld-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Add Fld 6" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE beginfld-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Field 1" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE beginfld-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Field 2" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE beginfld-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Field 3" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE beginfld-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Field 4" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE beginfld-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Field 5" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE endfld-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Field 1" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE endfld-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Field 2" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE endfld-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Field 3" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE endfld-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Field 4" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE endfld-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Field 5" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE save-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Save As" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sort-order-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Default" 
     LABEL "Sort Order 1" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sort-order-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort Order 2" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sort-order-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort Order 3" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sort-order-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort Order 4" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sort-order-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort Order 5" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 7.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29 BY 6.19.

DEFINE VARIABLE add-default-1 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE add-default-2 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE add-default-3 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE add-default-4 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE add-default-5 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE add-default-6 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE begin-default-1 AS LOGICAL INITIAL yes 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE begin-default-2 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE begin-default-3 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE begin-default-4 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE begin-default-5 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE end-default-1 AS LOGICAL INITIAL yes 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE end-default-2 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE end-default-3 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE end-default-4 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE end-default-5 AS LOGICAL INITIAL no 
     LABEL "Use Default Settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE query-default AS LOGICAL INITIAL yes 
     LABEL "Use Query Default" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE showaddr AS LOGICAL INITIAL no 
     LABEL "Show Addresses" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE showmiscflds AS LOGICAL INITIAL no 
     LABEL "Show Misc Fields" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE shownotes AS LOGICAL INITIAL no 
     LABEL "Show Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE showphones AS LOGICAL INITIAL no 
     LABEL "Show Phones" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     db-name AT ROW 1.24 COL 12 COLON-ALIGNED HELP
          "Select Database"
     tablename AT ROW 1.24 COL 56 COLON-ALIGNED HELP
          "Select Table"
     dbfield-1 AT ROW 2.67 COL 12 COLON-ALIGNED HELP
          "Select Field 1"
     beginfld-1 AT ROW 2.67 COL 56 COLON-ALIGNED HELP
          "Enter Beginning Field Name 1"
     begin-default-1 AT ROW 2.67 COL 89.6 HELP
          "Use Default Settings Indicator"
     sort-order-1 AT ROW 3.86 COL 12 COLON-ALIGNED HELP
          "Enter Sort Order Description 1"
     endfld-1 AT ROW 3.86 COL 56 COLON-ALIGNED HELP
          "Enter Ending Field 1"
     end-default-1 AT ROW 3.86 COL 89.6 HELP
          "Use Default Settings Indicator"
     dbfield-2 AT ROW 5.05 COL 12 COLON-ALIGNED HELP
          "Select Field 2"
     beginfld-2 AT ROW 5.05 COL 56 COLON-ALIGNED HELP
          "Enter Beginning Field Name 2"
     begin-default-2 AT ROW 5.05 COL 89.6 HELP
          "Use Default Settings Indicator"
     sort-order-2 AT ROW 6.24 COL 12 COLON-ALIGNED HELP
          "Enter Sort Order Description 2"
     endfld-2 AT ROW 6.24 COL 56 COLON-ALIGNED HELP
          "Enter Ending Field 2"
     end-default-2 AT ROW 6.24 COL 89.6 HELP
          "Use Default Settings Indicator"
     dbfield-3 AT ROW 7.43 COL 12 COLON-ALIGNED HELP
          "Select Field 3"
     beginfld-3 AT ROW 7.43 COL 56 COLON-ALIGNED HELP
          "Enter Beginning Field Name 3"
     begin-default-3 AT ROW 7.43 COL 89.6 HELP
          "Use Default Settings Indicator"
     sort-order-3 AT ROW 8.62 COL 12 COLON-ALIGNED HELP
          "Enter Sort Order Description 3"
     endfld-3 AT ROW 8.62 COL 56 COLON-ALIGNED HELP
          "Enter Ending Field 3"
     end-default-3 AT ROW 8.62 COL 89.6 HELP
          "Use Default Settings Indicator"
     dbfield-4 AT ROW 9.81 COL 12 COLON-ALIGNED HELP
          "Select Field 4"
     beginfld-4 AT ROW 9.81 COL 56 COLON-ALIGNED HELP
          "Enter Beginning Field Name 4"
     begin-default-4 AT ROW 9.81 COL 89.6 HELP
          "Use Default Settings Indicator"
     sort-order-4 AT ROW 11 COL 12 COLON-ALIGNED HELP
          "Enter Sort Order Description 4"
     endfld-4 AT ROW 11 COL 56 COLON-ALIGNED HELP
          "Enter Ending Field 4"
     end-default-4 AT ROW 11 COL 89.6 HELP
          "Use Default Settings Indicator"
     dbfield-5 AT ROW 12.19 COL 12 COLON-ALIGNED HELP
          "Select Field 5"
     beginfld-5 AT ROW 12.19 COL 56 COLON-ALIGNED HELP
          "Enter Beginning Field Name 5"
     begin-default-5 AT ROW 12.19 COL 89.6 HELP
          "Use Default Settings Indicator"
     sort-order-5 AT ROW 13.38 COL 12 COLON-ALIGNED HELP
          "Enter Sort Order Description 5"
     endfld-5 AT ROW 13.38 COL 56 COLON-ALIGNED HELP
          "Enter Ending Field 5"
     end-default-5 AT ROW 13.38 COL 89.6 HELP
          "Use Default Settings Indicator"
     addfld-1 AT ROW 15.29 COL 12 COLON-ALIGNED HELP
          "Enter Additional Field 1"
     add-default-1 AT ROW 15.29 COL 45.6 HELP
          "Use Default Settings Indicator"
     addfld-2 AT ROW 16.48 COL 12 COLON-ALIGNED HELP
          "Enter Additional Field 2"
     add-default-2 AT ROW 16.48 COL 45.6 HELP
          "Use Default Settings Indicator"
     shownotes AT ROW 16.48 COL 89 HELP
          "Enter Show Notes Indicator"
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     addfld-3 AT ROW 17.67 COL 12 COLON-ALIGNED HELP
          "Enter Additional Field 3"
     add-default-3 AT ROW 17.67 COL 45.6 HELP
          "Use Default Settings Indicator"
     showmiscflds AT ROW 17.67 COL 89 HELP
          "Enter Show Misc Fields Indicator"
     addfld-4 AT ROW 18.86 COL 12 COLON-ALIGNED HELP
          "Enter Additional Field 4"
     add-default-4 AT ROW 18.86 COL 45.6 HELP
          "Use Default Settings Indicator"
     showaddr AT ROW 18.86 COL 89 HELP
          "Enter Show Addresses Indicator"
     addfld-5 AT ROW 20.05 COL 12 COLON-ALIGNED HELP
          "Enter Additional Field 5"
     add-default-5 AT ROW 20.05 COL 45.6 HELP
          "Use Default Settings Indicator"
     showphones AT ROW 20.05 COL 89 HELP
          "Enter Show Phones Indicator"
     addfld-6 AT ROW 21.24 COL 12 COLON-ALIGNED HELP
          "Enter Additional Field 6"
     add-default-6 AT ROW 21.24 COL 45.6 HELP
          "Use Default Settings Indicator"
     query-default AT ROW 21.48 COL 89 HELP
          "Use Query Default Structure"
     Btn_Open AT ROW 22.67 COL 2 HELP
          "OPEN List Parameter File"
     Btn_Delete AT ROW 22.67 COL 24 HELP
          "DELETE Current List Parameter File"
     Btn_Save AT ROW 22.67 COL 39 HELP
          "SAVE Current List Parameters"
     save-name AT ROW 22.67 COL 64 COLON-ALIGNED HELP
          "Enter Save As Name"
     Btn_Cancel AT ROW 22.67 COL 84 HELP
          "CANCEL and EXIT List Parameters Create/Update"
     Btn_OK AT ROW 22.67 COL 99 HELP
          "OK to SAVE and EXIT List Parameter Create/Update"
     "List Request Additional Fields" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 14.57 COL 3
     "Show Parameters" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 14.57 COL 85
     RECT-3 AT ROW 14.81 COL 2
     RECT-4 AT ROW 14.81 COL 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 23.


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
         TITLE              = "List Parameters Create/Update"
         HEIGHT             = 23
         WIDTH              = 113
         MAX-HEIGHT         = 23
         MAX-WIDTH          = 113
         VIRTUAL-HEIGHT     = 23
         VIRTUAL-WIDTH      = 113
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

IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME






/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* List Parameters Create/Update */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* List Parameters Create/Update */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete C-Win
ON CHOOSE OF Btn_Delete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  IF SEARCH(listparm) = ? THEN
  RETURN NO-APPLY.
  MESSAGE "Delete this List Parameter File?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE deleteok AS LOGICAL.
  IF NOT deleteok THEN
  RETURN NO-APPLY.
  OS-DELETE VALUE(listparm).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CHOOSE"TO Btn_Save.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Open C-Win
ON CHOOSE OF Btn_Open IN FRAME DEFAULT-FRAME /* Open */
DO:
  RUN Open-ListParm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save C-Win
ON CHOOSE OF Btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  RUN Save-ListParm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME db-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL db-name C-Win
ON VALUE-CHANGED OF db-name IN FRAME DEFAULT-FRAME /* Database */
DO:
  tablename:LIST-ITEMS = "None".
  IF {&SELF-NAME}:SCREEN-VALUE NE "None" THEN
  RUN Get-Tables.
  ASSIGN
    dbfield-1:LIST-ITEMS = ""
    dbfield-2:LIST-ITEMS = ""
    dbfield-3:LIST-ITEMS = ""
    dbfield-4:LIST-ITEMS = ""
    dbfield-5:LIST-ITEMS = ""
    tablename:SCREEN-VALUE = tablename:ENTRY(1)
    sort-order-1:SCREEN-VALUE = "Default"
    sort-order-2:SCREEN-VALUE = ""
    sort-order-3:SCREEN-VALUE = ""
    sort-order-4:SCREEN-VALUE = ""
    sort-order-5:SCREEN-VALUE = ""
    beginfld-1:SCREEN-VALUE = ""
    beginfld-2:SCREEN-VALUE = ""
    beginfld-3:SCREEN-VALUE = ""
    beginfld-4:SCREEN-VALUE = ""
    beginfld-5:SCREEN-VALUE = ""
    endfld-1:SCREEN-VALUE = ""
    endfld-2:SCREEN-VALUE = ""
    endfld-3:SCREEN-VALUE = ""
    endfld-4:SCREEN-VALUE = ""
    endfld-5:SCREEN-VALUE = ""
    shownotes:SCREEN-VALUE = "yes"
    showmiscflds:SCREEN-VALUE = "yes"
    showaddr:SCREEN-VALUE = "no"
    showphones:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbfield-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbfield-1 C-Win
ON VALUE-CHANGED OF dbfield-1 IN FRAME DEFAULT-FRAME /* Field 1 */
DO:
  RUN Set-Defaults ({&SELF-NAME}:SCREEN-VALUE,1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbfield-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbfield-2 C-Win
ON VALUE-CHANGED OF dbfield-2 IN FRAME DEFAULT-FRAME /* Field 2 */
DO:
  RUN Set-Defaults ({&SELF-NAME}:SCREEN-VALUE,2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbfield-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbfield-3 C-Win
ON VALUE-CHANGED OF dbfield-3 IN FRAME DEFAULT-FRAME /* Field 3 */
DO:
  RUN Set-Defaults ({&SELF-NAME}:SCREEN-VALUE,3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbfield-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbfield-4 C-Win
ON VALUE-CHANGED OF dbfield-4 IN FRAME DEFAULT-FRAME /* Field 4 */
DO:
  RUN Set-Defaults ({&SELF-NAME}:SCREEN-VALUE,4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbfield-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbfield-5 C-Win
ON VALUE-CHANGED OF dbfield-5 IN FRAME DEFAULT-FRAME /* Field 5 */
DO:
  RUN Set-Defaults ({&SELF-NAME}:SCREEN-VALUE,5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tablename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tablename C-Win
ON VALUE-CHANGED OF tablename IN FRAME DEFAULT-FRAME /* Table */
DO:
  ASSIGN
    dbfield-1:SCREEN-VALUE = ""
    dbfield-2:SCREEN-VALUE = ""
    dbfield-3:SCREEN-VALUE = ""
    dbfield-4:SCREEN-VALUE = ""
    dbfield-5:SCREEN-VALUE = "".
  IF {&SELF-NAME}:SCREEN-VALUE NE "None" THEN
  RUN Get-Fields.
  ASSIGN
    sort-order-1:SCREEN-VALUE = "Default"
    sort-order-2:SCREEN-VALUE = ""
    sort-order-3:SCREEN-VALUE = ""
    sort-order-4:SCREEN-VALUE = ""
    sort-order-5:SCREEN-VALUE = ""
    beginfld-1:SCREEN-VALUE = ""
    beginfld-2:SCREEN-VALUE = ""
    beginfld-3:SCREEN-VALUE = ""
    beginfld-4:SCREEN-VALUE = ""
    beginfld-5:SCREEN-VALUE = ""
    endfld-1:SCREEN-VALUE = ""
    endfld-2:SCREEN-VALUE = ""
    endfld-3:SCREEN-VALUE = ""
    endfld-4:SCREEN-VALUE = ""
    endfld-5:SCREEN-VALUE = ""
    shownotes:SCREEN-VALUE = "yes"
    showmiscflds:SCREEN-VALUE = "yes"
    showaddr:SCREEN-VALUE = "no"
    showphones:SCREEN-VALUE = "no".
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
  RUN enable_UI.
  {methods/enhance.i}
  RUN Get-DBs.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear-Defaults C-Win 
PROCEDURE Clear-Defaults :
/*------------------------------------------------------------------------------
  Purpose:     Clear Default settings
  Parameters:  Field Number
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dbfield-no AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    CASE dbfield-no:
      WHEN 1 THEN
      ASSIGN
        beginfld-1:SCREEN-VALUE = ""
        endfld-1:SCREEN-VALUE = ""
        sort-order-1:SCREEN-VALUE = "".
      WHEN 2 THEN
      ASSIGN
        beginfld-2:SCREEN-VALUE = ""
        endfld-2:SCREEN-VALUE = ""
        sort-order-2:SCREEN-VALUE = "".
      WHEN 3 THEN
      ASSIGN
        beginfld-3:SCREEN-VALUE = ""
        endfld-3:SCREEN-VALUE = ""
        sort-order-3:SCREEN-VALUE = "".
      WHEN 4 THEN
      ASSIGN
        beginfld-4:SCREEN-VALUE = ""
        endfld-4:SCREEN-VALUE = ""
        sort-order-4:SCREEN-VALUE = "".
      WHEN 5 THEN
      ASSIGN
        beginfld-5:SCREEN-VALUE = ""
        endfld-5:SCREEN-VALUE = ""
        sort-order-5:SCREEN-VALUE = "".
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY db-name tablename dbfield-1 beginfld-1 begin-default-1 sort-order-1 
          endfld-1 end-default-1 dbfield-2 beginfld-2 begin-default-2 
          sort-order-2 endfld-2 end-default-2 dbfield-3 beginfld-3 
          begin-default-3 sort-order-3 endfld-3 end-default-3 dbfield-4 
          beginfld-4 begin-default-4 sort-order-4 endfld-4 end-default-4 
          dbfield-5 beginfld-5 begin-default-5 sort-order-5 endfld-5 
          end-default-5 addfld-1 add-default-1 addfld-2 add-default-2 shownotes 
          addfld-3 add-default-3 showmiscflds addfld-4 add-default-4 showaddr 
          addfld-5 add-default-5 showphones addfld-6 add-default-6 query-default 
          save-name 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE db-name tablename dbfield-1 beginfld-1 begin-default-1 sort-order-1 
         endfld-1 end-default-1 dbfield-2 beginfld-2 begin-default-2 
         sort-order-2 endfld-2 end-default-2 dbfield-3 beginfld-3 
         begin-default-3 sort-order-3 endfld-3 end-default-3 dbfield-4 
         beginfld-4 begin-default-4 sort-order-4 endfld-4 end-default-4 
         dbfield-5 beginfld-5 begin-default-5 sort-order-5 endfld-5 
         end-default-5 RECT-3 RECT-4 addfld-1 add-default-1 addfld-2 
         add-default-2 shownotes addfld-3 add-default-3 showmiscflds addfld-4 
         add-default-4 showaddr addfld-5 add-default-5 showphones addfld-6 
         add-default-6 query-default Btn_Open Btn_Delete Btn_Save save-name 
         Btn_Cancel Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-DBs C-Win 
PROCEDURE Get-DBs :
/*------------------------------------------------------------------------------
  Purpose:     Load Database Names
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO NUM-DBS:
      ldummy = db-name:ADD-LAST(LDBNAME(i)).
    END.
    ASSIGN
      db-name:SCREEN-VALUE = db-name:ENTRY(1)
      tablename:SCREEN-VALUE = tablename:ENTRY(1).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Fields C-Win 
PROCEDURE Get-Fields :
/*------------------------------------------------------------------------------
  Purpose:     Get Database Fields
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CREATE ALIAS dictdb FOR DATABASE VALUE(db-name:SCREEN-VALUE).
    RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (tablename:SCREEN-VALUE,OUTPUT item_list).
    ASSIGN
      dbfield-1:LIST-ITEMS = "," + item_list
      dbfield-2:LIST-ITEMS = "," + item_list
      dbfield-3:LIST-ITEMS = "," + item_list
      dbfield-4:LIST-ITEMS = "," + item_list
      dbfield-5:LIST-ITEMS = "," + item_list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Tables C-Win 
PROCEDURE Get-Tables :
/*------------------------------------------------------------------------------
  Purpose:     Get Database Tables
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CREATE ALIAS dictdb FOR DATABASE VALUE(db-name:SCREEN-VALUE).
    RUN Get_Procedure IN Persistent-Handle (INPUT "filelist.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (OUTPUT item_list).
    tablename:LIST-ITEMS = tablename:LIST-ITEMS + "," + item_list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open-ListParm C-Win 
PROCEDURE Open-ListParm :
/*------------------------------------------------------------------------------
  Purpose:     Open List Parameters File
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE chrfld AS CHARACTER EXTENT 50 NO-UNDO.
  DEFINE VARIABLE fldcnt AS INTEGER NO-UNDO.

  ASSIGN
    init-dir = "methods\listobjs\scopdefs"
    listparm = "".
  SYSTEM-DIALOG GET-FILE listparm
      TITLE      "Choose List Parameter to OPEN..."
      FILTERS    "Parameter Files (*.i)" "*.i"
      INITIAL-DIR init-dir
      MUST-EXIST
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN NO-APPLY.
  INPUT FROM VALUE(listparm) NO-ECHO.
  IMPORT ^.
  DO fldcnt = 1 TO EXTENT(chrfld):
    IMPORT chrfld[fldcnt].
  END.
  INPUT CLOSE.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      save-name:SCREEN-VALUE = chrfld[1]
      db-name:SCREEN-VALUE = chrfld[2].
    RUN Get-Tables.
    tablename:SCREEN-VALUE = chrfld[3].
    RUN Get-Fields.
    ASSIGN
      dbfield-1:SCREEN-VALUE = chrfld[4]
      sort-order-1:SCREEN-VALUE = chrfld[5]
      beginfld-1:SCREEN-VALUE = chrfld[6]
      begin-default-1:SCREEN-VALUE = chrfld[7]
      endfld-1:SCREEN-VALUE = chrfld[8]
      end-default-1:SCREEN-VALUE = chrfld[9]
      dbfield-2:SCREEN-VALUE = chrfld[10]
      sort-order-2:SCREEN-VALUE = chrfld[11]
      beginfld-2:SCREEN-VALUE = chrfld[12]
      begin-default-2:SCREEN-VALUE = chrfld[13]
      endfld-2:SCREEN-VALUE = chrfld[14]
      end-default-2:SCREEN-VALUE = chrfld[15]
      dbfield-3:SCREEN-VALUE = chrfld[16]
      sort-order-3:SCREEN-VALUE = chrfld[17]
      beginfld-3:SCREEN-VALUE = chrfld[18]
      begin-default-3:SCREEN-VALUE = chrfld[19]
      endfld-3:SCREEN-VALUE = chrfld[20]
      end-default-3:SCREEN-VALUE = chrfld[21]
      dbfield-4:SCREEN-VALUE = chrfld[22]
      sort-order-4:SCREEN-VALUE = chrfld[23]
      beginfld-4:SCREEN-VALUE = chrfld[24]
      begin-default-4:SCREEN-VALUE = chrfld[25]
      endfld-4:SCREEN-VALUE = chrfld[26]
      end-default-4:SCREEN-VALUE = chrfld[27]
      dbfield-5:SCREEN-VALUE = chrfld[28]
      sort-order-5:SCREEN-VALUE = chrfld[29]
      beginfld-5:SCREEN-VALUE = chrfld[30]
      begin-default-5:SCREEN-VALUE = chrfld[31]
      endfld-5:SCREEN-VALUE = chrfld[32]
      end-default-5:SCREEN-VALUE = chrfld[33]
      addfld-1:SCREEN-VALUE = chrfld[34]
      add-default-1:SCREEN-VALUE = chrfld[35]
      addfld-2:SCREEN-VALUE = chrfld[36]
      add-default-2:SCREEN-VALUE = chrfld[37]
      addfld-3:SCREEN-VALUE = chrfld[38]
      add-default-3:SCREEN-VALUE = chrfld[39]
      addfld-4:SCREEN-VALUE = chrfld[40]
      add-default-4:SCREEN-VALUE = chrfld[41]
      addfld-5:SCREEN-VALUE = chrfld[42]
      add-default-5:SCREEN-VALUE = chrfld[43]
      addfld-6:SCREEN-VALUE = chrfld[44]
      add-default-6:SCREEN-VALUE = chrfld[45]
      shownotes:SCREEN-VALUE = chrfld[46]
      showmiscflds:SCREEN-VALUE = chrfld[47]
      showaddr:SCREEN-VALUE = chrfld[48]
      showphones:SCREEN-VALUE = chrfld[49]
      query-default:SCREEN-VALUE = chrfld[50].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-addfld-Defines C-Win 
PROCEDURE Output-addfld-Defines :
/*------------------------------------------------------------------------------
  Purpose:     Output additional fields scoped-defines
  Parameters:  Screen-Values
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER addfld AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER add-default AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER fld# AS INTEGER NO-UNDO.

  PUT UNFORMATTED
    "~&~{&DEFINETYPE}-define ADDFLD-" fld# " " addfld SKIP
    "~&~{&DEFINETYPE}-define ADD-DEFAULT-" fld# " " add-default SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-addfld-Values C-Win 
PROCEDURE Output-addfld-Values :
/*------------------------------------------------------------------------------
  Purpose:     Output additional field values
  Parameters:  Screen Values
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER addfld AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER add-default AS CHARACTER NO-UNDO.

  EXPORT addfld.
  EXPORT add-default.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-Defines C-Win 
PROCEDURE Output-Defines :
/*------------------------------------------------------------------------------
  Purpose:     Output Scoped-define Lines
  Parameters:  Screen Values
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dbfield AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER beginfld AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER begin-default AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER endfld AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER end-default AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER fld# AS INTEGER NO-UNDO.

  DEFINE VARIABLE data-type AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF dbfield NE ? THEN
    DO:
      data-type = "STRING".
      RUN Get_Procedure IN Persistent-Handle ("get_type.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (tablename:SCREEN-VALUE,dbfield,OUTPUT data-type).
      IF data-type NE "STRING" THEN
      PUT UNFORMATTED "~&~{&DEFINETYPE}-define DATATYP" fld# " " CAPS(data-type) SKIP.
    END.
    PUT UNFORMATTED "~&~{&DEFINETYPE}-define DBFIELD" fld# " ".
    IF dbfield NE ? THEN
    PUT UNFORMATTED tablename:SCREEN-VALUE "." dbfield.
    PUT UNFORMATTED SKIP
      "~&~{&DEFINETYPE}-define BEGINFLD" fld# " " beginfld SKIP
      "~&~{&DEFINETYPE}-define BEGIN-DEFAULT-" fld# " " begin-default SKIP
      "~&~{&DEFINETYPE}-define ENDFLD" fld# " " endfld SKIP
      "~&~{&DEFINETYPE}-define END-DEFAULT-" fld# " " end-default SKIP.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-Values C-Win 
PROCEDURE Output-Values :
/*------------------------------------------------------------------------------
  Purpose:     Output Value Lines
  Parameters:  Screen Values
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dbfield AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER sort-order AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER beginfld AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER begin-default AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER endfld AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER end-default AS CHARACTER NO-UNDO.

  EXPORT dbfield.
  EXPORT sort-order.
  EXPORT beginfld.
  EXPORT begin-default.
  EXPORT endfld.
  EXPORT end-default.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save-ListParm C-Win 
PROCEDURE Save-ListParm :
/*------------------------------------------------------------------------------
  Purpose:     Save List Parameter File
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF tablename:SCREEN-VALUE = "None" AND
       save-name:SCREEN-VALUE = "" THEN
    RETURN.
    IF save-name:SCREEN-VALUE = "" THEN
    save-name:SCREEN-VALUE = tablename:SCREEN-VALUE.
    OUTPUT TO VALUE("methods/listobjs/scopdefs/" + save-name:SCREEN-VALUE + ".i").
    PUT UNFORMATTED "/* " save-name:SCREEN-VALUE ".i" SKIP.
      EXPORT save-name:SCREEN-VALUE.
      EXPORT db-name:SCREEN-VALUE.
      EXPORT tablename:SCREEN-VALUE.
    RUN Output-Values(dbfield-1:SCREEN-VALUE,sort-order-1:SCREEN-VALUE,
                       beginfld-1:SCREEN-VALUE,begin-default-1:SCREEN-VALUE,
                       endfld-1:SCREEN-VALUE,end-default-1:SCREEN-VALUE).
    RUN Output-Values(dbfield-2:SCREEN-VALUE,sort-order-2:SCREEN-VALUE,
                       beginfld-2:SCREEN-VALUE,begin-default-2:SCREEN-VALUE,
                       endfld-2:SCREEN-VALUE,end-default-2:SCREEN-VALUE).
    RUN Output-Values(dbfield-3:SCREEN-VALUE,sort-order-3:SCREEN-VALUE,
                       beginfld-3:SCREEN-VALUE,begin-default-3:SCREEN-VALUE,
                       endfld-3:SCREEN-VALUE,end-default-3:SCREEN-VALUE).
    RUN Output-Values(dbfield-4:SCREEN-VALUE,sort-order-4:SCREEN-VALUE,
                       beginfld-4:SCREEN-VALUE,begin-default-4:SCREEN-VALUE,
                       endfld-4:SCREEN-VALUE,end-default-4:SCREEN-VALUE).
    RUN Output-Values(dbfield-5:SCREEN-VALUE,sort-order-5:SCREEN-VALUE,
                       beginfld-5:SCREEN-VALUE,begin-default-5:SCREEN-VALUE,
                       endfld-5:SCREEN-VALUE,end-default-5:SCREEN-VALUE).
    RUN Output-addfld-Values(addfld-1:SCREEN-VALUE,add-default-1:SCREEN-VALUE).
    RUN Output-addfld-Values(addfld-2:SCREEN-VALUE,add-default-2:SCREEN-VALUE).
    RUN Output-addfld-Values(addfld-3:SCREEN-VALUE,add-default-3:SCREEN-VALUE).
    RUN Output-addfld-Values(addfld-4:SCREEN-VALUE,add-default-4:SCREEN-VALUE).
    RUN Output-addfld-Values(addfld-5:SCREEN-VALUE,add-default-5:SCREEN-VALUE).
    RUN Output-addfld-Values(addfld-6:SCREEN-VALUE,add-default-6:SCREEN-VALUE).
    EXPORT shownotes:SCREEN-VALUE.
    EXPORT showmiscflds:SCREEN-VALUE.
    EXPORT showaddr:SCREEN-VALUE.
    EXPORT showphones:SCREEN-VALUE.
    EXPORT query-default:SCREEN-VALUE.
    PUT UNFORMATTED "*/" SKIP(1)
      "~&~{&DEFINETYPE}-define TABLENAME " tablename:SCREEN-VALUE SKIP.
    RUN Output-Defines(dbfield-1:SCREEN-VALUE,beginfld-1:SCREEN-VALUE,
                       begin-default-1:SCREEN-VALUE,endfld-1:SCREEN-VALUE,
                       end-default-1:SCREEN-VALUE,1).
    RUN Output-Defines(dbfield-2:SCREEN-VALUE,beginfld-2:SCREEN-VALUE,
                       begin-default-2:SCREEN-VALUE,endfld-2:SCREEN-VALUE,
                       end-default-2:SCREEN-VALUE,2).
    RUN Output-Defines(dbfield-3:SCREEN-VALUE,beginfld-3:SCREEN-VALUE,
                       begin-default-3:SCREEN-VALUE,endfld-3:SCREEN-VALUE,
                       end-default-3:SCREEN-VALUE,3).
    RUN Output-Defines(dbfield-4:SCREEN-VALUE,beginfld-4:SCREEN-VALUE,
                       begin-default-4:SCREEN-VALUE,endfld-4:SCREEN-VALUE,
                       end-default-4:SCREEN-VALUE,4).
    RUN Output-Defines(dbfield-5:SCREEN-VALUE,beginfld-5:SCREEN-VALUE,
                       begin-default-5:SCREEN-VALUE,endfld-5:SCREEN-VALUE,
                       end-default-5:SCREEN-VALUE,5).
    PUT UNFORMATTED "~&~{&DEFINETYPE}-define LISTORDER " sort-order-1:SCREEN-VALUE.
    IF sort-order-2:SCREEN-VALUE NE "" THEN
    PUT UNFORMATTED
    "," sort-order-2:SCREEN-VALUE.
    IF sort-order-3:SCREEN-VALUE NE "" THEN
    PUT UNFORMATTED
    "," sort-order-3:SCREEN-VALUE.
    IF sort-order-4:SCREEN-VALUE NE "" THEN
    PUT UNFORMATTED
    "," sort-order-4:SCREEN-VALUE.
    IF sort-order-5:SCREEN-VALUE NE "" THEN
    PUT UNFORMATTED
    "," sort-order-5:SCREEN-VALUE.
    PUT UNFORMATTED SKIP.
    RUN Output-addfld-Defines(addfld-1:SCREEN-VALUE,add-default-1:SCREEN-VALUE,1).
    RUN Output-addfld-Defines(addfld-2:SCREEN-VALUE,add-default-2:SCREEN-VALUE,2).
    RUN Output-addfld-Defines(addfld-3:SCREEN-VALUE,add-default-3:SCREEN-VALUE,3).
    RUN Output-addfld-Defines(addfld-4:SCREEN-VALUE,add-default-4:SCREEN-VALUE,4).
    RUN Output-addfld-Defines(addfld-5:SCREEN-VALUE,add-default-5:SCREEN-VALUE,5).
    RUN Output-addfld-Defines(addfld-6:SCREEN-VALUE,add-default-6:SCREEN-VALUE,6).
    PUT UNFORMATTED SKIP
      "~&~{&DEFINETYPE}-define DISPLAYFLDS "
      "~{&ADDFLD-1} ~{&ADDFLD-2} ~{&ADDFLD-3} "
      "~{&ADDFLD-4} ~{&ADDFLD-5} ~{&ADDFLD-6}" SKIP
      "~&~{&DEFINETYPE}-define SHOWNOTES " shownotes:SCREEN-VALUE SKIP
      "~&~{&DEFINETYPE}-define SHOWMISCFLDS " showmiscflds:SCREEN-VALUE SKIP
      "~&~{&DEFINETYPE}-define SHOWADDRESSES " showaddr:SCREEN-VALUE SKIP
      "~&~{&DEFINETYPE}-define SHOWPHONES " showphones:SCREEN-VALUE SKIP
      "~&~{&DEFINETYPE}-define SAVENAME " save-name:SCREEN-VALUE SKIP
      "~&~{&DEFINETYPE}-define QUERYDEFAULT " query-default:SCREEN-VALUE SKIP.
    OUTPUT CLOSE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Defaults C-Win 
PROCEDURE Set-Defaults :
/*------------------------------------------------------------------------------
  Purpose:     Set Beginfld, Endfld, and Sort-Order Default Values
  Parameters:  Field Value 1 thru 5 to indicate dbfield1 thru dbfield5
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dbfield-name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER dbfield-no AS INTEGER NO-UNDO.

  RUN Clear-Defaults(dbfield-no).
  IF dbfield-name = ? THEN
  RETURN.

  DO WITH FRAME {&FRAME-NAME}:
    CREATE ALIAS dictdb FOR DATABASE VALUE(db-name:SCREEN-VALUE).
    RUN Get_Procedure IN Persistent-Handle (INPUT "fld_lbls.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (tablename:SCREEN-VALUE,dbfield-name,OUTPUT item_list).
    CASE dbfield-no:
      WHEN 1 THEN
      ASSIGN
        beginfld-1:SCREEN-VALUE = "begin_" + dbfield-name
        endfld-1:SCREEN-VALUE = "end_" + dbfield-name
        sort-order-1:SCREEN-VALUE = item_list.
      WHEN 2 THEN
      ASSIGN
        beginfld-2:SCREEN-VALUE = "begin_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        endfld-2:SCREEN-VALUE = "end_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        sort-order-2:SCREEN-VALUE = item_list.
      WHEN 3 THEN
      ASSIGN
        beginfld-3:SCREEN-VALUE = "begin_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        endfld-3:SCREEN-VALUE = "end_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        sort-order-3:SCREEN-VALUE = item_list.
      WHEN 4 THEN
      ASSIGN
        beginfld-4:SCREEN-VALUE = "begin_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        endfld-4:SCREEN-VALUE = "end_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        sort-order-4:SCREEN-VALUE = item_list.
      WHEN 5 THEN
      ASSIGN
        beginfld-5:SCREEN-VALUE = "begin_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        endfld-5:SCREEN-VALUE = "end_" + tablename:SCREEN-VALUE + "_" + dbfield-name
        sort-order-5:SCREEN-VALUE = item_list.
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i Btn_Open}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


