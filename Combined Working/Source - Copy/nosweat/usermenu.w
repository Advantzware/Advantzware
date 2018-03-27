&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: userMenu.w

  Description: usermenu interface

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.22.2016

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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
&ENDIF

DEFINE VARIABLE sourceMenu AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttUserMenu NO-UNDO
    FIELD menuOrder AS INTEGER   LABEL "#"     FORMAT ">>9"
    FIELD prgmName  AS CHARACTER LABEL "Name"  FORMAT "x(12)"
    FIELD level     AS CHARACTER EXTENT 3      FORMAT "x(30)"
    FIELD levelType AS CHARACTER EXTENT 3      FORMAT "x(8)"
    FIELD mainMenu  AS LOGICAL
    FIELD isMenu    AS LOGICAL
    FIELD updated   AS LOGICAL
        INDEX menuOrder IS PRIMARY
              menuOrder
        .

DEFINE BUFFER bttUserMenu FOR ttUserMenu.

DEFINE TEMP-TABLE ttMenu NO-UNDO
    FIELD prgmName AS CHARACTER LABEL "Name"      FORMAT "x(12)"
    FIELD prgTitle AS CHARACTER LABEL "Menu Name" FORMAT "x(40)"
        INDEX prgTitle IS PRIMARY
              prgTitle
        .

DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD prgmName AS CHARACTER LABEL "Name"      FORMAT "x(12)"
    FIELD prgTitle AS CHARACTER LABEL "Item Name" FORMAT "x(40)"
        INDEX prgTitle IS PRIMARY
              prgTitle
        .

DEFINE TEMP-TABLE ttblItem NO-UNDO 
    FIELD menuOrder AS INTEGER 
    FIELD menu1     AS CHARACTER FORMAT "x(12)"
    FIELD menu2     AS CHARACTER 
    FIELD seq       AS INTEGER 
    FIELD mneumonic AS CHARACTER
    FIELD adm       AS LOGICAL
    FIELD mainMenu  AS LOGICAL
        INDEX ttblItems IS PRIMARY UNIQUE menuOrder menu2
        INDEX menu2 menu2 menuOrder
        .
    
DEFINE TEMP-TABLE ttblMenu NO-UNDO 
    FIELD menuName  AS CHARACTER 
    FIELD menuCount AS INTEGER 
    FIELD menuGuid  AS CHARACTER 
    FIELD mneumonic AS CHARACTER 
        INDEX ttblMenu IS PRIMARY UNIQUE menuName
        .

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME ttItem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttItem ttMenu ttUserMenu

/* Definitions for BROWSE ttItem                                        */
&Scoped-define FIELDS-IN-QUERY-ttItem ttItem.prgTitle   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttItem   
&Scoped-define SELF-NAME ttItem
&Scoped-define QUERY-STRING-ttItem FOR EACH ttItem
&Scoped-define OPEN-QUERY-ttItem OPEN QUERY {&SELF-NAME} FOR EACH ttItem.
&Scoped-define TABLES-IN-QUERY-ttItem ttItem
&Scoped-define FIRST-TABLE-IN-QUERY-ttItem ttItem


/* Definitions for BROWSE ttMenu                                        */
&Scoped-define FIELDS-IN-QUERY-ttMenu ttMenu.prgTitle   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttMenu   
&Scoped-define SELF-NAME ttMenu
&Scoped-define QUERY-STRING-ttMenu FOR EACH ttMenu
&Scoped-define OPEN-QUERY-ttMenu OPEN QUERY {&SELF-NAME} FOR EACH ttMenu.
&Scoped-define TABLES-IN-QUERY-ttMenu ttMenu
&Scoped-define FIRST-TABLE-IN-QUERY-ttMenu ttMenu


/* Definitions for BROWSE ttUserMenu                                    */
&Scoped-define FIELDS-IN-QUERY-ttUserMenu ttUserMenu.menuOrder ttUserMenu.levelType[1] NO-LABEL ttUserMenu.level[1] ttUserMenu.levelType[2] NO-LABEL ttUserMenu.level[2] ttUserMenu.levelType[3] NO-LABEL ttUserMenu.level[3]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttUserMenu   
&Scoped-define SELF-NAME ttUserMenu
&Scoped-define QUERY-STRING-ttUserMenu FOR EACH ttUserMenu
&Scoped-define OPEN-QUERY-ttUserMenu OPEN QUERY {&SELF-NAME} FOR EACH ttUserMenu.
&Scoped-define TABLES-IN-QUERY-ttUserMenu ttUserMenu
&Scoped-define FIRST-TABLE-IN-QUERY-ttUserMenu ttUserMenu


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-ttItem}~
    ~{&OPEN-QUERY-ttMenu}~
    ~{&OPEN-QUERY-ttUserMenu}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS userName btnReset showOption filterMenu ~
btnLoadMenu loadMenu filterItem ttMenu ttUserMenu ttItem btnAddItem ~
btnAddMenu btnDefault btnExit btnMoveDown btnMoveUp btnRemove btnSave ~
btnShiftLeft btnShiftRight 
&Scoped-Define DISPLAYED-OBJECTS userName showOption filterMenu loadMenu ~
filterItem 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddItem 
     IMAGE-UP FILE "Graphics/32x32/add.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Add Item"
     FONT 4.

DEFINE BUTTON btnAddMenu 
     IMAGE-UP FILE "Graphics/32x32/add.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Add Menu"
     FONT 4.

DEFINE BUTTON btnDefault 
     IMAGE-UP FILE "Graphics/32x32/drop_down_list.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "System Default"
     FONT 4.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnLoadMenu 
     LABEL "Load Menu" 
     SIZE 14 BY 1.

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/32x32/nav_down.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Move Down"
     FONT 4.

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/32x32/nav_up.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Move Up"
     FONT 4.

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/32x32/error.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Remove"
     FONT 4.

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/nav_refresh.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Reset"
     FONT 4.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Save"
     FONT 4.

DEFINE BUTTON btnShiftLeft 
     IMAGE-UP FILE "Graphics/32x32/nav_left.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Shift Left"
     FONT 4.

DEFINE BUTTON btnShiftRight 
     IMAGE-UP FILE "Graphics/32x32/nav_right.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Shift Right"
     FONT 4.

DEFINE VARIABLE showOption AS CHARACTER FORMAT "X(256)":U INITIAL "Show Only Unused" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Show Only Unused","Show All","Show All Menus","Show All Items" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "Show Option" NO-UNDO.

DEFINE VARIABLE userName AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 18 BY 1 TOOLTIP "Select User"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE filterItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filter" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE filterMenu AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filter" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE loadMenu AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Base Only", "",
"Addon Only", "_addon",
"Combined", "_plus"
     SIZE 52 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttItem FOR 
      ttItem SCROLLING.

DEFINE QUERY ttMenu FOR 
      ttMenu SCROLLING.

DEFINE QUERY ttUserMenu FOR 
      ttUserMenu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttItem C-Win _FREEFORM
  QUERY ttItem DISPLAY
      ttItem.prgTitle
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 18.81
         TITLE "<-- Double Click to Add Item <--".

DEFINE BROWSE ttMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttMenu C-Win _FREEFORM
  QUERY ttMenu DISPLAY
      ttMenu.prgTitle
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 18.81
         TITLE "--> Double Click to Add Menu -->".

DEFINE BROWSE ttUserMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttUserMenu C-Win _FREEFORM
  QUERY ttUserMenu DISPLAY
      ttUserMenu.menuOrder
    ttUserMenu.levelType[1] NO-LABEL
    ttUserMenu.level[1] LABEL "[Level 1]: Menu"
    ttUserMenu.levelType[2] NO-LABEL
    ttUserMenu.level[2] LABEL "[Level 2]: Menu / Item"
    ttUserMenu.levelType[3] NO-LABEL
    ttUserMenu.level[3] LABEL "[Level 3]: Item"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 128 BY 18.81
         TITLE "Double Click to Remove Menu / Item".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     userName AT ROW 1.48 COL 63 COLON-ALIGNED HELP
          "Select User Account ID" WIDGET-ID 4
     btnReset AT ROW 1 COL 47 HELP
          "RESET Menu Structure from Last Saved File" WIDGET-ID 8
     showOption AT ROW 1.48 COL 138 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     filterMenu AT ROW 3.14 COL 6 COLON-ALIGNED WIDGET-ID 42
     btnLoadMenu AT ROW 3.14 COL 69 WIDGET-ID 50
     loadMenu AT ROW 3.14 COL 87 NO-LABEL WIDGET-ID 44
     filterItem AT ROW 3.14 COL 181 COLON-ALIGNED WIDGET-ID 40
     ttMenu AT ROW 4.33 COL 2 WIDGET-ID 300
     ttUserMenu AT ROW 4.33 COL 48 WIDGET-ID 200
     ttItem AT ROW 4.33 COL 177 WIDGET-ID 400
     btnAddItem AT ROW 1 COL 194 HELP
          "SAVE Menu Structure" WIDGET-ID 32
     btnAddMenu AT ROW 1 COL 20 HELP
          "SAVE Menu Structure" WIDGET-ID 34
     btnDefault AT ROW 1 COL 1 HELP
          "Load Menu Structure from System DEFAULT Menu List" WIDGET-ID 6
     btnExit AT ROW 1 COL 214 HELP
          "SAVE Menu Structure" WIDGET-ID 20
     btnMoveDown AT ROW 1 COL 96 HELP
          "RESET Menu Structure from Last Saved File" WIDGET-ID 28
     btnMoveUp AT ROW 1 COL 87 HELP
          "RESET Menu Structure from Last Saved File" WIDGET-ID 30
     btnRemove AT ROW 1 COL 107 HELP
          "SAVE Menu Structure" WIDGET-ID 36
     btnSave AT ROW 1 COL 168 HELP
          "SAVE Menu Structure" WIDGET-ID 10
     btnShiftLeft AT ROW 1 COL 118 HELP
          "RESET Menu Structure from Last Saved File" WIDGET-ID 24
     btnShiftRight AT ROW 1 COL 127 HELP
          "RESET Menu Structure from Last Saved File" WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 222.2 BY 22.33 WIDGET-ID 100.


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
         TITLE              = "User Menu"
         HEIGHT             = 22.33
         WIDTH              = 222.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 222.2
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 222.2
         RESIZE             = no
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
/* BROWSE-TAB ttMenu filterItem DEFAULT-FRAME */
/* BROWSE-TAB ttUserMenu ttMenu DEFAULT-FRAME */
/* BROWSE-TAB ttItem ttUserMenu DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttItem
/* Query rebuild information for BROWSE ttItem
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttItem.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttItem */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttMenu
/* Query rebuild information for BROWSE ttMenu
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttMenu.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttMenu */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttUserMenu
/* Query rebuild information for BROWSE ttUserMenu
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUserMenu.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttUserMenu */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Menu */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddItem C-Win
ON CHOOSE OF btnAddItem IN FRAME DEFAULT-FRAME
DO:
    RUN pInsert (ttItem.prgmName,ttItem.prgTitle,"").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddMenu C-Win
ON CHOOSE OF btnAddMenu IN FRAME DEFAULT-FRAME
DO:
    RUN pInsert (ttMenu.prgmName,ttMenu.prgTitle,"[MENU]").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefault C-Win
ON CHOOSE OF btnDefault IN FRAME DEFAULT-FRAME
DO:
  OS-COPY "./menu.lst" VALUE("usermenu/" + userName + "/menu.lst").
  RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoadMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoadMenu C-Win
ON CHOOSE OF btnLoadMenu IN FRAME DEFAULT-FRAME /* Load Menu */
DO:
    RUN pLoadMenu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown C-Win
ON CHOOSE OF btnMoveDown IN FRAME DEFAULT-FRAME
DO:
    RUN pMove (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp C-Win
ON CHOOSE OF btnMoveUp IN FRAME DEFAULT-FRAME
DO:
    RUN pMove (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME
DO:
    RUN pRemove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME
DO:
    RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME
DO:
  RUN pValidate.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  OUTPUT TO VALUE("usermenu/" + userName + "/menu.lst").
  RUN pSave.
  OUTPUT CLOSE.
  OS-COPY VALUE("usermenu/" + userName + "/menu.lst") VALUE("usermenu/" + userName + "/menu.fol").
  OS-COPY VALUE("usermenu/" + userName + "/menu.lst") VALUE("usermenu/" + userName + "/menu.cor") .
  RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShiftLeft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShiftLeft C-Win
ON CHOOSE OF btnShiftLeft IN FRAME DEFAULT-FRAME
DO:
    RUN pShift (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShiftRight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShiftRight C-Win
ON CHOOSE OF btnShiftRight IN FRAME DEFAULT-FRAME
DO:
    RUN pShift (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterItem C-Win
ON ENTRY OF filterItem IN FRAME DEFAULT-FRAME /* Filter */
DO:
    {&SELF-NAME}:SET-SELECTION(1,256).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterItem C-Win
ON LEAVE OF filterItem IN FRAME DEFAULT-FRAME /* Filter */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pGetPrgrms.
    {&SELF-NAME}:SET-SELECTION(1,256).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterItem C-Win
ON RETURN OF filterItem IN FRAME DEFAULT-FRAME /* Filter */
DO:
  APPLY "LEAVE":U TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterMenu C-Win
ON ENTRY OF filterMenu IN FRAME DEFAULT-FRAME /* Filter */
DO:
    {&SELF-NAME}:SET-SELECTION(1,256).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterMenu C-Win
ON LEAVE OF filterMenu IN FRAME DEFAULT-FRAME /* Filter */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pGetPrgrms.
    {&SELF-NAME}:SET-SELECTION(1,256).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterMenu C-Win
ON RETURN OF filterMenu IN FRAME DEFAULT-FRAME /* Filter */
DO:
  APPLY "LEAVE":U TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadMenu C-Win
ON VALUE-CHANGED OF loadMenu IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME showOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showOption C-Win
ON VALUE-CHANGED OF showOption IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    RUN pGetPrgrms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttItem
&Scoped-define SELF-NAME ttItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttItem C-Win
ON DEFAULT-ACTION OF ttItem IN FRAME DEFAULT-FRAME /* <-- Double Click to Add Item <-- */
DO:
    APPLY "CHOOSE":U TO btnAddItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttMenu
&Scoped-define SELF-NAME ttMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttMenu C-Win
ON DEFAULT-ACTION OF ttMenu IN FRAME DEFAULT-FRAME /* --> Double Click to Add Menu --> */
DO:
    APPLY "CHOOSE":U TO btnAddMenu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttUserMenu
&Scoped-define SELF-NAME ttUserMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttUserMenu C-Win
ON DEFAULT-ACTION OF ttUserMenu IN FRAME DEFAULT-FRAME /* Double Click to Remove Menu / Item */
DO:
    RUN pRemove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME userName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL userName C-Win
ON VALUE-CHANGED OF userName IN FRAME DEFAULT-FRAME /* User */
DO:
  ASSIGN {&SELF-NAME}.
  APPLY 'CHOOSE' TO btnReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttItem
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
  RUN pReSize.
  RUN pGetUsers.
  RUN enable_UI.
  RUN pReset.
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
  DISPLAY userName showOption filterMenu loadMenu filterItem 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE userName btnReset showOption filterMenu btnLoadMenu loadMenu 
         filterItem ttMenu ttUserMenu ttItem btnAddItem btnAddMenu btnDefault 
         btnExit btnMoveDown btnMoveUp btnRemove btnSave btnShiftLeft 
         btnShiftRight 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMenu C-Win 
PROCEDURE pGetMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPrgrm     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMenu      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLength    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMneumonic AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lMainMenu  AS LOGICAL   NO-UNDO.

    EMPTY TEMP-TABLE ttUserMenu.
    EMPTY TEMP-TABLE ttblMenu.
    EMPTY TEMP-TABLE ttblItem.

    REPEAT:
        IMPORT cPrgrm cMenu.
        
        IF CAN-DO ("mainmenu,file",cPrgrm) THEN NEXT.
        IF CAN-DO ("rule,skip",cPrgrm) THEN NEXT.

        lMainMenu = cMenu EQ "mainmenu".
        IF lMainMenu THEN cMenu = "file".
        
        FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ cPrgrm NO-ERROR.
        ASSIGN cMneumonic = SUBSTRING (prgrms.prgtitle,1,1) WHEN AVAILABLE prgrms.

        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cMenu NO-ERROR.
        ASSIGN cMneumonic = ttblMenu.mneumonic + cMneumonic WHEN AVAILABLE ttblMenu.

        IF INDEX (cPrgrm,".") EQ 0 THEN DO:
            FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cPrgrm NO-ERROR.
            IF NOT AVAILABLE ttblMenu THEN DO:         
                CREATE ttblMenu.
                ASSIGN
                    ttblMenu.menuName  = cPrgrm
                    ttblMenu.mneumonic = cMneumonic 
                   .
            END.
        END.
    
        IF NOT AVAILABLE ttblMenu THEN NEXT.
        
        IF NOT CAN-FIND (FIRST prgrms WHERE prgrms.prgmname EQ cPrgrm) THEN NEXT.

        IF LENGTH (cMneumonic) EQ 3 THEN
        ASSIGN cMneumonic = SUBSTRING (cMneumonic,1,2) + STRING (ttblMenu.menuCount).  

        CREATE ttblItem.
        ASSIGN 
            idx                = idx + 1
            ttblMenu.menuCount = ttblMenu.menuCount + 1
            ttblItem.menuOrder = idx
            ttblItem.menu1     = cPrgrm
            ttblItem.menu2     = cMenu
            ttblItem.mneumonic = cMneumonic
            ttblItem.mainMenu  = lMainMenu
            .
            
    END. /* repeat */
    
    FOR EACH ttblItem USE-INDEX menu2 BREAK BY ttblItem.menu2 :
        IF FIRST-OF (ttblItem.menu2) THEN ASSIGN idx = 0.
        ASSIGN
            idx          = idx + 1
            ttblItem.seq = idx
           .

        FIND FIRST prgrms NO-LOCK
             WHERE prgrms.prgmname EQ ttblItem.menu1
             NO-ERROR.
        IF NOT AVAILABLE prgrms THEN NEXT.
        
        CREATE ttUserMenu.
        ASSIGN
            ttUserMenu.menuOrder = ttblItem.menuOrder
            ttUserMenu.prgmName  = ttblItem.menu1
            ttUserMenu.mainMenu  = ttblItem.mainMenu
            ttUserMenu.isMenu    = INDEX(prgrms.prgmname,".") EQ 0
            iLength              = LENGTH(ttblItem.mneumonic)
           .
        IF iLength GT 3 THEN iLength = 3.
        ASSIGN
            ttUserMenu.level[iLength]     = prgrms.prgtitle
            ttUserMenu.levelType[iLength] = IF ttUserMenu.isMenu THEN "[MENU]"
                                            ELSE "[" + ttblItem.mneumonic + "]"
           .
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPrgrms C-Win 
PROCEDURE pGetPrgrms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lUsed AS LOGICAL NO-UNDO.

    EMPTY TEMP-TABLE ttMenu.
    EMPTY TEMP-TABLE ttItem.

    FOR EACH prgrms NO-LOCK
        WHERE prgrms.prgTitle  GT ""
          AND prgrms.menu_item EQ TRUE
        :
        lUsed = CAN-FIND(FIRST ttUserMenu WHERE ttUserMenu.prgmName EQ prgrms.prgmName).
        IF showOption EQ "Show Only Unused" AND lUsed THEN NEXT.
        IF INDEX(prgrms.prgmName,".") EQ 0 THEN DO:
            IF showOption EQ "Show All Items" AND lUsed THEN NEXT.
            IF filterMenu NE "" AND NOT prgrms.prgTitle MATCHES("*" + filterMenu + "*") THEN NEXT.
            CREATE ttMenu.
            ASSIGN
                ttMenu.prgmName = prgrms.prgmName
                ttMenu.prgTitle = prgrms.prgTitle
                .
        END. /* menu */
        ELSE DO:
            IF showOption EQ "Show All Menus" AND lUsed THEN NEXT.
            IF filterItem NE "" AND NOT prgrms.prgTitle MATCHES("*" + filterItem + "*") THEN NEXT.
            CREATE ttItem.
            ASSIGN
                ttItem.prgmName = prgrms.prgmName
                ttItem.prgTitle = prgrms.prgTitle
                .
        END. /* item */
    END. /* each prgrms */

    {&OPEN-QUERY-ttMenu}
    {&OPEN-QUERY-ttItem}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUsers C-Win 
PROCEDURE pGetUsers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    userName:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
    FOR EACH users NO-LOCK:
        userName:ADD-LAST(users.user_id).
    END. /* each users */
    ASSIGN
        userName:SCREEN-VALUE = USERID("NoSweat")
        userName.
    IF NOT CAN-DO("ASI,NoSweat",USERID("NoSweat")) THEN
    DISABLE userName WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInsert C-Win 
PROCEDURE pInsert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgTitle AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dMenuOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.

    dMenuOrder = ttUserMenu.menuOrder + 1.
    DO idx = 1 TO EXTENT(ttUserMenu.level):
        IF ttUserMenu.level[idx] NE "" THEN LEAVE.
    END. /* do idx */
    FOR EACH ttUserMenu
        WHERE ttUserMenu.menuOrder GE dMenuOrder
          AND ttUserMenu.updated   EQ FALSE
        :
        ASSIGN
            ttUserMenu.menuOrder = ttUserMenu.menuOrder + 1
            ttUserMenu.updated   = TRUE
            .
    END. /* each ttusermenu */
    IF ipcType EQ "[MENU]" AND idx EQ 3 THEN idx = 2.
    CREATE ttUserMenu.
    ASSIGN
        ttUserMenu.menuOrder      = dMenuOrder
        ttUserMenu.prgmName       = ipcPrgmName
        ttUserMenu.level[idx]     = ipcPrgTitle
        ttUserMenu.levelType[idx] = ipcType
        ttUserMenu.isMenu         = ipcType EQ "[MENU]"
        rRowID                    = ROWID(ttUserMenu)
        .
    FOR EACH ttUserMenu:
        ttUserMenu.updated = FALSE.
    END. /* each ttusermenu */
    {&OPEN-QUERY-ttUserMenu}
    REPOSITION ttUserMenu TO ROWID(rRowID).
    RUN pGetPrgrms.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadMenu C-Win
PROCEDURE pLoadMenu:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        sourceMenu = SEARCH("stdMenu/menu" + loadMenu + ".lst").
        IF sourceMenu EQ ? THEN RETURN.
        INPUT FROM VALUE(sourceMenu) NO-ECHO.
        RUN pGetMenu.
        INPUT CLOSE.
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
        RUN pGetPrgrms.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMove C-Win 
PROCEDURE pMove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.

    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

    IF NOT AVAILABLE ttUserMenu THEN RETURN.
    /* already at top */
    IF ipiMove EQ -1 AND ttUserMenu.menuOrder EQ 1 THEN RETURN.
    IF ipiMove EQ 1 THEN DO:
        FIND LAST bttUserMenu NO-ERROR.
        IF NOT AVAILABLE bttUserMenu THEN RETURN.
        /* already at bottom */
        IF bttUserMenu.menuOrder EQ ttUserMenu.menuOrder THEN RETURN.
    END. /* eq 1 */
    FIND FIRST bttUserMenu
         WHERE bttUserMenu.menuOrder EQ ttUserMenu.menuOrder + ipiMove
         NO-ERROR.
    IF NOT AVAILABLE bttUserMenu THEN RETURN.
    ASSIGN
        bttUserMenu.menuOrder = ttUserMenu.menuOrder
        ttUserMenu.menuOrder  = ttUserMenu.menuOrder + ipiMove
        rRowID                = ROWID(ttUserMenu)
        .
    {&OPEN-QUERY-ttUserMenu}
    REPOSITION ttUserMenu TO ROWID(rRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemove C-Win 
PROCEDURE pRemove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dMenuOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.

    dMenuOrder = ttUserMenu.menuOrder - 1.
    DELETE ttUserMenu.
    FOR EACH ttUserMenu
        WHERE ttUserMenu.menuOrder GT dMenuOrder
          AND ttUserMenu.updated   EQ FALSE
        :
        ASSIGN
            ttUserMenu.menuOrder = ttUserMenu.menuOrder - 1
            ttUserMenu.updated   = TRUE
            .
    END. /* each ttusermenu */
    FOR EACH ttUserMenu:
        ttUserMenu.updated = FALSE.
    END. /* each ttusermenu */
    IF dMenuOrder EQ 0 THEN dMenuOrder = 1.
    {&OPEN-QUERY-ttUserMenu}
    FIND FIRST ttUserMenu WHERE ttUserMenu.menuOrder EQ dMenuOrder NO-ERROR.
    IF NOT AVAILABLE ttUserMenu THEN RETURN.
    rRowID = ROWID(ttUserMenu).
    REPOSITION ttUserMenu TO ROWID(rRowID).
    RUN pGetPrgrms.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReset C-Win 
PROCEDURE pReset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        sourceMenu = SEARCH("usermenu/" + userName + "/menu.lst").
        IF sourceMenu EQ ? THEN sourceMenu = SEARCH("menu.lst").
        INPUT FROM VALUE(sourceMenu) NO-ECHO.
        RUN pGetMenu.
        INPUT CLOSE.
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
        RUN pGetPrgrms.
    END.

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
    DEFINE VARIABLE iDiff AS INTEGER NO-UNDO.

    ASSIGN
        {&WINDOW-NAME}:WINDOW-STATE = 1
        {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 50
        {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        iDiff = {&WINDOW-NAME}:HEIGHT-PIXELS - FRAME {&FRAME-NAME}:HEIGHT-PIXELS
        FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        ttUserMenu:HEIGHT-PIXELS = ttUserMenu:HEIGHT-PIXELS + iDiff
        ttMenu:HEIGHT-PIXELS = ttMenu:HEIGHT-PIXELS + iDiff
        ttItem:HEIGHT-PIXELS = ttItem:HEIGHT-PIXELS + iDiff
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave C-Win 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLevel AS CHARACTER NO-UNDO EXTENT 3.
    DEFINE VARIABLE cMenu  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER   NO-UNDO.

    PUT UNFORMATTED
        "mainmenu mainmenu" SKIP
        "file mainmenu" SKIP
        .
    cLevel[1] = "file".
    FOR EACH ttUserMenu:
        DO idx = 1 TO EXTENT(cLevel):
            IF ttUserMenu.level[idx] EQ "" THEN NEXT.
            PUT UNFORMATTED
                ttUserMenu.prgmName " "
               (IF ttUserMenu.mainMenu THEN "mainmenu" ELSE cLevel[idx])
                SKIP
                .
            IF ttUserMenu.isMenu THEN
            cLevel[idx + 1] = ttUserMenu.prgmName.
        END. /* do idx */
    END. /* each ttusermenu */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShift C-Win 
PROCEDURE pShift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiShift AS INTEGER NO-UNDO.

    DEFINE VARIABLE iShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.

    DO idx = 1 TO EXTENT(ttUserMenu.level):
        IF ttUserMenu.level[idx] NE "" THEN LEAVE.
    END. /* do idx */
    iShift = idx + ipiShift.
    IF iShift EQ 0 OR iShift EQ 4 THEN RETURN.
    IF iShift EQ 3 AND ttUserMenu.isMenu THEN RETURN.
    ASSIGN
        ttUserMenu.level[iShift]     = ttUserMenu.level[idx]
        ttUserMenu.levelType[iShift] = ttUserMenu.levelType[idx]
        ttUserMenu.level[idx]        = ""
        ttUserMenu.levelType[idx]    = ""
        .
    BROWSE ttUserMenu:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidate C-Win 
PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

