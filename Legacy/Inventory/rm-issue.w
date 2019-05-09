&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: rm-issue.w

  Description: Sharp Shooter Raw Material Issue

  Input Parameters:
        ipcCompany  : Company Code
        ipcLocation : Location code
        ipcJobno    : Primary Job Number
        ipiJobno2   : Secondary Job Number
        ipiFormno   : Form Number
        ipiBlankno  : Blank Number
        ipcRMItem   : Raw Material Item ID

  Output Parameters:
      <none>

  History: 
          
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
DEFINE VARIABLE ipcCompany  AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE ipcLocation AS CHARACTER NO-UNDO INITIAL "MAIN".
DEFINE VARIABLE ipcJobno    AS CHARACTER NO-UNDO INITIAL "W13648".
DEFINE VARIABLE ipiJobno2   AS INTEGER   NO-UNDO INITIAL 00.
DEFINE VARIABLE ipiFormno   AS INTEGER   NO-UNDO INITIAL 01.
DEFINE VARIABLE ipiBlankno  AS INTEGER   NO-UNDO INITIAL 01.
DEFINE VARIABLE ipcRMItem   AS CHARACTER NO-UNDO INITIAL "INK".

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs        AS         HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs              AS         HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated                AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage                AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems        AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems        AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems       AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cRMListItems            AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cFormattedJobno         AS         CHARACTER NO-UNDO.
DEFINE VARIABLE lSwitchJob              AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE lMoveToOnhand           AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE iTotTags                AS         INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand              AS         INTEGER   NO-UNDO.
DEFINE VARIABLE iCount                  AS         INTEGER   NO-UNDO.

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{wip/keyboardDefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity ttBrowseInventory.quantityOriginal ttBrowseInventory.locationID ttBrowseInventory.stockIDAlias ttBrowseInventory.jobID ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.inventoryStatus NE "Created"     BY ttBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.inventoryStatus NE "Created"     BY ttBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btJobLookup RECT-27 RECT-28 btChange fiJobNo ~
cbJobNo2 cbFormNo cbBlankNo cbRMItem br-table btKeyboard-1 btExit btFirst ~
btLast btNext btPrevious btnNumPad 
&Scoped-Define DISPLAYED-OBJECTS fiJobNo cbJobNo2 fiOrder fiCust cbFormNo ~
cbBlankNo fiItem cbRMItem fiRMItemID fiTag fiSize fiUOM fiMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btChange 
     LABEL "Change" 
     SIZE 24 BY 2.91
     FONT 37.

DEFINE BUTTON btExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btJobLookup 
     IMAGE-UP FILE "Graphics/32x32/binocular2.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Job Lookup" 
     SIZE 8 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btKeyboard-1 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btKeyboard-2 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPost 
     LABEL "Post" 
     SIZE 40 BY 2.38
     FONT 37.

DEFINE BUTTON btPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE VARIABLE cbBlankNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbFormNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 10 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbRMItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiCust AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 101 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiOrder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRMItemID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiSize AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1
     FONT 35 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 202 BY 32.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80.2 BY 3.33.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80.2 BY 3.33.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-hand"
      ttBrowseInventory.quantityOriginal WIDTH 25 COLUMN-LABEL "Qty Original"      
      ttBrowseInventory.locationID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.stockIDAlias WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)"
      ttBrowseInventory.jobID WIDTH 25 COLUMN-LABEL "Job #" FORMAT "X(20)"
      ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 18.86
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btJobLookup AT ROW 3.43 COL 29.8 WIDGET-ID 156
     btChange AT ROW 1.95 COL 2 WIDGET-ID 8
     fiJobNo AT ROW 1.95 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbJobNo2 AT ROW 1.95 COL 80.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fiOrder AT ROW 2.67 COL 123.6 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     fiCust AT ROW 2.67 COL 163.2 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     cbFormNo AT ROW 3.52 COL 54.6 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cbBlankNo AT ROW 3.52 COL 80.8 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fiItem AT ROW 3.86 COL 123.6 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     cbRMItem AT ROW 5.29 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     fiRMItemID AT ROW 8.29 COL 122.4 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiTag AT ROW 8.52 COL 16.6 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiSize AT ROW 9.71 COL 120.2 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     fiUOM AT ROW 9.71 COL 150.6 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     br-table AT ROW 11.71 COL 2 WIDGET-ID 200
     btPost AT ROW 31.05 COL 151 WIDGET-ID 38
     fiMessage AT ROW 31.24 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     btKeyboard-1 AT ROW 1.91 COL 75 WIDGET-ID 136
     btKeyboard-2 AT ROW 8.43 COL 99 WIDGET-ID 142
     btExit AT ROW 1.95 COL 192 WIDGET-ID 84
     btFirst AT ROW 11.67 COL 192 WIDGET-ID 128
     btLast AT ROW 28.29 COL 192 WIDGET-ID 130
     btNext AT ROW 23.91 COL 192.2 WIDGET-ID 132
     btPrevious AT ROW 15.71 COL 192.2 WIDGET-ID 134
     btnNumPad AT ROW 2.67 COL 98 WIDGET-ID 138
     "Job #:" VIEW-AS TEXT
          SIZE 11 BY .95 AT ROW 2.14 COL 30 WIDGET-ID 12
          FONT 36
     "Blank #:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 3.71 COL 68 WIDGET-ID 58
          FONT 36
     "RM ID:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 8.38 COL 114 WIDGET-ID 74
          FONT 34
     "Item #:" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 3.95 COL 113 WIDGET-ID 70
          FONT 34
     "Job Details" VIEW-AS TEXT
          SIZE 18.4 BY .62 AT ROW 1.67 COL 115.2 WIDGET-ID 16
          FONT 35
     "Cust #:" VIEW-AS TEXT
          SIZE 11.6 BY .81 AT ROW 2.71 COL 153 WIDGET-ID 66
          FONT 34
     "UOM :" VIEW-AS TEXT
          SIZE 8.6 BY .81 AT ROW 9.81 COL 143.4 WIDGET-ID 150
          FONT 34
     "Tag:" VIEW-AS TEXT
          SIZE 8.2 BY 1.19 AT ROW 8.62 COL 10 WIDGET-ID 22
          FONT 36
     "Form #:" VIEW-AS TEXT
          SIZE 14.6 BY .95 AT ROW 3.71 COL 42 WIDGET-ID 48
          FONT 36
     "Order #:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 2.71 COL 112 WIDGET-ID 64
          FONT 34
     "RM Item:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 5.43 COL 28 WIDGET-ID 154
          FONT 36
     "Size :" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 9.76 COL 114 WIDGET-ID 78
          FONT 34
     "Tag Details" VIEW-AS TEXT
          SIZE 19.2 BY .76 AT ROW 7.24 COL 113.8 WIDGET-ID 28
          FONT 35
     RECT-2 AT ROW 2.43 COL 97 WIDGET-ID 140
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 126
     RECT-25 AT ROW 1.95 COL 111 WIDGET-ID 14
     RECT-26 AT ROW 7.62 COL 111 WIDGET-ID 144
     RECT-27 AT ROW 6.95 COL 2.2 WIDGET-ID 18
     RECT-28 AT ROW 11.33 COL 2.2 WIDGET-ID 146
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.86
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Issue RM"
         HEIGHT             = 32.86
         WIDTH              = 202
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 273.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-table fiUOM F-Main */
ASSIGN 
       btJobLookup:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btKeyboard-1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btKeyboard-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btPost IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btPost:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiCust IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOrder IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRMItemID IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSize IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTag IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-25 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-26 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory
    WHERE ttBrowseInventory.inventoryStatus NE "Created"
    BY ttBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Issue RM */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Issue RM */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
        
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE OBJECT hdJobProcs.
        
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChange W-Win
ON CHOOSE OF btChange IN FRAME F-Main /* Change */
DO:
    RUN pEnableJobEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst W-Win
ON CHOOSE OF btFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJobLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJobLookup W-Win
ON CHOOSE OF btJobLookup IN FRAME F-Main /* Job Lookup */
DO:
    APPLY "HELP" TO fiJobno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-1 W-Win
ON CHOOSE OF btKeyboard-1 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiJobNo.
    RUN pKeyboard (
        fiJobNo:HANDLE, 
        "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-2 W-Win
ON CHOOSE OF btKeyboard-2 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTag.
    RUN pKeyboard (
        fiTag:HANDLE, 
        "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast W-Win
ON CHOOSE OF btLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext W-Win
ON CHOOSE OF btNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad W-Win
ON CHOOSE OF btnNumPad IN FRAME F-Main /* NumPad */
DO:
    ASSIGN
        lKeyboard = NOT lKeyboard
        RECT-2:BGCOLOR = IF lKeyboard THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrevious W-Win
ON CHOOSE OF btPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbBlankNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBlankNo W-Win
ON VALUE-CHANGED OF cbBlankNo IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFormNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFormNo W-Win
ON VALUE-CHANGED OF cbFormNo IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbJobNo2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbJobNo2 W-Win
ON VALUE-CHANGED OF cbJobNo2 IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbRMItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbRMItem W-Win
ON VALUE-CHANGED OF cbRMItem IN FRAME F-Main
DO:
    RUN pDisableJobEntry.
    
    RUN pRebuildBrowse (
        ipcCompany,
        cFormattedJobNo,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE),
        cbRMItem:SCREEN-VALUE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON ENTRY OF fiJobNo IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON HELP OF fiJobNo IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        ipcCompany, 
        "job-no",  /* Job No lookup ID */
        OUTPUT cFieldsValue, 
        OUTPUT cFoundValue, 
        OUTPUT recFoundRecID
        ).
    APPLY "LEAVE":U TO SELF.
    
    IF cFoundValue NE "" THEN DO:
        SELF:SCREEN-VALUE      = cFoundValue.
        APPLY "LEAVE" TO SELF.
    END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON LEAVE OF fiJobNo IN FRAME F-Main
DO:
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidJob  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iJobFormat AS INTEGER   NO-UNDO INITIAL 6.
        
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.
    
    ASSIGN 
        fiOrder:SCREEN-VALUE = ""
        fiCust:SCREEN-VALUE  = ""
        fiItem:SCREEN-VALUE  = ""
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListitems = ""
        cMessage          = ""
        .            
    
    RUN JobParser IN hdJobProcs (
        SELF:SCREEN-VALUE,
        OUTPUT cJobNo,
        OUTPUT cJobNo2,
        OUTPUT cFormNo,
        OUTPUT cBlankNo,
        OUTPUT lParse,
        OUTPUT cMessage
        ).

    IF cMessage NE "" THEN DO:
        MESSAGE cMessage
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, SELF:SCREEN-VALUE, 6, TRUE
                      ).                                  

    IF lParse THEN
        ASSIGN
            ipcJobNo        = cJobNo    
            cFormattedJobno = DYNAMIC-FUNCTION (
                              "fAddSpacesToString" IN hdJobProcs, cJobNo, 6, TRUE
                              )
            .
            
    RUN GetSecondaryJobForJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        INPUT-OUTPUT cJobno2ListItems
        ).
    
    DO iCount = 1 TO NUM-ENTRIES(cJobno2ListItems):
        RUN GetFormnoForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cFormnoListItems
            ).
    
        RUN GetBlanknoForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cBlanknoListItems
            ).
    END.

    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobno2ListItems      = "00"
            cbJobno2:LIST-ITEMS   = cJobno2ListItems 
            cbJobno2:SCREEN-VALUE = "00"
            .
    ELSE
        cbJobNo2:LIST-ITEMS = cJobno2ListItems.
 
    IF cFormnoListItems EQ "" THEN
        ASSIGN
            cFormnoListItems      = "00"
            cbFormno:LIST-ITEMS   = cFormnoListItems 
            cbFormno:SCREEN-VALUE = "00"
            .
    ELSE
        cbFormNo:LIST-ITEMS = cFormnoListItems.

    IF cBlanknoListItems EQ "" THEN
        ASSIGN
            cBlanknoListItems       = "00"
            cbBlankNo:LIST-ITEMS   = cBlanknoListItems
            cbBlankNo:SCREEN-VALUE = "00"
            .
    ELSE
        cbBlankNo:LIST-ITEMS = cBlanknoListItems.

    IF lParse THEN
        ASSIGN
            cbJobNo2:SCREEN-VALUE  = IF INDEX(cJobno2ListItems,cJobNo2) GT 0 THEN 
                                         cJobNo2
                                     ELSE
                                         ENTRY(1,cJobno2ListItems)
            cbFormNo:SCREEN-VALUE  = IF INDEX(cFormnoListItems,cFormNo) GT 0 THEN 
                                         cFormNo
                                     ELSE
                                         ENTRY(1,cFormnoListItems)
            cbBlankNo:SCREEN-VALUE = IF INDEX(cBlanknoListitems,cBlankNo) GT 0 THEN 
                                         cBlankNo
                                     ELSE
                                         ENTRY(1,cBlanknoListitems)
            .
                                 
    RUN ValidateJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        "", /* Blank Machine Code */                                 
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).
                 
    IF NOT lValidJob THEN                                                           
        ASSIGN 
            cbJobNo2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
            cbFormNo:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
            cbBlankNo:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
            .
    
    RUN pUpdateJobDetails.
    
    RUN pUpdateRMItemList (
        ipcCompany,
        cFormattedJobno,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main
DO:    
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    RUN pTagScan (
        ipcCompany,
        SELF:SCREEN-VALUE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{wip/pNavigate.i}
{wip/pKeyboard.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fiJobNo cbJobNo2 fiOrder fiCust cbFormNo cbBlankNo fiItem cbRMItem 
          fiRMItemID fiTag fiSize fiUOM fiMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btJobLookup RECT-27 RECT-28 btChange fiJobNo cbJobNo2 cbFormNo 
         cbBlankNo cbRMItem br-table btKeyboard-1 btExit btFirst btLast btNext 
         btPrevious btnNumPad 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableJobEntry W-Win 
PROCEDURE pDisableJobEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiJobNo:SENSITIVE       = FALSE
        cbJobNo2:SENSITIVE      = FALSE
        cbFormNo:SENSITIVE      = FALSE
        cbBlankNo:SENSITIVE     = FALSE
        cbRMItem:SENSITIVE      = FALSE
        btKeyboard-1:SENSITIVE  = FALSE
        btJobLookup:SENSITIVE   = FALSE        
        fiTag:SENSITIVE         = TRUE
        btKeyboard-2:SENSITIVE  = TRUE
        .        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableJobEntry W-Win 
PROCEDURE pEnableJobEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiJobNo:SENSITIVE       = TRUE
        cbJobNo2:SENSITIVE      = TRUE
        cbFormNo:SENSITIVE      = TRUE
        cbBlankNo:SENSITIVE     = TRUE
        cbRMItem:SENSITIVE      = TRUE
        btKeyboard-1:SENSITIVE  = TRUE
        btJobLookup:SENSITIVE   = TRUE
        fiTag:SENSITIVE         = FALSE
        btKeyboard-2:SENSITIVE  = FALSE
        .        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany NO-ERROR .
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation  .

    IF ipcJobNo NE "" THEN 
        RUN pJobScan(
            ipcCompany,
            ipcJobno,
            ipiJobno2,
            ipiFormno,
            ipiBlankno,
            ipcRMItem,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobScan W-Win 
PROCEDURE pJobScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcRMItem   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER  oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER  opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.
    
    ASSIGN 
        cJobno2ListItems  = STRING(ipiJobno2,"99")
        cFormnoListItems  = STRING(ipiFormno,"99")
        cBlanknoListitems = STRING(ipiBlankno,"99")
        .            
    DO WITH FRAME {&FRAME-NAME}:
    END.
           
    ASSIGN 
        fiOrder:SCREEN-VALUE   = ""
        fiCust:SCREEN-VALUE    = ""
        fiItem:SCREEN-VALUE    = ""
        cbJobNo2:LIST-ITEMS    = cJobno2ListItems
        cbFormNo:LIST-ITEMS    = cFormnoListItems
        cbBlankNo:LIST-ITEMS   = cBlanknoListItems
        cbJobNo2:SCREEN-VALUE  = STRING(ipiJobno2,"99")
        cbFormNo:SCREEN-VALUE  = STRING(ipiFormno,"99")
        cbBlankNo:SCREEN-VALUE = STRING(ipiBlankno,"99")
        fiJobNo:SCREEN-VALUE   = ipcJobno
        cFormattedJobNo        = ipcJobno.
                          
    RUN ValidateJob IN hdJobProcs (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT "", /* Blank Machine code */
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).
                      
    IF NOT lValidJob THEN DO: 
        ASSIGN
            opcMessage = "Invalid Job Entry"
            oplSuccess = FALSE
            .
            
        RETURN.
    END.  

    RUN pUpdateJobDetails.
    
    RUN pUpdateRMItemList (
        ipcCompany,
        ipcJobno,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
        
    IF cbRMItem:LOOKUP (ipcRMItem) EQ ? OR
       cbRMItem:LOOKUP (ipcRMItem) EQ 0 THEN DO:
        ASSIGN
            opcMessage = "Invalid Tag RM Item for the existing job"
            oplSuccess = FALSE.

        RUN pEnableJobEntry.
        
        RETURN.
    END. 
    ELSE DO:
        cbRMItem:SCREEN-VALUE = ipcRMItem.  
        APPLY "VALUE-CHANGED" TO cbRMItem.
    END.
        
    ASSIGN
        opcMessage = "Success"
        oplSuccess = TRUE
        .                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOnValueChangedOfJobDetails W-Win 
PROCEDURE pOnValueChangedOfJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN ValidateJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        "", /* Blank Machine Code */
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).
                                    
    IF lValidJob THEN DO:       
    END.
    
    RUN pUpdateJobDetails.
    
    RUN pUpdateRMItemList (
        ipcCompany,
        cFormattedJobno,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildBrowse W-Win 
PROCEDURE pRebuildBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRMItem     AS CHARACTER NO-UNDO.

    RUN RebuildRMBrowse IN hdInventoryProcs (
        ipcCompany,
        ipcJobno,
        "", /* Blank Machine Code */
        ipiJobno2,
        ipiFormno,
        ipiBlankno,
        ipcRMItem,
        OUTPUT iTotTags,
        OUTPUT iTotOnHand
        ).
    
    fiMessage:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "RM tags for Job: Total: " + STRING(iTotTags) + " Remaining On-Hand: " + STRING(iTotOnHand).
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag           AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJobNo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRMItem     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lValidInv   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiTag:SCREEN-VALUE = ipcTag
        cMessage = "".
  
    RUN pGetInventoryStockDetails IN hdInventoryProcs (
        ipcCompany,
        ipcTag,
        OUTPUT lValidInv,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttInventoryStockDetails
        ).
  
    IF lValidInv THEN DO:
        FIND FIRST ttInventoryStockDetails
             WHERE ttInventoryStockDetails.stockIDAlias EQ ipcTag NO-ERROR.
        IF AVAILABLE ttInventoryStockDetails THEN
            ASSIGN
                cJobNo   = ttInventoryStockDetails.jobID
                cRMItem  = ttInventoryStockDetails.rmItemID
                iJobNo2  = ttInventoryStockDetails.jobID2
                iFormNo  = ttInventoryStockDetails.formNo
                iBlankNo = ttInventoryStockDetails.blankNo
                .
  
        IF fiJobno:SCREEN-VALUE   NE cJobNo OR
           cbJobno2:SCREEN-VALUE  NE STRING(iJobNo2,"99") OR
           cbFormno:SCREEN-VALUE  NE STRING(iFormNo,"99") OR
           cbBlankno:SCREEN-VALUE NE STRING(iBlankNo,"99") OR
           cbRMItem:SCREEN-VALUE  NE cRMItem
            THEN DO:
            MESSAGE "Tag belongs to different Job context. Do you want to switch Job?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                TITLE "Continue?" UPDATE lSwitchJob AS LOGICAL.
            IF lSwitchJob THEN DO:
                RUN pJobScan (
                    ipcCompany,
                    cJobNo,
                    iJobNo2,
                    iFormNo,
                    iBlankNo,
                    cRMItem,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
                
                IF NOT lSuccess THEN DO:
                    MESSAGE cMessage 
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN.
                END.                    
            END.
            ELSE
                RETURN.
        END.
  
        cFormattedJobno = cJobNo.
  
        IF AVAILABLE ttInventoryStockDetails THEN DO:
            ASSIGN
                fiRMItemid:SCREEN-VALUE = ttInventoryStockDetails.rmItemID
                fiUOM:SCREEN-VALUE      = ttInventoryStockDetails.quantityUOM
/*                 fiSize:SCREEN-VALUE     = ttInventoryStockDetails.machineID */
                .
  
            IF ttInventoryStockDetails.inventoryStatus EQ gcStatusStockConsumed THEN DO:
                MESSAGE "Tag is already consumed. Do you want to move the tag to On-hand?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                    TITLE "Continue?" UPDATE lMoveToOnhand AS LOGICAL.
                IF lMoveToOnhand THEN
                    RUN CreateTransactionReceived IN hdInventoryProcs (
                        ipcCompany,
                        ttInventoryStockDetails.inventoryStockID,
                        TRUE, /* Post Transaction */
                        OUTPUT lCreated,
                        OUTPUT cMessage
                        ).
                ELSE
                    RETURN.
            END.
            ELSE IF ttInventoryStockDetails.inventoryStatus EQ gcStatusStockReceived THEN DO:
                RUN CreateTransactionConsume in hdInventoryProcs (
                    ipcCompany,
                    ttInventoryStockDetails.inventoryStockID,
                    ttInventoryStockDetails.quantity,
                    ttInventoryStockDetails.quantityUOM,
                    TRUE, /* Post Transaction */
                    OUTPUT lCreated,
                    OUTPUT cMessage
                    ).
            END.
        END.
    END.
    ELSE DO:
        MESSAGE "Invalid tag"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
  
    RUN pRebuildBrowse (
        ipcCompany,
        cFormattedJobNo,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE),
        cbRMItem:SCREEN-VALUE
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateJobDetails W-Win 
PROCEDURE pUpdateJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrdno   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustno  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIno     AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN GetJobHdrDetails IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        OUTPUT iOrdno,
        OUTPUT cCustno,
        OUTPUT cIno).
        
    ASSIGN 
        fiOrder:SCREEN-VALUE = STRING(iOrdno)
        fiCust:SCREEN-VALUE  = STRING(cCustno)
        fiItem:SCREEN-VALUE  = STRING(cIno). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRMItemList W-Win 
PROCEDURE pUpdateRMItemList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
        
    RUN GetRMItemsForJob IN hdJobProcs (
        ipcCompany,
        ipcJobNo,
        ipiJobNo2,
        ipiFormNo,
        ipiBlankNo,
        OUTPUT cRMListItems
        ).
    
    cbRMItem:LIST-ITEMS IN FRAME {&FRAME-NAME} = cRMListitems.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttBrowseInventory"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

