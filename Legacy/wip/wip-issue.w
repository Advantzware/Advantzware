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

  File: wip-issue.w

  Description: Issues a Work In Process tag for an item

  Input Parameters:
    ipcCompany     :Company code
    ipcLocation    :Location code
    ipcJobno       :Primary Job number
    ipiJobno2      :Second Job number
    ipiFormno      :Form number of the Job
    ipiBlankno     :Blank number of the Job

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
DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobno    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJobno2   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiFormno   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiBlankno  AS INTEGER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs        AS         HANDLE    NO-UNDO.
DEFINE VARIABLE hInventoryQuery         AS         HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated                AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage                AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems        AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems        AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems       AS         CHARACTER NO-UNDO.
DEFINE VARIABLE lSwitchJob              AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE lMoveToOnhand           AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE iTotTags                AS         INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand              AS         INTEGER   NO-UNDO.

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{wip/keyboardDefs.i}

DEFINE TEMP-TABLE ttBrowseInventory
    LIKE ttInventoryStockLoadtagWIP.

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
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-exit btnKeyboard-2 btnNumPad btnKeyboard ~
RECT-26 RECT-29 bt-change ls-jobno cb-jobno2 cb-formno cb-blankno btnFirst ~
ls-tag br-table btnLast btnNext btnPrevious 
&Scoped-Define DISPLAYED-OBJECTS ls-jobno cb-jobno2 ls-order ls-cust ~
cb-formno cb-blankno ls-item ls-wipitemid ls-tag ls-lastop ls-message 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-change 
     LABEL "Change" 
     SIZE 24 BY 2.91
     FONT 37.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON bt-post 
     LABEL "Post" 
     SIZE 40 BY 2.38
     FONT 37.

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btnKeyboard 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboard-2 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE VARIABLE cb-blankno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-formno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 10 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-jobno2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-cust AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE ls-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE ls-jobno AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-lastop AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE ls-message AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 139 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-order AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE ls-tag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-wipitemid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
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
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80.2 BY 3.62.

DEFINE RECTANGLE RECT-29
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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 20.05
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-exit AT ROW 1.24 COL 192 WIDGET-ID 84
     btnKeyboard-2 AT ROW 7.19 COL 99 WIDGET-ID 142
     btnNumPad AT ROW 2.67 COL 98 WIDGET-ID 138
     btnKeyboard AT ROW 3.38 COL 32 WIDGET-ID 136
     bt-change AT ROW 1.95 COL 2 WIDGET-ID 8
     ls-jobno AT ROW 1.95 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cb-jobno2 AT ROW 1.95 COL 80.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     ls-order AT ROW 2.67 COL 123.6 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     ls-cust AT ROW 2.67 COL 163.2 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     cb-formno AT ROW 3.38 COL 54.6 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cb-blankno AT ROW 3.52 COL 80.8 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     ls-item AT ROW 3.86 COL 123.6 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     btnFirst AT ROW 10.76 COL 192 WIDGET-ID 128
     ls-wipitemid AT ROW 6.95 COL 133.8 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     ls-tag AT ROW 7.19 COL 16.6 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     ls-lastop AT ROW 8.38 COL 134 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     br-table AT ROW 10.52 COL 2 WIDGET-ID 200
     bt-post AT ROW 31.05 COL 151 WIDGET-ID 38
     btnLast AT ROW 27.76 COL 192 WIDGET-ID 130
     ls-message AT ROW 31.24 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     btnNext AT ROW 23.38 COL 192.2 WIDGET-ID 132
     btnPrevious AT ROW 14.81 COL 192.2 WIDGET-ID 134
     "Cust #:" VIEW-AS TEXT
          SIZE 11.6 BY .81 AT ROW 2.71 COL 153 WIDGET-ID 66
          FONT 34
     "WIP ID:" VIEW-AS TEXT
          SIZE 11.6 BY .81 AT ROW 6.95 COL 124 WIDGET-ID 74
          FONT 34
     "Tag:" VIEW-AS TEXT
          SIZE 8.2 BY 1.19 AT ROW 7.29 COL 10 WIDGET-ID 22
          FONT 36
     "Last Operation:" VIEW-AS TEXT
          SIZE 21.6 BY .81 AT ROW 8.38 COL 114 WIDGET-ID 78
          FONT 34
     "Order #:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 2.71 COL 112 WIDGET-ID 64
          FONT 34
     "Job #:" VIEW-AS TEXT
          SIZE 11 BY .95 AT ROW 2.14 COL 30 WIDGET-ID 12
          FONT 36
     "Job Details" VIEW-AS TEXT
          SIZE 18.4 BY .62 AT ROW 1.67 COL 113.6 WIDGET-ID 16
          FONT 35
     "Blank #:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 3.67 COL 68 WIDGET-ID 58
          FONT 36
     "Form #:" VIEW-AS TEXT
          SIZE 14.6 BY .95 AT ROW 3.52 COL 42 WIDGET-ID 48
          FONT 36
     "Tag Details" VIEW-AS TEXT
          SIZE 19.2 BY .76 AT ROW 5.91 COL 113.8 WIDGET-ID 28
          FONT 35
     "Item #:" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 3.95 COL 113 WIDGET-ID 70
          FONT 34
     RECT-25 AT ROW 1.95 COL 111 WIDGET-ID 14
     RECT-26 AT ROW 5.62 COL 2.2 WIDGET-ID 18
     RECT-27 AT ROW 6.19 COL 111 WIDGET-ID 26
     RECT-29 AT ROW 10.14 COL 2.2 WIDGET-ID 60
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 126
     RECT-2 AT ROW 2.43 COL 97 WIDGET-ID 140
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.86
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Issue WIP"
         HEIGHT             = 32.86
         WIDTH              = 202
         MAX-HEIGHT         = 32.86
         MAX-WIDTH          = 202
         VIRTUAL-HEIGHT     = 32.86
         VIRTUAL-WIDTH      = 202
         MAX-BUTTON         = no
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
/* BROWSE-TAB br-table ls-lastop F-Main */
/* SETTINGS FOR BUTTON bt-post IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-post:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboard:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboard-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN ls-cust IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-lastop IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-message IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-order IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-wipitemid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-25 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-27 IN FRAME F-Main
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
BY ttBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Issue WIP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Issue WIP */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
        
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-change
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-change W-Win
ON CHOOSE OF bt-change IN FRAME F-Main /* Change */
DO:
    RUN enableJobEntry. 
    
    ASSIGN
        ls-tag:SCREEN-VALUE = "".
        
    APPLY "ENTRY" TO ls-jobno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard W-Win
ON CHOOSE OF btnKeyboard IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO ls-jobno.
    RUN pKeyboard (ls-jobno:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard-2 W-Win
ON CHOOSE OF btnKeyboard-2 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO ls-tag.
    RUN pKeyboard (ls-tag:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (SELF).
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


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-blankno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-blankno W-Win
ON VALUE-CHANGED OF cb-blankno IN FRAME F-Main
DO:
    RUN onValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-formno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-formno W-Win
ON VALUE-CHANGED OF cb-formno IN FRAME F-Main
DO:
    RUN onValueChangedOfJobDetails.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-jobno2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-jobno2 W-Win
ON VALUE-CHANGED OF cb-jobno2 IN FRAME F-Main
DO:
    RUN onValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-jobno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON ENTRY OF ls-jobno IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON HELP OF ls-jobno IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (ipcCompany, "job-no", OUTPUT cFieldsValue, OUTPUT cFoundValue, OUTPUT recFoundRecID).
    SELF:SCREEN-VALUE = cFoundValue.
    APPLY "LEAVE":U TO SELF.
    ASSIGN
        cb-jobno2:SCREEN-VALUE  = ENTRY(3,cFieldsValue,"|")
        cb-formno:SCREEN-VALUE  = ENTRY(4,cFieldsValue,"|")
        cb-blankno:SCREEN-VALUE = ENTRY(5,cFieldsValue,"|")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON LEAVE OF ls-jobno IN FRAME F-Main
DO:
    DEFINE VARIABLE riJobMch AS ROWID NO-UNDO.
    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse   AS LOGICAL   NO-UNDO.
    
    IF VALID-HANDLE(hKeyboard) THEN
    DELETE OBJECT hKeyboard.

    ASSIGN 
        ls-order:SCREEN-VALUE = ""
        ls-cust:SCREEN-VALUE  = ""
        ls-item:SCREEN-VALUE  = ""
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListitems = "".            
    
    IF ls-jobno:SCREEN-VALUE = "" THEN
        RETURN.
        
    IF INDEX(SELF:SCREEN-VALUE,"-") NE 0 THEN DO:
        ASSIGN
            cJobNo   = ENTRY(1,SELF:SCREEN-VALUE,"-")
            cJobNo2  = ENTRY(2,SELF:SCREEN-VALUE,"-")
            cJobNo2  = ENTRY(1,cJobNo2,".")
            cFormNo  = ENTRY(2,SELF:SCREEN-VALUE,".")
            cBlankNo = ENTRY(3,SELF:SCREEN-VALUE,".")
            SELF:SCREEN-VALUE = cJobNo
            lParse   = YES
            .
    END. /* if index */

    FOR EACH job-mch NO-LOCK
       WHERE job-mch.job-no   = ls-jobno:SCREEN-VALUE:
          cJobno2ListItems  = IF cJobno2ListItems = "" THEN STRING(job-mch.job-no2,"99")
                              ELSE IF INDEX(cJobno2Listitems,STRING(job-mch.job-no2,"99")) > 0 THEN cJobno2ListItems
                              ELSE cJobno2ListItems + "," + STRING(job-mch.job-no2,"99").
          cFormnoListItems  = IF cFormnoListItems = "" THEN STRING(job-mch.frm,"99")
                              ELSE IF INDEX(cFormnoListitems,STRING(job-mch.frm,"99")) > 0 THEN cFormnoListItems
                              ELSE cFormnoListItems + "," + STRING(job-mch.frm,"99").
          cBlanknoListItems = IF cBlanknoListItems = "" THEN STRING(job-mch.blank-no,"99")
                              ELSE IF INDEX(cBlanknoListitems,STRING(job-mch.blank-no,"99")) > 0 THEN cBlanknoListItems
                              ELSE cBlanknoListItems + "," + STRING(job-mch.blank-no,"99").
    END.
           
    RUN getJobDetails(INPUT ipcCompany,
                      INPUT ls-jobno:SCREEN-VALUE,
                      INPUT cb-jobno2:SCREEN-VALUE,
                      INPUT cb-formno:SCREEN-VALUE,
                      INPUT cb-blankno:SCREEN-VALUE,
                      OUTPUT riJobMch).
    FIND FIRST job-mch WHERE ROWID(job-mch) = riJobMch NO-ERROR.
    
    IF cJobno2ListItems = "" THEN
        ASSIGN 
            cJobno2ListItems = "00"
            cb-jobno2:LIST-ITEMS = cJobno2ListItems 
            cb-jobno2:SCREEN-VALUE = "00".
    ELSE
        cb-jobno2:LIST-ITEMS = cJobno2ListItems.
 
    IF cFormnoListItems = "" THEN
        ASSIGN
            cFormnoListItems = "00"
            cb-formno:LIST-ITEMS = cFormnoListItems 
            cb-formno:SCREEN-VALUE = "00".
    ELSE
        cb-formno:LIST-ITEMS = cFormnoListItems.

    IF cBlanknoListItems = "" THEN
        ASSIGN
            cBlanknoListItems = "00"
            cb-blankno:LIST-ITEMS = cBlanknoListItems
            cb-blankno:SCREEN-VALUE = "00".
    ELSE
        cb-blankno:LIST-ITEMS = cBlanknoListItems.
                   
    IF AVAILABLE job-mch THEN DO:
                   
        ASSIGN 
            cb-jobno2:SCREEN-VALUE  = STRING(job-mch.job-no2)
            cb-formno:SCREEN-VALUE  = STRING(job-mch.frm)
            cb-blankno:SCREEN-VALUE = STRING(job-mch.blank-no).                               
                                         
        RUN updateJobDetails(BUFFER job-mch).

    END.
    ELSE 
        ASSIGN 
            cb-jobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
            cb-formno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
            cb-blankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems).

    RUN rebuildTempTable(INPUT ipcCompany,
                         INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).     
    IF lParse THEN
    ASSIGN
        cb-jobno2:SCREEN-VALUE  = cJobNo2
        cb-formno:SCREEN-VALUE  = cFormNo
        cb-blankno:SCREEN-VALUE = cBlankNo
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tag W-Win
ON ENTRY OF ls-tag IN FRAME F-Main
DO:    
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Qwerty").

    IF ls-jobno:SENSITIVE AND ls-jobno:SCREEN-VALUE <> "" THEN
        RUN onValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tag W-Win
ON LEAVE OF ls-tag IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
    DELETE OBJECT hKeyboard.

    IF SELF:SCREEN-VALUE = "" THEN
        RETURN.
        
    RUN tagScan(INPUT ipcCompany, 
                INPUT SELF:SCREEN-VALUE).  

    SELF:SCREEN-VALUE = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableJobEntry W-Win 
PROCEDURE disableJobEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        ls-jobno:SENSITIVE IN FRAME {&FRAME-NAME}   = FALSE
        cb-jobno2:SENSITIVE IN FRAME {&FRAME-NAME}  = FALSE
        cb-formno:SENSITIVE IN FRAME {&FRAME-NAME}  = FALSE
        cb-blankno:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableJobEntry W-Win 
PROCEDURE enableJobEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        ls-jobno:SENSITIVE IN FRAME {&FRAME-NAME}   = TRUE
        cb-jobno2:SENSITIVE IN FRAME {&FRAME-NAME}  = TRUE
        cb-formno:SENSITIVE IN FRAME {&FRAME-NAME}  = TRUE
        cb-blankno:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
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
  DISPLAY ls-jobno cb-jobno2 ls-order ls-cust cb-formno cb-blankno ls-item 
          ls-wipitemid ls-tag ls-lastop ls-message 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE bt-exit btnKeyboard-2 btnNumPad btnKeyboard RECT-26 RECT-29 bt-change 
         ls-jobno cb-jobno2 cb-formno cb-blankno btnFirst ls-tag br-table 
         btnLast btnNext btnPrevious 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getJobDetails W-Win 
PROCEDURE getJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCOmpany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opriJobMch  AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    FIND FIRST bf-job-mch NO-LOCK
         WHERE bf-job-mch.company  = ipcCompany
           AND bf-job-mch.job-no   = ipcJobno
           AND bf-job-mch.job-no2  = ipiJobno2
           AND bf-job-mch.frm      = ipiFormno
           AND bf-job-mch.blank-no = ipiBlankno NO-ERROR.
    IF AVAILABLE bf-job-mch THEN
        ASSIGN 
            opriJobMch = ROWID(bf-job-mch).
        
    RELEASE bf-job-mch.
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init W-Win 
PROCEDURE init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* ***************************  Main Block  *************************** */
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany NO-ERROR .
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation  .

    IF ipcJobNo NE "" THEN 
        RUN jobScan(INPUT ipcCompany,
                    INPUT ipcJobno,
                    INPUT ipiJobno2,
                    INPUT ipiFormno,
                    INPUT ipiBlankno).
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobScan W-Win 
PROCEDURE jobScan :
/*------------------------------------------------------------------------------
  Purpose: Internal Procedure to be called when the user scans the job     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE riJobMch AS ROWID NO-UNDO.
    
    ASSIGN 
        cJobno2ListItems  = STRING(ipiJobno2,"99")
        cFormnoListItems  = STRING(ipiFormno,"99")
        cBlanknoListitems = STRING(ipiBlankno,"99").            
           
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            ls-order:SCREEN-VALUE   = ""
            ls-cust:SCREEN-VALUE    = ""
            ls-item:SCREEN-VALUE    = ""
            cb-jobno2:LIST-ITEMS    = cJobno2ListItems
            cb-formno:LIST-ITEMS    = cFormnoListItems
            cb-blankno:LIST-ITEMS   = cBlanknoListItems
            cb-jobno2:SCREEN-VALUE  = STRING(ipiJobno2,"99")
            cb-formno:SCREEN-VALUE  = STRING(ipiFormno,"99")
            cb-blankno:SCREEN-VALUE = STRING(ipiBlankno,"99")
            ls-jobno:SCREEN-VALUE   = ipcJobno.
    END.
    
    RUN getJobDetails(INPUT ipcCompany,
                      INPUT ls-jobno:SCREEN-VALUE,
                      INPUT cb-jobno2:SCREEN-VALUE,
                      INPUT cb-formno:SCREEN-VALUE,
                      INPUT cb-blankno:SCREEN-VALUE,
                      OUTPUT riJobMch).
    FIND FIRST job-mch 
         WHERE ROWID(job-mch) = riJobMch NO-ERROR.
    
    IF NOT AVAILABLE job-mch THEN DO: 
        MESSAGE "Invalid Job scan" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO ls-jobno in FRAME {&FRAME-NAME}.
        RETURN ERROR.
    END.    
           
    IF AVAILABLE job-mch THEN DO:
        RUN disableJobEntry.

        RUN updateJobDetails(BUFFER job-mch).

        RUN rebuildTempTable(INPUT ipcCompany,
                             INPUT job-mch.job-no,
                             INPUT job-mch.job-no2,
                             INPUT job-mch.frm,
                             INPUT job-mch.blank-no).
        
        APPLY "ENTRY" TO ls-tag IN FRAME {&FRAME-NAME}.
    END.    
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
  RUN init.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onValueChangedOfJobDetails W-Win 
PROCEDURE onValueChangedOfJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE riJobMch AS ROWID NO-UNDO.
    

    RUN getJobDetails(INPUT ipcCompany,
                      INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                      INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                      INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                      INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                      OUTPUT riJobMch).
    FIND FIRST job-mch 
         WHERE ROWID(job-mch) = riJobMch NO-ERROR.
              
    IF AVAILABLE job-mch THEN DO:
        RUN disableJobEntry.

        RUN updateJobDetails(BUFFER job-mch).
        RUN rebuildTempTable(INPUT ipcCompany,
                             INPUT job-mch.job-no,
                             INPUT job-mch.job-no2,
                             INPUT job-mch.frm,
                             INPUT job-mch.blank-no).
        
        APPLY "ENTRY" TO ls-tag IN FRAME {&FRAME-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebuildTempTable W-Win 
PROCEDURE rebuildTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno AS INTEGER   NO-UNDO.
    
    ASSIGN 
        iTotTags   = 0
        iTotOnHand = 0
        .           
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock NO-LOCK
        WHERE inventoryStock.company EQ ipcCompany
          AND inventoryStock.jobID   EQ ipcJobno
          AND inventoryStock.jobID2  EQ ipiJobno2   
          AND inventoryStock.formNo  EQ ipiFormno   
          AND inventoryStock.blankNo EQ ipiBlankno
          AND inventoryStock.inventoryStatus NE "Created"
        :
         CREATE ttBrowseInventory.
         BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
         ttBrowseinventory.locationID = InventoryStock.warehouseID + " " + InventoryStock.locationID.
         
         ASSIGN
             iTotTags = iTotTags + 1.
         
         IF inventoryStock.inventoryStatus = gcStatusStockReceived THEN
             iTotOnHand = iTotOnHand + 1.
    END.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    ls-message:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "WIP tags for Job: Total: " + STRING(iTotTags) + " Remaining On-Hand: " + STRING(iTotOnHand).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tagScan W-Win 
PROCEDURE tagScan :
/*------------------------------------------------------------------------------
  Purpose: Internal procedure to be called when the user scans the tag  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStockIDAlias  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER buf-inventoryStock FOR inventoryStock.
    
    ls-tag:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ipcStockIDAlias.
    
    FIND FIRST buf-inventoryStock NO-LOCK
         WHERE buf-inventoryStock.company      = ipcCompany
           AND buf-inventoryStock.stockIDAlias = ipcStockIDAlias NO-ERROR.
    IF AVAILABLE buf-InventoryStock THEN DO:
        IF ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
            RUN jobScan(INPUT ipcCompany,
                        INPUT buf-inventoryStock.jobID,
                        INPUT buf-inventoryStock.jobID2,
                        INPUT buf-inventoryStock.formNo,
                        INPUT buf-inventoryStock.blankNo).
        END.
        ELSE IF ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}   <> STRING(buf-inventoryStock.jobID) OR 
                cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  <> STRING(buf-inventoryStock.jobID2,"99") OR
                cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME}  <> STRING(buf-inventoryStock.formNo,"99") OR
                cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> STRING(buf-inventoryStock.blankNo,"99")
            THEN DO:
            MESSAGE "Tag belongs to different Job context. Do you want to switch Job?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                TITLE "Continue?" UPDATE lSwitchJob AS LOGICAL.
            IF lSwitchJob THEN
                RUN jobScan(INPUT ipcCompany,   
                            INPUT buf-inventoryStock.jobID,
                            INPUT buf-inventoryStock.jobID2,
                            INPUT buf-inventoryStock.formNo,
                            INPUT buf-inventoryStock.blankno).
            ELSE
                RETURN.    
        END.
        
        ASSIGN 
            ls-wipitemid:SCREEN-VALUE IN FRAME {&FRAME-NAME} = buf-inventoryStock.wipItemID
            ls-lastop:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = buf-inventoryStock.machineID.
            
        IF buf-inventoryStock.inventoryStatus = gcStatusStockConsumed THEN DO:
            MESSAGE "Tag is already consumed. Do you want to move the tag to On-hand?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                TITLE "Continue?" UPDATE lMoveToOnhand AS LOGICAL.
            IF lMoveToOnhand THEN
                RUN CreateTransactionReceived IN hdInventoryProcs 
                        (INPUT ipcCompany,
                         INPUT buf-inventoryStock.inventoryStockID,
                         INPUT TRUE,
                         OUTPUT lCreated,
                         OUTPUT cMessage).                
            ELSE
                RETURN.
        END.
        ELSE IF buf-inventoryStock.inventoryStatus = gcStatusStockReceived THEN DO:
            RUN CreateTransactionConsume in hdInventoryProcs
                        (INPUT  ipcCompany,
                         INPUT  buf-inventoryStock.inventoryStockID,
                         INPUT  buf-inventoryStock.quantity,
                         INPUT  buf-inventoryStock.quantityUOM,
                         INPUT  TRUE,
                         OUTPUT lCreated,
                         OUTPUT cMessage).
        END.
    END.
    ELSE IF NOT AVAILABLE buf-inventoryStock THEN DO:
        MESSAGE "Invalid tag" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    RUN rebuildTempTable(INPUT ipcCompany,
                         INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).     

    RELEASE buf-inventoryStock.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateJobDetails W-Win 
PROCEDURE updateJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bf-job-mch FOR job-mch.
    
    FIND FIRST job-hdr NO-LOCK 
         WHERE job-hdr.company = bf-job-mch.company
           AND job-hdr.job-no  = bf-job-mch.job-no
           AND job-hdr.job-no2 = bf-job-mch.job-no2
           AND job-hdr.frm     = bf-job-mch.frm NO-ERROR.
    IF AVAIL job-hdr THEN
         ASSIGN 
            ls-order:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(job-hdr.ord-no)
            ls-cust:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = STRING(job-hdr.cust-no)
            ls-item:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = STRING(job-hdr.i-no). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

